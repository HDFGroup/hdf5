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

/****************/
/* Module Setup */
/****************/

#define H5A_PACKAGE		/*suppress error about including H5Apkg  */
#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes	  			*/
#include "H5Dpkg.h"		/* Datasets 				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MDprivate.h"        /* MDS operations			*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Tprivate.h"		/* Datatypes				*/

#ifdef H5_HAVE_PARALLEL

/* Declare a free list to manage the H5D_t and H5D_shared_t structs */
H5FL_DEFINE_STATIC(H5D_t);

/* Declare the external free list to manage the H5D_chunk_info_t struct */
H5FL_EXTERN(H5D_chunk_info_t);


/*-------------------------------------------------------------------------
 * Function:	H5MD_attr_create
 *
 * Purpose:
 *      This creats a lightweight attribute at the client side. Used now on
 *      metadata clients
 *
 * Return: attribute structure on success, NULL on Failuer
 *
 * Programmer:	Mohamad Chaarawi
 *		October 2012
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5MD_attr_create(const char *name, H5T_t *type, H5S_t *space, hid_t acpl_id)
{
    hssize_t	snelmts;	/* elements in attribute */
    size_t	nelmts;		/* elements in attribute */
    H5A_t	*attr = NULL;   /* Attribute created */
    H5A_t	*ret_value = NULL;   /* Attribute created */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(name);

    /* Build the attribute information */
    if(NULL == (attr = H5FL_CALLOC(H5A_t)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, NULL, "memory allocation failed for attribute info")

    if(NULL == (attr->shared = H5FL_CALLOC(H5A_shared_t)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, NULL, "can't allocate shared attr structure")

    /* If the creation property list is H5P_DEFAULT, use the default character encoding */
    if(acpl_id == H5P_DEFAULT)
        attr->shared->encoding = H5F_DEFAULT_CSET;
    else {
        H5P_genplist_t  *ac_plist;      /* ACPL Property list */

        /* Get a local copy of the attribute creation property list */
        if(NULL == (ac_plist = (H5P_genplist_t *)H5I_object(acpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list")

        if(H5P_get(ac_plist, H5P_STRCRT_CHAR_ENCODING_NAME, &(attr->shared->encoding)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get character encoding flag")
    } /* end else */

    /* Copy the attribute name */
    attr->shared->name = H5MM_xstrdup(name);

    /* Copy datatype */
    if(NULL == (attr->shared->dt = H5T_copy(type, H5T_COPY_ALL)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "can't get shared datatype info")

    /* Copy the dataspace for the attribute */
    attr->shared->ds = H5S_copy(space, FALSE, TRUE);

    /* Check whether datatype is committed & increment ref count
     * (to maintain ref. count incr/decr similarity with "shared message"
     *      type of datatype sharing)
     */
    if(H5T_committed(attr->shared->dt)) {
        /* Increment the reference count on the shared datatype */
        if(H5T_link(attr->shared->dt, 1, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, NULL, "unable to adjust shared datatype link count")
    } /* end if */

    /* Get # of elements for attribute's dataspace */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, NULL, "dataspace is invalid")
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, size_t);

    attr->shared->data_size = nelmts * H5T_GET_SIZE(attr->shared->dt);

    attr->obj_opened = TRUE;

    ret_value = attr;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MD_attr_create() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_attr_close
 *
 * Purpose:	Frees lightweight attribute.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *              October 2012
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_attr_close(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr);
    HDassert(attr->shared);

    /* Free dynamicly allocated items */
    if(H5A_free(attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release attribute info")

    /* Destroy shared attribute struct */
    attr->shared = H5FL_FREE(H5A_shared_t, attr->shared);

    attr->shared = NULL;
    attr = H5FL_FREE(H5A_t, attr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_attr_close() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_dset_create
 *
 * Purpose:	This routine just creates a dataset struct that the client
 *              uses to access raw data in the raw data file.
 *
 * Return:	Success:	Pointer to a new dataset
 *
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *		October 2012
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5MD_dset_create(H5F_t *file, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t UNUSED dapl_id)
{
    const H5T_t         *type, *dt;             /* Datatype for dataset */
    H5D_t		*new_dset = NULL;
    hbool_t             has_vl_type = FALSE;    /* Flag to indicate a VL-type for dataset */
    H5G_loc_t           dset_loc;               /* Dataset location */
    H5D_t		*ret_value;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(file);
    HDassert(H5I_DATATYPE == H5I_get_type(type_id));
    HDassert(H5I_GENPROP_LST == H5I_get_type(dcpl_id));

    /* Get the dataset's datatype */
     if(NULL == (dt = (const H5T_t *)H5I_object(type_id)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a datatype")

    /* Get the actual datatype object if this is a named datatype */
    if(NULL == (type = (const H5T_t *)H5T_get_named_type(dt)))
        type = dt;

    /* Check if the datatype is "sensible" for use in a dataset */
    if(H5T_is_sensible(type) != TRUE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "datatype is not sensible")

    /* Check if the datatype is/contains a VL-type */
    if(H5T_detect_class(type, H5T_VLEN, FALSE))
        has_vl_type = TRUE;

    /* Initialize the dataset object */
    if(NULL == (new_dset = H5FL_CALLOC(H5D_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set up & reset dataset location */
    dset_loc.oloc = &(new_dset->oloc);
    dset_loc.path = &(new_dset->path);
    H5G_loc_reset(&dset_loc);

    /* Initialize the shared dataset space */
    if(NULL == (new_dset->shared = H5D__new(dcpl_id, TRUE, has_vl_type)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy & initialize datatype for dataset */
    if(H5D__init_type(file, new_dset, type_id, type) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't copy datatype")

    if(space_id) {
        const H5S_t         *space = NULL;          /* Dataspace for dataset */

        /* Get the dataset's dataspace */
        if(NULL == (space = (const H5S_t *)H5I_object(space_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a dataspace")

        /* Check if the dataspace has an extent set (or is NULL) */
        if(!H5S_has_extent(space))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "dataspace extent has not been set.")

        /* Copy & initialize dataspace for dataset */
        if(H5D__init_space(file, new_dset, space) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "can't copy dataspace")
    }

    /* Set the dataset's checked_filters flag to enable writing */
    new_dset->shared->checked_filters = TRUE;

    /* Success */
    ret_value = new_dset;

done:
    if(!ret_value && new_dset && new_dset->shared) {
        if(new_dset->shared)
            H5D__dest(new_dset->shared);
        new_dset->oloc.file = NULL;
        new_dset = H5FL_FREE(H5D_t, new_dset);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_dset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_dset_read
 *
 * Purpose:	Reads (part of) a DATASET into application memory
 *		BUF. This uses the lightweight MDS H5D_t struct.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		October, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_dset_read(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
              const H5S_t *file_space, hid_t dxpl_id, void *buf/*out*/)
{
    H5D_chunk_map_t fm;                 /* Chunk file<->memory mapping */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
    H5D_type_info_t type_info;          /* Datatype info for operation */
    hbool_t type_info_init = FALSE;     /* Whether the datatype info has been initialized */
    H5S_t * projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                        /* projection of the supplied mem_space to a new  */
                                        /* data space with rank equal to that of          */
                                        /* file_space.                                    */
                                        /*                                                */
                                        /* This field is only used if                     */
                                        /* H5S_select_shape_same() returns TRUE when      */
                                        /* comparing the mem_space and the data_space,    */
                                        /* and the mem_space have different rank.         */
                                        /*                                                */
                                        /* Note that if this variable is used, the        */
                                        /* projected mem space must be discarded at the   */
                                        /* end of the function to avoid a memory leak.    */
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
    hbool_t     io_op_init = FALSE;     /* Whether the I/O op has been initialized */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(dataset && dataset->oloc.file);

    if(!file_space)
        file_space = dataset->shared->space;
    if(!mem_space)
        mem_space = file_space;
    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dst dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    /* Fill the DXPL cache values for later use */
    if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache")

    /* Set up datatype info for operation */
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, mem_type_id, FALSE, &type_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info")
    type_info_init = TRUE;

    /* Collective access is not permissible without a MPI based VFD */
    if(dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE && 
            !(H5F_HAS_FEATURE(dataset->oloc.file, H5FD_FEAT_HAS_MPI)))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")

    /* Make certain that the number of elements in each selection is the same */
    if(nelmts != (hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if(!(H5S_has_extent(file_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if(!(H5S_has_extent(mem_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* H5S_select_shape_same() has been modified to accept topologically identical
     * selections with different rank as having the same shape (if the most 
     * rapidly changing coordinates match up), but the I/O code still has 
     * difficulties with the notion.
     *
     * To solve this, we check to see if H5S_select_shape_same() returns true, 
     * and if the ranks of the mem and file spaces are different.  If the are, 
     * construct a new mem space that is equivalent to the old mem space, and 
     * use that instead.
     *
     * Note that in general, this requires us to touch up the memory buffer as 
     * well.
     */
    if(TRUE == H5S_select_shape_same(mem_space, file_space) &&
            H5S_GET_EXTENT_NDIMS(mem_space) != H5S_GET_EXTENT_NDIMS(file_space)) {
        void *adj_buf = NULL;   /* Pointer to the location in buf corresponding  */
                                /* to the beginning of the projected mem space.  */

        /* Attempt to construct projected dataspace for memory dataspace */
        if(H5S_select_construct_projection(mem_space, &projected_mem_space,
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), buf, &adj_buf, type_info.dst_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
        HDassert(projected_mem_space);
        HDassert(adj_buf);

        /* Switch to using projected memory dataspace & adjusted buffer */
        mem_space = projected_mem_space;
        buf = adj_buf;
    } /* end if */

    /* Set up I/O operation */
    io_info.op_type = H5D_IO_OP_READ;
    io_info.u.rbuf = buf;
    if(H5D__ioinfo_init(dataset, dxpl_cache, dxpl_id, &type_info, &store, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "unable to set up I/O operation")
    io_info_init = TRUE;

    /* Call storage method's I/O initialization routine */
    HDmemset(&fm, 0, sizeof(H5D_chunk_map_t));

    if(io_info.layout_ops.io_init && 
       (*io_info.layout_ops.io_init)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    io_op_init = TRUE;

    /* Adjust I/O info for any parallel I/O */
    if(H5D__ioinfo_adjust(&io_info, dataset, dxpl_id, file_space, mem_space, &type_info, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")

    /* Invoke correct "high level" I/O routine */
    if((*io_info.io_ops.multi_read)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data")

done:
    /* Shut down the I/O op information */
    if(io_op_init && io_info.layout_ops.io_term && (*io_info.layout_ops.io_term)(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")
    /* Shut down io_info struct */
    if(io_info_init)
        if(H5D__ioinfo_term(&io_info) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")
    /* Shut down datatype info for operation */
    if(type_info_init && H5D__typeinfo_term(&type_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* discard projected mem space if it was created */
    if(NULL != projected_mem_space)
        if(H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_dset_read() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_dset_write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. This uses the lightweight MDS H5D_t struct.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		October 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_dset_write(H5D_t *dataset, hid_t mem_type_id, const H5S_t *mem_space,
               const H5S_t *file_space, hid_t dxpl_id, const void *buf)
{
    H5D_chunk_map_t fm;                 /* Chunk file<->memory mapping */
    H5D_io_info_t io_info;              /* Dataset I/O info     */
    H5D_type_info_t type_info;          /* Datatype info for operation */
    hbool_t type_info_init = FALSE;     /* Whether the datatype info has been initialized */
    H5S_t * projected_mem_space = NULL; /* If not NULL, ptr to dataspace containing a     */
                                        /* projection of the supplied mem_space to a new  */
                                        /* data space with rank equal to that of          */
                                        /* file_space.                                    */
                                        /*                                                */
                                        /* This field is only used if                     */
                                        /* H5S_select_shape_same() returns TRUE when      */
                                        /* comparing the mem_space and the data_space,    */
                                        /* and the mem_space have different rank.         */
                                        /*                                                */
                                        /* Note that if this variable is used, the        */
                                        /* projected mem space must be discarded at the   */
                                        /* end of the function to avoid a memory leak.    */
    H5D_storage_t store;                /*union of EFL and chunk pointer in file space */
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    hbool_t     io_info_init = FALSE;   /* Whether the I/O info has been initialized */
    hbool_t     io_op_init = FALSE;     /* Whether the I/O op has been initialized */
    H5D_dxpl_cache_t _dxpl_cache;       /* Data transfer property cache buffer */
    H5D_dxpl_cache_t *dxpl_cache = &_dxpl_cache;   /* Data transfer property cache */
    H5F_t *file = dataset->oloc.file;
    herr_t	ret_value = SUCCEED;	/* Return value	*/

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(dataset && file);

    /* Check if we are allowed to write to this file */
    if(0 == (H5F_INTENT(file) & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "no write intent on file");

    /* Fill the DXPL cache values for later use */
    if(H5D__get_dxpl_cache(dxpl_id, &dxpl_cache) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't fill dxpl cache");

    /* Set up datatype info for operation */
    if(H5D__typeinfo_init(dataset, dxpl_cache, dxpl_id, mem_type_id, TRUE, &type_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up type info");
    type_info_init = TRUE;

    if(H5T_detect_class(type_info.mem_type, H5T_VLEN, FALSE) > 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing VL datatypes yet");

    /* If MPI based VFD is used, no VL datatype support yet. */
    /* This is because they use the global heap in the file and we don't */
    /* support parallel access of that yet */
    /* We should really use H5T_detect_class() here, but it will be difficult
     * to detect the type of the reference if it is nested... -QAK
     */
    if(H5T_get_class(type_info.mem_type, TRUE) == H5T_REFERENCE &&
       H5T_get_ref_type(type_info.mem_type) == H5R_DATASET_REGION)
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "Parallel IO does not support writing region reference datatypes yet");

    /* Can't write to chunked datasets with filters, in parallel */
    if(dataset->shared->layout.type == H5D_CHUNKED &&
       dataset->shared->dcpl_cache.pline.nused > 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "cannot write to chunked storage with filters in parallel");

    /* Collective access is not permissible without a MPI based VFD */
    if(dxpl_cache->xfer_mode == H5FD_MPIO_COLLECTIVE && 
            !(H5F_HAS_FEATURE(dataset->oloc.file, H5FD_FEAT_HAS_MPI)))
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "collective access for MPI-based drivers only")

    /* Initialize dataspace information */
    if(!file_space)
        file_space = dataset->shared->space;
    if(!mem_space)
        mem_space = file_space;

    /* H5S_select_shape_same() has been modified to accept topologically 
     * identical selections with different rank as having the same shape 
     * (if the most rapidly changing coordinates match up), but the I/O 
     * code still has difficulties with the notion.
     *
     * To solve this, we check to see if H5S_select_shape_same() returns 
     * true, and if the ranks of the mem and file spaces are different.  
     * If the are, construct a new mem space that is equivalent to the 
     * old mem space, and use that instead.
     *
     * Note that in general, this requires us to touch up the memory buffer 
     * as well.
     */
    if(TRUE == H5S_select_shape_same(mem_space, file_space) &&
            H5S_GET_EXTENT_NDIMS(mem_space) != H5S_GET_EXTENT_NDIMS(file_space)) {
        void *adj_buf = NULL;   /* Pointer to the location in buf corresponding  */
                                /* to the beginning of the projected mem space.  */

        /* Attempt to construct projected dataspace for memory dataspace */
        if(H5S_select_construct_projection(mem_space, &projected_mem_space,
                (unsigned)H5S_GET_EXTENT_NDIMS(file_space), buf, &adj_buf, type_info.src_type_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to construct projected memory dataspace")
        HDassert(projected_mem_space);
        HDassert(adj_buf);

        /* Switch to using projected memory dataspace & adjusted buffer */
        mem_space = projected_mem_space;
        buf = adj_buf;
    } /* end if */

    if((snelmts = H5S_GET_SELECT_NPOINTS(mem_space)) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts, snelmts, hssize_t, hsize_t);

    /* Make certain that the number of elements in each selection is the same */
    if(nelmts != (hsize_t)H5S_GET_SELECT_NPOINTS(file_space))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src and dest data spaces have different sizes")

    /* Make sure that both selections have their extents set */
    if(!(H5S_has_extent(file_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file dataspace does not have extent set")
    if(!(H5S_has_extent(mem_space)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "memory dataspace does not have extent set")

    /* Set up I/O operation */
    io_info.op_type = H5D_IO_OP_WRITE;
    io_info.u.wbuf = buf;

    if(H5D__ioinfo_init(dataset, dxpl_cache, dxpl_id, &type_info, &store, &io_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to set up I/O operation")
    io_info_init = TRUE;

    /* Call storage method's I/O initialization routine */
    HDmemset(&fm, 0, sizeof(H5D_chunk_map_t));
    if(io_info.layout_ops.io_init && 
       (*io_info.layout_ops.io_init)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize I/O info")
    io_op_init = TRUE;

    /* Adjust I/O info for any parallel I/O */
    if(H5D__ioinfo_adjust(&io_info, dataset, dxpl_id, file_space, mem_space, &type_info, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to adjust I/O info for parallel I/O")

    /* Invoke correct "high level" I/O routine */
    if((*io_info.io_ops.multi_write)(&io_info, &type_info, nelmts, file_space, mem_space, &fm) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data")

done:
    /* Shut down the I/O op information */
    if(io_op_init && io_info.layout_ops.io_term && (*io_info.layout_ops.io_term)(&fm) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down I/O op info")

    /* Shut down io_info struct */
    if(io_info_init && H5D__ioinfo_term(&io_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't shut down io_info")

    /* Shut down datatype info for operation */
    if(type_info_init && H5D__typeinfo_term(&type_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down type info")

    /* discard projected mem space if it was created */
    if(NULL != projected_mem_space)
        if(H5S_close(projected_mem_space) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to shut down projected memory dataspace")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_dset_write() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_dset_close
 *
 * Purpose:	closes lightweight client dataset
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mohamad Chaarawi
 *		November 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_dset_close(H5D_t *dataset)
{
    unsigned free_failed;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(dataset);

    if(H5D_CHUNKED == dataset->shared->layout.type) {
        /* Check for skip list for iterating over chunks during I/O to close */
        if(dataset->shared->cache.chunk.sel_chunks) {
            HDassert(H5SL_count(dataset->shared->cache.chunk.sel_chunks) == 0);
            H5SL_close(dataset->shared->cache.chunk.sel_chunks);
            dataset->shared->cache.chunk.sel_chunks = NULL;
        } /* end if */

        /* Check for cached single chunk dataspace */
        if(dataset->shared->cache.chunk.single_space) {
            (void)H5S_close(dataset->shared->cache.chunk.single_space);
            dataset->shared->cache.chunk.single_space = NULL;
        } /* end if */

        /* Check for cached single element chunk info */
        if(dataset->shared->cache.chunk.single_chunk_info) {
            dataset->shared->cache.chunk.single_chunk_info = H5FL_FREE(H5D_chunk_info_t, dataset->shared->cache.chunk.single_chunk_info);
            dataset->shared->cache.chunk.single_chunk_info = NULL;
        } /* end if */
    }

    /*
     * Release datatype, dataspace and creation property list -- there isn't
     * much we can do if one of these fails, so we just continue.
     */
    free_failed = (unsigned)(H5I_dec_ref(dataset->shared->type_id) < 0 || 
                             H5S_close(dataset->shared->space) < 0 ||
                             H5I_dec_ref(dataset->shared->dcpl_id) < 0);

    /*
     * Free memory.  Before freeing the memory set the file pointer to NULL.
     * We always check for a null file pointer in other H5D functions to be
     * sure we're not accessing an already freed dataset (see the HDassert()
     * above).
     */
    dataset->oloc.file = NULL;

    if(H5D__dest(dataset->shared) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't free shared dataset struct\n");

    //dataset->shared = H5FL_FREE(H5D_shared_t, dataset->shared);

   /* Release the dataset's path info */
   if(H5G_name_free(&(dataset->path)) < 0)
       free_failed = TRUE;

    /* Free the dataset's memory structure */
    dataset = H5FL_FREE(H5D_t, dataset);

    /* Check if anything failed in the middle... */
    if(free_failed)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't free a component of the dataset, but the dataset was freed anyway.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_dset_close() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_file_open
 *
 * Purpose:	Opens (or creates) a file. This function is the same as H5F_open
 *              except that it opens a raw data file, meaning no metadata is 
 *              read/written in that file (superblock, root group, etc...
 *
 * Return:	Success:	A new file pointer.
 *		Failure:	NULL
 *
 * Programmer:	Mohamad Chaarawi
 *		October 2012
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5MD_file_open(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
    hid_t dxpl_id)
{
    H5F_t              *file = NULL;        /*the success return value      */
    H5F_file_t         *shared = NULL;      /*shared part of `file'         */
    H5FD_t             *lf = NULL;          /*file driver part of `shared'  */
    unsigned            tent_flags;         /*tentative flags               */
    H5FD_class_t       *drvr;               /*file driver class info        */
    H5P_genplist_t     *a_plist;            /*file access property list     */
    H5F_close_degree_t  fc_degree;          /*file close degree             */
    H5F_t              *ret_value;          /*actual return value           */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * If the driver has a `cmp' method then the driver is capable of
     * determining when two file handles refer to the same file and the
     * library can insure that when the application opens a file twice
     * that the two handles coordinate their operations appropriately.
     * Otherwise it is the application's responsibility to never open the
     * same file more than once at a time.
     */
    if(NULL == (drvr = H5FD_get_class(fapl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "unable to retrieve VFL class")

    /*
     * Opening a file is a two step process. First we try to open the
     * file in a way which doesn't affect its state (like not truncating
     * or creating it) so we can compare it with files that are already
     * open. If that fails then we try again with the full set of flags
     * (only if they're different than the original failed attempt).
     * However, if the file driver can't distinquish between files then
     * there's no reason to open the file tentatively because it's the
     * application's responsibility to prevent this situation (there's no
     * way for us to detect it here anyway).
     */
    if(drvr->cmp)
	tent_flags = flags & ~(H5F_ACC_CREAT|H5F_ACC_TRUNC|H5F_ACC_EXCL);
    else
	tent_flags = flags;

    if(NULL == (lf = H5FD_open(name, tent_flags, fapl_id, HADDR_UNDEF))) {
	if(tent_flags == flags) {
#ifndef H5_USING_MEMCHECKER
            time_t mytime = HDtime(NULL);

	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: time = %s, name = '%s', tent_flags = %x", HDctime(&mytime), name, tent_flags)
#else /* H5_USING_MEMCHECKER */
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: name = '%s', tent_flags = %x", name, tent_flags)
#endif /* H5_USING_MEMCHECKER */
        } /* end if */
        H5E_clear_stack(NULL);
	tent_flags = flags;
	if(NULL == (lf = H5FD_open(name, tent_flags, fapl_id, HADDR_UNDEF))) {
#ifndef H5_USING_MEMCHECKER
            time_t mytime = HDtime(NULL);

	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: time = %s, name = '%s', tent_flags = %x", HDctime(&mytime), name, tent_flags)
#else /* H5_USING_MEMCHECKER */
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file: name = '%s', tent_flags = %x", name, tent_flags)
#endif /* H5_USING_MEMCHECKER */
        } /* end if */
    } /* end if */

    /* Is the file already open? */
    if((shared = H5F_sfile_search(lf)) != NULL) {
	/*
	 * The file is already open, so use that one instead of the one we
	 * just opened. We only one one H5FD_t* per file so one doesn't
	 * confuse the other.  But fail if this request was to truncate the
	 * file (since we can't do that while the file is open), or if the
	 * request was to create a non-existent file (since the file already
	 * exists), or if the new request adds write access (since the
	 * readers don't expect the file to change under them).
	 */
	if(H5FD_close(lf) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
	if(flags & H5F_ACC_TRUNC)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to truncate a file which is already open")
	if(flags & H5F_ACC_EXCL)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file exists")
	if((flags & H5F_ACC_RDWR) && 0 == (shared->flags & H5F_ACC_RDWR))
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "file is already open for read-only")

        /* Allocate new "high-level" file struct */
        if((file = H5F_new(shared, fcpl_id, fapl_id, NULL)) == NULL)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create new file object")
    } /* end if */
    else {
        /* Check if tentative open was good enough */
        if(flags != tent_flags) {
            /*
             * This file is not yet open by the library and the flags we used to
             * open it are different than the desired flags. Close the tentative
             * file and open it for real.
             */
            if(H5FD_close(lf) < 0) {
                file = NULL; /*to prevent destruction of wrong file*/
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to close low-level file info")
            } /* end if */
            if(NULL == (lf = H5FD_open(name, flags, fapl_id, HADDR_UNDEF))) {
                file = NULL; /*to prevent destruction of wrong file*/
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
            } /* end if */
        } /* end if */

        if(NULL == (file = H5F_new(NULL, fcpl_id, fapl_id, lf)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to create new file object")
        file->shared->flags = flags;
    } /* end else */

    /* Short cuts */
    shared = file->shared;
    lf = shared->lf;

    /*
     * The intent at the top level file struct are not necessarily the same as
     * the flags at the bottom.	 The top level describes how the file can be
     * accessed through the HDF5 library.  The bottom level describes how the
     * file can be accessed through the C library.
     */
    file->intent = flags;
    file->open_name = H5MM_xstrdup(name);

    /* Get the file access property list, for future queries */
    if(NULL == (a_plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not file access property list")

    /*
     * Decide the file close degree.  If it's the first time to open the
     * file, set the degree to access property list value; if it's the
     * second time or later, verify the access property list value matches
     * the degree in shared file structure.
     */
    if(H5P_get(a_plist, H5F_ACS_CLOSE_DEGREE_NAME, &fc_degree) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get file close degree")

    if(shared->nrefs == 1) {
        if(fc_degree == H5F_CLOSE_DEFAULT)
            shared->fc_degree = lf->cls->fc_degree;
        else
            shared->fc_degree = fc_degree;
    } else if(shared->nrefs > 1) {
        if(fc_degree == H5F_CLOSE_DEFAULT && shared->fc_degree != lf->cls->fc_degree)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "file close degree doesn't match")
        if(fc_degree != H5F_CLOSE_DEFAULT && fc_degree != shared->fc_degree)
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "file close degree doesn't match")
    } /* end if */

    /* Formulate the absolute path for later search of target file for external links */
    if(H5_build_extpath(name, &file->extpath) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to build extpath")

    /* Formulate the actual file name, after following symlinks, etc. */
    if(H5F_build_actual_name(file, a_plist, name, &file->actual_name) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL, "unable to build actual name")

    /* Success */
    ret_value = file;

done:
    if(!ret_value && file)
        if(H5F_dest(file, dxpl_id, FALSE) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "problems closing file")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_file_open() */


/*-------------------------------------------------------------------------
 * Function:    H5MD_file_get_obj_count
 *
 * Purpose:	Private function return the number of opened object IDs
 *		(files, datasets, groups, datatypes) in the same file.
 *
 * Return:      SUCCEED on success, FAIL on failure.
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_file_get_obj_count(const H5F_t *f, unsigned types, hbool_t app_ref, size_t *obj_id_count_ptr)
{
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(obj_id_count_ptr);

    /* Perform the query */
    if((ret_value = H5MD_file_get_objects(f, types, 0, NULL, app_ref, obj_id_count_ptr)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_objects failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_file_get_obj_count() */


/*-------------------------------------------------------------------------
 * Function:    H5MD_file_get_obj_ids
 *
 * Purpose:     Private function to return a list of opened object IDs.
 *
 * Return:      Non-negative on success; can't fail.
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MD_file_get_obj_ids(const H5F_t *f, unsigned types, size_t max_objs, hid_t *oid_list, hbool_t app_ref, size_t *obj_id_count_ptr)
{
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(obj_id_count_ptr);

    /* Perform the query */
    if((ret_value = H5MD_file_get_objects(f, types, max_objs, oid_list, app_ref, obj_id_count_ptr)) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_objects failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_file_get_obj_ids() */


/*---------------------------------------------------------------------------
 * Function:	H5MD_file_get_objects
 *
 * Purpose:	This function is called by H5MD_file_get_obj_count or
 *		H5MD_file_get_obj_ids to get number of object IDs and/or a
 *		list of opened object IDs (in return value).
 * Return:	Non-negative on success; Can't fail.
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5MD_file_get_objects(const H5F_t *f, unsigned types, size_t max_index, hid_t *obj_id_list, hbool_t app_ref, size_t *obj_id_count_ptr)
{
    size_t obj_id_count=0;      /* Number of open IDs */
    H5F_olist_t olist;          /* Structure to hold search results */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(obj_id_count_ptr);

    /* Set up search information */
    olist.obj_id_list  = (max_index==0 ? NULL : obj_id_list);
    olist.obj_id_count = &obj_id_count;
    olist.list_index   = 0;
    olist.max_index   = max_index;

    /* Determine if we are searching for local or global objects */
    if(types & H5F_OBJ_LOCAL) {
        olist.file_info.local = TRUE;
        olist.file_info.ptr.file = f;
    } /* end if */
    else {
        olist.file_info.local = FALSE;
        olist.file_info.ptr.shared = f ? f->shared : NULL;
    } /* end else */

    /* Iterate through file IDs to count the number, and put their
     * IDs on the object list.  */
    if(types & H5F_OBJ_FILE) {
        olist.obj_type = H5I_FILE;
        if(H5I_iterate(H5I_FILE, H5MD_file_get_objects_cb, &olist, app_ref) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(1)")
    } /* end if */

    /* Search through dataset IDs to count number of datasets, and put their
     * IDs on the object list */
    if(types & H5F_OBJ_DATASET) {
        olist.obj_type = H5I_DATASET;
        if(H5I_iterate(H5I_DATASET, H5MD_file_get_objects_cb, &olist, app_ref) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(2)")
    } /* end if */

    /* Search through group IDs to count number of groups, and put their
     * IDs on the object list */
    if(types & H5F_OBJ_GROUP) {
        olist.obj_type = H5I_GROUP;
        if(H5I_iterate(H5I_GROUP, H5MD_file_get_objects_cb, &olist, app_ref) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(3)")
    } /* end if */

    /* Search through datatype IDs to count number of named datatypes, and put their
     * IDs on the object list */
    if(types & H5F_OBJ_DATATYPE) {
        olist.obj_type = H5I_DATATYPE;
        if(H5I_iterate(H5I_DATATYPE, H5MD_file_get_objects_cb, &olist, app_ref) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(4)")
    } /* end if */

    /* Search through attribute IDs to count number of attributes, and put their
     * IDs on the object list */
    if(types & H5F_OBJ_ATTR) {
        olist.obj_type = H5I_ATTR;
        if(H5I_iterate(H5I_ATTR, H5MD_file_get_objects_cb, &olist, app_ref) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADITER, FAIL, "iteration failed(5)")
    } /* end if */

    /* Set the number of objects currently open */
    *obj_id_count_ptr = obj_id_count;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_file_get_objects() */


/*-------------------------------------------------------------------------
 * Function:	H5MD_file_get_objects_cb
 *
 * Purpose:	H5F_get_objects' callback function.  It verifies if an
 * 		object is in the file, and either count it or put its ID
 *		on the list.
 *
 * Return:      TRUE if the array of object IDs is filled up.
 *              FALSE otherwise.
 *
 * Programmer:  Mohamad Chaarawi
 *              November, 2012
 *
 *-------------------------------------------------------------------------
 */
int
H5MD_file_get_objects_cb(void *obj_ptr, hid_t obj_id, void *key)
{
    H5F_olist_t *olist = (H5F_olist_t *)key;    /* Alias for search info */
    H5F_t *f = ((H5MD_object_t *)obj_ptr)->raw_file;
    int ret_value = FALSE;    /* Return value */
    
    FUNC_ENTER_NOAPI_NOINIT

    HDassert(obj_ptr);
    HDassert(olist);

    /* Count file IDs */
    if(olist->obj_type == H5I_FILE) {
        if((olist->file_info.local &&
            (!olist->file_info.ptr.file || (olist->file_info.ptr.file && f == olist->file_info.ptr.file) ))
           ||  (!olist->file_info.local &&
                ( !olist->file_info.ptr.shared || (olist->file_info.ptr.shared && f->shared == olist->file_info.ptr.shared) ))) {
            /* Add the object's ID to the ID list, if appropriate */
            if(olist->obj_id_list) {
                olist->obj_id_list[olist->list_index] = obj_id;
		olist->list_index++;
	    }

            /* Increment the number of open objects */
	    if(olist->obj_id_count)
	    	(*olist->obj_id_count)++;

            /* Check if we've filled up the array.  Return TRUE only if
             * we have filled up the array. Otherwise return FALSE(RET_VALUE is
             * preset to FALSE) because H5I_iterate needs the return value of 
 	     * FALSE to continue the iteration. */
            if(olist->max_index>0 && olist->list_index>=olist->max_index)
                HGOTO_DONE(TRUE)  /* Indicate that the iterator should stop */
	}
    } /* end if */
    else { /* either count opened object IDs or put the IDs on the list */
        if(olist->obj_type == H5I_DATATYPE) {
            H5MD_object_t *dt = NULL;

            if(NULL != (dt = (H5MD_object_t *)H5T_get_named_type((H5T_t *)obj_ptr)))
                f = dt->raw_file;
        }
        if((olist->file_info.local &&
            ( (!olist->file_info.ptr.file && olist->obj_type == H5I_DATATYPE && H5T_is_immutable((H5T_t *)obj_ptr) == FALSE)
              || (!olist->file_info.ptr.file && olist->obj_type != H5I_DATATYPE)
              || (f == olist->file_info.ptr.file)))
           || (!olist->file_info.local &&
               ((!olist->file_info.ptr.shared && olist->obj_type == H5I_DATATYPE && H5T_is_immutable((H5T_t *)obj_ptr) == FALSE)
                || (!olist->file_info.ptr.shared && olist->obj_type != H5I_DATATYPE)
                || (f && f->shared == olist->file_info.ptr.shared)))) {
            /* Add the object's ID to the ID list, if appropriate */
            if(olist->obj_id_list) {
            	olist->obj_id_list[olist->list_index] = obj_id;
		olist->list_index++;
	    } /* end if */

            /* Increment the number of open objects */
	    if(olist->obj_id_count)
            	(*olist->obj_id_count)++;

            /* Check if we've filled up the array.  Return TRUE only if
             * we have filled up the array. Otherwise return FALSE(RET_VALUE is
             * preset to FALSE) because H5I_iterate needs the return value of 
	     * FALSE to continue iterating. */
            if(olist->max_index>0 && olist->list_index>=olist->max_index)
                HGOTO_DONE(TRUE)  /* Indicate that the iterator should stop */
    	} /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MD_file_get_objects_cb() */

#endif /* H5_HAVE_PARALLEL */
