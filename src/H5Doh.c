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

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Opkg.h"             /* Object headers			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/
static void *H5O_dset_get_copy_file_udata(void);
static void H5O_dset_free_copy_file_udata(void *);
static htri_t H5O_dset_isa(H5O_t *loc);
static hid_t H5O_dset_open(const H5G_loc_t *obj_loc, hid_t lapl_id,
    hid_t dxpl_id, hbool_t app_ref);
static void *H5O_dset_create(H5F_t *f, void *_crt_info, H5G_loc_t *obj_loc,
    hid_t dxpl_id);
static H5O_loc_t *H5O_dset_get_oloc(hid_t obj_id);
static herr_t H5O_dset_bh_info(H5F_t *f, hid_t dxpl_id, H5O_t *oh,
    H5_ih_info_t *bh_info);
static htri_t H5O_dset_compare(const H5F_t *f1, const H5F_t *f2,
    const H5O_t *oh1, const H5O_t *oh2, haddr_t addr1, haddr_t addr2,
    hid_t dxpl1_id, hid_t dxpl2_id, H5O_cmp_t *cmp_info);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* This message derives from H5O object class */
const H5O_obj_class_t H5O_OBJ_DATASET[1] = {{
    H5O_TYPE_DATASET,		/* object type			*/
    "dataset",			/* object name, for debugging	*/
    H5O_dset_get_copy_file_udata, /* get 'copy file' user data	*/
    H5O_dset_free_copy_file_udata, /* free 'copy file' user data	*/
    H5O_dset_isa, 		/* "isa" message		*/
    H5O_dset_open, 		/* open an object of this class */
    H5O_dset_create, 		/* create an object of this class */
    H5O_dset_get_oloc, 		/* get an object header location for an object */
    H5O_dset_bh_info,		/* get the index & heap info for an object */
    H5O_dset_compare            /* compare two datasets */
}};

/* Declare a free list to manage the H5D_copy_file_ud_t struct */
H5FL_DEFINE(H5D_copy_file_ud_t);


/*-------------------------------------------------------------------------
 * Function:	H5O_dset_get_copy_file_udata
 *
 * Purpose:	Allocates the user data needed for copying a dataset's
 *		object header from file to file.
 *
 * Return:	Success:	Non-NULL pointer to user data
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 21, 2005
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_dset_get_copy_file_udata(void)
{
    void *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_dset_get_copy_file_udata)

    /* Allocate space for the 'copy file' user data for copying datasets */
    if(NULL == (ret_value = H5FL_CALLOC(H5D_copy_file_ud_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_get_copy_file_udata() */


/*-------------------------------------------------------------------------
 * Function:	H5O_dset_free_copy_file_udata
 *
 * Purpose:	Release the user data needed for copying a dataset's
 *		object header from file to file.
 *
 * Return:	<none>
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 21, 2005
 *
 * Modifications: Peter Cao
 *                Tuesday, December 27, 2005
 *                Free filter pipeline for copying a dataset
 *
 *-------------------------------------------------------------------------
 */
static void
H5O_dset_free_copy_file_udata(void *_udata)
{
    H5D_copy_file_ud_t *udata = (H5D_copy_file_ud_t *)_udata;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_dset_free_copy_file_udata)

    /* Sanity check */
    HDassert(udata);

    /* Release copy of dataset's dataspace extent, if it was set */
    if(udata->src_space_extent)
        H5O_msg_free(H5O_SDSPACE_ID, udata->src_space_extent);

    /* Release copy of dataset's datatype, if it was set */
    if(udata->src_dtype)
        H5T_close(udata->src_dtype);

    /* Release copy of dataset's filter pipeline, if it was set */
    if(udata->common.src_pline)
        H5O_msg_free(H5O_PLINE_ID, udata->common.src_pline);

    /* Release space for 'copy file' user data */
    udata = H5FL_FREE(H5D_copy_file_ud_t, udata);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5O_dset_free_copy_file_udata() */


/*-------------------------------------------------------------------------
 * Function:	H5O_dset_isa
 *
 * Purpose:	Determines if an object has the requisite messages for being
 *		a dataset.
 *
 * Return:	Success:	TRUE if the required dataset messages are
 *				present; FALSE otherwise.
 *
 *		Failure:	FAIL if the existence of certain messages
 *				cannot be determined.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5O_dset_isa(H5O_t *oh)
{
    htri_t	exists;                 /* Flag if header message of interest exists */
    htri_t	ret_value = TRUE;       /* Return value */

    FUNC_ENTER_NOAPI(H5O_dset_isa, FAIL)

    HDassert(oh);

    /* Datatype */
    if((exists = H5O_msg_exists_oh(oh, H5O_DTYPE_ID)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    else if(!exists)
	HGOTO_DONE(FALSE)

    /* Layout */
    if((exists = H5O_msg_exists_oh(oh, H5O_SDSPACE_ID)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    else if(!exists)
	HGOTO_DONE(FALSE)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_isa() */


/*-------------------------------------------------------------------------
 * Function:	H5O_dset_open
 *
 * Purpose:	Open a dataset at a particular location
 *
 * Return:	Success:	Open object identifier
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static hid_t
H5O_dset_open(const H5G_loc_t *obj_loc, hid_t lapl_id, hid_t dxpl_id, hbool_t app_ref)
{
    H5D_t       *dset = NULL;           /* Dataset opened */
    htri_t  isdapl;                 /* lapl_id is a dapl */
    hid_t   dapl_id;                /* dapl to use to open this dataset */
    hid_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_dset_open)

    HDassert(obj_loc);

    /* If the lapl passed in is a dapl, use it.  Otherwise, use the default dapl */
    if(lapl_id == H5P_DEFAULT)
        isdapl = FALSE;
    else
        if((isdapl = H5P_isa_class(lapl_id, H5P_DATASET_ACCESS)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCOMPARE, FAIL, "unable to compare property list classes")

    if(isdapl)
        dapl_id = lapl_id;
    else
        dapl_id = H5P_DATASET_ACCESS_DEFAULT;

    /* Open the dataset */
    if(NULL == (dset = H5D_open(obj_loc, dapl_id, dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "unable to open dataset")

    /* Register an ID for the dataset */
    if((ret_value = H5I_register(H5I_DATASET, dset, app_ref)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataset")

done:
    if(ret_value < 0)
        if(dset && H5D_close(dset) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_open() */


/*-------------------------------------------------------------------------
 * Function:	H5O_dset_create
 *
 * Purpose:	Create a dataset in a file
 *
 * Return:	Success:	Pointer to the dataset data structure
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, April 11, 2007
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_dset_create(H5F_t *f, void *_crt_info, H5G_loc_t *obj_loc, hid_t dxpl_id)
{
    H5D_obj_create_t *crt_info = (H5D_obj_create_t *)_crt_info; /* Dataset creation parameters */
    H5D_t *dset = NULL;         /* New dataset created */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_dset_create)

    /* Sanity checks */
    HDassert(f);
    HDassert(crt_info);
    HDassert(obj_loc);

    /* Create the the dataset */
    if(NULL == (dset = H5D_create(f, crt_info->type_id, crt_info->space, crt_info->dcpl_id, crt_info->dapl_id, dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to create dataset")

    /* Set up the new dataset's location */
    if(NULL == (obj_loc->oloc = H5D_oloc(dset)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get object location of dataset")
    if(NULL == (obj_loc->path = H5D_nameof(dset)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get path of dataset")

    /* Set the return value */
    ret_value = dset;

done:
    if(ret_value == NULL)
        if(dset && H5D_close(dset) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, NULL, "unable to release dataset")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_create() */


/*-------------------------------------------------------------------------
 * Function:	H5O_dset_get_oloc
 *
 * Purpose:	Retrieve the object header location for an open object
 *
 * Return:	Success:	Pointer to object header location
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5O_loc_t *
H5O_dset_get_oloc(hid_t obj_id)
{
    H5D_t       *dset;                  /* Dataset opened */
    H5O_loc_t	*ret_value;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_dset_get_oloc)

    /* Get the dataset */
    if(NULL == (dset = (H5D_t *)H5I_object(obj_id)))
        HGOTO_ERROR(H5E_OHDR, H5E_BADATOM, NULL, "couldn't get object from ID")

    /* Get the dataset's object header location */
    if(NULL == (ret_value = H5D_oloc(dset)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, NULL, "unable to get object location from object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_get_oloc() */


/*-------------------------------------------------------------------------
 * Function:    H5O_dset_bh_info
 *
 * Purpose:     Returns the amount of btree storage that is used for chunked
 *              dataset.
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi
 *              July 11, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dset_bh_info(H5F_t *f, hid_t dxpl_id, H5O_t *oh, H5_ih_info_t *bh_info)
{
    H5O_layout_t        layout;         	/* Data storage layout message */
    H5O_pline_t         pline;                  /* I/O pipeline message */
    H5O_efl_t           efl;			/* External File List message */
    hbool_t             layout_read = FALSE;    /* Whether the layout message was read */
    hbool_t             pline_read = FALSE;     /* Whether the I/O pipeline message was read */
    hbool_t             efl_read = FALSE;       /* Whether the external file list message was read */
    htri_t		exists;                 /* Flag if header message of interest exists */
    herr_t      	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_dset_bh_info)

    /* Sanity check */
    HDassert(f);
    HDassert(oh);
    HDassert(bh_info);

    /* Get the layout message from the object header */
    if(NULL == H5O_msg_read_oh(f, dxpl_id, oh, H5O_LAYOUT_ID, &layout))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find layout message")
    layout_read = TRUE;

    /* Check for chunked dataset storage */
    if(layout.type == H5D_CHUNKED && H5D_chunk_is_space_alloc(&layout.storage)) {
        /* Check for I/O pipeline message */
        if((exists = H5O_msg_exists_oh(oh, H5O_PLINE_ID)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
        else if(exists) {
            if(NULL == H5O_msg_read_oh(f, dxpl_id, oh, H5O_PLINE_ID, &pline))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find I/O pipeline message")
            pline_read = TRUE;
        } /* end else if */
        else
            HDmemset(&pline, 0, sizeof(pline));

        if(H5D_chunk_bh_info(f, dxpl_id, &layout, &pline, &(bh_info->index_size)) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't determine chunked dataset btree info")
    } /* end if */

    /* Check for External File List message in the object header */
    if((exists = H5O_msg_exists_oh(oh, H5O_EFL_ID)) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_NOTFOUND, FAIL, "unable to check for EFL message")

    if(exists && H5D_efl_is_space_alloc(&layout.storage)) {
        /* Start with clean EFL info */
        HDmemset(&efl, 0, sizeof(efl));

	/* Get External File List message from the object header */
	if(NULL == H5O_msg_read_oh(f, dxpl_id, oh, H5O_EFL_ID, &efl))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find EFL message")
        efl_read = TRUE;

	/* Get size of local heap for EFL message's file list */
	if(H5D_efl_bh_info(f, dxpl_id, &efl, &(bh_info->heap_size)) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't determine EFL heap info")
    } /* end if */

done:
    /* Free messages, if they've been read in */
    if(layout_read && H5O_msg_reset(H5O_LAYOUT_ID, &layout) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset data storage layout message")
    if(pline_read && H5O_msg_reset(H5O_PLINE_ID, &pline) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset I/O pipeline message")
    if(efl_read && H5O_msg_reset(H5O_EFL_ID, &efl) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset external file list message")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_bh_info() */


/*-------------------------------------------------------------------------
 * Function:    H5O_dset_compare
 *
 * Purpose:     fnord
 *
 * Return:      Success:        0 (equal) or 1 (not equal)
 *              Failure:        negative
 *
 * Programmer:  Neil Fortner
 *              October 27, 2010
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_dset_compare(const H5F_t *f1, const H5F_t *f2, const H5O_t *oh1,
    const H5O_t *oh2, haddr_t addr1, haddr_t addr2, hid_t dxpl1_id,
    hid_t dxpl2_id, H5O_cmp_t UNUSED *cmp_info)
{
    H5S_t               *space1;                /* Dataspace message */
    H5S_t               *space2;                /* Dataspace message */
    H5O_layout_t        layout1;                /* Data storage layout message */
    H5O_layout_t        layout2;                /* Data storage layout message */
    H5O_pline_t         pline1 = H5O_CRT_PIPELINE_DEF; /* I/O pipeline message */
    H5O_pline_t         pline2 = H5O_CRT_PIPELINE_DEF; /* I/O pipeline message */
    H5T_t               *dtype1;                /* Datatype message */
    H5T_t               *dtype2;                /* Datatype message */
    H5O_fill_t          fill1;                  /* Fill value message */
    H5O_fill_t          fill2;                  /* Fill value message */
    hbool_t             space_read = FALSE;     /* Whether the dataspace messages were read */
    hbool_t             layout_read = FALSE;    /* Whether the layout messages were read */
    hbool_t             pline_read = FALSE;     /* Whether the I/O pipeline messages were read */
    hbool_t             dtype_read = FALSE;     /* Whether the datatype messages were read */
    hbool_t             fill1_read = FALSE;     /* Whether the fill value message was read */
    hbool_t             fill2_read = FALSE;     /* Whether the fill value message was read */
    int                 ndims1;
    int                 ndims2;
    hsize_t             dims1[H5O_LAYOUT_NDIMS];
    hsize_t             dims2[H5O_LAYOUT_NDIMS];
    H5D_cmp_ud_t        udata;
    htri_t              tri_ret;                /* htri_t return value */
    unsigned            i;
    htri_t              ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5O_dset_compare, FAIL)

    /* Sanity check */
    HDassert(f1);
    HDassert(f2);
    HDassert(oh1);
    HDassert(oh2);

    /* Save the dset oh addresses in udata */
    udata.addr1 = addr1;
    udata.addr2 = addr2;

    /* Get the dataspace messages */
    if(NULL == (space1 = H5S_read_oh(f1, oh1, dxpl1_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't read dataspace message")
    if(NULL == (space2 = H5S_read_oh(f2, oh2, dxpl2_id))) {
        if(H5S_close(space1) < 0)
            HERROR(H5E_DATASET, H5E_CANTFREE, "can't close dataspace");
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't read dataspace message")
    } /* end if */
    space_read = TRUE;

    /* Check if the dataspace extents are identical */
    if((ndims1 = H5S_get_simple_extent_dims(space1, dims1, NULL)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataspace dimensions")
    if((ndims2 = H5S_get_simple_extent_dims(space2, dims2, NULL)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataspace dimensions")
    if(ndims1 != ndims2 || HDmemcmp(dims1, dims2, (size_t)ndims1 * sizeof(dims1[0])))
        HGOTO_DONE(TRUE)

    /* Save the dataspace in udata */
    udata.space = space1;

    /* Get the layout messages */
    if(NULL == H5O_msg_read_oh(f1, dxpl1_id, oh1, H5O_LAYOUT_ID, &layout1))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find layout message")
    if(NULL == H5O_msg_read_oh(f2, dxpl2_id, oh2, H5O_LAYOUT_ID, &layout2)) {
        if(H5O_msg_reset(H5O_LAYOUT_ID, &layout1) < 0)
            HERROR(H5E_DATASET, H5E_CANTRESET, "unable to reset data storage layout message");
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find layout message")
    } /* end if */
    layout_read = TRUE;

    /* Check if the layout messages are identical */
    /* Will eventually want to check if they are compatible, even if not
     * identical -NAF */
    /*FIXME fnord Note we do not handle external datasets! */
    if(layout1.type != layout2.type)
        HGOTO_DONE(TRUE)
    else if(layout1.type == H5D_CHUNKED) {
        if(layout1.u.chunk.ndims != layout2.u.chunk.ndims ||
                HDmemcmp(layout1.u.chunk.dim, layout2.u.chunk.dim,
                layout1.u.chunk.ndims * sizeof(layout1.u.chunk.dim[0])))
            HGOTO_DONE(TRUE)
        HDassert(layout1.u.chunk.size == layout2.u.chunk.size);
        HDassert(layout1.u.chunk.nchunks == layout2.u.chunk.nchunks);
        HDassert(!HDmemcmp(layout1.u.chunk.chunks, layout2.u.chunk.chunks,
                layout1.u.chunk.ndims * sizeof(layout1.u.chunk.dim[0])));
        HDassert(!HDmemcmp(layout1.u.chunk.down_chunks,
                layout2.u.chunk.down_chunks, layout1.u.chunk.ndims
                * sizeof(layout1.u.chunk.dim[0])));
    } /* end if */

    /* Get the pipeline messages */
    if((tri_ret = H5O_msg_exists_oh(oh1, H5O_PLINE_ID)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    if(tri_ret) {
        /* Make sure both objects have a pipeline - mix/match not supported yet
         */
        if((tri_ret = H5O_msg_exists_oh(oh2 , H5O_PLINE_ID)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
        if(!tri_ret)
            HGOTO_DONE(TRUE)

        if(NULL == H5O_msg_read_oh(f1, dxpl1_id, oh1, H5O_PLINE_ID, &pline1))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find pipeline message")
        if(NULL == H5O_msg_read_oh(f2, dxpl2_id, oh2, H5O_PLINE_ID, &pline2)) {
            if(H5O_msg_reset(H5O_PLINE_ID, &pline1) < 0)
                HERROR(H5E_DATASET, H5E_CANTRESET, "unable to reset pipeline message");
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find pipeline message")
        } /* end if */
        pline_read = TRUE;

        /* Check if the pipeline messages are identical */
        /* Will eventually want to handle different pipelines */
        if(pline1.nused != pline2.nused)
            HGOTO_DONE(TRUE)
        else
            for(i=0; i<pline1.nused; i++)
                if(pline1.filter[i].id != pline2.filter[i].id
                        || pline1.filter[i].cd_nelmts != pline2.filter[i].cd_nelmts
                        || HDmemcmp(pline1.filter[i].cd_values,
                        pline2.filter[i].cd_values, pline1.filter[i].cd_nelmts
                        * sizeof(pline1.filter[i].cd_values[0])))
                    HGOTO_DONE(TRUE)
    } /* end if */
    else {
        /* Make sure object 2 doesn't have a pipeline */
        if((tri_ret = H5O_msg_exists_oh(oh2 , H5O_PLINE_ID)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
        if(tri_ret)
            HGOTO_DONE(TRUE)
    } /* end else */

    /* Save the plines in udata */
    udata.pline1 = &pline1;
    udata.pline2 = &pline2;

    /* Get the datatype messages */
    if(NULL == (dtype1 = (H5T_t *)H5O_msg_read_oh(f1, dxpl1_id, oh1,
            H5O_DTYPE_ID, NULL)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find datatype message")
    if(NULL == (dtype2 = (H5T_t *)H5O_msg_read_oh(f2, dxpl2_id, oh2,
            H5O_DTYPE_ID, NULL)))
            {
        (void)H5O_msg_free(H5O_DTYPE_ID, dtype1);
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find datatype message")
    } /* end if */
    dtype_read = TRUE;

    /* Check if the datatypes are identical */
    if(H5T_cmp(dtype1, dtype2, FALSE))
        HGOTO_DONE(TRUE)

    /* Check for vlen or reference types - not supported currently */
    if(H5T_detect_class(dtype1, H5T_VLEN, FALSE))
        HGOTO_DONE(TRUE)
    if(H5T_detect_class(dtype1, H5T_REFERENCE, FALSE))
        HGOTO_DONE(TRUE)

    /* Get the fill value messages */
    if((tri_ret = H5O_msg_exists_oh(oh1, H5O_FILL_NEW_ID)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    if(tri_ret) {
        if(NULL == H5O_msg_read_oh(f1, dxpl1_id, oh1, H5O_FILL_NEW_ID, &fill1))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't read fill message")
        fill1_read = TRUE;
    } /* end if */
    else {
        if((tri_ret = H5O_msg_exists_oh(oh1, H5O_FILL_ID)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
        if(tri_ret) {
            if(NULL == H5O_msg_read_oh(f1, dxpl1_id, oh1, H5O_FILL_ID, &fill1))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't read fill message")
            fill1_read = TRUE;
        } else {
            fill1.fill_time = H5D_FILL_TIME_NEVER;
            fill1.fill_defined = FALSE;
        } /* end else */
    } /* end else */
    if((tri_ret = H5O_msg_exists_oh(oh2, H5O_FILL_NEW_ID)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    if(tri_ret) {
        if(NULL == H5O_msg_read_oh(f2, dxpl2_id, oh2, H5O_FILL_NEW_ID, &fill2))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't read fill message")
        fill2_read = TRUE;
    } /* end if */
    else {
        if((tri_ret = H5O_msg_exists_oh(oh2, H5O_FILL_ID)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
        if(tri_ret) {
            if(NULL == H5O_msg_read_oh(f2, dxpl2_id, oh2, H5O_FILL_ID, &fill2))
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't read fill message")
            fill2_read = TRUE;
        } else {
            fill2.fill_time = H5D_FILL_TIME_NEVER;
            fill2.fill_defined = FALSE;
        } /* end else */
    } /* end else */

    /* Check if fill values are identical */
    if(fill1.fill_defined && (fill1.fill_time == H5D_FILL_TIME_IFSET
            || fill1.fill_time == H5D_FILL_TIME_ALLOC)) {
        if(fill2.fill_defined && (fill2.fill_time == H5D_FILL_TIME_IFSET
                || fill2.fill_time == H5D_FILL_TIME_ALLOC)) {
            /* Both fill values are defined */
            HDassert(fill1.size == fill2.size);
            if(HDmemcmp(fill1.buf, fill2.buf, (size_t)fill1.size))
                udata.fill_identical = FALSE;
            else
                udata.fill_identical = TRUE;
        } /* end if */
        else {
            /* fill1 is defined but fill2 isn't */
            /* Check for a non-zero character in fill1 (is there an ISO C
             * function that does this?) */
            udata.fill_identical = TRUE;
            for(i=0; i<fill1.size; i++)
                if(((uint8_t *)fill1.buf)[i] != (uint8_t)0) {
                    udata.fill_identical = FALSE;
                    break;
                } /* end if */
        } /* end else */
    } /* end if */
    else {
        if(fill2.fill_defined && (fill2.fill_time == H5D_FILL_TIME_IFSET
                || fill2.fill_time == H5D_FILL_TIME_ALLOC)) {
            /* fill2 is defined but fill1 isn't */
            /* Check for a non-zero character in fill2 (is there an ISO C
             * function that does this?) */
            udata.fill_identical = TRUE;
            for(i=0; i<fill2.size; i++)
                if(((uint8_t *)fill2.buf)[i] != (uint8_t)0) {
                    udata.fill_identical = FALSE;
                    break;
                } /* end if */
        } /* end if */
        else
            /* Both fill1 and fill2 are undefined */
            udata.fill_identical = TRUE;
    } /* end else */

    /* Perform the comparison */
    HDassert(layout1.ops->compare);
    if((ret_value = layout1.ops->compare(f1, f2, &layout1, &layout2, dxpl1_id,
            dxpl2_id, &udata)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCOMPARE, FAIL, "unable to compare datasets")

done:
    /* Free messages, if they've been read in */
    if(space_read) {
        if(H5S_close(space1) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "can't close dataspace")
        if(H5S_close(space2) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "can't close dataspace")
    } /* end if */
    if(layout_read) {
        if(H5O_msg_reset(H5O_LAYOUT_ID, &layout1) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset data storage layout message")
        if(H5O_msg_reset(H5O_LAYOUT_ID, &layout2) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset data storage layout message")
    } /* end if */
    if(pline_read) {
        if(H5O_msg_reset(H5O_PLINE_ID, &pline1) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset I/O pipeline message")
        if(H5O_msg_reset(H5O_PLINE_ID, &pline2) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset I/O pipeline message")
    } /* end if */
    if(dtype_read) {
        (void)H5O_msg_free(H5O_DTYPE_ID, dtype1);
        (void)H5O_msg_free(H5O_DTYPE_ID, dtype2);
    } /* end if */
    /* Note that we take advantage of the fact that both versions of the fill
     * message use the same reset routine.  If this changes, we will have to
     * change this to pass the correct message ID. */
    if(fill1_read)
        (void)H5O_msg_reset(H5O_FILL_NEW_ID, &fill1);
    if(fill2_read)
        (void)H5O_msg_reset(H5O_FILL_NEW_ID, &fill2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_compare() */

