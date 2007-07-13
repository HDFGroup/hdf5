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
static hid_t H5O_dset_open(const H5G_loc_t *obj_loc, hid_t dxpl_id);
static void *H5O_dset_create(H5F_t *f, void *_crt_info, H5G_loc_t *obj_loc,
    hid_t dxpl_id);
static H5O_loc_t *H5O_dset_get_oloc(hid_t obj_id);


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
    H5O_dset_get_oloc 		/* get an object header location for an object */
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
    if (udata->src_pline)
        H5O_msg_free(H5O_PLINE_ID, udata->src_pline);

    /* Release space for 'copy file' user data */
    H5FL_FREE(H5D_copy_file_ud_t, udata);

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
H5O_dset_open(const H5G_loc_t *obj_loc, hid_t dxpl_id)
{
    H5D_t       *dset = NULL;           /* Dataset opened */
    hid_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_dset_open)

    HDassert(obj_loc);

    /* Open the dataset */
    if(NULL == (dset = H5D_open(obj_loc, dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "unable to open dataset")

    /* Register an ID for the dataset */
    if((ret_value = H5I_register(H5I_DATASET, dset)) < 0)
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

    /* Create the the dqtaset */
    if(NULL == (dset = H5D_create(f, crt_info->type_id, crt_info->space, crt_info->dcpl_id, dxpl_id)))
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
    if(NULL == (dset = H5I_object(obj_id)))
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
herr_t
H5O_dset_bh_info(H5O_loc_t *oloc, H5O_t *oh, hid_t dxpl_id, H5_ih_info_t *bh_info)
{
    H5O_layout_t        layout;         	/* Data storage layout message */
    herr_t      	ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5O_dset_bh_info, FAIL)

    /* Sanity check */
    HDassert(oloc);
    HDassert(oh);
    HDassert(bh_info);

    if (NULL==H5O_msg_read_real(oloc->file, dxpl_id, oh, H5O_LAYOUT_ID, &layout))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't find LAYOUT message")
    else {
	if ((layout.type == H5D_CHUNKED) && (layout.u.chunk.addr != HADDR_UNDEF)) {
	    ret_value = H5D_obj_istore_bh_info(oloc, &layout, dxpl_id, &(bh_info->index_size));
	    if (ret_value < 0)
		HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "can't determine chunked dataset btree info")
	}
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_bh_info() */
