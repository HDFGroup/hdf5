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

/*-------------------------------------------------------------------------
 *
 * Created:	H5Ddeprec.c
 *		April 5 2007
 *		Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:	Deprecated functions from the H5D interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5D__init_deprec_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets 				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5VLprivate.h"	/* VOL plugins				*/

/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

#ifndef H5_NO_DEPRECATED_SYMBOLS
static herr_t H5D__extend(H5D_t *dataset, const hsize_t *size, hid_t dxpl_id);
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*--------------------------------------------------------------------------
NAME
   H5D__init_deprec_interface -- Initialize interface-specific information
USAGE
    herr_t H5D__init_deprec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5D_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5D__init_deprec_interface(void)
{
    FUNC_ENTER_STATIC_NOERR

    FUNC_LEAVE_NOAPI(H5D_init())
} /* H5D__init_deprec_interface() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:	H5Dcreate1
 *
 * Purpose:	Creates a new dataset named NAME at LOC_ID, opens the
 *		dataset for access, and associates with that dataset constant
 *		and initial persistent properties including the type of each
 *		datapoint as stored in the file (TYPE_ID), the size of the
 *		dataset (SPACE_ID), and other initial miscellaneous
 *		properties (DCPL_ID).
 *
 *		All arguments are copied into the dataset, so the caller is
 *		allowed to derive new types, data spaces, and creation
 *		parameters from the old ones and reuse them in calls to
 *		create other datasets.
 *
 * Return:	Success:	The object ID of the new dataset.  At this
 *				point, the dataset is ready to receive its
 *				raw data.  Attempting to read raw data from
 *				the dataset will probably return the fill
 *				value.	The dataset should be closed when
 *				the caller is no longer interested in it.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate1(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id,
	  hid_t dcpl_id)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    void    *obj = NULL;        /* object token of loc_id */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5VL_loc_params_t loc_params;
    H5P_genplist_t  *plist;            /* Property list pointer */
    hid_t lcpl_id = H5P_LINK_CREATE_DEFAULT;
    hid_t           ret_value;          /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("i", "i*siii", loc_id, name, type_id, space_id, dcpl_id);

    if(H5P_DEFAULT == dcpl_id)
        dcpl_id = H5P_DATASET_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(dcpl_id, H5P_DATASET_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not dataset create property list ID")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* set creation properties */
    if(H5P_set(plist, H5VL_DSET_TYPE_ID, &type_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for datatype id")
    if(H5P_set(plist, H5VL_DSET_SPACE_ID, &space_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for space id")
    if(H5P_set(plist, H5VL_DSET_LCPL_ID, &lcpl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for lcpl id")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = (void *)H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_create(obj, loc_params, vol_plugin, name, dcpl_id, 
                                           H5P_DATASET_ACCESS_DEFAULT, H5AC_dxpl_id, H5_EVENT_STACK_NULL)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

    /* Get an atom for the dataset */
    if((ret_value = H5I_register2(H5I_DATASET, dset, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    if (ret_value < 0 && dset)
        if(H5VL_dataset_close (dset, vol_plugin, H5AC_dxpl_id, H5_EVENT_STACK_NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Dcreate1() */


/*-------------------------------------------------------------------------
 * Function:	H5Dopen1
 *
 * Purpose:	Finds a dataset named NAME at LOC_ID, opens it, and returns
 *		its ID.	 The dataset should be close when the caller is no
 *		longer interested in it.
 *
 * Note:	Deprecated in favor of H5Dopen2
 *
 * Return:	Success:	A new dataset ID
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen1(hid_t loc_id, const char *name)
{
    void    *dset = NULL;       /* dset token from VOL plugin */
    void    *obj = NULL;        /* object token of loc_id */
    H5VL_t  *vol_plugin;        /* VOL plugin information */
    H5VL_loc_params_t loc_params;
    hid_t        dapl_id = H5P_DATASET_ACCESS_DEFAULT; /* dapl to use to open dataset */
    hid_t        ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("i", "i*s", loc_id, name);

    /* Check args */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    loc_params.type = H5VL_OBJECT_BY_SELF;
    loc_params.obj_type = H5I_get_type(loc_id);

    /* get the file object */
    if(NULL == (obj = (void *)H5VL_get_object(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")

    /* get the plugin pointer */
    if (NULL == (vol_plugin = (H5VL_t *)H5I_get_aux(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "ID does not contain VOL information")

    /* Create the dataset through the VOL */
    if(NULL == (dset = H5VL_dataset_open(obj, loc_params, vol_plugin, name, dapl_id, H5AC_dxpl_id, H5_EVENT_STACK_NULL)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create dataset")

    /* Get an atom for the dataset */
    if((ret_value = H5I_register2(H5I_DATASET, dset, vol_plugin, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize dataset handle")

done:
    if (ret_value < 0 && dset)
        if(H5VL_dataset_close (dset, vol_plugin, H5AC_dxpl_id, H5_EVENT_STACK_NULL) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "unable to release dataset")
    FUNC_LEAVE_API(ret_value)
} /* end H5Dopen1() */


/*-------------------------------------------------------------------------
 * Function:	H5Dextend
 *
 * Purpose:	This function makes sure that the dataset is at least of size
 *		SIZE. The dimensionality of SIZE is the same as the data
 *		space of the dataset being changed.
 *
 * Note:	Deprecated in favor of H5Dset_extent
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dextend(hid_t dset_id, const hsize_t size[])
{
    H5D_t	*dset;
    herr_t       ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", dset_id, size);

    /* Check args */
    if(NULL == (dset = (H5D_t *)H5I_object_verify(dset_id, H5I_DATASET)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset")
    if(!size)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no size specified")

    /* Increase size */
    if(H5D__extend(dset, size, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to extend dataset")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Dextend() */


/*-------------------------------------------------------------------------
 * Function:	H5D__extend
 *
 * Purpose:	Increases the size of a dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__extend(H5D_t *dataset, const hsize_t *size, hid_t dxpl_id)
{
    htri_t changed;                     /* Flag to indicate that the dataspace was successfully extended */
    H5S_t *space;                       /* Dataset's dataspace */
    int     rank;                       /* Dataspace # of dimensions */
    hsize_t curr_dims[H5O_LAYOUT_NDIMS];/* Current dimension sizes */
    H5O_fill_t *fill;                   /* Dataset's fill value */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check args */
    HDassert(dataset);
    HDassert(size);

    /* Check if the filters in the DCPL will need to encode, and if so, can they? */
    if(H5D__check_filters(dataset) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't apply filters")

    /*
     * NOTE: Restrictions on extensions were checked when the dataset was
     *	     created.  All extensions are allowed here since none should be
     *	     able to muck things up.
     */

    /* Retrieve the current dimensions */
    space = dataset->shared->space;
    if((rank = H5S_get_simple_extent_dims(space, curr_dims, NULL)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataset dimensions")

    /* Increase the size of the data space */
    if((changed = H5S_extend(space, size)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to increase size of data space")

    /* Updated the dataset's info if the dataspace was successfully extended */
    if(changed) {
        /* Update the index values for the cached chunks for this dataset */
        if(H5D_CHUNKED == dataset->shared->layout.type) {
            if(H5D__chunk_set_info(dataset) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to update # of chunks")
            if(H5D__chunk_update_cache(dataset, dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to update cached chunk indices")
        } /* end if */

	/* Allocate space for the new parts of the dataset, if appropriate */
        fill = &dataset->shared->dcpl_cache.fill;
        if(fill->alloc_time == H5D_ALLOC_TIME_EARLY)
            if(H5D__alloc_storage(dataset, dxpl_id, H5D_ALLOC_EXTEND, FALSE,
                    curr_dims) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize dataset with fill value")

        /* Mark the dataspace as dirty, for later writing to the file */
        if(H5D__mark(dataset, dxpl_id, H5D_MARK_SPACE) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to mark dataspace as dirty")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__extend() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

