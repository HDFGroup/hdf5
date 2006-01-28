/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
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
static htri_t H5O_dset_isa(H5O_t *loc);
static void *H5O_dset_get_copy_file_udata(void);
static void H5O_dset_free_copy_file_udata(void *);

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
    H5G_DATASET,		/* object type			*/
    "dataset",			/* object name, for debugging	*/
    H5O_dset_get_copy_file_udata, /* get 'copy file' user data	*/
    H5O_dset_free_copy_file_udata, /* free 'copy file' user data	*/
    H5O_dset_isa 		/* "isa" message		*/
}};

/* Declare a free list to manage the H5D_copy_file_ud_t struct */
H5FL_DEFINE(H5D_copy_file_ud_t);


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
    if((exists = H5O_exists_oh(oh, H5O_DTYPE_ID, 0)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    else if(!exists)
	HGOTO_DONE(FALSE)

    /* Layout */
    if((exists = H5O_exists_oh(oh, H5O_SDSPACE_ID, 0)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to read object header")
    else if(!exists)
	HGOTO_DONE(FALSE)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dset_isa() */


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

    /* Release copy of dataset's datatype, if it was set */
    if(udata->src_dtype)
        H5T_close(udata->src_dtype);

    /* Release copy of dataset's filter pipeline, if it was set */
    if (udata->src_pline) 
        H5O_free(H5O_PLINE_ID, udata->src_pline);

    /* Release space for 'copy file' user data */
    H5FL_FREE(H5D_copy_file_ud_t, udata);

    FUNC_LEAVE_NOAPI_VOID
} /* end H5O_dset_free_copy_file_udata() */

