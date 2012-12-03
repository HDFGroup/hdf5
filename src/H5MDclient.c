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
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Tprivate.h"		/* Datatypes				*/

#ifdef H5_HAVE_PARALLEL

/* Declare a free list to manage the H5D_t and H5D_shared_t structs */
H5FL_DEFINE_STATIC(H5D_t);
H5FL_DEFINE_STATIC(H5D_shared_t);


/*-------------------------------------------------------------------------
 * Function:	H5A__mdc_create
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
H5A__mdc_create(const char *name, H5T_t *type, H5S_t *space, hid_t acpl_id)
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
} /* H5A__mdc_create() */


/*-------------------------------------------------------------------------
 * Function:	H5A__mdc_close
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
H5A__mdc_close(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

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
} /* end H5A__mdc_close() */


/*-------------------------------------------------------------------------
 * Function:	H5D__mdc_create
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
H5D__mdc_create(H5F_t *file, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t UNUSED dapl_id)
{
    const H5T_t         *type, *dt;             /* Datatype for dataset */
    H5D_t		*new_dset = NULL;
    hbool_t             has_vl_type = FALSE;    /* Flag to indicate a VL-type for dataset */
    H5G_loc_t           dset_loc;               /* Dataset location */
    H5D_t		*ret_value;             /* Return value */

    FUNC_ENTER_PACKAGE

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
        if(new_dset->shared) {
            new_dset->shared = H5FL_FREE(H5D_shared_t, new_dset->shared);
        } /* end if */
        new_dset->oloc.file = NULL;
        new_dset = H5FL_FREE(H5D_t, new_dset);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mdc_create() */


/*-------------------------------------------------------------------------
 * Function:	H5D__mdc_close
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
H5D__mdc_close(H5D_t *dataset)
{
    unsigned free_failed = FALSE;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check args */
    HDassert(dataset);

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
} /* end H5D__mdc_close() */


/*-------------------------------------------------------------------------
 * Function:	H5F_raw_open
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
H5F_mdc_open(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
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

    FUNC_ENTER_NOAPI(NULL)

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
} /* end H5F_mdc_open() */

#endif /* H5_HAVE_PARALLEL */
