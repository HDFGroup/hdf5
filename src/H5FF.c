/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              March, 2013
 *
 * Purpose:	Wrappers around existing HDF5 to support Exascale FastForward
 *              functionality.
 *              
 */


/****************/
/* Module Setup */
/****************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FFprivate.h"        /* FastForward wrappers                 */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5VLiod.h"		/* IOD plugin - tmp      		*/
#include "H5VLiod_client.h"	/* Client IOD - tmp			*/


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


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5FF_set_async_flag
 *
 * Purpose:	Helper routine to set up asynchronous I/O properties
 *
 * Return:	Success:	0
 *		Failure:	<0
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FF_set_async_flag(H5P_genplist_t *plist, const H5_request_t *req)
{
    hbool_t     do_async;               /* Whether we're going to do async. I/O */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(plist);
    HDassert(req);

    /* Check if we are performing asynchronous I/O */
    do_async = req ? TRUE : FALSE;

    /* Set the async. I/O operation flag */
    if(H5P_set(plist, H5P_ASYNC_FLAG_NAME, &do_async) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set async flag")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FF_set_async_flag() */


/*-------------------------------------------------------------------------
 * Function:	H5FF_get_async_req
 *
 * Purpose:	Helper routine to get asynchronous I/O request
 *
 * Return:	Success:	0
 *		Failure:	<0
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FF_get_async_req(H5P_genplist_t *plist, H5_request_t *req)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(plist);
    HDassert(req);

    /* Retrieve the async. I/O operation request */
    if(H5P_get(plist, H5P_ASYNC_REQ_NAME, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get async request")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FF_get_async_req() */


/*-------------------------------------------------------------------------
 * Function:	H5FF_reset_async_flag
 *
 * Purpose:	Helper routine to reset asynchronous I/O properties
 *
 * Return:	Success:	0
 *		Failure:	<0
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FF_reset_async_flag(H5P_genplist_t *plist)
{
    hbool_t     do_async = H5P_ASYNC_FLAG_DEF;  /* Default async I/O flag */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Reset the async. I/O operation flag */
    if(H5P_set(plist, H5P_ASYNC_FLAG_NAME, &do_async) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set async flag")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FF_reset_async_flag() */


/*-------------------------------------------------------------------------
 * Function:	H5Fcreate_ff
 *
 * Purpose:	Asynchronous wrapper around H5Fcreate().
 *
 * Return:	Success:	The placeholder ID for a new file.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate_ff(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id,
    H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == fapl_id) {
        if((fapl_id = H5Pcopy(H5P_FILE_ACCESS_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_FILE, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Fcreate(filename, flags, fcpl_id, fapl_id)) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(fapl_id) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Fcreate_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Fopen_ff
 *
 * Purpose:	Asynchronous wrapper around H5Fopen().
 *
 * Return:	Success:	The placeholder ID for a new file.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen_ff(const char *filename, unsigned flags, hid_t fapl_id,
    H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == fapl_id) {
        if((fapl_id = H5Pcopy(H5P_FILE_ACCESS_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_FILE, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_FILE, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Fopen(filename, flags, fapl_id)) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(fapl_id) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Fopen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gcreate_ff
 *
 * Purpose:	Asynchronous wrapper around H5Gcreate().
 *
 * Return:	Success:	The placeholder ID for a group.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gcreate_ff(hid_t loc_id, const char *name, hid_t lcpl_id,
    hid_t gcpl_id, hid_t gapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == gapl_id) {
        if((gapl_id = H5Pcopy(H5P_GROUP_ACCESS_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(gapl_id, H5P_GROUP_ACCESS))
            HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gapl_id)))
        HGOTO_ERROR(H5E_SYM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Gcreate2(loc_id, name, lcpl_id, gcpl_id, gapl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTCREATE, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(gapl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Gcreate_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Gopen_ff
 *
 * Purpose:	Asynchronous wrapper around H5Gopen().
 *
 * Return:	Success:	The placeholder ID for a group.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gopen_ff(hid_t loc_id, const char *name, hid_t gapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == gapl_id) {
        if((gapl_id = H5Pcopy(H5P_GROUP_ACCESS_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(gapl_id, H5P_GROUP_ACCESS))
            HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(gapl_id)))
        HGOTO_ERROR(H5E_SYM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Gopen2(loc_id, name, gapl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(gapl_id) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Gopen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dcreate_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dcreate().
 *
 * Return:	Success:	The placeholder ID for a new dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate_ff(hid_t loc_id, const char *name, hid_t type_id,
    hid_t space_id, hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == dapl_id) {
        if((dapl_id = H5Pcopy(H5P_DATASET_ACCESS_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(dapl_id, H5P_DATASET_ACCESS))
            HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Dcreate2(loc_id, name, type_id, space_id, lcpl_id, dcpl_id, dapl_id)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(dapl_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Dcreate_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dopen_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dopen().
 *
 * Return:	Success:	The placeholder ID for a dataset.  When
 *                              the asynchronous operation completes, this
 *                              ID will transparently be modified to be a
 *                              "normal" ID.
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen_ff(hid_t loc_id, const char *name, hid_t dapl_id,
    uint64_t /* UNUSED */ trans, H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    hid_t       ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == dapl_id) {
        if((dapl_id = H5Pcopy(H5P_DATASET_ACCESS_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(dapl_id, H5P_DATASET_ACCESS))
            HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dapl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Dopen2(loc_id, name, dapl_id)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(dapl_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Dopen_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dwrite().
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t dxpl_id, const void *buf,
    uint64_t /* UNUSED */ trans, H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == dxpl_id) {
        if((dxpl_id = H5Pcopy(H5P_DATASET_XFER_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Dwrite(dset_id, mem_type_id, mem_space_id, file_space_id, dxpl_id, buf)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(dxpl_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Dwrite_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5Dread_ff
 *
 * Purpose:	Asynchronous wrapper around H5Dread().
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread_ff(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
    hid_t file_space_id, hid_t dxpl_id, void *buf/*out*/,
    uint64_t /* UNUSED */ trans, H5_request_t *req)
{
    H5P_genplist_t  *plist;             /* Property list pointer */
    hbool_t     pl_copied = FALSE;      /* Whether the property list was copied */
    hbool_t     pl_modified = FALSE;    /* Whether the property list was modified */
    herr_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)

    /* Get property list */
    if(H5P_DEFAULT == dxpl_id) {
        if((dxpl_id = H5Pcopy(H5P_DATASET_XFER_DEFAULT)) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "can't copy default property list")
        pl_copied = TRUE;
    } /* end if */
    else
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "wrong type of property list")

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set up the properties for an asynchronous operation */
    if(H5FF_set_async_flag(plist, req) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set async request")
    pl_modified = TRUE;


    /* Perform the wrapped operation, possibly asynchronously */
    if((ret_value = H5Dread(dset_id, mem_type_id, mem_space_id, file_space_id, dxpl_id, buf)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to perform wrapped operation")


    /* Retrieve async request from property, if non-NULL */
    if(req)
        if(H5FF_get_async_req(plist, req) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get async request")

done:
    if(pl_copied) {
        if(H5Pclose(dxpl_id) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied property list")
    } /* end if */
    else if(pl_modified) {
        /* Clear the async info from property list, if not copied */
        if(H5FF_reset_async_flag(plist) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTRESET, FAIL, "unable to reset modified property list")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Dread_ff() */


/*-------------------------------------------------------------------------
 * Function:	H5AOtest
 *
 * Purpose:	Test for an asynchronous operation's completion
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AOtest(H5_request_t *req, H5_status_t *status)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

/* I believe that the VOL interface needs to be expanded with a 'test' callback,
    since the H5_request_t is pointing at a H5VL_iod_request_t [currently]
    and I can't get down to the VOL plugin from here.

    [And the 'test' client callback needs to release the request structure that
        the plugin allocated, if the operation has completed]
*/

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5AOtest() */


/*-------------------------------------------------------------------------
 * Function:	H5AOwait
 *
 * Purpose:	Wait for an asynchronous operation to complete
 *
 * Return:	Success:	SUCCEED
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, March 20, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AOwait(H5_request_t *req, H5_status_t *status)
{
    H5VL_iod_request_t *request = *((H5VL_iod_request_t **)req);
    fs_status_t tmp_status;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)

    if(H5VL_IOD_PENDING == request->state) {
        if(H5VL_iod_request_wait(request->obj->file, request) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to wait for request")
    }
    *status = request->status;

#if 0

        fs_wait(*((fs_request_t *)request->req), FS_MAX_IDLE_TIME, &tmp_status);
        if(tmp_status)
            *status = H5AO_SUCCEEDED;
        else
            *status = H5AO_FAILED;
    }
    else if(H5VL_IOD_COMPLETED == request->state) {
        *status = request->status;
    }
#endif

    request->req = H5MM_xfree(request->req);
    request = H5MM_xfree(request);
    req = NULL;

/* I believe that the VOL interface needs to be expanded with a 'wait' callback,
    since the H5_request_t is pointing at a H5VL_iod_request_t [currently]
    and I can't get down to the VOL plugin from here.

    [And the 'wait' client callback needs to release the request structure that
        the plugin allocated]
*/

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5AOwait() */

