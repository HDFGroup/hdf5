/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#include <H5private.h>		/* Generic Functions			*/
#include <H5Iprivate.h>		/* IDs			  		*/
#include <H5ACprivate.h>	/* Cache			  	*/
#include <H5Dprivate.h>		/* Dataset functions			*/
#include <H5Eprivate.h>		/* Error handling		  	*/
#include <H5Gprivate.h>		/* Group headers		  	*/
#include <H5HLprivate.h>	/* Name heap				*/
#include <H5MFprivate.h>	/* File space allocation header		*/
#include <H5MMprivate.h>	/* Memory management			*/
#include <H5Oprivate.h>		/* Object headers		  	*/
#include <H5Pprivate.h>		/* Property lists			*/
#include <H5Sprivate.h>		/* Dataspace functions rky 980813       */
#include <H5TBprivate.h>	/* Temporary buffers        		*/
#include <H5Vprivate.h>		/* Vector and array functions		*/
#include <H5Zprivate.h>		/* Data filters				*/

#define PABLO_MASK	H5D_mask

/*
 * A dataset is the following struct.
 */
struct H5D_t {
    H5G_entry_t		ent;		/*cached object header stuff	*/
    H5T_t		*type;		/*datatype of this dataset	*/
    H5D_create_t	*create_parms;	/*creation parameters		*/
    H5O_layout_t	layout;		/*data layout			*/
};

/* Default dataset creation property list */
const H5D_create_t	H5D_create_dflt = {
    H5D_CONTIGUOUS,		/* Layout				*/
    1,				/* Chunk dimensions			*/
    {1, 1, 1, 1, 1, 1, 1, 1,	/* Chunk size.	These default values....*/
     1, 1, 1, 1, 1, 1, 1, 1,	/*...are quite useless.	 Larger chunks..*/
     1, 1, 1, 1, 1, 1, 1, 1,	/*...produce fewer, but larger I/O......*/
     1, 1, 1, 1, 1, 1, 1, 1},	/*...requests.				*/

    /* Fill value */
    {NULL, 0, NULL},		/* No fill value			*/

    /* External file list */
    {H5F_ADDR_UNDEF,		/* External file list heap address	*/
     0,				/*...slots allocated			*/
     0,				/*...slots used				*/
     NULL}, 			/*...slot array				*/

    /* Filters */
    {0, 0, NULL} 		/* No filters in pipeline		*/
};

/* Default dataset transfer property list */
const H5D_xfer_t	H5D_xfer_dflt = {
    1024*1024,			/* Temporary buffer size		*/
    NULL,			/* Type conversion buffer or NULL	*/
    NULL, 			/* Background buffer or NULL		*/
    H5T_BKG_NO,			/* Type of background buffer needed	*/
    {0.1, 0.5, 0.9},		/* B-tree node splitting ratios		*/
#ifndef HAVE_PARALLEL
    1,				/* Cache the hyperslab blocks by default*/
#else
    0,				/*
				 * Don't cache the hyperslab blocks by
				 * default (for parallel)
				 */
#endif /* HAVE_PARALLEL */
    0,              		/*
				 * Default to no upper limit on hyperslab
				 * block size to cache
				 */
#ifdef HAVE_PARALLEL
    H5D_XFER_DFLT,      	/* Independent data transfer      	*/
#endif
};

/* Interface initialization? */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT H5D_init_interface
static herr_t H5D_init_interface(void);
static void H5D_term_interface(void);
static herr_t H5D_init_storage(H5D_t *dataset, const H5S_t *space);


/*--------------------------------------------------------------------------
NAME
   H5D_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5D_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

--------------------------------------------------------------------------*/
static herr_t
H5D_init_interface(void)
{
    herr_t		    ret_value = SUCCEED;
    FUNC_ENTER(H5D_init_interface, FAIL);

    /* Initialize the atom group for the dataset IDs */
    if ((ret_value = H5I_init_group(H5I_DATASET, H5I_DATASETID_HASHSIZE,
				    H5D_RESERVED_ATOMS,
				    (herr_t (*)(void *)) H5D_close)) != FAIL) {
	ret_value = H5_add_exit(H5D_term_interface);
    }
    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5D_term_interface
 PURPOSE
    Terminate various H5D objects
 USAGE
    void H5D_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5D_term_interface(void)
{
    H5I_destroy_group(H5I_DATASET);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dcreate
 *
 * Purpose:	Creates a new dataset named NAME at LOC_ID, opens the
 *		dataset for access, and associates with that dataset constant
 *		and initial persistent properties including the type of each
 *		datapoint as stored in the file (TYPE_ID), the size of the
 *		dataset (SPACE_ID), and other initial miscellaneous
 *		properties (PLIST_ID).
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
 * Errors:
 *		ARGS	  BADTYPE	Not a data space. 
 *		ARGS	  BADTYPE	Not a dataset creation plist. 
 *		ARGS	  BADTYPE	Not a file. 
 *		ARGS	  BADTYPE	Not a type. 
 *		ARGS	  BADVALUE	No name. 
 *		DATASET	  CANTINIT	Can't create dataset. 
 *		DATASET	  CANTREGISTER	Can't register dataset. 
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate(hid_t loc_id, const char *name, hid_t type_id, hid_t space_id,
	  hid_t plist_id)
{
    H5G_entry_t		   *loc = NULL;
    H5T_t		   *type = NULL;
    H5S_t		   *space = NULL;
    H5D_t		   *new_dset = NULL;
    hid_t		    ret_value = FAIL;
    const H5D_create_t	   *create_parms = NULL;

    FUNC_ENTER(H5Dcreate, FAIL);
    H5TRACE5("i","isiii",loc_id,name,type_id,space_id,plist_id);

    /* Check arguments */
    if (NULL == (loc = H5G_loc(loc_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    if (H5I_DATATYPE != H5I_get_type(type_id) ||
	NULL == (type = H5I_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a type");
    }
    if (H5I_DATASPACE != H5I_get_type(space_id) ||
	NULL == (space = H5I_object(space_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (H5P_DEFAULT==plist_id) {
	create_parms = &H5D_create_dflt;
    } else if (H5P_DATASET_CREATE != H5P_get_class(plist_id) ||
	       NULL == (create_parms = H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a dataset creation property list");
    }

    /* build and open the new dataset */
    if (NULL == (new_dset = H5D_create(loc, name, type, space,
				       create_parms))) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		      "unable to create dataset");
    }
    /* Register the new dataset to get an ID for it */
    if ((ret_value = H5I_register(H5I_DATASET, new_dset)) < 0) {
	H5D_close(new_dset);
	HRETURN_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
		      "unable to register dataset");
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dopen
 *
 * Purpose:	Finds a dataset named NAME at LOC_ID, opens it, and returns
 *		its ID.	 The dataset should be close when the caller is no
 *		longer interested in it.
 *
 * Return:	Success:	A new dataset ID
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen(hid_t loc_id, const char *name)
{
    H5G_entry_t	*loc = NULL;		/*location holding the dataset	*/
    H5D_t	*dataset = NULL;	/*the dataset			*/
    hid_t	ret_value = FAIL;

    FUNC_ENTER(H5Dopen, FAIL);
    H5TRACE2("i","is",loc_id,name);

    /* Check args */
    if (NULL == (loc = H5G_loc(loc_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    
    /* Find the dataset */
    if (NULL == (dataset = H5D_open(loc, name))) {
	HRETURN_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "dataset not found");
    }
    
    /* Create an atom for the dataset */
    if ((ret_value = H5I_register(H5I_DATASET, dataset)) < 0) {
	H5D_close(dataset);
	HRETURN_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
		      "can't register dataset");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dclose
 *
 * Purpose:	Closes access to a dataset (DATASET_ID) and releases
 *		resources used by it. It is illegal to subsequently use that
 *		same dataset ID in calls to other dataset functions.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a dataset. 
 *		DATASET	  CANTINIT	Can't free. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dclose(hid_t dset_id)
{
    H5D_t		   *dset = NULL;	/* dataset object to release */

    FUNC_ENTER(H5Dclose, FAIL);
    H5TRACE1("e","i",dset_id);

    /* Check args */
    if (H5I_DATASET != H5I_get_type(dset_id) ||
	NULL == (dset = H5I_object(dset_id)) ||
	NULL == dset->ent.file) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    if (H5I_dec_ref(dset_id) < 0) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't free");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dget_space
 *
 * Purpose:	Returns a copy of the file data space for a dataset.
 *
 * Return:	Success:	ID for a copy of the data space.  The data
 *				space should be released by calling
 *				H5Sclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 28, 1998
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space is not constant and is no longer cached by the dataset
 *	struct.
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_space(hid_t dset_id)
{
    H5D_t	*dset = NULL;
    H5S_t	*space = NULL;
    hid_t	ret_value = FAIL;
    
    FUNC_ENTER (H5Dget_space, FAIL);
    H5TRACE1("i","i",dset_id);

    /* Check args */
    if (H5I_DATASET!=H5I_get_type (dset_id) ||
	NULL==(dset=H5I_object (dset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }

    /* Read the data space message and return a data space object */
    if (NULL==(space=H5D_get_space (dset))) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to get data space");
    }

    /* Create an atom */
    if ((ret_value=H5I_register (H5I_DATASPACE, space))<0) {
	H5S_close (space);
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register data space");
    }

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_get_space
 *
 * Purpose:	Returns the data space associated with the dataset.
 *
 * Return:	Success:	Ptr to a copy of the data space.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 25, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5D_get_space(H5D_t *dset)
{
    H5S_t	*space = NULL;
    
    FUNC_ENTER(H5D_get_space, NULL);
    assert(dset);

    if (NULL==(space=H5S_read(&(dset->ent)))) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		      "unable to load space info from dataset header");
    }

    FUNC_LEAVE(space);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_type
 *
 * Purpose:	Returns a copy of the file data type for a dataset.
 *
 * Return:	Success:	ID for a copy of the data type.	 The data
 *				type should be released by calling
 *				H5Tclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, February  3, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 1 Jun 1998
 *	If the dataset has a named data type then a handle to the opened data
 *	type is returned.  Otherwise the returned data type is read-only.  If
 *	atomization of the data type fails then the data type is closed.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_type(hid_t dset_id)
{
    
    H5D_t	*dset = NULL;
    H5T_t	*copied_type = NULL;
    hid_t	ret_value = FAIL;
    
    FUNC_ENTER (H5Dget_type, FAIL);
    H5TRACE1("i","i",dset_id);

    /* Check args */
    if (H5I_DATASET!=H5I_get_type (dset_id) ||
	NULL==(dset=H5I_object (dset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }

    /* Copy the data type and mark it read-only */
    if (NULL==(copied_type=H5T_copy (dset->type, H5T_COPY_REOPEN))) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to copy the data type");
    }
    if (H5T_lock (copied_type, FALSE)<0) {
	H5T_close (copied_type);
	HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		       "unable to lock transient data type");
    }
    
    /* Create an atom */
    if ((ret_value=H5I_register (H5I_DATATYPE, copied_type))<0) {
	H5T_close (copied_type);
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register data type");
    }

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_create_plist
 *
 * Purpose:	Returns a copy of the dataset creation property list.
 *
 * Return:	Success:	ID for a copy of the dataset creation
 *				property list.  The template should be
 *				released by calling H5Pclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, February  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_create_plist(hid_t dset_id)
{
    H5D_t		*dset = NULL;
    H5D_create_t	*copied_parms = NULL;
    hid_t		ret_value = FAIL;
    
    FUNC_ENTER (H5Dget_create_plist, FAIL);
    H5TRACE1("i","i",dset_id);

    /* Check args */
    if (H5I_DATASET!=H5I_get_type (dset_id) ||
	NULL==(dset=H5I_object (dset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }

    /* Copy the creation property list */
    if (NULL==(copied_parms=H5P_copy (H5P_DATASET_CREATE,
				      dset->create_parms))) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to copy the creation property list");
    }

    /* Copy the dataset type into the fill value message */
    if (!copied_parms->fill.type &&
	NULL==(copied_parms->fill.type=H5T_copy(dset->type,
						H5T_COPY_TRANSIENT))) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		      "unable to copy dataset data type for fill value");
    }
    
    /* Create an atom */
    if ((ret_value=H5I_register ((H5I_type_t)(H5I_TEMPLATE_0+
					       H5P_DATASET_CREATE),
				 copied_parms))<0) {
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register creation property list");
    }

    FUNC_LEAVE (ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DSET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		MEM_SPACE_ID and FILE_SPACE_ID.	 The data points are
 *		converted from their file type to the MEM_TYPE_ID specified. 
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a data space. 
 *		ARGS	  BADTYPE	Not a data type. 
 *		ARGS	  BADTYPE	Not a dataset. 
 *		ARGS	  BADTYPE	Not xfer parms. 
 *		ARGS	  BADVALUE	No output buffer. 
 *		DATASET	  READERROR	Can't read data. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t plist_id, void *buf/*out*/)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    const H5D_xfer_t	   *xfer_parms = NULL;

    FUNC_ENTER(H5Dread, FAIL);
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (H5I_DATASET != H5I_get_type(dset_id) ||
	NULL == (dset = H5I_object(dset_id)) ||
	NULL == dset->ent.file) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    if (H5I_DATATYPE != H5I_get_type(mem_type_id) ||
	NULL == (mem_type = H5I_object(mem_type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (H5S_ALL != mem_space_id) {
	if (H5I_DATASPACE != H5I_get_type(mem_space_id) ||
	    NULL == (mem_space = H5I_object(mem_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
	/* Check for valid selection */
	if(H5S_select_valid(mem_space)!=TRUE) {
	    HRETURN_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,
			  "selection+offset not within extent");
	}
    }
    if (H5S_ALL != file_space_id) {
	if (H5I_DATASPACE != H5I_get_type(file_space_id) ||
	    NULL == (file_space = H5I_object(file_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
	/* Check for valid selection */
	if(H5S_select_valid(file_space)!=TRUE) {
	    HRETURN_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,
			  "selection+offset not within extent");
	}
    }
    if (H5P_DEFAULT == plist_id) {
	xfer_parms = &H5D_xfer_dflt;
    } else if (H5P_DATASET_XFER != H5P_get_class(plist_id) ||
	       NULL == (xfer_parms = H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    }
    if (!buf) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
    }

    /* read raw data */
    if (H5D_read(dset, mem_type, mem_space, file_space, xfer_parms,
		 buf/*out*/) < 0) {
	HRETURN_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DSET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		MEM_SPACE_ID and FILE_SPACE_ID arguments. The data points
 *		are converted from their current type (MEM_TYPE_ID) to their
 *		file data type.	 Additional miscellaneous data transfer
 *		properties can be passed to this function with the
 *		PLIST_ID argument.
 *
 *		The FILE_SPACE_ID can be the constant H5S_ALL which indicates
 *		that the entire file data space is to be referenced.
 *
 *		The MEM_SPACE_ID can be the constant H5S_ALL in which case
 *		the memory data space is the same as the file data space
 *		defined when the dataset was created.
 *
 *		The number of elements in the memory data space must match
 *		the number of elements in the file data space.
 *
 *		The PLIST_ID can be the constant H5P_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t plist_id, const void *buf)
{
    H5D_t		   *dset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    const H5D_xfer_t	   *xfer_parms = NULL;

    FUNC_ENTER(H5Dwrite, FAIL);
    H5TRACE6("e","iiiiix",dset_id,mem_type_id,mem_space_id,file_space_id,
             plist_id,buf);

    /* check arguments */
    if (H5I_DATASET != H5I_get_type(dset_id) ||
	NULL == (dset = H5I_object(dset_id)) ||
	NULL == dset->ent.file) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    if (H5I_DATATYPE != H5I_get_type(mem_type_id) ||
	NULL == (mem_type = H5I_object(mem_type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (H5S_ALL != mem_space_id) {
	if (H5I_DATASPACE != H5I_get_type(mem_space_id) ||
	    NULL == (mem_space = H5I_object(mem_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
	/* Check for valid selection */
	if (H5S_select_valid(mem_space)!=TRUE) {
	    HRETURN_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,
			  "selection+offset not within extent");
	}
    }
    if (H5S_ALL != file_space_id) {
	if (H5I_DATASPACE != H5I_get_type(file_space_id) ||
	    NULL == (file_space = H5I_object(file_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
	/* Check for valid selection */
	if (H5S_select_valid(file_space)!=TRUE) {
	    HRETURN_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL,
			  "selection+offset not within extent");
	}
    }
    if (H5P_DEFAULT == plist_id) {
	xfer_parms = &H5D_xfer_dflt;
    } else if (H5P_DATASET_XFER != H5P_get_class(plist_id) ||
	       NULL == (xfer_parms = H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    }
    if (!buf) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
    }

    /* write raw data */
    if (H5D_write(dset, mem_type, mem_space, file_space, xfer_parms,
		  buf) < 0) {
	HRETURN_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "can't write data");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dextend
 *
 * Purpose:	This function makes sure that the dataset is at least of size
 *		SIZE. The dimensionality of SIZE is the same as the data
 *		space of the dataset being changed.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dextend(hid_t dset_id, const hsize_t *size)
{
    H5D_t	*dset = NULL;
    
    FUNC_ENTER (H5Dextend, FAIL);
    H5TRACE2("e","i*h",dset_id,size);

    /* Check args */
    if (H5I_DATASET!=H5I_get_type (dset_id) ||
	NULL==(dset=H5I_object (dset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    if (!size) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no size specified");
    }

    /* Increase size */
    if (H5D_extend (dset, size)<0) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to extend dataset");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_new
 *
 * Purpose:	Creates a new, empty dataset structure
 *
 * Return:	Success:	Pointer to a new dataset descriptor.
 *
 *		Failure:	NULL
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_new(const H5D_create_t *create_parms)
{
    H5D_t	*ret_value = NULL;	/*return value			*/
    
    FUNC_ENTER(H5D_new, NULL);

    /* check args */
    /* Nothing to check */
    
    if (NULL==(ret_value = H5MM_calloc(sizeof(H5D_t)))) {
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed");
    }
    if(create_parms!=NULL) {
        ret_value->create_parms = H5P_copy (H5P_DATASET_CREATE, create_parms);
    } else {
        ret_value->create_parms = H5P_copy (H5P_DATASET_CREATE,
					    &H5D_create_dflt);
    }
    H5F_addr_undef(&(ret_value->ent.header));

    /* Success */

done:
    FUNC_LEAVE(ret_value);
}   /* end H5D_new() */


/*-------------------------------------------------------------------------
 * Function:	H5D_create
 *
 * Purpose:	Creates a new dataset with name NAME in file F and associates
 *		with it a datatype TYPE for each element as stored in the
 *		file, dimensionality information or dataspace SPACE, and
 *		other miscellaneous properties CREATE_PARMS.  All arguments
 *		are deep-copied before being associated with the new dataset,
 *		so the caller is free to subsequently modify them without
 *		affecting the dataset.
 *
 * Return:	Success:	Pointer to a new dataset
 *
 *		Failure:	NULL
 *
 * Errors:
 *		DATASET	  CANTINIT	Can't update dataset header. 
 *		DATASET	  CANTINIT	Problem with the dataset name. 
 *		DATASET	  CANTINIT	Fail in file space allocation for
 *					chunks
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space message is no longer cached in the dataset struct.
 *
 * 	Robb Matzke, 27 Jul 1998
 *	Added the MTIME message to the dataset object header.
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_create(H5G_entry_t *loc, const char *name, const H5T_t *type,
	   const H5S_t *space, const H5D_create_t *create_parms)
{
    H5D_t		*new_dset = NULL;
    H5D_t		*ret_value = NULL;
    intn		i, ndims;
    hsize_t		max_dim[H5O_LAYOUT_NDIMS];
    H5O_efl_t		*efl = NULL;
    H5F_t		*f = NULL;

    FUNC_ENTER(H5D_create, NULL);

    /* check args */
    assert (loc);
    assert (name && *name);
    assert (type);
    assert (space);
    assert (create_parms);
    if (create_parms->pline.nfilters>0 &&
	H5D_CHUNKED!=create_parms->layout) {
	HGOTO_ERROR (H5E_DATASET, H5E_BADVALUE, NULL,
		     "filters can only be used with chunked layout");
    }

    /* What file is the dataset being added to? */
    if (NULL==(f=H5G_insertion_file(loc, name))) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to locate insertion point");
    }

#ifdef HAVE_PARALLEL
    /* If MPIO is used, no filter support yet. */
    if (f->shared->access_parms->driver == H5F_LOW_MPIO &&
	create_parms->pline.nfilters>0) {
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, NULL,
		     "Parallel IO does not support filters yet");
    }
#endif
    
    /* Initialize the dataset object */
    if (NULL==(new_dset = H5D_new(create_parms))) {
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed");
    }
    new_dset->type = H5T_copy(type, H5T_COPY_ALL);
    efl = &(new_dset->create_parms->efl);

    /* Total raw data size */
    new_dset->layout.type = new_dset->create_parms->layout;
    new_dset->layout.ndims = H5S_get_simple_extent_ndims(space) + 1;
    assert((unsigned)(new_dset->layout.ndims) <= NELMTS(new_dset->layout.dim));
    new_dset->layout.dim[new_dset->layout.ndims-1] = H5T_get_size(type);

    switch (new_dset->create_parms->layout) {
    case H5D_CONTIGUOUS:
	/*
	 * The maximum size of the dataset cannot exceed the storage size.
	 * Also, only the slowest varying dimension of a simple data space
	 * can be extendible.
	 */
	if ((ndims=H5S_get_simple_extent_dims(space, new_dset->layout.dim,
					      max_dim))<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
			"unable to initialize contiguous storage");
	}
	for (i=1; i<ndims; i++) {
	    if (max_dim[i]>new_dset->layout.dim[i]) {
		HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
			     "only the first dimension can be extendible");
	    }
	}
	if (efl->nused>0) {
	    hsize_t max_points = H5S_get_npoints_max (space);
	    hsize_t max_storage = H5O_efl_total_size (efl);

	    if (H5S_UNLIMITED==max_points) {
		if (H5O_EFL_UNLIMITED!=max_storage) {
		    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
				 "unlimited data space but finite storage");
		}
	    } else if (max_points * H5T_get_size (type) < max_points) {
		HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
			     "data space * type size overflowed");
	    } else if (max_points * H5T_get_size (type) > max_storage) {
		HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
			     "data space size exceeds external storage size");
	    }
	} else if (ndims>0 && max_dim[0]>new_dset->layout.dim[0]) {
	    HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, NULL,
			 "extendible contiguous non-external dataset");
	}
	break;

    case H5D_CHUNKED:
	/*
	 * Chunked storage allows any type of data space extension, so we
	 * don't even bother checking.
	 */
	if (new_dset->create_parms->chunk_ndims !=
	    H5S_get_simple_extent_ndims(space)) {
	    HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, NULL,
		   "dimensionality of chunks doesn't match the data space");
	}
	if (efl->nused>0) {
	    HGOTO_ERROR (H5E_DATASET, H5E_BADVALUE, NULL,
			 "external storage not supported with chunked layout");
	}
	for (i=0; i<new_dset->layout.ndims-1; i++) {
	    new_dset->layout.dim[i] = new_dset->create_parms->chunk_size[i];
	}
	break;

    default:
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
    }

    /* Create (open for write access) an object header */
    if (H5O_create(f, 96, &(new_dset->ent)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to create dataset object header");
    }

    /* Convert the fill value to the dataset type and write the message */
    if (H5O_fill_convert(&(new_dset->create_parms->fill), new_dset->type)<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to convert fill value to dataset type");
    }
    if (new_dset->create_parms->fill.buf &&
	H5O_modify(&(new_dset->ent), H5O_FILL, 0, H5O_FLAG_CONSTANT,
		   &(new_dset->create_parms->fill))<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to update fill value header message");
    }
    
    /* Update the type and space header messages */
    if (H5O_modify(&(new_dset->ent), H5O_DTYPE, 0,
		   H5O_FLAG_CONSTANT|H5O_FLAG_SHARED, new_dset->type)<0 ||
	H5S_modify(&(new_dset->ent), space) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to update type or space header messages");
    }

    /* Update the filters message */
    if (new_dset->create_parms->pline.nfilters>0 &&
	H5O_modify (&(new_dset->ent), H5O_PLINE, 0, H5O_FLAG_CONSTANT,
		    &(new_dset->create_parms->pline))<0) {
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		     "unable to update filter header message");
    }

    /*
     * Add a modification time message.
     */
    if (H5O_touch(&(new_dset->ent), TRUE)<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to update modification time message");
    }
    
    /*
     * Initialize storage.  We assume that external storage is already
     * initialized by the caller, or at least will be before I/O is
     * performed.
     */
    if (0==efl->nused) {
	if (H5F_arr_create(f, &(new_dset->layout)) < 0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
			"unable to initialize storage");
	}
    } else {
	H5F_addr_undef (&(new_dset->layout.addr));
    }

    /* Update layout message */
    if (H5O_modify (&(new_dset->ent), H5O_LAYOUT, 0, H5O_FLAG_CONSTANT,
		    &(new_dset->layout)) < 0) {
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		     "unable to update layout message");
    }

    /* Update external storage message */
    if (efl->nused>0) {
	size_t heap_size = H5HL_ALIGN (1);
	for (i=0; i<efl->nused; i++) {
	    heap_size += H5HL_ALIGN (HDstrlen (efl->slot[i].name)+1);
	}
	if (H5HL_create (f, heap_size, &(efl->heap_addr))<0 ||
	    (size_t)(-1)==H5HL_insert (f, &(efl->heap_addr), 1, "")) {
	    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
			 "unable to create external file list name heap");
	}
	if (H5O_modify (&(new_dset->ent), H5O_EFL, 0, H5O_FLAG_CONSTANT,
			efl)<0) {
	    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
			 "unable to update external file list message");
	}
    }

    /* Initialize the raw data */
    if (H5D_init_storage(new_dset, space)<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to initialize storage");
    }
    
    /* Give the dataset a name */
    if (H5G_insert(loc, name, &(new_dset->ent)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to name dataset");
    }

    /* Success */
    ret_value = new_dset;

  done:
    if (!ret_value && new_dset) {
	if (new_dset->type) H5T_close(new_dset->type);
	if (new_dset->create_parms) {
	    H5P_close (H5P_DATASET_CREATE, new_dset->create_parms);
	    new_dset->create_parms = NULL;
	}
	if (H5F_addr_defined(&(new_dset->ent.header))) {
	    H5O_close(&(new_dset->ent));
	}
	new_dset->ent.file = NULL;
	H5MM_xfree(new_dset);
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5D_open
 *
 * Purpose:	Finds a dataset named NAME in file F and builds a descriptor
 *		for it, opening it for access.
 *
 * Return:	Success:	Pointer to a new dataset descriptor.
 *
 *		Failure:	NULL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 * 	Robb Matzke, 9 Jun 1998
 *	The data space message is no longer cached in the dataset struct.
 *	
 *  	Quincey Koziol, 12 Oct 1998
 *  	Moved guts of function into H5D_open_oid
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_open(H5G_entry_t *loc, const char *name)
{
    H5D_t	*dataset = NULL;	/*the dataset which was found	*/
    H5D_t	*ret_value = NULL;	/*return value			*/
    
    FUNC_ENTER(H5D_open, NULL);

    /* check args */
    assert (loc);
    assert (name && *name);
    
    if (NULL==(dataset = H5D_new(NULL))) {
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed");
    }

    /* Find the dataset object */
    if (H5G_find(loc, name, NULL, &(dataset->ent)) < 0) {
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found");
    }
    /* Open the dataset object */
    if (H5D_open_oid(dataset, NULL) < 0) {
        HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found");
    }

    /* Success */
    ret_value = dataset;

done:
    FUNC_LEAVE(ret_value);
}   /* end H5D_open() */

/*-------------------------------------------------------------------------
 * Function:	H5D_open_oid
 *
 * Purpose:	Opens a dataset for access.
 *
 * Return:	SUCCEED/FAIL
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 12, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_open_oid(H5D_t *dataset, H5G_entry_t *ent)
{
    herr_t	ret_value = FAIL;	/*return value			*/
    intn	i;
    H5S_t	*space = NULL;
    
    FUNC_ENTER(H5D_open_oid, FAIL);

    /* check args */
    assert (dataset);
    
    /* Copy over the symbol table information if it's provided */
    if(ent!=NULL)
        HDmemcpy(&(dataset->ent),ent,sizeof(H5G_entry_t));

    /* Find the dataset object */
    if (H5O_open(&(dataset->ent)) < 0) {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "unable to open");
    }
    
    /* Get the type and space */
    if (NULL==(dataset->type=H5O_read(&(dataset->ent), H5O_DTYPE, 0, NULL))) {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		    "unable to load type info from dataset header");
    }
    if (NULL==(space=H5S_read (&(dataset->ent)))) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to read data space info from dataset header");
    }

    /* Get the optional fill value message */
    if (NULL==H5O_read(&(dataset->ent), H5O_FILL, 0,
		       &(dataset->create_parms->fill))) {
        H5E_clear();
        HDmemset(&(dataset->create_parms->fill), 0,
                sizeof(dataset->create_parms->fill));
    }

    /* Get the optional filters message */
    if (NULL==H5O_read (&(dataset->ent), H5O_PLINE, 0,
			&(dataset->create_parms->pline))) {
        H5E_clear ();
        HDmemset (&(dataset->create_parms->pline), 0,
                sizeof(dataset->create_parms->pline));
    }

#ifdef HAVE_PARALLEL
    /* If MPIO is used, no filter support yet. */
    if (dataset->ent.file->shared->access_parms->driver == H5F_LOW_MPIO &&
            dataset->create_parms->pline.nfilters>0){
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "Parallel IO does not support filters yet");
    }
#endif
    
    /*
     * Get the raw data layout info.  It's actually stored in two locations:
     * the storage message of the dataset (dataset->storage) and certain
     * values are copied to the dataset create plist so the user can query
     * them.
     */
    if (NULL==H5O_read(&(dataset->ent), H5O_LAYOUT, 0, &(dataset->layout))) {
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                "unable to read data layout message");
    }
    switch (dataset->layout.type) {
        case H5D_CONTIGUOUS:
            dataset->create_parms->layout = H5D_CONTIGUOUS;
            break;

        case H5D_CHUNKED:
        /*
         * Chunked storage.  The creation plist's dimension is one less than
         * the chunk dimension because the chunk includes a dimension for the
         * individual bytes of the data type.
         */
            dataset->create_parms->layout = H5D_CHUNKED;
            dataset->create_parms->chunk_ndims = dataset->layout.ndims - 1;
            for (i = 0; i < dataset->layout.ndims - 1; i++) {
                dataset->create_parms->chunk_size[i] = dataset->layout.dim[i];
            }
            break;

        default:
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
			"not implemented yet");
    }

    /* Get the external file list message, which might not exist */
    if (NULL==H5O_read (&(dataset->ent), H5O_EFL, 0,
			&(dataset->create_parms->efl)) &&
	!H5F_addr_defined (&(dataset->layout.addr))) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "storage address is undefined an no external file list");
    }

    /*
     * Make sure all storage is properly initialized for chunked datasets.
     * This is especially important for parallel I/O where the B-tree must
     * be fully populated before I/O can happen.
     */
    if ((dataset->ent.file->intent & H5F_ACC_RDWR) &&
	H5D_CHUNKED==dataset->layout.type) {
        if (H5D_init_storage(dataset, space)<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                "unable to initialize file storage");
        }
    }

    /* Success */
    ret_value = SUCCEED;

done:
    if (space)
        H5S_close (space);
    if (ret_value<0 && dataset) {
        if (H5F_addr_defined(&(dataset->ent.header))) {
            H5O_close(&(dataset->ent));
        }
        if (dataset->type) {
            H5T_close(dataset->type);
        }
        if (dataset->create_parms) {
            H5P_close (H5P_DATASET_CREATE, dataset->create_parms);
        }
        dataset->ent.file = NULL;
        H5MM_xfree(dataset);
    }
    FUNC_LEAVE(ret_value);
}   /* end H5D_open_oid() */

/*-------------------------------------------------------------------------
 * Function:	H5D_close
 *
 * Purpose:	Insures that all data has been saved to the file, closes the
 *		dataset object header, and frees all resources used by the
 *		descriptor.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		DATASET	  CANTINIT	Couldn't free the type or space,
 *					but the dataset was freed anyway. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space message is no longer cached in the dataset struct.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_close(H5D_t *dataset)
{
    uintn		    free_failed;

    FUNC_ENTER(H5D_close, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);

    /*
     * Release data type and creation property list -- there isn't much we
     * can do if one of these fails, so we just continue.
     */
    free_failed = (H5T_close(dataset->type) < 0 ||
		   H5P_close (H5P_DATASET_CREATE, dataset->create_parms)); 

    /* Close the dataset object */
    H5O_close(&(dataset->ent));

    /*
     * Free memory.  Before freeing the memory set the file pointer to NULL.
     * We always check for a null file pointer in other H5D functions to be
     * sure we're not accessing an already freed dataset (see the assert()
     * above).
     */
    dataset->ent.file = NULL;
    H5MM_xfree(dataset);

    if (free_failed) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		      "couldn't free the type or creation property list, "
		      "but the dataset was freed anyway.");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5D_read
 *
 * Purpose:	Reads (part of) a DATASET into application memory BUF. See
 *		H5Dread() for complete details.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *	Robb Matzke, 9 Jun 1998
 *	The data space is no longer cached in the dataset struct.
 *
 * 	Robb Matzke, 11 Aug 1998
 *	Added timing calls around all the data space I/O functions.
 *
 * 	rky 980918
 *	Added must_convert to do non-optimized read when necessary.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_read(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	 const H5S_t *file_space, const H5D_xfer_t *xfer_parms,
	 void *buf/*out*/)
{
    hsize_t		nelmts;			/*number of elements	*/
    size_t		smine_start;		/*strip mine start loc	*/
    size_t		n, smine_nelmts;	/*elements per strip	*/
    hid_t       tconv_id=FAIL, bkg_id=FAIL;   /* Conversion buffer IDs */
    uint8		*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8		*bkg_buf = NULL;	/*background buffer	*/
    H5T_conv_t		tconv_func = NULL;	/*conversion function	*/
    hid_t		src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t		*sconv=NULL;		/*space conversion funcs*/
    H5S_sel_iter_t 	mem_iter;        /* mem selection iteration info*/
    H5S_sel_iter_t	bkg_iter;	     /*background iteration info*/
    H5S_sel_iter_t	file_iter;            /*file selection iter info*/
    H5T_cdata_t		*cdata = NULL;		/*type conversion data	*/
    herr_t		ret_value = FAIL;
    herr_t		status;
    size_t		src_type_size;		/*size of source type	*/
    size_t		dst_type_size;	      /*size of destination type*/
    size_t		target_size;		/*desired buffer size	*/
    size_t		request_nelmts;		/*requested strip mine	*/
    H5T_bkg_t		need_bkg;		/*type of background buf*/
    H5S_t		*free_this_space=NULL;	/*data space to free	*/
    hbool_t             must_convert;           /*have to xfer the slow way */
#ifdef H5T_DEBUG
    H5_timer_t		timer;
#endif

    FUNC_ENTER(H5D_read, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(xfer_parms);
    assert(buf);

    /* Initialize these before any errors can occur */
    HDmemset(&mem_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&bkg_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&file_iter,0,sizeof(H5S_sel_iter_t));

    if (!file_space) {
        if (NULL==(free_this_space=H5S_read (&(dataset->ent)))) {
            HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
                 "unable to read data space from dataset header");
        }
        file_space = free_this_space;
    }
    if (!mem_space) mem_space = file_space;
    nelmts = H5S_get_select_npoints(mem_space);

#ifdef HAVE_PARALLEL
    /*
     * Check if collective data transfer requested.
     */
    if (xfer_parms->xfer_mode == H5D_XFER_COLLECTIVE){
	/*
	 * Verify that the file can support collective access. The check may
	 * not be necessarily since collective access can always be simulated
	 * by independent access.  Nevertheless, must check driver is MPIO
	 * before using those access_mode which exists only for MPIO case.
	 */
	if (dataset->ent.file->shared->access_parms->driver != H5F_LOW_MPIO)
	    HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
			 "collective access not permissible");
    }
#endif

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
    if (nelmts!=H5S_get_select_npoints (file_space)) {
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "src and dest data spaces have different sizes");
    }
    if (NULL == (tconv_func = H5T_find(dataset->type, mem_type,
				       xfer_parms->need_bkg, &cdata))) {
        HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		    "unable to convert between src and dest data types");
    } else if (H5T_conv_noop!=tconv_func) {
        if ((src_id=H5I_register(H5I_DATATYPE,
				 H5T_copy(dataset->type, H5T_COPY_ALL)))<0 ||
	    (dst_id=H5I_register(H5I_DATATYPE,
				 H5T_copy(mem_type, H5T_COPY_ALL)))<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
                "unable to register types for conversion");
        }
    }
    if (NULL==(sconv=H5S_find(mem_space, file_space))) {
        HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "unable to convert from file to memory data space");
    }
#ifdef HAVE_PARALLEL
    /* rky 980813 This is a temporary KLUGE.
     * The sconv functions should be set by H5S_find,
     * or we should use a different way to call the MPI-IO
     * mem-and-file-dataspace-xfer functions
     * (the latter in case the arguments to sconv_funcs
     * turn out to be inappropriate for MPI-IO).  */
    if (H5_mpi_opt_types_g &&
        H5F_LOW_MPIO==dataset->ent.file->shared->access_parms->driver) {
	sconv->read = H5S_mpio_spaces_read;
    }
#endif /*HAVE_PARALLEL*/

    /*
     * If there is no type conversion then try reading directly into the
     * application's buffer.  This saves at least one mem-to-mem copy.
     */
    if (H5T_conv_noop==tconv_func && sconv->read) {
        status = (sconv->read)(dataset->ent.file, &(dataset->layout),
			       &(dataset->create_parms->pline),
			       &(dataset->create_parms->efl),
			       H5T_get_size (dataset->type), file_space,
			       mem_space, xfer_parms->xfer_mode,
			       buf/*out*/, &must_convert );
        if (status<0) {
	    /* Supports only no conversion, type or space, for now. */
	    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL,
		        "optimized read failed");
	}
	if (must_convert) {
	    /* sconv->read cannot do a direct transfer;
	     * fall through and xfer the data in the more roundabout way */
	} else {
	    /* direct xfer accomplished successfully */
	    goto succeed;
	}
#ifdef H5D_DEBUG
	if (H5DEBUG(D)) {
	    fprintf (H5DEBUG(D), "H5D: data space conversion could not be "
		     "optimized for this case (using general method "
		     "instead)\n");
	}
#endif
        H5E_clear ();
    }
	
    /*
     * This is the general case.  Figure out the strip mine size.
     */
    src_type_size = H5T_get_size(dataset->type);
    dst_type_size = H5T_get_size(mem_type);
    target_size = xfer_parms->buf_size;
    request_nelmts = target_size / MAX(src_type_size, dst_type_size);
    if (request_nelmts<=0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "temporary buffer max size is too small");
    }
    if ((sconv->f->init)(&(dataset->layout), file_space, &file_iter)<0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize file selection information");
    } 
    if ((sconv->m->init)(&(dataset->layout), mem_space, &mem_iter)<0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize memory selection information");
    } 
    if ((sconv->m->init)(&(dataset->layout), mem_space, &bkg_iter)<0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize background selection information");
    } 
#ifdef QAK
    printf("%s: check 3.0, request_nelmts=%d\n",FUNC,(int)request_nelmts);
#endif

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties. Instead of allocating a
     * buffer which is the exact size, we allocate the target size.  The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (cdata->need_bkg) {
        need_bkg = MAX (cdata->need_bkg, xfer_parms->need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    }
    if (NULL==(tconv_buf=xfer_parms->tconv_buf)) {
        /* Allocate temporary buffer */
        if ((tconv_id = H5TB_get_buf(target_size, 1, (void **)&tconv_buf))<0) {
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
                 "memory allocation failed for type conversion");
        }
    }
    if (need_bkg && NULL==(bkg_buf=xfer_parms->bkg_buf)) {
        /* Allocate temporary buffer */
        if ((bkg_id=H5TB_get_buf(request_nelmts*dst_type_size, 1,
				 (void **)&bkg_buf))<0) {
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
                 "memory allocation failed for type conversion");
        }
    }

#ifdef QAK
    printf("%s: check 4.0, nelmts=%d, need_bkg=%d\n",
	   FUNC,(int)nelmts,(int)need_bkg);
#endif
    
    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        smine_nelmts = (sconv->f->avail)(file_space,&file_iter,
					 MIN(request_nelmts,
					     (nelmts-smine_start)));
#ifdef QAK
	printf("%s: check 5.0, nelmts=%d, smine_start=%d, smine_nelmts=%d\n",
	       FUNC,(int)nelmts,(int)smine_start,(int)smine_nelmts);
#endif
	
        /*
         * Gather the data from disk into the data type conversion
         * buffer. Also gather data from application to background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        n = (sconv->f->gath)(dataset->ent.file, &(dataset->layout),
			     &(dataset->create_parms->pline),
			     &(dataset->create_parms->fill),
			     &(dataset->create_parms->efl), src_type_size,
			     file_space, &file_iter, smine_nelmts,
			     xfer_parms, tconv_buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].gath_timer), &timer);
	sconv->stats[1].gath_nbytes += n * src_type_size;
	sconv->stats[1].gath_ncalls++;
#endif
	if (n!=smine_nelmts) {
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");
        }
	
#ifdef QAK
	printf("%s: check 6.0\n",FUNC);
	printf("%s: check 6.5\n",FUNC);
	{
	    int i;
	    uint16 *b;

	    if(qak_debug) {
		b=tconv_buf;
		printf("\ntconv_buf:");
		for (i=0; i<smine_nelmts; i++,b++) {
		    printf("(%d)%u ",i,(unsigned)*b);
		}
		printf("\n");
	    }
	}
#endif
	
        if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
	    H5_timer_begin(&timer);
#endif
            n = (sconv->m->gath)(buf, dst_type_size, mem_space, &bkg_iter,
				 smine_nelmts, bkg_buf/*out*/);
#ifdef H5S_DEBUG
	    H5_timer_end(&(sconv->stats[1].bkg_timer), &timer);
	    sconv->stats[1].bkg_nbytes += n * dst_type_size;
	    sconv->stats[1].bkg_ncalls++;
#endif
	    if (n!=smine_nelmts) {
                HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed");
            }
        }
	
#ifdef QAK
	printf("%s: check 7.0\n",FUNC);
#endif

        /*
         * Perform data type conversion.
         */
#ifdef H5T_DEBUG
        H5T_timer_begin (&timer, cdata);
#endif
        cdata->command = H5T_CONV_CONV;
        status = (tconv_func)(src_id, dst_id, cdata, smine_nelmts, tconv_buf,
			      bkg_buf);
#ifdef H5T_DEBUG
        H5T_timer_end (&timer, cdata, smine_nelmts);
#endif
        if (status<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                "data type conversion failed");
        }

#ifdef QAK
	printf("%s: check 8.0\n",FUNC);
#endif
	
        /*
         * Scatter the data into memory.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        status = (sconv->m->scat)(tconv_buf, dst_type_size, mem_space,
				  &mem_iter, smine_nelmts, buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[1].scat_timer), &timer);
	sconv->stats[1].scat_nbytes += smine_nelmts * dst_type_size;
	sconv->stats[1].scat_ncalls++;
#endif
	if (status<0) {
            HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "scatter failed");
        }
	
#ifdef QAK
	printf("%s: check 9.0\n",FUNC);
#endif /* QAK */
    }
    
 succeed:
    ret_value = SUCCEED;
    
 done:
    /* Release selection iterators */
    H5S_sel_iter_release(file_space,&file_iter);
    H5S_sel_iter_release(mem_space,&mem_iter);
    H5S_sel_iter_release(mem_space,&bkg_iter);

    if (src_id >= 0) H5I_dec_ref(src_id);
    if (dst_id >= 0) H5I_dec_ref(dst_id);
    if (tconv_buf && NULL==xfer_parms->tconv_buf) H5TB_release_buf(tconv_id);
    if (bkg_buf && NULL==xfer_parms->bkg_buf) H5TB_release_buf (bkg_id);
    if (free_this_space) H5S_close (free_this_space);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. See H5Dwrite() for complete details.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 * 	Robb Matzke, 9 Jun 1998
 *	The data space is no longer cached in the dataset struct.
 *
 * 	rky 980918
 *	Added must_convert to do non-optimized read when necessary.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_write(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	  const H5S_t *file_space, const H5D_xfer_t *xfer_parms,
	  const void *buf)
{
    hsize_t		nelmts;			/*total number of elmts	*/
    size_t		smine_start;		/*strip mine start loc	*/
    size_t		n, smine_nelmts;	/*elements per strip	*/
    hid_t       tconv_id=FAIL, bkg_id=FAIL;   /* Conversion buffer IDs */
    uint8		*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8		*bkg_buf = NULL;	/*background buffer	*/
    H5T_conv_t		tconv_func = NULL;	/*conversion function	*/
    hid_t		src_id = -1, dst_id = -1;/*temporary type atoms */
    H5S_conv_t		*sconv=NULL;		/*space conversion funcs*/
    H5S_sel_iter_t	mem_iter;      /*memory selection iteration info*/
    H5S_sel_iter_t	bkg_iter;            /*background iteration info*/
    H5S_sel_iter_t	file_iter;       /*file selection iteration info*/
    H5T_cdata_t		*cdata = NULL;		/*type conversion data	*/
    herr_t		ret_value = FAIL, status;
    size_t		src_type_size;		/*size of source type	*/
    size_t		dst_type_size;	      /*size of destination type*/
    size_t		target_size;		/*desired buffer size	*/
    size_t		request_nelmts;		/*requested strip mine	*/
    H5T_bkg_t		need_bkg;		/*type of background buf*/
    H5S_t		*free_this_space=NULL;	/*data space to free	*/
    hbool_t             must_convert;           /*have to xfer the slow way */
#ifdef H5T_DEBUG
    H5_timer_t		timer;
#endif

    FUNC_ENTER(H5D_write, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(xfer_parms);
    assert(buf);

    /* Initialize these before any errors can occur */
    HDmemset(&mem_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&bkg_iter,0,sizeof(H5S_sel_iter_t));
    HDmemset(&file_iter,0,sizeof(H5S_sel_iter_t));

    if (0==(dataset->ent.file->intent & H5F_ACC_RDWR)) {
	HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
		    "no write intent on file");
    }

    if (!file_space) {
	if (NULL==(free_this_space=H5S_read (&(dataset->ent)))) {
	    HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
			 "unable to read data space from dataset header");
	}
	file_space = free_this_space;
    }
    if (!mem_space) mem_space = file_space;
    nelmts = H5S_get_select_npoints(mem_space);

#ifdef HAVE_PARALLEL
    /*
     * Check if collective data transfer requested.
     */
    if (xfer_parms->xfer_mode == H5D_XFER_COLLECTIVE){
	/*
	 * Verify that the file can support collective access.  The check may
	 * not be necessarily since collective access can always be simulated
	 * by independent access.  Nevertheless, must check driver is MPIO
	 * before using those access_mode which exists only for MPIO case.
	 */
	if (dataset->ent.file->shared->access_parms->driver != H5F_LOW_MPIO)
	    HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
			 "collective access not permissible");
    }
#endif

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information. If a data
     * type conversion is necessary then register data type atoms. Data type
     * conversion is necessary if the user has set the `need_bkg' to a high
     * enough value in xfer_parms since turning off data type conversion also
     * turns off background preservation.
     */
#ifdef QAK
    printf("%s: check 0.5, nelmts=%d, mem_space->rank=%d\n", FUNC,
	   (int)nelmts, mem_space->extent.u.simple.rank);
#endif /* QAK */
    if (nelmts!=H5S_get_select_npoints (file_space)) {
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "src and dest data spaces have different sizes");
    }
    if (NULL == (tconv_func = H5T_find(mem_type, dataset->type,
				       xfer_parms->need_bkg, &cdata))) {
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		    "unable to convert between src and dest data types");
    } else if (H5T_conv_noop!=tconv_func) {
	if ((src_id = H5I_register(H5I_DATATYPE,
				   H5T_copy(mem_type, H5T_COPY_ALL)))<0 ||
	    (dst_id = H5I_register(H5I_DATATYPE,
				   H5T_copy(dataset->type, H5T_COPY_ALL)))<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
			"unable to register types for conversion");
	}
    }
#ifdef QAK
    printf("%s: after H5T_find, tconv_func=%p\n",FUNC,tconv_func);
#endif /* QAK */
    if (NULL==(sconv=H5S_find(mem_space, file_space))) {
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "unable to convert from memory to file data space");
    }
#ifdef HAVE_PARALLEL
    /* rky 980813 This is a temporary KLUGE.
     * The sconv functions should be set by H5S_find,
     * or we should use a different way to call the MPI-IO
     * mem-and-file-dataspace-xfer functions
     * (the latter in case the arguments to sconv_funcs
     * turn out to be inappropriate for MPI-IO).  */
    if (H5_mpi_opt_types_g &&
        H5F_LOW_MPIO==dataset->ent.file->shared->access_parms->driver) {
	sconv->write = H5S_mpio_spaces_write;
    }
#endif /*HAVE_PARALLEL*/
    
    /*
     * If there is no type conversion then try writing directly from
     * application buffer to file.
     */
    if (H5T_conv_noop==tconv_func && sconv->write) {
	status = (sconv->write)(dataset->ent.file, &(dataset->layout),
				&(dataset->create_parms->pline),
				&(dataset->create_parms->efl),
				H5T_get_size (dataset->type), file_space,
				mem_space, xfer_parms->xfer_mode, buf,
				&must_convert /*out*/ );
        if (status<0) {
	    /* Supports only no conversion, type or space, for now. */
	    HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL,
		        "optimized write failed");
	}
	if (must_convert) {
	    /* sconv->write cannot do a direct transfer;
	     * fall through and xfer the data in the more roundabout way */
	} else {
	    /* direct xfer accomplished successfully */
	    goto succeed;
	}
#ifdef H5D_DEBUG
	if (H5DEBUG(D)) {
	    fprintf (H5DEBUG(D), "H5D: data space conversion could not be "
		     "optimized for this case (using general method "
		     "instead)\n");
	}
#endif
	H5E_clear ();
    }

    /*
     * This is the general case.  Figure out the strip mine size.
     */
    src_type_size = H5T_get_size(mem_type);
    dst_type_size = H5T_get_size(dataset->type);
    target_size = xfer_parms->buf_size;
    request_nelmts = target_size / MAX (src_type_size, dst_type_size);
    if (request_nelmts<=0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "temporary buffer max size is too small");
    }
    if ((sconv->f->init)(&(dataset->layout), file_space, &file_iter)<0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize file selection information");
    } 
    if ((sconv->m->init)(&(dataset->layout), mem_space, &mem_iter)<0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize memory selection information");
    } 
    if ((sconv->f->init)(&(dataset->layout), file_space, &bkg_iter)<0) {
        HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize memory selection information");
    }
    
#ifdef QAK
    printf("%s: check 3.0, request_nelmts=%d\n",FUNC,(int)request_nelmts);
#endif

    /*
     * Get a temporary buffer for type conversion unless the app has already
     * supplied one through the xfer properties.  Instead of allocating a
     * buffer which is the exact size, we allocate the target size. The
     * malloc() is usually less resource-intensive if we allocate/free the
     * same size over and over.
     */
    if (cdata->need_bkg) {
        need_bkg = MAX (cdata->need_bkg, xfer_parms->need_bkg);
    } else {
        need_bkg = H5T_BKG_NO; /*never needed even if app says yes*/
    }
    if (NULL==(tconv_buf=xfer_parms->tconv_buf)) {
        /* Allocate temporary buffer */
        if ((tconv_id=H5TB_get_buf(target_size, 1, (void **)&tconv_buf))<0) {
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
                 "memory allocation failed for type conversion");
        }
    }
    if (need_bkg && NULL==(bkg_buf=xfer_parms->bkg_buf)) {
        /* Allocate temporary buffer */
        if ((bkg_id=H5TB_get_buf(request_nelmts*dst_type_size, 1,
				 (void **)&bkg_buf))<0) {
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
                 "memory allocation failed for type conversion");
        }
    }

#ifdef QAK
    printf("%s: check 4.0, nelmts=%d, need_bkg=%d\n",
	   FUNC,(int)nelmts,(int)need_bkg);
#endif

    /* Start strip mining... */
    for (smine_start=0; smine_start<nelmts; smine_start+=smine_nelmts) {
        /* Go figure out how many elements to read from the file */
        smine_nelmts = (sconv->f->avail)(file_space,&file_iter,
					 MIN(request_nelmts,
					     (nelmts-smine_start)));
	
#ifdef QAK
	printf("%s: check 5.0, nelmts=%d, smine_start=%d, smine_nelmts=%d\n",
	       FUNC,(int)nelmts,(int)smine_start,(int)smine_nelmts);
#endif
	
        /*
         * Gather data from application buffer into the data type conversion
         * buffer. Also gather data from the file into the background buffer
         * if necessary.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
        n = (sconv->m->gath)(buf, src_type_size, mem_space, &mem_iter,
			     smine_nelmts, tconv_buf/*out*/);
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].gath_timer), &timer);
	sconv->stats[0].gath_nbytes += n * src_type_size;
	sconv->stats[0].gath_ncalls++;
#endif
	if (n!=smine_nelmts) {
            HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed");
        }

#ifdef QAK
	{
	    int i;
	    uint16 *b;

	    if(qak_debug) {
		b=tconv_buf;
		printf("\ntconv_buf:");
		for (i=0; i<smine_nelmts; i++,b++) {
		    printf("(%d)%d ",i,(int)*b);
		}
		printf("\n");
	    }
	}
	printf("%s: check 6.0\n",FUNC);
#endif
	
        if (H5T_BKG_YES==need_bkg) {
#ifdef H5S_DEBUG
	    H5_timer_begin(&timer);
#endif
            n = (sconv->f->gath)(dataset->ent.file, &(dataset->layout),
				 &(dataset->create_parms->pline),
				 &(dataset->create_parms->fill),
				 &(dataset->create_parms->efl), dst_type_size,
				 file_space, &bkg_iter, smine_nelmts,
				 xfer_parms, bkg_buf/*out*/);
#ifdef H5S_DEBUG
	    H5_timer_end(&(sconv->stats[0].bkg_timer), &timer);
	    sconv->stats[0].bkg_nbytes += n * dst_type_size;
	    sconv->stats[0].bkg_ncalls++;
#endif
	    if (n!=smine_nelmts) {
                HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL,
			     "file gather failed");
            }
        }

        /*
         * Perform data type conversion.
         */
#ifdef H5T_DEBUG
        H5T_timer_begin (&timer, cdata);
#endif
        cdata->command = H5T_CONV_CONV;
        status = (tconv_func) (src_id, dst_id, cdata, smine_nelmts, tconv_buf,
			       bkg_buf);
#ifdef H5T_DEBUG
        H5T_timer_end (&timer, cdata, smine_nelmts);
#endif
        if (status<0) {
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                "data type conversion failed");
        }
#ifdef QAK
	printf("%s: check 6.3\n",FUNC);
#endif

        /*
         * Scatter the data out to the file.
         */
#ifdef H5S_DEBUG
	H5_timer_begin(&timer);
#endif
	status = (sconv->f->scat)(dataset->ent.file, &(dataset->layout),
				  &(dataset->create_parms->pline),
				  &(dataset->create_parms->fill),
				  &(dataset->create_parms->efl), dst_type_size,
				  file_space, &file_iter, smine_nelmts,
				  xfer_parms, tconv_buf);
#ifdef QAK
	printf("%s: check 6.35\n",FUNC);
#endif
#ifdef H5S_DEBUG
	H5_timer_end(&(sconv->stats[0].scat_timer), &timer);
	sconv->stats[0].scat_nbytes += smine_nelmts * dst_type_size;
	sconv->stats[0].scat_ncalls++;
#endif
	if (status<0) {
            HGOTO_ERROR (H5E_DATASET, H5E_WRITEERROR, FAIL, "scatter failed");
        }
    }

#ifdef QAK
	printf("%s: check 6.4\n",FUNC);
#endif
    /*
     * Update modification time.  We have to do this explicitly because
     * writing to a dataset doesn't necessarily change the object header.
     */
    if (H5O_touch(&(dataset->ent), FALSE)<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		    "unable to update modification time");
    }

 succeed:
    ret_value = SUCCEED;
    
 done:
    /* Release selection iterators */
    H5S_sel_iter_release(file_space,&file_iter);
    H5S_sel_iter_release(mem_space,&mem_iter);
    H5S_sel_iter_release(file_space,&bkg_iter);

    if (src_id >= 0) H5I_dec_ref(src_id);
    if (dst_id >= 0) H5I_dec_ref(dst_id);
    if (tconv_buf && NULL==xfer_parms->tconv_buf) H5TB_release_buf(tconv_id);
    if (bkg_buf && NULL==xfer_parms->bkg_buf) H5TB_release_buf (bkg_id);
    if (free_this_space) H5S_close (free_this_space);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_extend
 *
 * Purpose:	Increases the size of a dataset.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_extend (H5D_t *dataset, const hsize_t *size)
{
    herr_t	changed, ret_value=FAIL;
    H5S_t	*space = NULL;

    FUNC_ENTER (H5D_extend, FAIL);

    /* Check args */
    assert (dataset);
    assert (size);

    /*
     * NOTE: Restrictions on extensions were checked when the dataset was
     *	     created.  All extensions are allowed here since none should be
     *	     able to muck things up.
     */

    /* Increase the size of the data space */
    if (NULL==(space=H5S_read (&(dataset->ent)))) {
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to read data space info from dataset header");
    }
    if ((changed=H5S_extend (space, size))<0) {
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to increase size of data space");
    }

    if (changed>0){
	/* Save the new dataspace in the file if necessary */
	if (H5S_modify (&(dataset->ent), space)<0) {
	    HGOTO_ERROR (H5E_DATASET, H5E_WRITEERROR, FAIL,
		       "unable to update file with new dataspace");
	}

	/* Initialize the new parts of the dataset */
#ifdef LATER
	if (H5S_select_all(space)<0 ||
	    H5S_select_hyperslab(space, H5S_SELECT_DIFF, zero, NULL,
				 old_dims, NULL)<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
			"unable to select new extents for fill value");
	}
#else
	/*
	 * We don't have the H5S_SELECT_DIFF operator yet.  We really only
	 * need it for contiguous datasets because the chunked datasets will
	 * either fill on demand during I/O or attempt a fill of all chunks.
	 */
	if (H5D_CONTIGUOUS==dataset->layout.type &&
	    dataset->create_parms->fill.buf) {
	    HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
			"unable to select fill value region");
	}
#endif
	if (H5D_init_storage(dataset, space)<0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
			"unable to initialize dataset with fill value");
	}
    }


    ret_value = SUCCEED;

 done:
    H5S_close (space);
    FUNC_LEAVE (ret_value);
}
    

/*-------------------------------------------------------------------------
 * Function:	H5D_entof
 *
 * Purpose:	Returns a pointer to the entry for a dataset.
 *
 * Return:	Success:	Ptr to entry
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, April 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5D_entof (H5D_t *dataset)
{
    return dataset ? &(dataset->ent) : NULL;
}


/*-------------------------------------------------------------------------
 * Function:	H5D_typeof
 *
 * Purpose:	Returns a pointer to the dataset's data type.  The data type
 *		is not copied.
 *
 * Return:	Success:	Ptr to the dataset's data type, uncopied.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, June  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5D_typeof (H5D_t *dset)
{
    FUNC_ENTER (H5D_typeof, NULL);
    assert (dset);
    assert (dset->type);
    FUNC_LEAVE (dset->type);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_get_file
 *
 * Purpose:	Returns the dataset's file pointer.
 *
 * Return:	Success:	Ptr to the dataset's file pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, October 22, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5D_get_file (const H5D_t *dset)
{
    FUNC_ENTER (H5D_get_file, NULL);
    assert (dset);
    assert (dset->ent.file);
    FUNC_LEAVE (dset->ent.file);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_init_storage
 *
 * Purpose:	Initialize the data for a new dataset.  If a selection is
 *		defined for SPACE then initialize only that part of the
 *		dataset.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, October  5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_init_storage(H5D_t *dset, const H5S_t *space)
{
    intn		ndims;
    hsize_t		dim[H5O_LAYOUT_NDIMS];
    hsize_t		npoints, ptsperbuf;
    size_t		size, bufsize=8*1024;
    hid_t		buf_id = -1;
    haddr_t		addr;
    herr_t		ret_value = FAIL;
    void		*buf = NULL;
    
    FUNC_ENTER(H5D_init_storage, FAIL);
    assert(dset);
    assert(space);

    switch (dset->layout.type) {
    case H5D_CONTIGUOUS:
	/*
	 * If the fill value is set then write it to the specified selection
	 * even if it is all zero.  This allows the application to force
	 * filling when the underlying storage isn't initialized to zero.
	 */
	npoints = H5S_get_simple_extent_npoints(space);
	if (dset->create_parms->fill.buf &&
	    npoints==H5S_get_select_npoints(space)) {
	    /*
	     * Fill the entire current extent with the fill value.  We can do
	     * this quite efficiently by making sure we copy the fill value
	     * in relatively large pieces.
	     */
	    ptsperbuf = MAX(1, bufsize/dset->create_parms->fill.size);
	    bufsize = ptsperbuf * dset->create_parms->fill.size;
	    if ((buf_id=H5TB_get_buf(bufsize, TRUE, &buf))<0) {
		HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
			    "unable to get buffer for fill value");
	    }
	    H5V_array_fill(buf, dset->create_parms->fill.buf,
			   dset->create_parms->fill.size, ptsperbuf);
	    if (dset->create_parms->efl.nused) {
		H5F_addr_reset(&addr);
	    } else {
		addr = dset->layout.addr;
	    }
	    
	    while (npoints>0) {
		size = MIN(ptsperbuf, npoints) * dset->create_parms->fill.size;
		if (dset->create_parms->efl.nused) {
		    if (H5O_efl_write(dset->ent.file,
				      &(dset->create_parms->efl),
				      &addr, size, buf)<0) {
			HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
				    "unable to write fill value to dataset");
		    }
		} else {
		    if (H5F_block_write(dset->ent.file, &addr, size,
					H5D_XFER_DFLT, buf)<0) {
			HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
				    "unable to write fill value to dataset");
		    }
		}
		npoints -= MIN(ptsperbuf, npoints);
		H5F_addr_inc(&addr, size);
	    }
	} else if (dset->create_parms->fill.buf) {
	    /*
	     * Fill the specified selection with the fill value.
	     */
	    HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
			"unable to initialize dataset with fill value");
	}
	break;

    case H5D_CHUNKED:
	/*
	 * If the dataset is accessed via parallel I/O, allocate file space
	 * for all chunks now and initialize each chunk with the fill value.
	 */
	if (H5F_LOW_MPIO==dset->ent.file->shared->access_parms->driver) {
	    /* We only handle simple data spaces so far */
	    if ((ndims=H5S_get_simple_extent_dims(space, dim, NULL))<0) {
		HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
			    "unable to get simple data space info");
	    }
	    dim[ndims] = dset->layout.dim[ndims];
	    ndims++;

	    if (H5F_istore_allocate(dset->ent.file, &(dset->layout),
				    dim, H5D_xfer_dflt.split_ratios,
				    &(dset->create_parms->pline),
				    &(dset->create_parms->fill))<0) {
		HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
			    "unable to allocate all chunks of dataset");
	    }
	}
	break;
    }
    ret_value = SUCCEED;

 done:
    if (buf_id>=0 && H5TB_release_buf(buf_id)<0) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		      "unable to release fill value temporary buffer");
    }
    FUNC_LEAVE(ret_value);
}

