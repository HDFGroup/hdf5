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

#include <H5private.h>		/* Generic Functions			  */
#include <H5Aprivate.h>		/* Atoms			  */
#include <H5ACprivate.h>	/* Cache			  */
#include <H5Pprivate.h>		/* Templates				  */
#include <H5Dprivate.h>		/* Dataset functions			  */
#include <H5Eprivate.h>		/* Error handling		  */
#include <H5Gprivate.h>		/* Group headers		  */
#include <H5Mprivate.h>		/* Meta data				  */
#include <H5MFprivate.h>	/* File space allocation header		  */
#include <H5MMprivate.h>	/* Memory management			  */
#include <H5Mprivate.h>		/* Meta-Object API			  */
#include <H5Oprivate.h>		/* Object headers		  */

#define PABLO_MASK	H5D_mask

/*
 * A dataset is the following struct.
 */
struct H5D_t {
    H5G_entry_t		    ent;	/*cached object header stuff	*/
    H5T_t		   *type;	/*datatype of this dataset	*/
    H5S_t		   *space;	/*dataspace of this dataset	*/
    H5D_create_t	    create_parms; /*creation parameters		*/
    H5O_layout_t	    layout;	/*data layout			*/
};

/* Default dataset creation template */
const H5D_create_t	H5D_create_dflt =
{
    H5D_CONTIGUOUS,		/* Layout				*/
    1,				/* Chunk dimensions			*/
    {1, 1, 1, 1, 1, 1, 1, 1,	/* Chunk size.	These default values.... */
     1, 1, 1, 1, 1, 1, 1, 1,	/*...are quite useless.	 Larger chunks.. */
     1, 1, 1, 1, 1, 1, 1, 1,	/*...produce fewer, but larger I/O...... */
     1, 1, 1, 1, 1, 1, 1, 1},	/*...requests.				*/
};

/* Default dataset transfer template */
const H5D_xfer_t	H5D_xfer_dflt =
{
    0,				/* Place holder - remove this later	*/
};

/* Interface initialization? */
static hbool_t		interface_initialize_g = FALSE;
#define INTERFACE_INIT H5D_init_interface
static herr_t		H5D_init_interface(void);
static void		H5D_term_interface(void);


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
    if ((ret_value = H5A_init_group(H5_DATASET, H5A_DATASETID_HASHSIZE,
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
    H5A_destroy_group(H5_DATASET);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dcreate
 *
 * Purpose:	Creates a new dataset named NAME in file FILE_ID, opens the
 *		dataset for access, and associates with that dataset constant
 *		and initial persistent properties including the type of each
 *		datapoint as stored in the file (TYPE_ID), the size of the
 *		dataset (SPACE_ID), and other initial miscellaneous
 *		properties (CREATE_PARMS_ID).
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
 *		ARGS	  BADTYPE	Not a dataset creation template. 
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
H5Dcreate(hid_t file_id, const char *name, hid_t type_id, hid_t space_id,
	  hid_t create_parms_id)
{
    H5F_t		   *f = NULL;
    H5T_t		   *type = NULL;
    H5S_t		   *space = NULL;
    H5D_t		   *new_dset = NULL;
    hid_t		    ret_value = FAIL;
    const H5D_create_t	   *create_parms = NULL;

    FUNC_ENTER(H5Dcreate, FAIL);

    /* Check arguments */
    if (H5_FILE != H5A_group(file_id) ||
	NULL == (f = H5A_object(file_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    if (H5_DATATYPE != H5A_group(type_id) ||
	NULL == (type = H5A_object(type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a type");
    }
    if (H5_DATASPACE != H5A_group(space_id) ||
	NULL == (space = H5A_object(space_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
    }
    if (create_parms_id >= 0) {
	if (H5P_DATASET_CREATE != H5Pget_class(create_parms_id) ||
	    NULL == (create_parms = H5A_object(create_parms_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "not a dataset creation template");
	}
    } else {
	create_parms = &H5D_create_dflt;
    }

    /* build and open the new dataset */
    if (NULL == (new_dset = H5D_create(f, name, type, space, create_parms))) {
	HRETURN_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't create dataset");
    }
    /* Register the new datatype and get an ID for it */
    if ((ret_value = H5A_register(H5_DATASET, new_dset)) < 0) {
	H5D_close(new_dset);
	HRETURN_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
		      "can't register dataset");
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dopen
 *
 * Purpose:	Finds a dataset named NAME in file FILE_ID, opens it, and
 *		returns its ID.	 The dataset should be close when the caller
 *		is no longer interested in it.
 *
 * Return:	Success:	A new dataset ID
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS	  BADTYPE	Not a file. 
 *		ARGS	  BADVALUE	No name. 
 *		DATASET	  CANTREGISTER	Can't register dataset. 
 *		DATASET	  NOTFOUND	Dataset not found. 
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen(hid_t file_id, const char *name)
{
    H5F_t	*file = NULL;		/*file holding the dataset	*/
    H5D_t	*dataset = NULL;	/*the dataset			*/
    hid_t	ret_value = FAIL;

    FUNC_ENTER(H5Dopen, FAIL);

    /* Check args */
    if (H5_FILE != H5A_group(file_id) ||
	NULL == (file = H5A_object(file_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    
    /* Find the dataset */
    if (NULL == (dataset = H5D_open(file, name))) {
	HRETURN_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "dataset not found");
    }
    
    /* Create an atom for the dataset */
    if ((ret_value = H5A_register(H5_DATASET, dataset)) < 0) {
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
H5Dclose(hid_t dataset_id)
{
    H5D_t		   *dataset = NULL;	/* dataset object to release */

    FUNC_ENTER(H5Dclose, FAIL);

    /* Check args */
    if (H5_DATASET != H5A_group(dataset_id) ||
	NULL == (dataset = H5A_object(dataset_id)) ||
	NULL == dataset->ent.file) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    if (H5A_dec_ref(dataset_id) < 0) {
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
 *              Wednesday, January 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_space (hid_t dataset_id)
{
    H5D_t	*dataset = NULL;
    H5S_t	*copied_space = NULL;
    hid_t	ret_value = FAIL;
    
    FUNC_ENTER (H5Dget_space, FAIL);

    /* Check args */
    if (H5_DATASET!=H5A_group (dataset_id) ||
	NULL==(dataset=H5A_object (dataset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }

    /* Copy the data space */
    if (NULL==(copied_space=H5S_copy (dataset->space))) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to copy the data space");
    }

    /* Create an atom */
    if ((ret_value=H5A_register (H5_DATASPACE, copied_space))<0) {
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register data space");
    }

    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dget_type
 *
 * Purpose:	Returns a copy of the file data type for a dataset.
 *
 * Return:	Success:	ID for a copy of the data type.  The data
 *				type should be released by calling
 *				H5Tclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, February  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_type (hid_t dataset_id)
{
    
    H5D_t	*dataset = NULL;
    H5T_t	*copied_type = NULL;
    hid_t	ret_value = FAIL;
    
    FUNC_ENTER (H5Dget_type, FAIL);

    /* Check args */
    if (H5_DATASET!=H5A_group (dataset_id) ||
	NULL==(dataset=H5A_object (dataset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }

    /* Copy the data type */
    if (NULL==(copied_type=H5T_copy (dataset->type))) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to copy the data type");
    }

    /* Create an atom */
    if ((ret_value=H5A_register (H5_DATATYPE, copied_type))<0) {
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register data type");
    }

    FUNC_LEAVE (ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dget_create_parms
 *
 * Purpose:	Returns a copy of the dataset creation template.
 *
 * Return:	Success:	ID for a copy of the dataset creation
 *				template.  The template should be released by
 *				calling H5Pclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, February  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dget_create_parms (hid_t dataset_id)
{
    H5D_t		*dataset = NULL;
    H5D_create_t	*copied_parms = NULL;
    hid_t		ret_value = FAIL;
    
    FUNC_ENTER (H5Dget_create_parms, FAIL);

    /* Check args */
    if (H5_DATASET!=H5A_group (dataset_id) ||
	NULL==(dataset=H5A_object (dataset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }

    /* Copy the creation template */
    if (NULL==(copied_parms=H5P_copy (H5P_DATASET_CREATE,
				      &(dataset->create_parms)))) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to copy the creation template");
    }

    /* Create an atom */
    if ((ret_value=H5A_register ((group_t)(H5_TEMPLATE_0+H5P_DATASET_CREATE),
				 copied_parms))<0) {
	HRETURN_ERROR (H5E_ATOM, H5E_CANTREGISTER, FAIL,
		       "unable to register creation template");
    }

    FUNC_LEAVE (ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DATASET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		MEM_SPACE_ID and FILE_SPACE_ID.	 The data points are
 *		converted from their file type to the MEM_TYPE_ID specified. 
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the XFER_PARMS_ID argument.
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
 *		The XFER_PARMS_ID can be the constant H5P_DEFAULT in which
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
H5Dread(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id,
	hid_t file_space_id, hid_t xfer_parms_id, void *buf/*out*/)
{
    H5D_t		   *dataset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    const H5D_xfer_t	   *xfer_parms = NULL;

    FUNC_ENTER(H5Dread, FAIL);

    /* check arguments */
    if (H5_DATASET != H5A_group(dataset_id) ||
	NULL == (dataset = H5A_object(dataset_id)) ||
	NULL == dataset->ent.file) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    if (H5_DATATYPE != H5A_group(mem_type_id) ||
	NULL == (mem_type = H5A_object(mem_type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (H5S_ALL != mem_space_id) {
	if (H5_DATASPACE != H5A_group(mem_space_id) ||
	    NULL == (mem_space = H5A_object(mem_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
    }
    if (H5S_ALL != file_space_id) {
	if (H5_DATASPACE != H5A_group(file_space_id) ||
	    NULL == (file_space = H5A_object(file_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
    }
    if (H5P_DEFAULT == xfer_parms_id) {
	xfer_parms = &H5D_xfer_dflt;
    } else if (H5P_DATASET_XFER != H5Pget_class(xfer_parms_id) ||
	       NULL == (xfer_parms = H5A_object(xfer_parms_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    }
    if (!buf) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
    }

    /* read raw data */
    if (H5D_read(dataset, mem_type, mem_space, file_space, xfer_parms,
		 buf/*out*/) < 0) {
	HRETURN_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't read data");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DATASET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		MEM_SPACE_ID and FILE_SPACE_ID arguments. The data points
 *		are converted from their current type (MEM_TYPE_ID) to their
 *		file data type.	 Additional miscellaneous data transfer
 *		properties can be passed to this function with the
 *		XFER_PARMS_ID argument.
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
 *		The XFER_PARMS_ID can be the constant H5P_DEFAULT in which
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
H5Dwrite(hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id,
	 hid_t file_space_id, hid_t xfer_parms_id, const void *buf)
{
    H5D_t		   *dataset = NULL;
    const H5T_t		   *mem_type = NULL;
    const H5S_t		   *mem_space = NULL;
    const H5S_t		   *file_space = NULL;
    const H5D_xfer_t	   *xfer_parms = NULL;

    FUNC_ENTER(H5Dwrite, FAIL);

    /* check arguments */
    if (H5_DATASET != H5A_group(dataset_id) ||
	NULL == (dataset = H5A_object(dataset_id)) ||
	NULL == dataset->ent.file) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    if (H5_DATATYPE != H5A_group(mem_type_id) ||
	NULL == (mem_type = H5A_object(mem_type_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (H5S_ALL != mem_space_id) {
	if (H5_DATASPACE != H5A_group(mem_space_id) ||
	    NULL == (mem_space = H5A_object(mem_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
    }
    if (H5S_ALL != file_space_id) {
	if (H5_DATASPACE != H5A_group(file_space_id) ||
	    NULL == (file_space = H5A_object(file_space_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
	}
    }
    if (H5P_DEFAULT == xfer_parms_id) {
	xfer_parms = &H5D_xfer_dflt;
    } else if (H5P_DATASET_XFER != H5Pget_class(xfer_parms_id) ||
	       NULL == (xfer_parms = H5A_object(xfer_parms_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
    }
    if (!buf) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
    }

    /* write raw data */
    if (H5D_write(dataset, mem_type, mem_space, file_space, xfer_parms,
		  buf) < 0) {
	HRETURN_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "can't write data");
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
 *              Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dextend (hid_t dataset_id, const size_t *size)
{
    H5D_t	*dataset = NULL;
    
    FUNC_ENTER (H5Dextend, FAIL);

    /* Check args */
    if (H5_DATASET!=H5A_group (dataset_id) ||
	NULL==(dataset=H5A_object (dataset_id))) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
    }
    if (!size) {
	HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no size specified");
    }

    /* Increase size */
    if (H5D_extend (dataset, size)<0) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to extend dataset");
    }

    FUNC_LEAVE (SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5D_find_name
 *
 * Purpose:	This is a callback for H5Mfind_name().	It does the same
 *		thing as H5Dopen() except it takes an extra argument which
 *		isn't used.
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
hid_t
H5D_find_name(hid_t file_id, group_t UNUSED, const char *name)
{
    return H5Dopen(file_id, name);
}

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
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_create(H5F_t *f, const char *name, const H5T_t *type, const H5S_t *space,
	   const H5D_create_t *create_parms)
{
    H5D_t		   *new_dset = NULL;
    H5D_t		   *ret_value = NULL;
    intn		    i;

    FUNC_ENTER(H5D_create, NULL);

    /* check args */
    assert(f);
    assert(name && *name);
    assert(type);
    assert(space);
    assert(create_parms);

    /* Initialize the dataset object */
    new_dset = H5MM_xcalloc(1, sizeof(H5D_t));
    H5F_addr_undef(&(new_dset->ent.header));
    new_dset->type = H5T_copy(type);
    new_dset->space = H5S_copy(space);
    new_dset->create_parms = *create_parms;

    /*
     * Create (open for write access) an object header.
     */
    if (H5O_create(f, 0, &(new_dset->ent)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to create dataset object header");
    }
    /* Update the type and space header messages */
    if (H5O_modify(&(new_dset->ent), H5O_DTYPE, 0, 0, new_dset->type) < 0 ||
	H5S_modify(&(new_dset->ent), new_dset->space) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "can't update type or space header messages");
    }

    /* Total raw data size */
    new_dset->layout.type = new_dset->create_parms.layout;
    new_dset->layout.ndims = H5S_get_ndims(space) + 1;
    assert(new_dset->layout.ndims <= NELMTS(new_dset->layout.dim));
    new_dset->layout.dim[new_dset->layout.ndims - 1] = H5T_get_size(type);

    switch (new_dset->create_parms.layout) {
    case H5D_CONTIGUOUS:
	if (H5S_get_dims(space, new_dset->layout.dim) < 0) {
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
			"unable to initialize contiguous storage");
	}
	break;

    case H5D_CHUNKED:
	if (new_dset->create_parms.chunk_ndims != H5S_get_ndims(space)) {
	    HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, NULL,
		   "dimensionality of chunks doesn't match the data space");
	}
	for (i = 0; i < new_dset->layout.ndims - 1; i++) {
	    new_dset->layout.dim[i] = new_dset->create_parms.chunk_size[i];
	}
	break;

    default:
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
    }

    /*
     * Initialize storage
     */
    if (H5F_arr_create(f, &(new_dset->layout)) < 0 ||
	H5O_modify(&(new_dset->ent), H5O_LAYOUT, 0, 0,
		   &(new_dset->layout)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to initialize storage");
    }

    /* Give the dataset a name */
    if (H5G_insert(name, &(new_dset->ent)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL, "unable to name dataset");
    }

    /* Success */
    ret_value = new_dset;

  done:
    if (!ret_value && new_dset) {
	if (new_dset->type)
	    H5T_close(new_dset->type);
	if (new_dset->space)
	    H5S_close(new_dset->space);
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
 *
 *-------------------------------------------------------------------------
 */
H5D_t		       *
H5D_open(H5F_t *f, const char *name)
{
    H5D_t	*dataset = NULL;	/*the dataset which was found	*/
    H5D_t	*ret_value = NULL;	/*return value			*/
    intn	i;

    FUNC_ENTER(H5D_open, NULL);

    /* check args */
    assert(f);
    assert(name && *name);

    dataset = H5MM_xcalloc(1, sizeof(H5D_t));
    dataset->create_parms = H5D_create_dflt;
    H5F_addr_undef(&(dataset->ent.header));

    /* Open the dataset object */
    if (H5G_find(f, name, NULL, &(dataset->ent)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, NULL, "not found");
    }
    if (H5O_open(f, &(dataset->ent)) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, NULL, "unable to open");
    }
    /* Get the type and space */
    if (NULL==(dataset->type=H5O_read(&(dataset->ent), H5O_DTYPE, 0, NULL)) ||
	NULL==(dataset->space=H5S_read(f, &(dataset->ent)))) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to load type or space info from dataset header");
    }
    /*
     * Get the raw data layout info.  It's actually stored in two locations:
     * the storage message of the dataset (dataset->storage) and certain
     * values are copied to the dataset create template so the user can query
     * them.
     */
    if (NULL==H5O_read(&(dataset->ent), H5O_LAYOUT, 0, &(dataset->layout))) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, NULL,
		    "unable to read data layout message");
    }
    switch (dataset->layout.type) {
    case H5D_CONTIGUOUS:
	dataset->create_parms.layout = H5D_CONTIGUOUS;
	break;

    case H5D_CHUNKED:
	/*
	 * Chunked storage.  The creation template's dimension is one less than
	 * the chunk dimension because the chunk includes a dimension for the
	 * individual bytes of the data type.
	 */
	dataset->create_parms.layout = H5D_CHUNKED;
	dataset->create_parms.chunk_ndims = dataset->layout.ndims - 1;
	for (i = 0; i < dataset->layout.ndims - 1; i++) {
	    dataset->create_parms.chunk_size[i] = dataset->layout.dim[i];
	}
	break;

    default:
	assert("not implemented yet" && 0);
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
    }

    /* Success */
    ret_value = dataset;

  done:
    if (!ret_value && dataset) {
	if (H5F_addr_defined(&(dataset->ent.header))) {
	    H5O_close(&(dataset->ent));
	}
	if (dataset->type)
	    H5T_close(dataset->type);
	if (dataset->space)
	    H5S_close(dataset->space);
	dataset->ent.file = NULL;
	H5MM_xfree(dataset);
    }
    FUNC_LEAVE(ret_value);
}

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
     * Release dataset type and space - there isn't much we can do if one of
     * these fails, so we just continue.
     */
    free_failed = (H5T_close(dataset->type) < 0 ||
		   H5S_close(dataset->space) < 0);

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
		      "couldn't free the type or space, but the dataset was "
		      "freed anyway.");
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
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_read(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	 const H5S_t *file_space, const H5D_xfer_t *xfer_parms,
	 void *buf/*out*/)
{
    size_t		nelmts	;		/*number of elements	*/
    uint8		*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8		*bkg_buf = NULL;	/*background buffer	*/
    H5T_conv_t		tconv_func = NULL;	/*conversion function	*/
    hid_t		src_id = -1, dst_id = -1;/*temporary type atoms */
    const H5S_conv_t	*sconv_func = NULL;	/*space conversion funcs*/
    H5S_number_t	numbering;		/*element numbering info*/
    H5T_cdata_t		*cdata = NULL;		/*type conversion data	*/
    herr_t		ret_value = FAIL;

    FUNC_ENTER(H5D_read, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(xfer_parms);
    assert(buf);
    if (!file_space) file_space = dataset->space;
    if (!mem_space) mem_space = file_space;
    
    /*
     * Convert data types to atoms because the conversion functions are
     * application-level functions.
     */
    if ((src_id = H5A_register(H5_DATATYPE, H5T_copy(dataset->type))) < 0 ||
	(dst_id = H5A_register(H5_DATATYPE, H5T_copy(mem_type))) < 0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
		    "unable to register types for conversion");
    }

    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information.
     */
    if (NULL == (tconv_func = H5T_find(dataset->type, mem_type, &cdata))) {
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		    "unable to convert between src and dest data types");
    }
    if (NULL==(sconv_func=H5S_find (mem_space, file_space))) {
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "unable to convert from file to memory data space");
    }
    if (sconv_func->init &&
	(sconv_func->init)(&(dataset->layout), mem_space, file_space,
			   &numbering/*out*/)<=0) {
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize element numbering information");
    } else {
	HDmemset (&numbering, 0, sizeof numbering);
    }
    if (H5S_get_npoints (mem_space)!=H5S_get_npoints (file_space)) {
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "src and dest data spaces have different sizes");
    }
    
    /*
     * Compute the size of the request and allocate scratch buffers.
     */
    nelmts = H5S_get_npoints(mem_space);
#ifndef LATER
    /*
     * Note: this prototype version allocates a buffer large enough to
     *	     satisfy the entire request; strip mining is not implemented.
     */
    {
	size_t src_size = nelmts * H5T_get_size(dataset->type);
	size_t dst_size = nelmts * H5T_get_size(mem_type);
	tconv_buf = H5MM_xmalloc(MAX(src_size, dst_size));
	if (cdata->need_bkg) bkg_buf = H5MM_xmalloc (dst_size);
    }
#endif

    /*
     * Gather the data from disk into the data type conversion buffer. Also
     * gather data from application to background buffer (this step is not
     * needed for most conversions, but we leave that as an exercise for
     * later ;-)
     */
    if ((sconv_func->fgath)(dataset->ent.file, &(dataset->layout),
			    H5T_get_size (dataset->type), file_space,
			    &numbering, 0, nelmts,
			    tconv_buf/*out*/)!=nelmts) {
	HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file gather failed");
    }
    if (H5T_BKG_YES==cdata->need_bkg) {
	if ((sconv_func->mgath)(buf, H5T_get_size (mem_type), mem_space,
				&numbering, 0, nelmts,
				bkg_buf/*out*/)!=nelmts) {
	    HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "mem gather failed");
	}
    }

    /*
     * Perform data type conversion.
     */
    cdata->command = H5T_CONV_CONV;
    cdata->ncalls++;
    if ((tconv_func) (src_id, dst_id, cdata, nelmts, tconv_buf, bkg_buf)<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		    "data type conversion failed");
    }
    cdata->nelmts += nelmts;

    /*
     * Scatter the data into memory.
     */
    if ((sconv_func->mscat)(tconv_buf, H5T_get_size (mem_type), mem_space,
			    &numbering, 0, nelmts, buf/*out*/)<0) {
	HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "scatter failed");
    }
    ret_value = SUCCEED;
    
  done:
    if (src_id >= 0) H5A_dec_ref(src_id);
    if (dst_id >= 0) H5A_dec_ref(dst_id);
    tconv_buf = H5MM_xfree(tconv_buf);
    bkg_buf = H5MM_xfree (bkg_buf);
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
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_write(H5D_t *dataset, const H5T_t *mem_type, const H5S_t *mem_space,
	  const H5S_t *file_space, const H5D_xfer_t *xfer_parms,
	  const void *buf)
{
    size_t		nelmts;
    uint8		*tconv_buf = NULL;	/*data type conv buffer	*/
    uint8		*bkg_buf = NULL;	/*background buffer	*/
    H5T_conv_t		tconv_func = NULL;	/*conversion function	*/
    hid_t		src_id = -1, dst_id = -1;/*temporary type atoms */
    const H5S_conv_t	*sconv_func = NULL;	/*space conversion funcs*/
    H5S_number_t	numbering;		/*element numbering info*/
    H5T_cdata_t		*cdata = NULL;		/*type conversion data	*/
    herr_t		ret_value = FAIL;

    FUNC_ENTER(H5D_write, FAIL);

    /* check args */
    assert(dataset && dataset->ent.file);
    assert(mem_type);
    assert(xfer_parms);
    assert(buf);
    if (!file_space) file_space = dataset->space;
    if (!mem_space) mem_space = file_space;

    /*
     * Convert data types to atoms because the conversion functions are
     * application-level functions.
     */
    if ((src_id = H5A_register(H5_DATATYPE, H5T_copy(mem_type)))<0 ||
	(dst_id = H5A_register(H5_DATATYPE, H5T_copy(dataset->type)))<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL,
		    "unable to register types for conversion");
    }
    
    /*
     * Locate the type conversion function and data space conversion
     * functions, and set up the element numbering information.
     */
    if (NULL == (tconv_func = H5T_find(mem_type, dataset->type, &cdata))) {
	HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		    "unable to convert between src and dest data types");
    }
    if (NULL==(sconv_func=H5S_find (mem_space, file_space))) {
	HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "unable to convert from memory to file data space");
    }
    if (sconv_func->init &&
	(sconv_func->init)(&(dataset->layout), mem_space, file_space,
			   &numbering/*out*/)<=0) {
	HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "unable to initialize element numbering information");
    } else {
	HDmemset (&numbering, 0, sizeof numbering);
    }
    if (H5S_get_npoints (mem_space)!=H5S_get_npoints (file_space)) {
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "src and dest data spaces have different sizes");
    }
    
    /*
     * Compute the size of the request and allocate scratch buffers.
     */
    nelmts = H5S_get_npoints(mem_space);
#ifndef LATER
    /*
     * Note: This prototype version allocates a buffer large enough to
     *	     satisfy the entire request; strip mining is not implemented.
     */
    {
	size_t src_size = nelmts * H5T_get_size(mem_type);
	size_t dst_size = nelmts * H5T_get_size(dataset->type);
	tconv_buf = H5MM_xmalloc(MAX(src_size, dst_size));
	if (cdata->need_bkg) bkg_buf = H5MM_xmalloc (dst_size);
    }
#endif


    /*
     * Gather data from application buffer into the data type conversion
     * buffer. Also gather data from the file into the background buffer
     * (this step is not needed for most conversions, but we leave that as an
     * exercise for later ;-)
     */
    if ((sconv_func->mgath)(buf, H5T_get_size (mem_type), mem_space,
			    &numbering, 0, nelmts, tconv_buf/*out*/)!=nelmts) {
	HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "mem gather failed");
    }
    if (H5T_BKG_YES==cdata->need_bkg) {
	if ((sconv_func->fgath)(dataset->ent.file, &(dataset->layout),
				H5T_get_size (dataset->type), file_space,
				&numbering, 0, nelmts,
				bkg_buf/*out*/)!=nelmts) {
	    HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "file gather failed");
	}
    }

    /*
     * Perform data type conversion.
     */
    cdata->command = H5T_CONV_CONV;
    cdata->ncalls++;
    if ((tconv_func) (src_id, dst_id, cdata, nelmts, tconv_buf, bkg_buf)<0) {
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
		    "data type conversion failed");
    }
    cdata->nelmts += nelmts;

    /*
     * Scatter the data out to the file.
     */
    if ((sconv_func->fscat)(dataset->ent.file, &(dataset->layout),
			    H5T_get_size (dataset->type), file_space,
			    &numbering, 0, nelmts, tconv_buf)<0) {
	HGOTO_ERROR (H5E_DATASET, H5E_WRITEERROR, FAIL, "scatter failed");
    }
    ret_value = SUCCEED;
    
  done:
    if (src_id >= 0) H5A_dec_ref(src_id);
    if (dst_id >= 0) H5A_dec_ref(dst_id);
    tconv_buf = H5MM_xfree(tconv_buf);
    bkg_buf = H5MM_xfree (bkg_buf);
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
 *              Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_extend (H5D_t *dataset, const size_t *size)
{
    herr_t	changed;

    FUNC_ENTER (H5D_extend, FAIL);

    /* Check args */
    assert (dataset);
    assert (size);

    /* This is only allowed for data spaces with chunked layout */
    if (H5D_CHUNKED!=dataset->layout.type) {
	HRETURN_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		       "size can only be increased for chunked datasets");
    }

    /* Increase the size of the data space */
    if ((changed=H5S_extend (dataset->space, size))<0) {
	HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		       "unable to increase size of data space");
    }

    /* Save the new dataspace in the file if necessary */
    if (changed>0 &&
	H5S_modify (&(dataset->ent), dataset->space)<0) {
	HRETURN_ERROR (H5E_DATASET, H5E_WRITEERROR, FAIL,
		       "unable to update file with new dataspace");
    }

    FUNC_LEAVE (SUCCEED);
}
    
