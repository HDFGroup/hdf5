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
static char RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */


#include <H5private.h>		/* Generic Functions			*/
#include <H5Aprivate.h>		/* Atoms				*/
#include <H5ACprivate.h>	/* Cache				*/
#include <H5Cprivate.h>		/* Templates				*/
#include <H5Dprivate.h>		/* Dataset functions			*/
#include <H5Eprivate.h>		/* Error handling			*/
#include <H5Gprivate.h>		/* Group headers			*/
#include <H5Mprivate.h>		/* Meta data				*/
#include <H5MFprivate.h>	/* File space allocation header		*/
#include <H5MMprivate.h>	/* Memory management 			*/
#include <H5Mprivate.h>		/* Meta-Object API			*/
#include <H5Oprivate.h>		/* Object headers			*/

#define PABLO_MASK	H5D_mask

/*
 * A dataset is the following struct.
 */
struct H5D_t {
   H5G_entry_t		ent;		/*cached object header stuff	*/
   H5T_t		*type;		/*datatype of this dataset	*/
   H5P_t		*space;		/*dataspace of this dataset	*/
   H5D_create_t		create_parms;	/*creation parameters		*/
   union {
      H5O_cstore_t	cstore;		/*contiguous storage info	*/
      H5O_istore_t	istore;		/*chunked storage info		*/
   } storage;
};

/* Default dataset creation template */
const H5D_create_t H5D_create_dflt = {
   H5D_CONTIGUOUS,		/* Layout				*/
   1, 				/* Chunk dimensions			*/
   {1, 1, 1, 1, 1, 1, 1, 1,	/* Chunk size.  These default values....*/
    1, 1, 1, 1, 1, 1, 1, 1,	/*...are quite useless.  Larger chunks..*/
    1, 1, 1, 1, 1, 1, 1, 1,	/*...produce fewer, but larger I/O......*/
    1, 1, 1, 1, 1, 1, 1, 1}, 	/*...requests.				*/
};

/* Default dataset transfer template */
const H5D_xfer_t H5D_xfer_dflt = {
   0,  				/* Place holder - remove this later	*/
};


/* Interface initialization? */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT H5D_init_interface
static herr_t H5D_init_interface(void);
static void H5D_term_interface (void);


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
H5D_init_interface (void)
{
   herr_t ret_value = SUCCEED;
   FUNC_ENTER (H5D_init_interface, FAIL);

   /* Initialize the atom group for the dataset IDs */
   if ((ret_value=H5Ainit_group (H5_DATASET, H5A_DATASETID_HASHSIZE,
				 H5D_RESERVED_ATOMS,
				 (herr_t (*)(void*))H5D_close))!=FAIL) {
      ret_value = H5_add_exit (H5D_term_interface);
   }
    
   FUNC_LEAVE (ret_value);
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
H5D_term_interface (void)
{
    H5Adestroy_group (H5_DATASET);
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
 *				value.  The dataset should be closed when
 *				the caller is no longer interested in it.
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS      BADTYPE       Not a data space. 
 *		ARGS      BADTYPE       Not a dataset creation template. 
 *		ARGS      BADTYPE       Not a file. 
 *		ARGS      BADTYPE       Not a type. 
 *		ARGS      BADVALUE      No name. 
 *		DATASET   CANTINIT      Can't create dataset. 
 *		DATASET   CANTREGISTER  Can't register dataset. 
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December  3, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dcreate (hid_t file_id, const char *name, hid_t type_id, hid_t space_id,
	   hid_t create_parms_id)
{
   H5F_t		*f = NULL;
   H5T_t		*type = NULL;
   H5P_t		*space = NULL;
   H5D_t		*new_dset = NULL;
   hid_t		ret_value = FAIL;
   const H5D_create_t	*create_parms = NULL;
   
   FUNC_ENTER (H5Dcreate, FAIL);
   H5ECLEAR;

   /* check arguments */
   if (H5_FILE!=H5Aatom_group (file_id) ||
       NULL==(f=H5Aatom_object (file_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
   }
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
   }
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(type=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a type");
   }
   if (H5_DATASPACE!=H5Aatom_group (space_id) ||
       NULL==(space=H5Aatom_object (space_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
   }
   if (create_parms_id>=0) {
      if (H5C_DATASET_CREATE!=H5Cget_class (create_parms_id) ||
	  NULL==(create_parms=H5Aatom_object (create_parms_id))) {
	 HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL,
			"not a dataset creation template");
      }
   } else {
      create_parms = &H5D_create_dflt;
   }

   /* build and open the new dataset */
   if (NULL==(new_dset=H5D_create (f, name, type, space, create_parms))) {
      HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "can't create dataset");
   }

   /* Register the new datatype and get an ID for it */
   if ((ret_value=H5Aregister_atom (H5_DATASET, new_dset))<0) {
      H5D_close (new_dset);
      HRETURN_ERROR (H5E_DATASET, H5E_CANTREGISTER, FAIL,
		     "can't register dataset");
   }

   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dopen
 *
 * Purpose:	Finds a dataset named NAME in file FILE_ID, opens it, and
 *		returns its ID.  The dataset should be close when the caller
 *		is no longer interested in it.
 *
 * Return:	Success:	A new dataset ID
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS      BADTYPE       Not a file. 
 *		ARGS      BADVALUE      No name. 
 *		DATASET   CANTREGISTER  Can't register dataset. 
 *		DATASET   NOTFOUND      Dataset not found. 
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Dopen (hid_t file_id, const char *name)
{
   H5F_t	*file = NULL;		/*file holding the dataset	*/
   H5D_t	*dataset = NULL;	/*the dataset			*/
   hid_t	ret_value = FAIL;

   FUNC_ENTER (H5Dopen, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_FILE!=H5Aatom_group (file_id) ||
       NULL==(file=H5Aatom_object (file_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
   }
   if (!name || !*name) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
   }

   /* Find the dataset */
   if (NULL==(dataset=H5D_open (file, name))) {
      HRETURN_ERROR (H5E_DATASET, H5E_NOTFOUND, FAIL, "dataset not found");
   }

   /* Create an atom for the dataset */
   if ((ret_value=H5Aregister_atom (H5_DATASET, dataset))<0) {
      H5D_close (dataset);
      HRETURN_ERROR (H5E_DATASET, H5E_CANTREGISTER, FAIL,
		     "can't register dataset");
   }

   FUNC_LEAVE (ret_value);
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
 *		ARGS      BADTYPE       Not a dataset. 
 *		DATASET   CANTINIT      Can't free. 
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dclose (hid_t dataset_id)
{
   H5D_t	*dataset = NULL;	 /* dataset object to release */

   FUNC_ENTER (H5Dclose, FAIL);
   H5ECLEAR;

   /* Check args */
   if (H5_DATASET!=H5Aatom_group (dataset_id) ||
       NULL==(dataset=H5Aatom_object (dataset_id)) ||
       NULL==dataset->ent.file) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
   }

   /*
    * Decrement the counter on the dataset.  It will be freed if the count
    * reaches zero.
    */
   if (H5A_dec_ref (dataset_id)<0) {
      HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL, "can't free");
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dread
 *
 * Purpose:	Reads (part of) a DATASET from the file into application
 *		memory BUF. The part of the dataset to read is defined with
 *		SPACE_ID (if SPACE_ID is negative then we assume that the
 *		caller desires to read the entire dataset).  The data points
 *		are converted from their file type to the TYPE_ID specified.
 *		Additional miscellaneous data transfer properties can be
 *		passed to this function with the XFER_PARMS_ID argument.
 *
 *		The SPACE_ID can be the constant H5P_ALL in which case the
 *		destination (memory) data space is the same as the source
 *		(file) data space defined when the dataset was created.
 *
 *		The XFER_PARMS_ID can be the constant H5C_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *		ARGS      BADTYPE       Not a data space. 
 *		ARGS      BADTYPE       Not a data type. 
 *		ARGS      BADTYPE       Not a dataset. 
 *		ARGS      BADTYPE       Not xfer parms. 
 *		ARGS      BADVALUE      No output buffer. 
 *		DATASET   READERROR     Can't read data. 
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dread (hid_t dataset_id, hid_t type_id, hid_t space_id,
	 hid_t xfer_parms_id,  void *buf/*out*/)
{
   H5D_t		*dataset = NULL;
   const H5T_t		*type = NULL;
   const H5P_t		*space = NULL;
   const H5D_xfer_t	*xfer_parms = NULL;
   
   FUNC_ENTER (H5Dread, FAIL);
   H5ECLEAR;

   /* check arguments */
   if (H5_DATASET!=H5Aatom_group (dataset_id) ||
       NULL==(dataset=H5Aatom_object (dataset_id)) ||
       NULL==dataset->ent.file) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
   }
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(type=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }
   if (H5P_ALL!=space_id) {
      if (H5_DATASPACE!=H5Aatom_group (space_id) ||
	  NULL==(space=H5Aatom_object (space_id))) {
	 HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
      }
   }
   if (H5C_DEFAULT==xfer_parms_id) {
      xfer_parms = &H5D_xfer_dflt;
   } else if (H5C_DATASET_XFER!=H5Cget_class (xfer_parms_id) ||
	      NULL==(xfer_parms=H5Aatom_object (xfer_parms_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
   }
   if (!buf) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
   }

   /* read raw data */
   if (H5D_read (dataset, type, space, xfer_parms, buf/*out*/)<0) {
      HRETURN_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "can't read data");
   }
   
   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Dwrite
 *
 * Purpose:	Writes (part of) a DATASET from application memory BUF to the
 *		file.  The part of the dataset to write is defined with the
 *		SPACE_ID (if SPACE_ID is negative then we assume that the
 *		caller desires to write the entire dataset). The data points
 *		are converted from their current type (TYPE_ID) to their file
 *		data type.  Additional miscellaneous data transfer properties
 *		can be passed to this function with the XFER_PARMS_ID
 *		argument.
 *
 *		The SPACE_ID can be the constant H5P_ALL in which case the
 *		source (memory) data space is the same as the destination
 *		(file) memory space which was defined when the dataset was
 * 		created.
 *
 *		The XFER_PARMS_ID can be the constant H5C_DEFAULT in which
 *		case the default data transfer properties are used.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Dwrite (hid_t dataset_id, hid_t type_id, hid_t space_id,
	  hid_t xfer_parms_id, const void *buf)
{
   H5D_t		*dataset = NULL;
   const H5T_t		*type = NULL;
   const H5P_t		*space = NULL;
   const H5D_xfer_t	*xfer_parms = NULL;
   
   FUNC_ENTER (H5Dwrite, FAIL);
   H5ECLEAR;

   /* check arguments */
   if (H5_DATASET!=H5Aatom_group (dataset_id) ||
       NULL==(dataset=H5Aatom_object (dataset_id)) ||
       NULL==dataset->ent.file) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataset");
   }
   if (H5_DATATYPE!=H5Aatom_group (type_id) ||
       NULL==(type=H5Aatom_object (type_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
   }
   if (H5P_ALL!=space_id) {
      if (H5_DATASPACE!=H5Aatom_group (space_id) ||
	  NULL==(space=H5Aatom_object (space_id))) {
	 HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data space");
      }
   }
   if (H5C_DEFAULT==xfer_parms_id) {
      xfer_parms = &H5D_xfer_dflt;
   } else if (H5C_DATASET_XFER!=H5Cget_class (xfer_parms_id) ||
	      NULL==(xfer_parms=H5Aatom_object (xfer_parms_id))) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not xfer parms");
   }
   if (!buf) {
      HRETURN_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no output buffer");
   }

   /* write raw data */
   if (H5D_write (dataset, type, space, xfer_parms, buf)<0) {
      HRETURN_ERROR (H5E_DATASET, H5E_READERROR, FAIL, "can't write data");
   }
   
   FUNC_LEAVE (SUCCEED);
}





   

/*-------------------------------------------------------------------------
 * Function:	H5D_find_name
 *
 * Purpose:	This is a callback for H5Mfind_name().  It does the same
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
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5D_find_name (hid_t file_id, group_t UNUSED, const char *name)
{
   return H5Dopen (file_id, name);
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
 *		DATASET   CANTINIT      Can't update dataset header. 
 *		DATASET   CANTINIT      Problem with the dataset name. 
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_create (H5F_t *f, const char *name, const H5T_t *type, const H5P_t *space,
	    const H5D_create_t *create_parms)
{
   H5D_t	*new_dset = NULL;
   H5D_t	*ret_value = NULL;
   size_t	nbytes;
   intn		ndims;

   FUNC_ENTER (H5D_create, NULL);

   /* check args */
   assert (f);
   assert (name && *name);
   assert (type);
   assert (space);
   assert (create_parms);
   
   /* Initialize the dataset object */
   new_dset = H5MM_xcalloc (1, sizeof(H5D_t));
   H5F_addr_undef (&(new_dset->ent.header));
   new_dset->type = H5T_copy (type);
   new_dset->space = H5P_copy (space);
   new_dset->create_parms = *create_parms;

   /*
    * Create (open for write access) an object header.
    */
   if (H5O_create (f, 0, &(new_dset->ent))<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		   "unable to create dataset object header");
   }

   /* Update the type and space header messages */
   if (H5O_modify (&(new_dset->ent), H5O_DTYPE, 0, 0, new_dset->type)<0 ||
       H5P_modify (f, &(new_dset->ent), new_dset->space)<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		   "can't update type or space header messages");
   }

   /* Total raw data size */
   nbytes = H5T_get_size (type) * H5P_get_npoints (space);

   /* Initialize storage */
   switch (new_dset->create_parms.layout) {
   case H5D_CONTIGUOUS:
      new_dset->storage.cstore.size = nbytes;
      if (H5MF_alloc (f, H5MF_RAW, nbytes,
		      &(new_dset->storage.cstore.addr))<0) {
	 HGOTO_ERROR (H5E_DATASET, H5E_NOSPACE, NULL,
		      "can't allocate raw file storage");
      }
      if (H5O_modify (&(new_dset->ent), H5O_CSTORE, 0, 0,
		      &(new_dset->storage.cstore))<0) {
	 HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		      "can't update dataset object header");
      }
      break;

   case H5D_CHUNKED:
      /*
       * The dimensionality of the chunk should match the dimensionality of
       * the data space.  We will add one more dimension here though, to
       * describe the individual bytes of a data point.  Therefore, there
       * must be room in the template for one more dimension size.
       */
      ndims = new_dset->create_parms.chunk_ndims;
      if (ndims != H5P_get_ndims (space)) {
	 HGOTO_ERROR (H5E_DATASET, H5E_BADVALUE, NULL,
		      "dimensionality of chunks doesn't match the data space");
      }
      assert (ndims<NELMTS (new_dset->create_parms.chunk_size));
      new_dset->create_parms.chunk_size[ndims] = H5T_get_size (type);

      if (H5F_istore_create (f, &(new_dset->storage.istore), ndims+1,
			     new_dset->create_parms.chunk_size)<0) {
	 HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		      "can't initialize chunked storage");
      }
      if (H5O_modify (&(new_dset->ent), H5O_ISTORE, 0, 0,
		      &(new_dset->storage.istore))<0) {
	 HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		      "can't update dataset object header");
      }
      break;
      
   default:
      assert ("not implemented yet" && 0);
      HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
   }

   /* Give the dataset a name */
   if (H5G_insert (name, &(new_dset->ent))<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL, "unable to name dataset");
   }

   /* Success */
   ret_value = new_dset;
   
   
 done:
   if (!ret_value && new_dset) {
      if (new_dset->type) H5T_close (new_dset->type);
      if (new_dset->space) H5P_close (new_dset->space);
      if (H5F_addr_defined (&(new_dset->ent.header))) {
	 H5O_close (&(new_dset->ent));
      }
      new_dset->ent.file = NULL;
      H5MM_xfree (new_dset);
   }
   
   FUNC_LEAVE (ret_value);
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
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5D_t *
H5D_open (H5F_t *f, const char *name)
{
   H5D_t	*dataset = NULL;	/*the dataset which was found	*/
   H5D_t	*ret_value = NULL;	/*return value			*/
   H5O_istore_t	*istore = NULL;
   intn		i;

   FUNC_ENTER (H5D_open, NULL);

   /* check args */
   assert (f);
   assert (name && *name);

   dataset = H5MM_xcalloc (1, sizeof(H5D_t));
   dataset->create_parms = H5D_create_dflt;
   H5F_addr_undef (&(dataset->ent.header));
    
   /* Open the dataset object */
   if (H5G_find (f, name, NULL, &(dataset->ent))<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_NOTFOUND, NULL, "not found");
   }
   if (H5O_open (f, &(dataset->ent))<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTOPENOBJ, NULL, "unable to open");
   }

   /* Get the type and space */
   if (NULL==(dataset->type=H5O_read (&(dataset->ent), H5O_DTYPE, 0, NULL)) ||
       NULL==(dataset->space=H5P_read (f, &(dataset->ent)))) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, NULL,
		   "can't load type of space info from dataset header");
   }

   /*
    * Get the raw data storage info.  It's actually stored in two locations:
    * the storage message of the dataset (dataset->storage) and certain
    * values are copied to the dataset create template so the user can query
    * them.
    */
   if (H5O_read (&(dataset->ent), H5O_CSTORE, 0, &(dataset->storage.cstore))) {
      /* Contiguous storage */
      dataset->create_parms.layout = H5D_CONTIGUOUS;
      
   } else if (H5O_read (&(dataset->ent), H5O_ISTORE, 0,
			&(dataset->storage.istore))) {
      /*
       * Chunked storage.  The creation template's dimension is one less than
       * the chunk dimension because the chunk includes a dimension for the
       * individual bytes of the data type.
       */
      istore = &(dataset->storage.istore);
      dataset->create_parms.layout = H5D_CHUNKED;
      dataset->create_parms.chunk_ndims = istore->ndims - 1;
      assert (istore->ndims<=NELMTS (dataset->create_parms.chunk_size));
      for (i=0; i<istore->ndims-1; i++) {
	 dataset->create_parms.chunk_size[i] = istore->alignment[i];
      }
      
   } else {
      assert ("not implemented yet" && 0);
      HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, NULL, "not implemented yet");
   }

   /* Success */
   ret_value = dataset;
   

 done:
   if (!ret_value && dataset) {
      if (H5F_addr_defined (&(dataset->ent.header))) {
	 H5O_close (&(dataset->ent));
      }
      if (dataset->type) H5T_close (dataset->type);
      if (dataset->space) H5P_close (dataset->space);
      dataset->ent.file = NULL;
      H5MM_xfree (dataset);
   }
   
   FUNC_LEAVE (ret_value);
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
 *		DATASET   CANTINIT      Couldn't free the type or space,
 *		                        but the dataset was freed anyway. 
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_close (H5D_t *dataset)
{
   hbool_t	free_failed;
   
   FUNC_ENTER (H5D_close, FAIL);

   /* check args */
   assert (dataset && dataset->ent.file);
   
   /* Close the dataset object */
   H5O_close (&(dataset->ent));

   /*
    * Release dataset type and space - there isn't much we can do if one of
    * these fails, so we just continue.
    */
   free_failed = (H5T_close (dataset->type)<0 ||
		  H5P_close (dataset->space)<0);

   /*
    * Free memory.  Before freeing the memory set the file pointer to NULL.
    * We always check for a null file pointer in other H5D functions to be
    * sure we're not accessing an already freed dataset (see the assert()
    * above).
    */
   dataset->ent.file = NULL;
   H5MM_xfree (dataset);

   if (free_failed) {
      HRETURN_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		     "couldn't free the type or space, but the dataset was "
		     "freed anyway.");
   }

   FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_read
 *
 * Purpose:	Reads (part of) a DATASET into application memory BUF. The
 *		SPACE argument determines what part of the dataset to read
 *		(the whole thing is read if SPACE is null) and individual
 *		data points are translated from their file data type to the
 *		specified TYPE.  The XFER_PARMS contains additional
 *		miscellaneous properties that control the data transfer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_read (H5D_t *dataset, const H5T_t *type, const H5P_t *space,
	  const H5D_xfer_t *xfer_parms, void *buf/*out*/)
{
   size_t	nelmts, src_size, dst_size;
   size_t	offset[H5O_ISTORE_NDIMS];
   size_t	size[H5O_ISTORE_NDIMS];
   intn		i;
   herr_t	ret_value = FAIL;
   uint8	*conv_buf = NULL;	/*data type conv buffer		*/
   H5T_conv_t	conv_func = NULL;	/*conversion function		*/
   hid_t	src_id=-1, dst_id=-1;	/*temporary type atoms		*/
   
   FUNC_ENTER (H5D_read, FAIL);

   /* check args */
   assert (dataset && dataset->ent.file);
   assert (type);
   assert (xfer_parms);
   assert (buf);

   if (H5D_CONTIGUOUS!=dataset->create_parms.layout) {
      HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		   "layout is not supported yet");
   }
   if (space && H5P_cmp (space, dataset->space)) {
      HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		   "space conversion not supported yet");
   }

   /*
    * Convert data types to atoms because the conversion functions are
    * application-level functions.
    */
   if ((src_id=H5Aregister_atom (H5_DATATYPE, H5T_copy (dataset->type)))<0 ||
       (dst_id=H5Aregister_atom (H5_DATATYPE, H5T_copy (type)))<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTREGISTER, FAIL,
		   "unable to register types for conversion");
   }

   /* Compute the size of the request and allocate scratch buffers */
   nelmts = H5P_get_npoints (dataset->space);
   src_size = nelmts * H5T_get_size (dataset->type);
   dst_size = nelmts * H5T_get_size (type);
   conv_buf = H5MM_xmalloc (MAX (src_size, dst_size));
   if (NULL==(conv_func=H5T_find (dataset->type, type))) {
      HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		   "unable to convert between src and dest data types");
   }

   /*
    * Read data into the data type conversion buffer.
    */
   switch (dataset->create_parms.layout) {
   case H5D_CONTIGUOUS:
      /* Read a block of contiguous data */
      if (H5F_block_read (dataset->ent.file, &(dataset->storage.cstore.addr),
			  src_size, conv_buf)<0) {
	 HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "read failed");
      }
      break;

   case H5D_CHUNKED:
      /* Read one or more chunks from indexed storage */
      for (i=0; i<dataset->storage.istore.ndims; i++) offset[i] = 0;
      H5P_get_dims (dataset->space, size);
      size[dataset->storage.istore.ndims-1] = H5T_get_size (dataset->type);
      if (H5F_istore_read (dataset->ent.file, &(dataset->storage.istore),
			   offset, size, conv_buf)<0) {
	 HGOTO_ERROR (H5E_IO, H5E_READERROR, FAIL, "read failed");
      }
      break;

   default:
      assert ("not implemented yet" && 0);
      HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "not implemented yet");
   }

   /*
    * Perform data type conversion.
    */
   if ((conv_func)(src_id, dst_id, nelmts, conv_buf, NULL)<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		   "data type conversion failed");
   }

   /*
    * Copy conversion buffer into destination.
    */
   HDmemcpy (buf, conv_buf, dst_size);
   
   ret_value = SUCCEED;
 done:
   if (src_id>=0) H5A_dec_ref (src_id);
   if (dst_id>=0) H5A_dec_ref (dst_id);
   conv_buf = H5MM_xfree (conv_buf);
   FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5D_write
 *
 * Purpose:	Writes (part of) a DATASET to a file from application memory
 *		BUF. The SPACE argument determines what part of the dataset
 *		to write (the whole thing is read if SPACE is null) and
 *		individual data points are translated from their memory data
 *		type (TYPE) to the file data type.  The XFER_PARMS contains
 *		additional miscellaneous properties that control the data
 *		transfer.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_write (H5D_t *dataset, const H5T_t *type, const H5P_t *space,
	   const H5D_xfer_t *xfer_parms, const void *buf)
{
   size_t	nelmts, src_size, dst_size;
   size_t	offset[H5O_ISTORE_NDIMS];
   size_t	size[H5O_ISTORE_NDIMS];
   intn		i;
   herr_t	ret_value = FAIL;
   uint8	*conv_buf = NULL;	/*data type conversion buffer	*/
   H5T_conv_t	conv_func = NULL;	/*data type conversion function	*/
   hid_t	src_id=-1, dst_id=-1;	/*temporary type atoms		*/
   
   FUNC_ENTER (H5D_write, FAIL);

   /* check args */
   assert (dataset && dataset->ent.file);
   assert (type);
   assert (xfer_parms);
   assert (buf);

   if (H5D_CONTIGUOUS!=dataset->create_parms.layout) {
      HRETURN_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "layout is not supported yet");
   }
   if (space && H5P_cmp (space, dataset->space)) {
      HRETURN_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		     "space conversion not supported yet");
   }

   /*
    * Convert data types to atoms because the conversion functions are
    * application-level functions.
    */
   if ((src_id=H5Aregister_atom (H5_DATATYPE, H5T_copy (dataset->type)))<0 ||
       (dst_id=H5Aregister_atom (H5_DATATYPE, H5T_copy (type)))<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTREGISTER, FAIL,
		   "unable to register types for conversion");
   }


   /* Compute the size of the request and allocate scratch buffers */
   nelmts = H5P_get_npoints (dataset->space);
   src_size = nelmts * H5T_get_size (type);
   dst_size = nelmts * H5T_get_size (dataset->type);
   conv_buf = H5MM_xmalloc (MAX (src_size, dst_size));
   if (NULL==(conv_func=H5T_find (type, dataset->type))) {
      HGOTO_ERROR (H5E_DATASET, H5E_UNSUPPORTED, FAIL,
		   "unable to convert between src and dest data types");
   }


   /*
    * Read data into the data type conversion buffer.
    */
   HDmemcpy (conv_buf, buf, src_size);

   /*
    * Perform data type conversion.
    */
   if ((conv_func)(src_id, dst_id, nelmts, conv_buf, NULL)<0) {
      HGOTO_ERROR (H5E_DATASET, H5E_CANTINIT, FAIL,
		   "data type conversion failed");
   }

   /*
    * Write data into the file.
    */
   switch (dataset->create_parms.layout) {
   case H5D_CONTIGUOUS:
      /* Write a contiguous chunk of data */
      if (H5F_block_write (dataset->ent.file, &(dataset->storage.cstore.addr),
			   dst_size, conv_buf)<0) {
	 HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "write failed");
      }
      break;

   case H5D_CHUNKED:
      /* Write one or more chunks to indexed storage */
      for (i=0; i<dataset->storage.istore.ndims; i++) offset[i] = 0;
      H5P_get_dims (dataset->space, size);
      size[dataset->storage.istore.ndims-1] = H5T_get_size (dataset->type);
      if (H5F_istore_write (dataset->ent.file, &(dataset->storage.istore),
			    offset, size, conv_buf)<0) {
	 HGOTO_ERROR (H5E_IO, H5E_WRITEERROR, FAIL, "write failed");
      }
      break;

   default:
      assert ("not implemented yet" && 0);
      HGOTO_ERROR (H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "not implemented yet");
   }

   ret_value = SUCCEED;
 done:
   if (src_id>=0) H5A_dec_ref (src_id);
   if (dst_id>=0) H5A_dec_ref (dst_id);
   conv_buf = H5MM_xfree (conv_buf);
   FUNC_LEAVE (ret_value);
}

