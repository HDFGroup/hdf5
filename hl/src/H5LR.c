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


/**********************/
/* Module Declaration */
/**********************/

#define H5LR_MODULE


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
//#include <string.h>
#include <stdlib.h>
//#include <stdio.h>
#include "H5LRpkg.h"            /* Lite References */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

hid_t   H5_MY_PKG_ERR;

/********************/
/* Package Typedefs */
/********************/
#define AT() 		printf ("	 at %s:%d in %s()...\n",	      \
				__FILE__, __LINE__, __FUNCTION__);
#define H5_FAILED()	{puts("*FAILED*");fflush(stdout);}
#define TEST_ERROR      {H5_FAILED(); AT(); goto error;}

/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/

/* Package initialization flag */
hbool_t H5_H5LR_init_g = FALSE;

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

int H5LRcreate_glb_counter;

/*-------------------------------------------------------------------------
 *
 * Private functions
 *
 *-------------------------------------------------------------------------
 */
/*
 * Operator function to be called by H5Lvisit_by_name.
 */

/* herr_t op_func_L (hid_t loc_id, const char *name, const H5L_info_t *info, */
/*             void *operator_data); */

/************************************************************

  Operator function for H5Lvisit_by_name.  This function simply
  retrieves the info for the object the current link points
  to, and calls the operator function for H5Ovisit.

 ************************************************************/
herr_t op_func_L (hid_t loc_id, const char *name, const H5L_info_t *info,
            hsize_t *operator_data)
{
    herr_t          status;
    H5O_info_t      infobuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Oget_info_by_name (loc_id, name, &infobuf, H5P_DEFAULT);


    return op_func (loc_id, name, &infobuf, operator_data);
}

herr_t op_func_L1 (hid_t loc_id, const char *name, const H5L_info_t *info,
            void *operator_data)
{
    herr_t          status;
    H5O_info_t      infobuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Oget_info_by_name (loc_id, name, &infobuf, H5P_DEFAULT);

    return op_func1 (loc_id, name, &infobuf, operator_data);
}

/************************************************************

  Operator function for H5Ovisit.  This function prints the
  name and type of the object passed to it.

 ************************************************************/
herr_t op_func (hid_t loc_id, const char *name, const H5O_info_t *info,
		hsize_t *operator_data)
{
    herr_t status; 
    hid_t dtype_id, dtype, native_type, space_id,space_id_ref, dset;
    int rank, ndim;
    hsize_t *dims;
    int i;
    hdset_reg_ref_t *ref_out;
    hdset_reg_ref_t ref_out2[4];
    H5S_sel_type sel_type;
    hsize_t size_loc;
    /*
     * Check if the current object is a region reference.
     */
    if(info->type == H5O_TYPE_DATASET) {
      size_loc = 0;
      dtype_id = H5Dopen2(loc_id, name, H5P_DEFAULT);
      dtype = H5Dget_type(dtype_id);
      native_type = H5Tget_native_type(dtype, H5T_DIR_DEFAULT);
      /* check if object is a region reference */
      if(H5Tget_class(native_type) == H5T_REFERENCE){

	space_id = H5Dget_space(dtype_id);

	rank = H5Sget_simple_extent_ndims(space_id);

	dims = (hsize_t *)malloc (sizeof (hsize_t) * rank);

	ndim = H5Sget_simple_extent_dims(space_id, dims, NULL);

/* 	ref_out = (hsize_t *)malloc (sizeof (hsize_t) * dims[0]); */

	/* loop through the region references */
	for (i=0; i<ndim; i++){
	  status = H5Dread(dtype_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, 
			   H5P_DEFAULT, ref_out2);
	}
	size_loc = 0;
	for (i=0; i<dims[0]; i++){
	  space_id_ref = H5Rget_region(dtype_id, H5R_DATASET_REGION, ref_out2[i]);
	  size_loc += (hsize_t)H5Sget_select_npoints(space_id_ref);
	}
	free(dims);
/* 	free(ref_out); */
        *operator_data += size_loc;
      }
      status = H5Dclose(dtype_id);
      if(status != 0) printf("error closing\n");
      status = H5Tclose(native_type);
      if(status != 0) printf("error closing\n");
    }

    return 0;
}
herr_t op_func1 (hid_t loc_id, const char *name, const H5O_info_t *info,
		void *operator_data)
{
    herr_t status; 
    hid_t dtype_id, dtype, native_type, space_id,space_id_ref, dset;
    int rank, ndim;
    hsize_t *dims;
    int i,j;
    hdset_reg_ref_t *ref_out;
    hdset_reg_ref_t ref_out2[4];
    H5S_sel_type sel_type;
    size_t size_loc;
    int *data;
    /*
     * Check if the current object is a region reference.
     */
    size_loc = 0;
    if(info->type == H5O_TYPE_DATASET) {
      dtype_id = H5Dopen2(loc_id, name, H5P_DEFAULT);
      dtype = H5Dget_type(dtype_id);
      native_type = H5Tget_native_type(dtype, H5T_DIR_DEFAULT);
      /* check if object is a region reference */
      if(H5Tget_class(native_type) == H5T_REFERENCE){

	space_id = H5Dget_space(dtype_id);

	rank = H5Sget_simple_extent_ndims(space_id);

	dims = (hsize_t *)malloc (sizeof (hsize_t) * rank);

	ndim = H5Sget_simple_extent_dims(space_id, dims, NULL);

/* 	ref_out = (hsize_t *)malloc (sizeof (hsize_t) * dims[0]); */

	/* loop through the region references */
	for (i=0; i<ndim; i++){
	  status = H5Dread(dtype_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, 
			   H5P_DEFAULT, ref_out2);
	}
	for (i=0; i<dims[0]; i++){
	  space_id_ref = H5Rget_region(dtype_id, H5R_DATASET_REGION, ref_out2[i]);
	  size_loc = (size_t)H5Sget_select_npoints(space_id_ref);

	  
	  status = H5LRread_region(dtype_id, ref_out2[i], H5T_NATIVE_INT,  &size_loc, ((int*)operator_data)+H5LRcreate_glb_counter);
/* 	  for (j=0; j<size_loc; j++){ */
/* 	    printf("size %d %d \n", H5LRcreate_glb_counter+j, ((int*)operator_data)[j] ); */
/* 	  } */
	  H5LRcreate_glb_counter += size_loc;
	}
	free(dims);
/* 	free(ref_out); */
      }
      status = H5Dclose(dtype_id);
      if(status != 0) printf("error closing\n");
      status = H5Tclose(native_type);
      if(status != 0) printf("error closing\n");
    }

    return 0;
}

/*-------------------------------------------------------------------------
 * Function: H5LR__pkg_init
 *
 * Purpose: Package initialization 
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Quincey Koziol
 *
 * Date: April 14, 2009
 *
 *-------------------------------------------------------------------------
 */

BEGIN_FUNC(PKGINIT, ERR,
herr_t, SUCCEED, FAIL,
H5LR__pkg_init(void))

CATCH

END_FUNC(PKGINIT)

/*-------------------------------------------------------------------------
 *
 * Public functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: H5LRget_region_info
 *
 * Purpose: Gets information about the data set pointed to by a
 *          region reference.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: February 17, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
herr_t, SUCCEED, FAIL,
H5LRget_region_info(hid_t obj_id,               /* -IN-      Id. of any object in a file associated with reference */
		    const hdset_reg_ref_t *ref, /* -IN-      Region reference to query                             */
		    size_t *len,                /* -OUT-     Size of the buffer path                               */
		    char *path,                 /* -OUT-     Full path that a region reference points to           */
		    int *rank,                  /* -OUT-     The number of dimensions of the dataset pointed by region reference */
		    hid_t *dtype,               /* -OUT-     Dataset datatype pointed by region reference          */
		    H5S_sel_type *sel_type,     /* -OUT-     Type fo the selection (point or hyperslab)            */
		    size_t *numelem,            /* -IN/OUT-  Number of coordinate blocks or selected elements      */
		    hsize_t *buf ) )            /* -OUT-     Buffer containing description of the region           */

  hid_t dset = -1, sid = -1; /* dataset and space id */

  /* local variables needed in case user supplies NULL for the input quantity */
  H5S_sel_type sel_type_loc;
  size_t numelem_loc = 0;
  
  herr_t status; /* state of the return value of api */
  hid_t current_stack_id = -1; /* current stack id */

  /* Determine the rank of the space */
  sid = H5Rget_region(obj_id, H5R_DATASET_REGION, ref);
  if(sid < 0)
       H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open region referenced")

  /* Try to open object */
  dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
  if(dset < 0)
       H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open the dataset associated to region reference")

  /* Determine the type of the dataspace selection */
  sel_type_loc = H5Sget_select_type(sid);
  if(sel_type)
    *sel_type = sel_type_loc;
    
  /* get the number of elements */
  if(sel_type_loc==H5S_SEL_HYPERSLABS){
    numelem_loc = (size_t)H5Sget_select_hyper_nblocks(sid);
  } else if(sel_type_loc==H5S_SEL_POINTS){
    numelem_loc = (size_t)H5Sget_select_npoints(sid);
  } else 
    H5E_THROW(H5E_BADSELECT, "H5LR: Failed to find selection type")

  if(numelem)
    *numelem = numelem_loc;

  if(numelem_loc == 0) 
    H5E_THROW(H5E_CANTCOUNT, "H5LR: Failed to count elements in dataspace")

  /* the dimensionality of the dataspace */
  if(rank) {
    *rank = (int)H5Sget_simple_extent_ndims(sid);
    if(*rank < 0)
      H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataset")
  }

  /* get the data type */
  if(dtype) {
    *dtype = (hid_t)H5Dget_type(dset);
    if(*dtype < 0)
      H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the data type")
  }

  /* Determine the size of the name buffer, with null character included */
  
  if(!path) {
    *len = (size_t)(1 + H5Iget_name (dset, NULL, (size_t)0));
  }
    
  if(path) {
    /* Get the data set name the region reference points to */
    status = H5Iget_name (dset, path, *len);
    if(status < 0)
      H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the name associated with the region reference")
  }

  if(buf) {
    /* get the corner coordinates of the hyperslab */
    if(sel_type_loc == H5S_SEL_HYPERSLABS) {
      /* get the list of hyperslab blocks currently selected */
      status = H5Sget_select_hyper_blocklist(sid, (hsize_t)0, (hsize_t)1, buf);
      if(status < 0)
	H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")
    } else if(sel_type_loc == H5S_SEL_POINTS) {
    /* get the list of elements currently selected */
        status = H5Sget_select_elem_pointlist(sid, (hsize_t)0, (hsize_t)1, buf);
        if(status < 0)
	  H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of selected elements")
    }
  }

CATCH

  current_stack_id = H5Eget_current_stack();

  /* Close the dataspace */
  if(sid > 0)
    status = H5Sclose(sid);
      
  /* Close the dataset */
  if(dset > 0)
    status = H5Dclose(dset);

  status = H5Eset_current_stack(current_stack_id);

END_FUNC(PUB)

/*-------------------------------------------------------------------------
 * Function: H5LRread_region
 *
 * Purpose: Read raw data pointed by a region reference 
 *          to an application buffer
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: February 17, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
herr_t, SUCCEED, FAIL,
H5LRread_region(hid_t obj_id,               /* -IN-      Id. of any object in a file associated with reference */
		const hdset_reg_ref_t *ref, /* -IN-      Region reference to query                             */
		hid_t mem_type,             /* -IN-      Id. of the memory datatype                            */
		size_t *numelem,            /* -IN/OUT-  Number of elements in the referenced region           */
		void *buf) )                /* -OUT-     Buffer containing data from the referenced region     */

    hid_t dset = -1, file_space = -1;   /* Identifier of the dataset's dataspace in the file */
    hid_t mem_space = -1;               /* Identifier of the memory dataspace                */
    herr_t status;                      /* API return status */
    hid_t current_stack_id = -1;        /* current error stack id */ 

    /* Open the HDF5 object referenced */
    dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);

    if(dset < 0)
       H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

    /* Retrieve the dataspace with the specified region selected */
    file_space = H5Rget_region(dset, H5R_DATASET_REGION, ref);
	
    if(file_space < 0)
      H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

    /* Check for anything to retrieve */
    if(numelem || buf) {
        hssize_t nelmts = 0;                /* The number of elements in selected region */

        /* Determine the number of elements the dataspace selection */
        if((nelmts = H5Sget_select_npoints(file_space)) < 0)
            H5E_THROW(H5E_CANTGET, "H5LR: Unable to retrieve number of elements in region")

        /* Set the number of elements in the region, if requested */
        if(numelem)
            *numelem = (size_t)nelmts;

        /* Check for retrieving the region's elements */
        if(buf) {
            hsize_t dims1[1] = {(hsize_t)nelmts};     /* The number of elements in memory dataspace */

            /* Create a new simple dataspace in memory and open it for access */
            if((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
                H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace for retrieving elements")

            /* Read the region data from the file_space into the mem_space */
            if(H5Dread(dset, mem_type, mem_space, file_space, H5P_DEFAULT, buf) < 0)
                H5E_THROW(H5E_READERROR, "H5LR: Unable to retrieve elements")
        } /* end if */
    } /* end if */

CATCH
   
   current_stack_id = H5Eget_current_stack();

   /* Close appropriate items */
   if(mem_space > 0)
     status =H5Sclose(mem_space);

   if(file_space > 0)
     status =H5Sclose(file_space);

   if(dset > 0)
     status =H5Dclose(dset);

   status = H5Eset_current_stack(current_stack_id);

END_FUNC(PUB)


/*-------------------------------------------------------------------------
 * Function: H5LRcreate_region_references
 *
 * Purpose: Create an array of region references using an array of paths
 *          to datasets in a file and an array of the corresponding
 *          hyperslab descriptions.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: February 17, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
herr_t, SUCCEED, FAIL,
H5LRcreate_region_references(hid_t file_id,
			     size_t num_elem,
			     const char **path,
			     const hsize_t *block_coord,
			     hdset_reg_ref_t *buf) )

  hid_t sid = -1;          /* identifier of the dataspace */
  hid_t dset_id = -1;      /* destination: dataset */
  herr_t status;           /* API return status */
  int nrank;               /* the dimensionality of the source dataspace */
  int i, j, nstart;        /* counters */
  hsize_t *start, *stride; /* offset of start of hyperslab, hyperslab stride */

  hid_t current_stack_id = -1; /* current error stack id */ 
  /* flags marking state of allocation */
  hbool_t start_alloc = FALSE;
  hbool_t stride_alloc = FALSE;

  nstart = 0;
  for(i=0; i<(int)num_elem; i++) {

      /* Open the dataset for a given the path */
      dset_id = H5Dopen2(file_id, path[i], H5P_DEFAULT);

      if(dset_id < 0)
	H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataset for given path")

      /* Get the dataspace of the dataset */
      sid = H5Dget_space(dset_id);

      if(sid < 0)
	H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataspace for given path")

      /* Find the rank of the dataspace */
      nrank = H5Sget_simple_extent_ndims(sid);
  
      if(nrank < 0)
	H5E_THROW(H5E_BADSELECT, "H5LR: Failed to find the rank of the dataspace")

      /* Create references */

      /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reference */
      /*          1   2        n      1   2        n                          */

      start = (hsize_t *)malloc (sizeof (hsize_t) * nrank);
      if(start == NULL)
	H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
      start_alloc = TRUE;
      stride = (hsize_t *)malloc (sizeof (hsize_t) * nrank);
      if(stride == NULL)
	H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
      stride_alloc = TRUE;

      for (j=0; j<nrank; j++) {
	start[j] = block_coord[nstart + j];
	stride[j] = block_coord[nstart + j + nrank] - start[j] + 1;
      }
      nstart += 2*nrank;

      /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
      /*          1   2        n      1   2        n                                       */
      
      status = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, stride, NULL);
      if(status < 0)
	H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

      /* get the number of element points in the current selection */

      status = (int)H5Sget_select_npoints(sid);
      if(status < 0)
	H5E_THROW(H5E_CANTCOUNT, "H5LR: Failed to retrieve number of points in hyperslab")

      /* Store dataset region */
      status = H5Rcreate(&buf[i], file_id, path[i], H5R_DATASET_REGION, sid);
      if(status < 0)
	H5E_THROW(H5E_CANTCREATE, "H5LR: Failed to create region reference to dataset")

    /* Close the dataspace */
      if(sid > 0) {
	status = H5Sclose(sid);
        sid = -1;
	if(status < 0) {
	  H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	}
      }

    /* Close the dataset */
      if(dset_id > 0) {
	status = H5Dclose(dset_id);
	dset_id = -1;
	if(status < 0) {
	  H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	}
      }
      free(start);
      free(stride);

      start_alloc = FALSE;
      stride_alloc = FALSE;

  } 
  CATCH

    current_stack_id = H5Eget_current_stack();

    if(start_alloc) free(start);
    if(stride_alloc) free(stride);

    /* Close the dataspace */
    if(sid > 0)
      status = H5Sclose(sid);
      
    /* Close the dataset */
    if(dset_id > 0)
      status = H5Dclose(dset_id);

    status = H5Eset_current_stack(current_stack_id);

END_FUNC(PUB)

/*-------------------------------------------------------------------------
 * Function: H5LRmake_dataset
 *
 * Purpose: Creates a dataset and writes data associated with a list of
 *          region references to it.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: February 17, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
	   herr_t, SUCCEED, FAIL,
	   H5LRmake_dataset(hid_t loc_id, const char *path, hid_t type_id, 
			    const size_t buf_size, const hid_t *loc_id_ref, const hdset_reg_ref_t *ref) )

  hid_t dset_ref = -1, sid_ref = -1; /* region reference dataset, dataspace */
  hid_t dset_id, memspace; /* dataset, dataspace */ 
  int nrank, rank_check=0; /* the dimensionality of the dataspace */
  size_t i;      /* counter  */
  int j, icnt;   /* counters */
  hsize_t *dims; /* array of the size of the dataspace */
  void *buf; /* buffer to hold data */
  herr_t status; /* API return status */
  hid_t  filespace; /* filespace */
  size_t numelem; /* number of elements in hyperslab */
  hsize_t *start; /* counter for offset of start of hyperslab */
  hsize_t *bounds_coor;  /* hyperslab corners */
  hid_t current_stack_id = -1; /* current error stack id */ 

  /* flags marking state of allocation */
  hbool_t dims_alloc        = FALSE;
  hbool_t bounds_coor_alloc = FALSE;
  hbool_t buf_alloc         = FALSE;
  hbool_t start_alloc       = FALSE;

  /* loop through the region references to get the overall size 
   * so we can specify the entire dataset and then write 
   * each region by hyperslab.
   */

  for (i=0; i<buf_size; i++) {
    dset_ref = H5Rdereference(loc_id_ref[i], H5R_DATASET_REGION, ref[i]);
    if(dset_ref < 0)
      H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

    /* Retrieve the dataspace with the specified region selected */
    sid_ref = H5Rget_region (dset_ref, H5R_DATASET_REGION, ref[i]);
    if(sid_ref < 0)
      H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

    /* get the rank of the region reference */
    nrank = H5Sget_simple_extent_ndims(sid_ref);
    if(nrank < 0)
      H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

    /* Allocate space for the dimension array */
    if(i == 0) {
      dims = (hsize_t *)malloc (sizeof (hsize_t) * nrank);
      if(dims == NULL)
	H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
      dims_alloc = TRUE;
      for (j=0; j<nrank; j++) dims[j]=0;
      rank_check = nrank;
    } else {
    /* check to make sure the ranks are all the same */
      if(nrank != rank_check)
        H5E_THROW(H5E_BADSELECT, "H5LR: data associated with a list of region references must all have the same dataspace ranks")
    }

    /* get extents of the referenced data */
    bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2);
    if(bounds_coor == NULL)
      H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    bounds_coor_alloc = TRUE;
    /* a region reference is only allowed to reference one block */
    status = H5Sget_select_hyper_blocklist(sid_ref, (hsize_t)0, (hsize_t)1, bounds_coor  );
    if(status < 0)
      H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

    for (j=0; j<nrank; j++) {
      if(j != nrank - 1) {
       dims[j] += bounds_coor[nrank +j] - bounds_coor[j] + 1;
      } 
      else {
       dims[j] = bounds_coor[nrank +j] - bounds_coor[j] + 1;
      }
    }

     /* closes */
    if(dset_ref > 0) {
      status = H5Dclose(dset_ref);
      dset_ref = -1;
      if(status < 0) {
	H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	  }
    }
    if(sid_ref > 0) {
      status = H5Sclose(sid_ref);
      sid_ref = -1;
      if(status < 0) {
	H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	  }
    }

    free(bounds_coor);
    bounds_coor_alloc = FALSE;

  }

/* Create the dataspace for the new dataset */
   filespace = H5Screate_simple(nrank, dims, NULL);
   if(filespace < 0)
     H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace")

   free(dims);
   dims_alloc = FALSE;
/* Create dataset */
   dset_id = H5Dcreate2(loc_id, path, type_id, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
   if(dset_id < 0)
       H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")


   start = (hsize_t *)malloc (sizeof (hsize_t) * nrank);
   if(start == NULL)
     H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
   start_alloc = TRUE;


/* loop over the region references to write data into new data set */

  icnt = 0;
  for (i=0; i<buf_size; i++) {
    dset_ref = H5Rdereference(loc_id_ref[i], H5R_DATASET_REGION, ref[i]);
    if(dset_ref < 0)
      H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

    /* Retrieve the dataspace with the specified region selected */
    sid_ref = H5Rget_region (dset_ref, H5R_DATASET_REGION, ref[i]);
    if(sid_ref < 0)
      H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

    /* Allocate space for the dimension array */
    dims = (hsize_t *)malloc (sizeof (hsize_t) * nrank);
    if(dims == NULL)
      H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    dims_alloc = TRUE;

    /* get extents of the referenced data */
    bounds_coor = (hsize_t *)malloc(sizeof (hsize_t) * nrank * 2);
    if(bounds_coor == NULL)
      H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    bounds_coor_alloc = TRUE;

    /* a region reference is only allowed to reference one block */
    status = H5Sget_select_hyper_blocklist(sid_ref, (hsize_t)0, (hsize_t)1, bounds_coor  );
    if(status < 0)
      H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

    for(j=0; j<nrank; j++) 
      dims[j] = bounds_coor[nrank +j] - bounds_coor[j] + 1;

    numelem = H5Sget_select_npoints(sid_ref);
    if(status < 0)
      H5E_THROW(H5E_CANTCOUNT, "H5LR: Failed to retrieve number of points in hyperslab")

    buf = malloc(sizeof(type_id) * numelem);
    if(buf == NULL)
      H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    buf_alloc = TRUE;

    /* read region reference data into buf */
    status= H5LRread_region(loc_id_ref[i],
			    (const hdset_reg_ref_t*)&ref[i], 
			    type_id,
			    &numelem,
			    buf );
    if(status < 0)
       H5E_THROW(H5E_NONE_MINOR, "H5LR: Failed in internal H5LRread_region")

    start[0] = icnt; 
    for (j=1; j<nrank; j++) start[j] = 0; /* start at the beginning for other dimensions */

    icnt += dims[0];
    memspace = H5Screate_simple(nrank, dims, NULL);
    /* Select hyperslab in the file */
    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, dims, NULL);
    if(status < 0)
      H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

    /* write the hyperslab data set */
    status = H5Dwrite(dset_id, type_id, memspace, filespace, H5P_DEFAULT, buf);
    if(status < 0)
      H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")

     /* closes */

    if(dset_ref > 0) {
      status = H5Dclose(dset_ref);
      dset_ref = -1;
      if(status < 0) {
	H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	  }
    }
    if(sid_ref > 0) {
      status = H5Sclose(sid_ref);
      sid_ref = -1;
      if(status < 0) {
	H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	  }
    }
    if(memspace > 0) {
      status = H5Sclose(memspace);
      memspace = -1;
      if(status < 0) {
	H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	  }
    }

    /* deallocate arrays */

    free(dims);
    free(bounds_coor);
    free(buf);

    dims_alloc = FALSE;
    bounds_coor_alloc = FALSE;
    buf_alloc = FALSE;

  }

  if(dset_id > 0) {
    status = H5Dclose(dset_id);
    dset_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    }
  }

  if(filespace > 0) {
    status = H5Sclose(filespace);
    filespace = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    }
  }

  free(start);
  start_alloc = FALSE;

CATCH

  current_stack_id = H5Eget_current_stack();

  if(dims_alloc) free(dims);
  if(bounds_coor_alloc) free(bounds_coor);
  if(buf_alloc) free(buf);
  if(start_alloc) free(start);

  if(filespace > 0)
    status = H5Sclose(filespace);

  if(memspace > 0)
    status = H5Sclose(memspace);

  if(dset_id > 0)
    status = H5Dclose(dset_id);

  if(dset_ref > 0)
    status = H5Dclose(dset_ref);

  if(sid_ref > 0)
    status = H5Sclose(sid_ref);

  status = H5Eset_current_stack(current_stack_id);

END_FUNC(PUB)

/*-------------------------------------------------------------------------
 * Function: H5LRcopy_region
 *
 * Purpose: Copies data from a region specified by a reference to a region
 *          in a destination dataset.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: February 17, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
herr_t, SUCCEED, FAIL,
H5LRcopy_region(hid_t obj_id,
		hdset_reg_ref_t *ref,
		const char *file,
		const char *path,
		const hsize_t *block_coord))

  herr_t status; /* API return status */
  hsize_t *dims; /* an array of the size of each dimension */
  hid_t file_space_id; /* identifier of the dataset's dataspace in the file */
  hid_t mem_space_id; /* identifier of the memory dataspace */
  hid_t dset_ref, sid_ref; /* region reference dataset, dataspace id */
  hid_t file_id, dset_id, type_id; /*file, dataset, datatype ids */
  int ndim;  /* the dimensionality of the dataspace */
  void *buf; /* buffer to store the dataset */
  size_t numelem; /* number of elements in dataspace */
  int nrank; /* the dimensionality of the dataspace */
  int i, j; /* counters */
  hsize_t *stride;  /* hyperslab stride */
  hsize_t *bounds_coor; /* hyperslab corners */
  hid_t dtype; /* data type of source */
  hid_t current_stack_id    = -1; /* current error stack id */ 
  /* flags marking state of allocation */
  hbool_t dims_alloc        = FALSE;
  hbool_t bounds_coor_alloc = FALSE;
  hbool_t buf_alloc         = FALSE;
  hbool_t stride_alloc      = FALSE;

  /* Region reference data */
  dset_ref = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
  if(dset_ref < 0)
    H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

  /* Region reference space */
  sid_ref = H5Rget_region(dset_ref, H5R_DATASET_REGION, ref);

  if(sid_ref < 0)
    H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

  /* Get the rank of the region reference */
  nrank = H5Sget_simple_extent_ndims(sid_ref);
  if(nrank < 0)
    H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

  bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2);
  if(bounds_coor == NULL)
    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
  bounds_coor_alloc = TRUE;

/* get the list of hyperslab blocks currently selected */
  status = H5Sget_select_hyper_blocklist(sid_ref, (hsize_t)0, (hsize_t)1, bounds_coor);  
  if(status < 0) {
       H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")
	 }

  numelem = 1;
  for (j=0; j<nrank; j++) {
    numelem = (bounds_coor[nrank +j] - bounds_coor[j] + 1)*numelem;
  }

  dtype = H5Dget_type(dset_ref);
  if(dtype < 0)
    H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the data type")

  type_id = H5Tget_native_type(dtype , H5T_DIR_DEFAULT );
  if(type_id < 0)
    H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the native data type")

  buf = malloc(sizeof(type_id) * numelem);
  if(buf == NULL){
    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
      }
  buf_alloc = TRUE;

  /* read data associated with region reference */
  status= H5LRread_region(obj_id,
			  (const hdset_reg_ref_t*)ref,
			  type_id,
			  &numelem,
			  buf);

  if(status < 0)
     H5E_THROW(H5E_NONE_MINOR, "H5LR: Failed in internal H5LRread_region")

  if(sid_ref > 0) {
    status = H5Sclose(sid_ref);
    sid_ref = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	}
  }
  if(dset_ref > 0) {
    status = H5Dclose(dset_ref);
    dset_ref = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	}
  }

/* Open the file */
   
   file_id = H5Fopen(file, H5F_ACC_RDWR,  H5P_DEFAULT);
   if(file_id < 0)
     H5E_THROW(H5E_CANTOPENFILE, "H5LR: Failed to open file")

/* Open the dataset for a given the path */
   dset_id = H5Dopen2(file_id, path, H5P_DEFAULT);
   if(dset_id < 0)
     H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataset")

/* Get the dataspace of the dataset */
   file_space_id = H5Dget_space(dset_id);
   if(file_space_id < 0)
     H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataspace for given path")

/* Find the rank of the dataspace */
   ndim = H5Sget_simple_extent_ndims(file_space_id);
   if(ndim < 0)
     H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

  /* Allocate space for the dimension array */
   dims = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
   if(dims == NULL)
     H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
   dims_alloc = TRUE;

  /* find the dimensions of each data space from the block coordinates */
  for (i=0; i<ndim; i++)
    dims[i] = block_coord[i+ndim] - block_coord[i] + 1;

  /* Create dataspace for writing the buffer */
  mem_space_id = H5Screate_simple(ndim, dims, NULL);
  if(mem_space_id < 0)
    H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace for retrieving elements")

/*   Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for writing memory dataset */
/*            1   2        n      1   2        n                                       */

   stride = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
   if(stride == NULL)
     H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
   stride_alloc = TRUE;
   
   for (i=0; i<ndim; i++) {
     stride[i] = block_coord[i + ndim] - block_coord[i] + 1;
   }

  status = H5Sselect_hyperslab(file_space_id,H5S_SELECT_SET, block_coord,NULL,stride,NULL);
  if(status < 0)
    H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

  status = H5Dwrite(dset_id, type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf);
  if(status < 0)
    H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")

  free(stride);
  free(buf);
  free(dims);
  free(bounds_coor);

  stride_alloc = FALSE;
  buf_alloc = FALSE;
  dims_alloc = FALSE;
  bounds_coor = FALSE;

/* closes */
  if(file_space_id > 0) {
    status = H5Sclose(file_space_id);
    file_space_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	}
  }
  if(mem_space_id > 0) {
    status = H5Sclose(mem_space_id);
    mem_space_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	}
  }

  if(dset_id > 0) {
    status = H5Dclose(dset_id);
    dset_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	}
  }
  if(file_id > 0) {
    status = H5Fclose(file_id);
    file_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	}
  }
      
  if(dtype > 0) {
    status = H5Tclose(dtype);
    dtype = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
	}
  }
  if(type_id > 0) {
    status = H5Tclose(type_id);
    type_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
	}
  }
  CATCH

      current_stack_id = H5Eget_current_stack();

      if(dims_alloc) free(dims);
      if(bounds_coor_alloc) free(bounds_coor);
      if(buf_alloc) free(buf);
      if(stride_alloc) free(stride);

    /* Close the dataspace */
      if(file_space_id > 0)
	status = H5Sclose(file_space_id);
      if(mem_space_id > 0)
	status = H5Sclose(mem_space_id);
      
    /* Close the dataset */
      if(dset_id > 0)
	status = H5Dclose(dset_id);
    /* Close the file */
      if(file_id > 0)
	status = H5Fclose(file_id);
    /* Close the datatypes */
      if(dtype > 0)
	status = H5Tclose(dtype);
      if(type_id > 0)
	status = H5Tclose(type_id);

      status = H5Eset_current_stack(current_stack_id);
      
END_FUNC(PUB)

/*-------------------------------------------------------------------------
 * Function: H5LRcopy_references
 *
 * Purpose: Copy data from the specified dataset to a new location and
 *          create a reference to it.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: February 17, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
herr_t, SUCCEED, FAIL,
H5LRcopy_references(hid_t obj_id, hdset_reg_ref_t *ref, const char *file,
		    const char *path, const hsize_t *block_coord_dset, hdset_reg_ref_t *ref_new) )

  herr_t status; /* API return status */
  hsize_t  *dims, *dims_src; /* an array of the size of each dimension in source and destination */
  hid_t file_space_id; /* identifier of the dataset's dataspace in the file */
  hid_t mem_space_id; /* identifier of the memory dataspace */
  hid_t  dset_id, type_id, file_id; /*dataset, datatype, file ids */
  hid_t did_src, sid_src; /* source dataset, dataspace */
  int ndim;   /*  the dimensionality of the dataspace */
  void *buf;  /* buffer to store the dataset */
  hsize_t  numelem_src; /* number of elements in source dataspace */
  hsize_t  numelem_dset; /* number of elements in destination dataspace */
  int nrank_src;        /* the dimensionality of the source dataspace */
  H5S_sel_type sel_type; /* selection type of hyperslab */
  int i, j; /* counters */ 
  hsize_t *stride;  /* hyperslab stride */
  hsize_t *bounds_coor; /* hyperslab corners */
  hid_t dtype; /* data type of source */

  hid_t current_stack_id = -1; /* current error stack id */ 
  /* flags marking state of allocation */
  hbool_t dims_src_alloc = FALSE;
  hbool_t dims_alloc = FALSE;
  hbool_t bounds_coor_alloc = FALSE;
  hbool_t buf_alloc = FALSE;
  hbool_t stride_alloc = FALSE;

  /* Region reference data */
  did_src = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
  if(did_src < 0)
    H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

  /* Region reference space */
  sid_src = H5Rget_region (did_src, H5R_DATASET_REGION, ref);
  if(sid_src < 0)
    H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

  /* Determine the type of the dataspace selection */
  sel_type = H5Sget_select_type(sid_src);
  if(sel_type < 0)
    H5E_THROW(H5E_BADSELECT, "H5LR: Failed to find selection type")

  /* Find the rank of the dataspace */
  nrank_src = H5Sget_simple_extent_ndims(sid_src);
  if(nrank_src < 0)
    H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

  /* Allocate space for the dimension array */
  dims_src = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src);
  if(dims_src == NULL)
     H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
  dims_src_alloc = TRUE;

  bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src * 2); 
  if(bounds_coor == NULL)
     H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
  bounds_coor_alloc = TRUE;

  /* get the list of hyperslab blocks currently selected */
  status = H5Sget_select_hyper_blocklist(sid_src, (hsize_t)0, (hsize_t)1, bounds_coor);
  if(status < 0)
       H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

  /* get the total number of elements in the source */  
  numelem_src = 1;
  for (j=0; j<nrank_src; j++) {
    dims_src[j] = bounds_coor[nrank_src + j] - bounds_coor[j] + 1;
    numelem_src = dims_src[j]*numelem_src;
  }

  /* get the datatype */
  dtype = H5Dget_type(did_src);
  if(dtype < 0)
       H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the data type")
  type_id = H5Tget_native_type(dtype , H5T_DIR_DEFAULT );
  if(type_id < 0)
    H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the native data type")

  buf = malloc(sizeof(type_id) * numelem_src);
  if(buf == NULL)
    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
  buf_alloc = TRUE;

  /* Create dataspace for reading buffer */
  mem_space_id = H5Screate_simple(nrank_src, dims_src, NULL);
  if(mem_space_id < 0)
     H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace for retrieving elements")

  /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
  /*          1   2        n      1   2        n                                       */

  status = H5Sselect_hyperslab(sid_src,H5S_SELECT_SET,bounds_coor,NULL, dims_src, NULL);
  if(status < 0)
    H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

  /* read dataset */
  status = H5Dread(did_src, type_id, mem_space_id, sid_src, H5P_DEFAULT, buf);
  if(status < 0)
    H5E_THROW(H5E_READERROR, "H5LR: Unable to retrieve elements")


  /* close */
  if(did_src > 0) {
    status = H5Dclose(did_src);
    did_src = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	}
  }
  if(sid_src > 0) {
    status = H5Sclose(sid_src);
    sid_src = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	}
  }
  if(mem_space_id > 0) {
    status = H5Sclose(mem_space_id);
    mem_space_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
	}
  }

  free(dims_src);
  dims_src_alloc = FALSE;

/* Open the file */
  file_id = H5Fopen(file, H5F_ACC_RDWR,  H5P_DEFAULT);
  if(file_id < 0)
    H5E_THROW(H5E_CANTOPENFILE, "H5LR: Failed to open file")

/* Open the dataset for a given the path */
  dset_id = H5Dopen2(file_id, path, H5P_DEFAULT);
  if(dset_id < 0)
    H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataset")

/* Get the dataspace of the dataset */
  file_space_id = H5Dget_space(dset_id);
  if(file_space_id < 0)
    H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataspace for given path")

/*   Find the rank of the dataspace */
  ndim = H5Sget_simple_extent_ndims(file_space_id);
  if(ndim < 0)
    H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

  /* Allocate space for the dimension array */
  dims = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
  if(dims == NULL)
    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
  dims_alloc = TRUE;

  numelem_dset = 1;
  /* find the dimensions of each data space from the block coordinates */
  for (i=0; i<ndim; i++) {
    dims[i] = block_coord_dset[i+ndim] - block_coord_dset[i] + 1;
    numelem_dset = numelem_dset*dims[i];
  }

  /* check the number of elements in the old and newly specified regions are the same */

  if( numelem_dset != numelem_src)
    H5E_THROW(H5E_BADSELECT, "H5LR: Different sized source and destination regions")
  

   /* Create dataspace for writing the buffer */
  mem_space_id = H5Screate_simple(ndim, dims, NULL);
  if(mem_space_id < 0)
    H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace for retrieving elements")

/*   Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for writing memory dataset */
/*            1   2        n      1   2        n                                       */


  stride = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
  if(stride == NULL)
    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
  stride_alloc = TRUE;

  for (i=0; i<ndim; i++) {
    stride[i] = block_coord_dset[i + ndim] - block_coord_dset[i] + 1;
  }

  status = H5Sselect_hyperslab(file_space_id,H5S_SELECT_SET, block_coord_dset,NULL, stride, NULL);
  if(status < 0)
    H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

  status = (int)H5Sget_select_npoints(file_space_id);
  if(status < 0)
    H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select points")

  status = H5Dwrite(dset_id, type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf);
  if(status < 0)
    H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")

  /* create reference */
  status = H5Rcreate(ref_new, file_id, path, H5R_DATASET_REGION, file_space_id);
  if(status < 0)
    H5E_THROW(H5E_CANTCREATE, "H5LR: Failed to create region reference to dataset")

  /* close the data */

  /* Close the dataset */
  if(dset_id > 0) {
    status = H5Dclose(dset_id);
    dset_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    }
  }
  /* Close the dataspace */
  if(file_space_id > 0) {
    status = H5Sclose(file_space_id);
    file_space_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    }
  }

  /* Close the dataspace */
  if(mem_space_id > 0) {
    status = H5Sclose(mem_space_id);
    mem_space_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    }
  }
  /* Close the datatypes */
  if(type_id > 0) {
    status = H5Tclose(type_id);
    type_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
    }
  }
  if(dtype > 0) {
    status = H5Tclose(dtype);
    dtype = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
    }
  }
  /* close file */
  if(file_id > 0) {
    status = H5Fclose(file_id);
    file_id = -1;
    if(status < 0) {
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    }
  }
  free(stride);
  free(buf);
  free(dims);
  free(bounds_coor);

  stride_alloc = FALSE;
  buf_alloc   = FALSE;
  dims_alloc = FALSE;
  bounds_coor = FALSE;

CATCH

  current_stack_id = H5Eget_current_stack();

  if(dims_alloc) free(dims);
  if(bounds_coor_alloc) free(bounds_coor);
  if(buf_alloc) free(buf);
  if(stride_alloc) free(stride);
  if(dims_src_alloc) free(dims_src);

  /* Close the dataspace */
  if(file_space_id > 0)
    status = H5Sclose(file_space_id);
  if(mem_space_id > 0)
    status = H5Sclose(mem_space_id);
      
  /* Close the dataset */
  if(dset_id > 0)
    status = H5Dclose(dset_id);
  /* Close the file */
  if(file_id > 0)
    status = H5Fclose(file_id);
  /* Close the datatypes */
  if(dtype > 0)
    status = H5Tclose(dtype);
  if(type_id > 0)
    status = H5Tclose(type_id);

  status = H5Eset_current_stack(current_stack_id);

END_FUNC(PUB)


/*-------------------------------------------------------------------------
 * Function: H5LRcreate_regref_to_all
 *
 * Purpose: Create a dataset with the region references to the data in all 
 *          datasets located under a specified group in a file.
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: M. Scot Breitenfeld
 *
 * Date: June 2, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PUB, ERR,
herr_t, SUCCEED, FAIL,
H5LRcreate_regref_to_all(hid_t loc_id, const char *group_path,
			 const char *ds_path, H5_index_t index_type, H5_iter_order_t order) )

  herr_t status; /* API return status */
  hid_t filespace;
  hid_t dset_id;
  hsize_t dims[1];
  void *data;
  hsize_t size_data; /* size of the current data set from region references */

  hid_t current_stack_id = -1; /* current error stack id */
  /* flags marking state of allocation */
  hbool_t data_alloc = FALSE;

/* first determine the size of the resulting dataset from the region references */

  size_data = 0;
  status = H5Lvisit_by_name( loc_id, group_path, index_type, order, op_func_L, &size_data, H5P_DEFAULT );

/* create the new dataset with the region references */

/* Create the dataspace for the new dataset */
  dims[0] = size_data;

  filespace = H5Screate_simple(1, dims, NULL);
  if(filespace < 0)
    H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace")

/* Create dataset */
   dset_id = H5Dcreate2(loc_id, ds_path, H5T_NATIVE_INT, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
   if(dset_id < 0)
       H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")

   data = malloc (sizeof (int) * size_data);

   if(data == NULL)
    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")

   data_alloc = TRUE;
   H5LRcreate_glb_counter = 0;
   status = H5Lvisit_by_name( loc_id, group_path, index_type, order, op_func_L1, data, H5P_DEFAULT );

/* write the data */

   status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
   status = H5Dclose(dset_id); 
   dset_id = -1;
   status = H5Sclose(filespace);
   filespace = -1;

/* deallocate memory */
   free(data);
   data_alloc = FALSE;

   

CATCH

   current_stack_id = H5Eget_current_stack();

   if(data_alloc) free(data);

  /* Close the dataspace */
  if(filespace > 0)
    status = H5Sclose(filespace);
      
  /* Close the dataset */
  if(dset_id > 0)
    status = H5Dclose(dset_id);

   status = H5Eset_current_stack(current_stack_id);

END_FUNC(PUB)


