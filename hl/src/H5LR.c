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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "H5LRprivate.h"
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
#define AT() 		printf("	 at %s:%d in %s()...\n",	      \
				__FILE__, __LINE__, __FUNCTION__);
#define H5_FAILED()	{puts("*FAILED*");fflush(stdout);}
#define TEST_ERROR      {H5_FAILED(); AT(); goto error;}

/********************/
/* Local Prototypes */
/********************/
static herr_t op_func_L(hid_t loc_id, const char *name, const H5L_info_t *info, void *operator_data);

static herr_t op_func(hid_t loc_id, const char *name, const H5O_info_t *info, void *operator_data);

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

/* THE FOLLOWING ARE USED BY HL API: H5LRcreate_regref_to_all */

/* starting index counter for placing region references into 1D array, used by H5LRcreate_regref_to_all */
static size_t  _regref_to_all_start;
/* size of the 1D array needed to hold all the region references created by H5LRcreate_regref_to_all */
static hsize_t _regref_to_all_size;
/* type of data to create region references for */
static H5R_type_t _regref_type;
   
/*-------------------------------------------------------------------------
 *
 * Private functions
 *
 *-------------------------------------------------------------------------
 */

/************************************************************

  Operator function used by HL API H5LRcreate_regref_to_all:
     (1) Finds the size (_regref_to_all_size) of the array needed to hold the 
         region references by finding: the number of datasets 
         (for _regref_type == H5R_DATASET_REGION) or number of objects
         (for _regref_type == H5R_OBJECT). This case (1) is run
         if operator_data is NULL.
     (2) Makes the array of region references pointing to either 
         datasets or objects (depending on _regref_type). This case (2)
         is run if operator_data is NOT NULL. 

 ************************************************************/

static herr_t 
op_func(hid_t loc_id, const char *name, const H5O_info_t *info, void *operator_data)
{
    herr_t status; 
    hid_t dtype_id = -1, space = -1; 
    hid_t obj_id = -1;
    size_t numel_size;
    const hdset_reg_ref_t *regref;
    hobj_ref_t regref_obj;

    if(_regref_type == H5R_DATASET_REGION) {
        if(info->type == H5O_TYPE_DATASET) {
	    /* getting the size of the region reference buffer */
	    if(operator_data == NULL) {
	        _regref_to_all_size += 1;
	        return 0;
	    } /* end if */ 
            else {
	        /* Open the dataset for a given the path */
	        if((dtype_id = H5Dopen2(loc_id, name, H5P_DEFAULT) ) < 0) goto out;
	  
	        /* Get the dataspace of the dataset */
	        if((space = H5Dget_space(dtype_id) ) < 0) goto out;
	  
	        /* Select the entire extent of the dataspace */
	        if(H5Sselect_all(space ) < 0 ) goto out;
	  
	        /* Store dataset region */
	        regref =((const hdset_reg_ref_t *)operator_data) + _regref_to_all_start;
	        status = H5Rcreate(regref, loc_id, name, H5R_DATASET_REGION, space);
	  
	        _regref_to_all_start += 1;
	  
	        if(H5Dclose(dtype_id) != 0) goto out;
	        dtype_id = -1;
	        if(H5Sclose(space) != 0) goto out;
	        space = -1;
	    } /* end else */
        } /* end if */
    } /* end if */ 
    else if(_regref_type == H5R_OBJECT) {
        /* getting the size of the region reference buffer */
        if(operator_data == NULL) {
	    _regref_to_all_size += 1;
	    return 0;
        } /* end if */ 
        else {
	    /* Store the object region reference */
	    regref_obj = ((const hobj_ref_t *)operator_data) + _regref_to_all_start;
	    status = H5Rcreate(regref_obj, loc_id, name, H5R_OBJECT, -1);
	  
	    _regref_to_all_start += 1;
	  
        } /* end else */
    } /* end else if */

    return 0;

 out:
    if(dtype_id > 0)
      H5Dclose(dtype_id);
    if(space > 0)
      H5Sclose(space);

    return -2;
}

/************************************************************

  Operator function for H5Lvisit_by_name, used by
  the HL API H5LRcreate_regref_to_all.  This function simply
  retrieves the info for the object the current link points
  to, and calls the operator function, op_func.

 ************************************************************/

static herr_t 
op_func_L(hid_t loc_id, const char *name, const H5L_info_t *info, void *operator_data)
{
    herr_t          status;
    H5O_info_t      infobuf;

    /*
     * Get type of the object and display its name and type.
     * The name of the object is passed to this function by
     * the Library.
     */
    status = H5Oget_info_by_name(loc_id, name, &infobuf, H5P_DEFAULT);
    if(status < 0) return -1;

    return op_func(loc_id, name, &infobuf, operator_data);

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
H5LRget_region_info(hid_t obj_id, const hdset_reg_ref_t *ref, size_t *len,                
    char *path, int *rank, hid_t *dtype, H5S_sel_type *sel_type,
    size_t *numelem,hsize_t *buf))

    hid_t dset = -1;  /* dataset id */
    hid_t space = -1; /* space id */

    /* If user supplied NULL for sel_type or numelem we need to define local variables */
    H5S_sel_type sel_type_loc; /* Local copy of type of selection */
    size_t numelem_loc = 0;    /* Number of coordinate blocks or selected elements */

    herr_t status; /* State of the return value of api */
    hid_t current_stack_id = -1; /* Current stack id */

    /* Set up a dataspace and selection as specified by a region reference */
    space = H5Rget_region(obj_id, H5R_DATASET_REGION, ref);
    if(space < 0)
        H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open region referenced")

    /* Try to open object */
    dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
    if(dset < 0)
        H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open the dataset associated to region reference")

    /* Determine the type of the dataspace selection */
    sel_type_loc = H5Sget_select_type(space);
    if(sel_type)
        *sel_type = sel_type_loc;

    /* get the number of elements */
    if(sel_type_loc == H5S_SEL_HYPERSLABS) {
        numelem_loc = (size_t)H5Sget_select_hyper_nblocks(space);
    } else if(sel_type_loc == H5S_SEL_POINTS) {
        numelem_loc = (size_t)H5Sget_select_npoints(space);
    } else {
        H5E_THROW(H5E_BADSELECT, "H5LR: Failed to find selection type")
    } /* end if */

    if(numelem)
        *numelem = numelem_loc;

    if(numelem_loc == 0) 
        H5E_THROW(H5E_CANTCOUNT, "H5LR: Failed to count elements in dataspace")

    /* the dimensionality of the dataspace */
    if(rank) {
        *rank = (int)H5Sget_simple_extent_ndims(space);
        if(*rank < 0)
            H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataset")
    } /* end if */

    /* get the data type */
    if(dtype) {
        *dtype = (hid_t)H5Dget_type(dset);
        if(*dtype < 0)
            H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the data type")
    } /* end if */

    /* Determine the size of the name buffer, with null character included */
    if(!path)
        *len = (size_t)(1 + H5Iget_name(dset, NULL, (size_t)0));
    
    if(path) {
        /* Get the data set name the region reference points to */
        status = H5Iget_name(dset, path, *len);
        if(status < 0)
            H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the name associated with the region reference")
    } /* end if */

    if(buf) {
        /* get the corner coordinates of the hyperslab */
        if(sel_type_loc == H5S_SEL_HYPERSLABS) {
            /* get the list of hyperslab blocks currently selected */
            status = H5Sget_select_hyper_blocklist(space, (hsize_t)0, (hsize_t)1, buf);
            if(status < 0)
	        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")
        } /* end if */
        else if(sel_type_loc == H5S_SEL_POINTS) {
            /* get the list of elements currently selected */
            status = H5Sget_select_elem_pointlist(space, (hsize_t)0, (hsize_t)1, buf);
            if(status < 0)
	        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of selected elements")
	} /* end else if */
    } /* end if */
CATCH
    current_stack_id = H5Eget_current_stack();

    /* Close the dataspace */
    if(space > 0)
        status = H5Sclose(space);
      
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
H5LRread_region(hid_t obj_id, const hdset_reg_ref_t *ref, hid_t mem_type,             
    size_t *numelem,void *buf))                

    hid_t dset = -1;                /* Identifier of the dataset's dataspace in the file */
    hid_t file_space = -1;          /* Identifier of the dataset's dataspace in the file */
    hid_t mem_space = -1;           /* Identifier of the memory dataspace */
    herr_t status;                  /* API return status */
    hid_t current_stack_id = -1;    /* current error stack id */ 

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
        hssize_t nelmts = 0;   /* The number of elements in selected region */

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
H5LRcreate_region_references(hid_t file_id, size_t num_elem, const char **path,
    const hsize_t *block_coord, hdset_reg_ref_t *buf))

    hid_t space = -1;        /* identifier of the dataspace */
    hid_t dset_id = -1;      /* destination: dataset */
    herr_t status;           /* API return status */
    int nrank;               /* the dimensionality of the source dataspace */
    int i;                   /* counter */
    int j;                   /* counter */
    int nstart;              /* counter for start of hyperslab*/
    hsize_t *start;          /* offset of start of hyperslab*/
    hsize_t *stride;         /* hyperslab stride */

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
        space = H5Dget_space(dset_id);

        if(space < 0)
	    H5E_THROW(H5E_CANTOPENOBJ, "H5LR: Failed to open dataspace for given path")

        /* Find the rank of the dataspace */
            nrank = H5Sget_simple_extent_ndims(space);
  
        if(nrank < 0)
	    H5E_THROW(H5E_BADSELECT, "H5LR: Failed to find the rank of the dataspace")

        /* Create references */

        /* Select (x , x , ..., x) x (y , y , ..., y) hyperslab for reference */
        /*          1   2        n      1   2        n                          */

        start = (hsize_t *)malloc(sizeof(hsize_t) * nrank);
        if(start == NULL)
	    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
        start_alloc = TRUE;
        stride = (hsize_t *)malloc(sizeof(hsize_t) * nrank);
        if(stride == NULL)
	    H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
        stride_alloc = TRUE;

        for(j=0; j<nrank; j++) {
	    start[j] = block_coord[nstart + j];
	    stride[j] = block_coord[nstart + j + nrank] - start[j] + 1;
        }
        nstart += 2*nrank;

        /* Select (x , x , ..., x) x (y , y , ..., y) hyperslab for reading memory dataset */
        /*          1   2        n      1   2        n                                       */
      
        status = H5Sselect_hyperslab(space, H5S_SELECT_SET, start, NULL, stride, NULL);
        if(status < 0)
	    H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

        /* get the number of element points in the current selection */

        status = (int)H5Sget_select_npoints(space);
        if(status < 0)
	    H5E_THROW(H5E_CANTCOUNT, "H5LR: Failed to retrieve number of points in hyperslab")

        /* Store dataset region */
        status = H5Rcreate(&buf[i], file_id, path[i], H5R_DATASET_REGION, space);
        if(status < 0)
	    H5E_THROW(H5E_CANTCREATE, "H5LR: Failed to create region reference to dataset")

        /* Close the dataspace */
        if(space > 0) {
	    status = H5Sclose(space);
	    space = -1;
	    if(status < 0)
	        H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
        }

        /* Close the dataset */
        if(dset_id > 0) {
	    status = H5Dclose(dset_id);
	    dset_id = -1;
	    if(status < 0)
	        H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
	} /* end if */
        free(start);
        free(stride);

        start_alloc = FALSE;
        stride_alloc = FALSE;

    } /* end for */
CATCH

    current_stack_id = H5Eget_current_stack();

    if(start_alloc) free(start);
    if(stride_alloc) free(stride);

    /* Close the dataspace */
    if(space > 0)
      status = H5Sclose(space);
      
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
    const size_t buf_size, const hid_t *loc_id_ref, const hdset_reg_ref_t *ref))

    hid_t dset_ref = -1; /* region reference dataset, dataspace */
    hid_t space_ref = -1; /* region reference dataset, dataspace */
    hid_t dset_id; /* dataset */
    hid_t memspace; /* memory dataspace */
    int nrank; /* the dimensionality of the dataspace */
    int rank_check=0; /* check for rank of the dataspace */
    size_t i;      /* counter  */
    int j;         /* counters */
    int icnt;      /* counters */
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

    for(i=0; i<buf_size; i++) {
        dset_ref = H5Rdereference(loc_id_ref[i], H5R_DATASET_REGION, ref[i]);
        if(dset_ref < 0)
            H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

        /* Retrieve the dataspace with the specified region selected */
        space_ref = H5Rget_region (dset_ref, H5R_DATASET_REGION, ref[i]);
        if(space_ref < 0)
            H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

        /* get the rank of the region reference */
        nrank = H5Sget_simple_extent_ndims(space_ref);
        if(nrank < 0)
            H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

        /* Allocate space for the dimension array */
        if(i == 0) {
            dims = (hsize_t *)malloc(sizeof(hsize_t) * nrank);
            if(dims == NULL)
	        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
            dims_alloc = TRUE;
            for(j=0; j<nrank; j++) 
                dims[j] = 0;
            rank_check = nrank;
	} /* end if */
	else {
        /* check to make sure the ranks are all the same */
            if(nrank != rank_check)
                H5E_THROW(H5E_BADSELECT, "H5LR: data associated with a list of region references must all have the same dataspace ranks")
        } /* end else */

        /* get extents of the referenced data */
        bounds_coor = (hsize_t *)malloc(sizeof(hsize_t) * nrank * 2);
        if(bounds_coor == NULL)
            H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
        bounds_coor_alloc = TRUE;
        /* a region reference is only allowed to reference one block */
        status = H5Sget_select_hyper_blocklist(space_ref, (hsize_t)0, (hsize_t)1, bounds_coor );
        if(status < 0)
            H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

        for(j=0; j<nrank; j++) {
            if(j != nrank-1) {
                dims[j] += bounds_coor[nrank +j] - bounds_coor[j] + 1;
            } /* end if */
            else {
                dims[j] = bounds_coor[nrank +j] - bounds_coor[j] + 1;
            } /* end else */
        } /*end for */

        /* closes */
       if(dset_ref > 0) {
           status = H5Dclose(dset_ref);
           dset_ref = -1;
           if(status < 0)
	       H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
       } /* end if */
       if(space_ref > 0) {
           status = H5Sclose(space_ref);
           space_ref = -1;
           if(status < 0)
	       H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
       } /* end if */

       free(bounds_coor);
       bounds_coor_alloc = FALSE;
    } /* end for */

    /* Create the dataspace for the new dataset */
    filespace = H5Screate_simple(nrank, dims, NULL);
    if(filespace < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace")

    free(dims);
    dims_alloc = FALSE;
    /* Create dataset */
    dset_id = H5Dcreate2(loc_id, path, type_id, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset_id < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")


    start = (hsize_t *)malloc(sizeof(hsize_t) * nrank);
    if(start == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    start_alloc = TRUE;

    /* loop over the region references to write data into new data set */
    icnt = 0;
    for(i=0; i<buf_size; i++) {
        dset_ref = H5Rdereference(loc_id_ref[i], H5R_DATASET_REGION, ref[i]);
        if(dset_ref < 0)
            H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to open object referenced")

        /* Retrieve the dataspace with the specified region selected */
        space_ref = H5Rget_region(dset_ref, H5R_DATASET_REGION, ref[i]);
        if(space_ref < 0)
            H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

        /* Allocate space for the dimension array */
        dims = (hsize_t *)malloc(sizeof(hsize_t) * nrank);
        if(dims == NULL)
            H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
        dims_alloc = TRUE;

        /* get extents of the referenced data */
        bounds_coor = (hsize_t *)malloc(sizeof(hsize_t) * nrank * 2);
        if(bounds_coor == NULL)
            H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
        bounds_coor_alloc = TRUE;

        /* a region reference is only allowed to reference one block */
        status = H5Sget_select_hyper_blocklist(space_ref, (hsize_t)0, (hsize_t)1, bounds_coor );
        if(status < 0)
            H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

        for(j=0; j<nrank; j++) 
            dims[j] = bounds_coor[nrank+j] - bounds_coor[j] + 1;

        numelem = H5Sget_select_npoints(space_ref);
        if(status < 0)
            H5E_THROW(H5E_CANTCOUNT, "H5LR: Failed to retrieve number of points in hyperslab")

        buf = malloc(sizeof(type_id) * numelem);
        if(buf == NULL)
            H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
        buf_alloc = TRUE;

        /* read region reference data into buf */
        status= H5LRread_region(loc_id_ref[i],(const hdset_reg_ref_t*)&ref[i],type_id,&numelem,buf);
        if(status < 0)
            H5E_THROW(H5E_NONE_MINOR, "H5LR: Failed in internal H5LRread_region")

        start[0] = icnt; 
        for(j=1; j<nrank; j++)
            start[j] = 0; /* start at the beginning for other dimensions */

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
            if(status < 0)
	        H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
        } /* end if */
        if(space_ref > 0) {
            status = H5Sclose(space_ref);
            space_ref = -1;
            if(status < 0)
	        H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
        } /* end if */
        if(memspace > 0) {
            status = H5Sclose(memspace);
            memspace = -1;
            if(status < 0)
	        H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
        } /* end if */

        /* deallocate arrays */

        free(dims);
        free(bounds_coor);
        free(buf);

        dims_alloc = FALSE;
        bounds_coor_alloc = FALSE;
        buf_alloc = FALSE;

    } /* end for */

    if(dset_id > 0) {
        status = H5Dclose(dset_id);
        dset_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */

    if(filespace > 0) {
        status = H5Sclose(filespace);
        filespace = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */

    free(start);
    start_alloc = FALSE;

CATCH

    current_stack_id = H5Eget_current_stack();

    if(dims_alloc) 
        free(dims);
    if(bounds_coor_alloc) 
        free(bounds_coor);
    if(buf_alloc) 
        free(buf);
    if(start_alloc) 
        free(start);

    if(filespace > 0)
        status = H5Sclose(filespace);

    if(memspace > 0)
        status = H5Sclose(memspace);

    if(dset_id > 0)
        status = H5Dclose(dset_id);

    if(dset_ref > 0)
        status = H5Dclose(dset_ref);

    if(space_ref > 0)
        status = H5Sclose(space_ref);

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
H5LRcopy_region(hid_t obj_id, hdset_reg_ref_t *ref,const char *file,
    const char *path, const hsize_t *block_coord))

    herr_t status; /* API return status */
    hsize_t *dims; /* an array of the size of each dimension */
    hid_t file_space_id; /* identifier of the dataset's dataspace in the file */
    hid_t mem_space_id; /* identifier of the memory dataspace */
    hid_t dset_ref, space_ref; /* region reference dataset, dataspace id */
    hid_t file_id; /* file ids */
    hid_t dset_id; /* dataset ids */
    hid_t type_id; /* datatype ids */
    int ndim;  /* the dimensionality of the dataspace */
    void *buf; /* buffer to store the dataset */
    size_t numelem; /* number of elements in dataspace */
    int nrank; /* the dimensionality of the dataspace */
    int i; /* counters */
    int j; /* counters */
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
    space_ref = H5Rget_region(dset_ref, H5R_DATASET_REGION, ref);

    if(space_ref < 0)
        H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

    /* Get the rank of the region reference */
    nrank = H5Sget_simple_extent_ndims(space_ref);
    if(nrank < 0)
        H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

    bounds_coor = (hsize_t *)malloc(sizeof(hsize_t) * nrank * 2);
    if(bounds_coor == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    bounds_coor_alloc = TRUE;

    /* get the list of hyperslab blocks currently selected */
    status = H5Sget_select_hyper_blocklist(space_ref, (hsize_t)0, (hsize_t)1, bounds_coor);  
    if(status < 0)
        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

    numelem = 1;
    for(j=0; j<nrank; j++)
        numelem = (bounds_coor[nrank +j] - bounds_coor[j] + 1)*numelem;

    dtype = H5Dget_type(dset_ref);
    if(dtype < 0)
        H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the data type")

    type_id = H5Tget_native_type(dtype , H5T_DIR_DEFAULT);
    if(type_id < 0)
        H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the native data type")

    buf = malloc(sizeof(type_id) * numelem);
    if(buf == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")

    buf_alloc = TRUE;

    /* read data associated with region reference */
    status= H5LRread_region(obj_id,(const hdset_reg_ref_t*)ref,type_id,&numelem,buf);

    if(status < 0)
        H5E_THROW(H5E_NONE_MINOR, "H5LR: Failed in internal H5LRread_region")

    if(space_ref > 0) {
        status = H5Sclose(space_ref);
        space_ref = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */
    if(dset_ref > 0) {
        status = H5Dclose(dset_ref);
        dset_ref = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */

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
    dims = (hsize_t *)malloc(sizeof(hsize_t) * ndim);
    if(dims == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    dims_alloc = TRUE;

    /* find the dimensions of each data space from the block coordinates */
    for(i=0; i<ndim; i++)
        dims[i] = block_coord[i+ndim] - block_coord[i] + 1;

    /* Create dataspace for writing the buffer */
    mem_space_id = H5Screate_simple(ndim, dims, NULL);
    if(mem_space_id < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace for retrieving elements")

    /*   Select (x , x , ..., x) x (y , y , ..., y) hyperslab for writing memory dataset */
    /*            1   2        n      1   2        n                                       */

    stride = (hsize_t *)malloc(sizeof(hsize_t) * ndim);
    if(stride == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    stride_alloc = TRUE;
   
    for(i=0; i<ndim; i++)
        stride[i] = block_coord[i + ndim] - block_coord[i] + 1;

    status = H5Sselect_hyperslab(file_space_id,H5S_SELECT_SET, block_coord, NULL, stride, NULL);
    if(status < 0)
        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

    status = H5Dwrite(dset_id, type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf);
    if(status < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")

    free(stride);
    stride_alloc = FALSE;
    free(buf);
    buf_alloc = FALSE;
    free(dims);
    dims_alloc = FALSE;
    free(bounds_coor);
    bounds_coor = FALSE;

    /* closes */
    if(file_space_id > 0) {
        status = H5Sclose(file_space_id);
        file_space_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */
    if(mem_space_id > 0) {
        status = H5Sclose(mem_space_id);
        mem_space_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */
    if(dset_id > 0) {
        status = H5Dclose(dset_id);
        dset_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */
    if(file_id > 0) {
        status = H5Fclose(file_id);
        file_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */
      
    if(dtype > 0) {
        status = H5Tclose(dtype);
        dtype = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
    } /* end if */
    if(type_id > 0) {
        status = H5Tclose(type_id);
        type_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
    } /* end if */
CATCH

    current_stack_id = H5Eget_current_stack();

    if(dims_alloc) 
        free(dims);
    if(bounds_coor_alloc) 
        free(bounds_coor);
    if(buf_alloc) 
        free(buf);
    if(stride_alloc) 
        free(stride);

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
    const char *path, const hsize_t *block_coord_dset, hdset_reg_ref_t *ref_new))

    herr_t status; /* API return status */
    hsize_t *dims; /* an array of the size of each dimension for the destination */
    hsize_t *dims_src; /* an array of the size of each dimension for the source */
    hid_t file_space_id; /* identifier of the dataset's dataspace in the file */
    hid_t mem_space_id; /* identifier of the memory dataspace */
    hid_t file_id; /* file ids */
    hid_t dset_id; /* dataset ids */
    hid_t type_id; /* datatype ids */
    hid_t did_src; /* source dataset */
    hid_t space_src; /* source dataspace */
    int ndim;   /*  the dimensionality of the dataspace */
    void *buf;  /* buffer to store the dataset */
    hsize_t  numelem_src; /* number of elements in source dataspace */
    hsize_t  numelem_dset; /* number of elements in destination dataspace */
    int nrank_src;        /* the dimensionality of the source dataspace */
    H5S_sel_type sel_type; /* selection type of hyperslab */
    int i; /* counters */
    int j; /* counters */ 
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
    space_src = H5Rget_region (did_src, H5R_DATASET_REGION, ref);
    if(space_src < 0)
        H5E_THROW(H5E_CANTGET, "H5LR: Retrieving dataspace referenced failed")

    /* Determine the type of the dataspace selection */
    sel_type = H5Sget_select_type(space_src);
    if(sel_type < 0)
        H5E_THROW(H5E_BADSELECT, "H5LR: Failed to find selection type")

    /* Find the rank of the dataspace */
    nrank_src = H5Sget_simple_extent_ndims(space_src);
    if(nrank_src < 0)
        H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

    /* Allocate space for the dimension array */
    dims_src = (hsize_t *)malloc(sizeof(hsize_t) * nrank_src);
    if(dims_src == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    dims_src_alloc = TRUE;

    bounds_coor = (hsize_t *)malloc(sizeof(hsize_t) * nrank_src * 2); 
    if(bounds_coor == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    bounds_coor_alloc = TRUE;

    /* get the list of hyperslab blocks currently selected */
    status = H5Sget_select_hyper_blocklist(space_src, (hsize_t)0, (hsize_t)1, bounds_coor);
    if(status < 0)
        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to find list of hyperslab blocks")

    /* get the total number of elements in the source */  
    numelem_src = 1;
    for(j=0; j<nrank_src; j++) {
        dims_src[j] = bounds_coor[nrank_src + j] - bounds_coor[j] + 1;
        numelem_src = dims_src[j]*numelem_src;
    } /* end for */ 

    /* get the datatype */
    dtype = H5Dget_type(did_src);
    if(dtype < 0)
        H5E_THROW(H5E_CANTGET, "H5LR: Failed to find the data type")
    type_id = H5Tget_native_type(dtype , H5T_DIR_DEFAULT);
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

    /* Select (x , x , ..., x) x (y , y , ..., y) hyperslab for reading memory dataset */
    /*          1   2        n      1   2        n                                       */

    status = H5Sselect_hyperslab(space_src,H5S_SELECT_SET,bounds_coor,NULL, dims_src, NULL);
    if(status < 0)
        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

    /* read dataset */
    status = H5Dread(did_src, type_id, mem_space_id, space_src, H5P_DEFAULT, buf);
    if(status < 0)
        H5E_THROW(H5E_READERROR, "H5LR: Unable to retrieve elements")


    /* close */
    if(did_src > 0) {
        status = H5Dclose(did_src);
        did_src = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */
    if(space_src > 0) {
        status = H5Sclose(space_src);
        space_src = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */
    if(mem_space_id > 0) {
        status = H5Sclose(mem_space_id);
        mem_space_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
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

    /* Find the rank of the dataspace */
    ndim = H5Sget_simple_extent_ndims(file_space_id);
    if(ndim < 0)
        H5E_THROW(H5E_NOTFOUND, "H5LR: Failed to find extents of dataspace")

    /* Allocate space for the dimension array */
    dims = (hsize_t *)malloc(sizeof(hsize_t) * ndim);
    if(dims == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    dims_alloc = TRUE;

    numelem_dset = 1;
    /* find the dimensions of each data space from the block coordinates */
    for(i=0; i<ndim; i++) {
        dims[i] = block_coord_dset[i+ndim] - block_coord_dset[i] + 1;
        numelem_dset = numelem_dset*dims[i];
    } /* end for */ 

    /* check the number of elements in the old and newly specified regions are the same */
    if(numelem_dset != numelem_src)
        H5E_THROW(H5E_BADSELECT, "H5LR: Different sized source and destination regions")
  

    /* Create dataspace for writing the buffer */
        mem_space_id = H5Screate_simple(ndim, dims, NULL);
        if(mem_space_id < 0)
            H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace for retrieving elements")

    /*   Select (x , x , ..., x) x (y , y , ..., y) hyperslab for writing memory dataset */
    /*            1   2        n      1   2        n                                       */


    stride = (hsize_t *)malloc(sizeof(hsize_t) * ndim);
    if(stride == NULL)
         H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")
    stride_alloc = TRUE;

    for(i=0; i<ndim; i++)
         stride[i] = block_coord_dset[i + ndim] - block_coord_dset[i] + 1;

    status = H5Sselect_hyperslab(file_space_id,H5S_SELECT_SET, block_coord_dset,NULL, stride, NULL);
    if(status < 0)
        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select hyperslab")

    if(H5Sget_select_npoints(file_space_id) < 0)
        H5E_THROW(H5E_CANTSELECT, "H5LR: Failed to select points")

    status = H5Dwrite(dset_id, type_id, mem_space_id, file_space_id, H5P_DEFAULT, buf);
    if(status < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataset")

    /* create reference */
    status = H5Rcreate(ref_new, file_id, path, H5R_DATASET_REGION, file_space_id);
    if(status < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Failed to create region reference to dataset")

    /* CLOSE THE DATA */

    /* Close the dataset */
    if(dset_id > 0) {
        status = H5Dclose(dset_id);
        dset_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */
    /* Close the dataspace */
    if(file_space_id > 0) {
        status = H5Sclose(file_space_id);
        file_space_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */
    /* Close the dataspace */
    if(mem_space_id > 0) {
        status = H5Sclose(mem_space_id);
        mem_space_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataspace")
    } /* end if */
    /* Close the datatypes */
    if(type_id > 0) {
        status = H5Tclose(type_id);
        type_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
    } /* end if */
    if(dtype > 0) {
        status = H5Tclose(dtype);
        dtype = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")
    } /* end if */
    /* close file */
    if(file_id > 0) {
        status = H5Fclose(file_id);
        file_id = -1;
        if(status < 0)
            H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close dataset")
    } /* end if */
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

    if(dims_alloc) 
        free(dims);
    if(bounds_coor_alloc) 
        free(bounds_coor);
    if(buf_alloc) 
        free(buf);
    if(stride_alloc) 
        free(stride);
    if(dims_src_alloc) 
        free(dims_src);

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
 * Purpose: Create a dataset with the region references to all datasets or
 *          objects recursively located under a specified group in a file.
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
    const char *ds_path, H5_index_t index_type, H5_iter_order_t order, H5R_type_t ref_type))

    herr_t status; /* API return status */
    hid_t filespace;
    hid_t dset_id;
    hsize_t dims[1];  
    void *data;       /* 1D array to hold data from region references */
    size_t size_ref;

    hid_t current_stack_id = -1; /* current error stack id */

    /* flags marking state of allocation */
    hbool_t data_alloc = FALSE;

    _regref_to_all_size = 0;
    _regref_type = ref_type;

    /* First determine the number of datasets or objects*/
    status = H5Lvisit_by_name(loc_id, group_path, index_type, order, op_func_L, NULL, H5P_DEFAULT);

    if((herr_t)*op_func_L < 0) {
        if((herr_t)*op_func_L == -1) {
            H5E_THROW(H5E_CANTCREATE, "H5LR: Failure in internal callback routine H5Oget_info_by_name ")
	} /* end if*/
        else if((herr_t)*op_func_L == -2) {
           H5E_THROW(H5E_CANTCREATE, "H5LR: Failure in internal callback loop over region references  ")
	} /* end else if */
    } /* end if */

    /* if there are no datasets or objects then return */
    if(_regref_to_all_size == 0) 
        goto catch_except;
  
    /* CREATE THE NEW DATASET REGION REFERENCES */

    /* Create the dataspace for the new dataset */
  
    dims[0] = (size_t)_regref_to_all_size;

    filespace = H5Screate_simple(1, dims, NULL);
    if(filespace < 0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create dataspace")

    /* _regref_to_all_size size of the current dataset from region references */
    if(_regref_type == H5R_DATASET_REGION) {
	data = (hdset_reg_ref_t*)malloc(sizeof(hdset_reg_ref_t) * dims[0]);
    } /* end if */
    else {
	data = (hobj_ref_t*)malloc(sizeof(hobj_ref_t) * dims[0]);
    } /* end else */

    if(data == NULL)
        H5E_THROW(H5E_CANTALLOC, "H5LR: Failed to allocate enough memory")

    data_alloc = TRUE;

    /* create the region references */
    _regref_to_all_start = 0;
    status = H5Lvisit_by_name(loc_id, group_path, index_type, order, op_func_L, data, H5P_DEFAULT);

    if(status <0)
        H5E_THROW(H5E_CANTCREATE, "H5LR: Failure in reading or writing data region references to datasets")

    if((herr_t)*op_func_L < 0) {
        if((herr_t)*op_func_L == -1) {
            H5E_THROW(H5E_CANTCREATE, "H5LR: Failure in internal callback routine H5Oget_info_by_name ")
	} /* end if */
        else if((herr_t)*op_func_L == -2)
            H5E_THROW(H5E_CANTCREATE, "H5LR: Failure in internal callback loop over region references  ")
    } /* end if */

    if(_regref_type == H5R_DATASET_REGION) {
        /* Create dataset */
        dset_id = H5Dcreate2(loc_id, ds_path, H5T_STD_REF_DSETREG, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if(dset_id < 0)
            H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create region references to datasets")

        /* write the data */
        status = H5Dwrite(dset_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    } /* end if */
    else {
        dset_id = H5Dcreate2(loc_id, ds_path, H5T_STD_REF_OBJ, filespace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if(dset_id < 0)
            H5E_THROW(H5E_CANTCREATE, "H5LR: Unable to create region references to objects")
        status = H5Dwrite(dset_id, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
    } /* end else */
    status = H5Dclose(dset_id);
    dset_id = -1;
    status = H5Sclose(filespace);
    filespace = -1;

    /* deallocate memory */
    free(data);
    data_alloc = FALSE;

    /* close resources */

    if(status < 0)
      H5E_THROW(H5E_CLOSEERROR, "H5LR: Failed to close datatype")


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


