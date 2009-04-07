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

#define H5LR_MODULE

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "H5ERROR.h"
#include "H5Eprivate.h"		/* Error handling */
#include "H5LRprivate.h"



/*-------------------------------------------------------------------------
 *
 * internal functions
 *
 *-------------------------------------------------------------------------
 */

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
   
herr_t H5LRget_region_info(hid_t obj_id,               /* -IN-      Id. of any object in a file associated with reference */
			   const hdset_reg_ref_t *ref, /* -IN-      Region reference to query                             */ 
			   size_t *len,                /* -IN/OUT-  Size of the buffer path                               */
			   char *path,                 /* -OUT-     Full path that a region reference points to           */
			   int *rank,                  /* -OUT-     The number of dimensions of the dataset pointed by region reference */
			   hid_t *dtype,               /* -OUT-     Dataset datatype pointed by region reference          */
			   H5S_sel_type *sel_type,     /* -OUT-     Type fo the selection (point or hyperslab)            */
			   size_t *numelem,            /* -IN/OUT-  Number of coordinate blocks or selected elements      */
			   hsize_t *buf )              /* -OUT-     Buffer containing description of the region           */

{
  hid_t dset = -1, sid = -1;
  hid_t ret_value = SUCCEED;          /* Return value */
  herr_t status;
  
  /* Determine the rank of the space */
  sid = H5Rget_region(obj_id, H5R_DATASET_REGION, ref);
  /* Determine the type of the dataspace selection */
  *sel_type = H5Sget_select_type(sid);

  if(*sel_type!=H5S_SEL_HYPERSLABS) printf("wrong select type\n");
    
  /* Try to open object */
  dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
  
  if(dset < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n INVALID OBJECT TYPE\n\t- Argument 1 \n");
  }

  /* Determine if the user only wants the size and rank returned */

  if( path == NULL) {

    /* Determine the size of the name buffer, with null character included */
    *len = (size_t)(1 + H5Iget_name (dset, NULL, (size_t)0));

    /* Determine the rank of the space */
      sid = H5Rget_region (obj_id,  H5R_DATASET_REGION, ref);
    if(sid < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }

      *rank = (int)H5Sget_simple_extent_ndims(sid);
    if(*rank < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }
  /* Determine the type of the dataspace selection */
      *sel_type = H5Sget_select_type(sid);

  /* get the number of elements */
      if(*sel_type==H5S_SEL_HYPERSLABS){
	*numelem = (size_t)H5Sget_select_hyper_nblocks(sid);
      } else if(*sel_type==H5S_SEL_POINTS){
	*numelem = (size_t)H5Sget_select_npoints(sid);
      }
      status = H5Sclose(sid);
    if(status < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }

    status = H5Dclose(dset);
    if(status < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }
    return SUCCEED;
  }

  /* Get the space identity region reference points to */
  sid = H5Rget_region (obj_id, H5R_DATASET_REGION, ref);
  if(sid < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }
  
  /* Get the data set name the region reference points to */
  status = H5Iget_name (dset, path, *len);
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }

  /* get the data type */
    *dtype = (hid_t)H5Dget_type(dset);
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }

/*       if((native_type = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) */

  /* get the number of elements */
    if(*sel_type==H5S_SEL_HYPERSLABS){
      *numelem = (size_t)H5Sget_select_hyper_nblocks(sid);
    } else if(*sel_type==H5S_SEL_POINTS){
      *numelem = (size_t)H5Sget_select_npoints(sid);
    }


  /* get the corner coordinates of the hyperslab */
  if(*sel_type == H5S_SEL_HYPERSLABS) {
    /* get the list of hyperslab blocks currently selected */
      status = H5Sget_select_hyper_blocklist(sid, (hsize_t)0, (hsize_t)1, buf);
  }    
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }

  status = H5Sclose(sid);
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }
  
  status = H5Dclose(dset);
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }
  return SUCCEED;

 done:
  return SUCCEED;
}

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

/* BEGIN_FUNC(scope, use_err, ret_typ, ret_init, err, func) */
BEGIN_FUNC(PUB, ERR,
herr_t, NULL, ret_value,
H5LRread_region(hid_t obj_id,               /* -IN-      Id. of any object in a file associated with reference */
		const hdset_reg_ref_t *ref, /* -IN-      Region reference to query                             */
		hid_t mem_type,             /* -IN-      Id. of the memory datatype                            */
		size_t *numelem,            /* -IN/OUT-  Number of elements in the referenced region           */
		void *buf                   /* -OUT-     Buffer containing data from the referenced region     */
		) )

  hid_t dset = -1, file_space = -1;  /* Identifier of the dataset's dataspace in the file */
  hid_t ret_value = SUCCEED;         /* Return value                                      */
  H5S_sel_type sel_type;             /* Type of the dataspace selection                   */
  hid_t mem_space;                   /* Identifier of the memory dataspace                */
  herr_t status;                     /* API's error status                                */
  hsize_t dims1[1];

  /* Open the HDF5 object referenced */ 

  H5E_BEGIN_TRY {
    dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
  } H5E_END_TRY
  
  if( dset < 0){
    H5Eclear2(H5E_DEFAULT);
    H5E_THROW(H5E_BADSELECT, "Unable to open object referenced");
  }

  /* Retrieve the dataspace with the specified region selected */
  file_space = H5Rget_region (dset, H5R_DATASET_REGION, ref);

  /* Determine the type of the dataspace selection */
  sel_type = H5Sget_select_type(file_space);

  /* Determine the number of elements the dataspace selection */
  dims1[0] = H5Sget_select_npoints(file_space);
  *numelem = (size_t)dims1[0];
  
  /* If only wanted the number of elments, return */
  if( buf == NULL) return SUCCEED;

  /* Create a new simple dataspace in memory and open it for access */
  mem_space = H5Screate_simple (1, dims1, NULL);

  /* Read the region data from the file_space into the mem_space */
  status = H5Dread (dset, mem_type, mem_space, file_space, H5P_DEFAULT, buf);

  /* Close appropriate items */
  H5E_BEGIN_TRY {
    status = H5Dclose(dset);
  } H5E_END_TRY

/*   if( (status = H5Dclose(dset) ) < 0 ) */
/*     H5E_THROW(H5E_CLOSEERROR, "Unable to close dataset"); */

  H5E_BEGIN_TRY {
    status = H5Sclose(mem_space);
  } H5E_END_TRY
  
  H5E_BEGIN_TRY {
    status = H5Sclose(file_space);
  } H5E_END_TRY


CATCH

   ret_value = FAIL;
/*   return ret_value; */


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

herr_t H5LRcreate_region_references(hid_t file_id,
				    size_t num_elem,
				    const char **path,
				    hsize_t *block_coord,
				    hdset_reg_ref_t *buf)
{
  hid_t ret_value = SUCCEED;          /* Return value */
  hid_t  sid1 = -1;
  hid_t dset_id;
  herr_t status;
  int nrank;
  int i, j, nstart;
  hsize_t *start, *count;

  nstart = 0;
  for(i=0; i<(int)num_elem; i++) {

      /* Open the dataset for a given the path */
      dset_id = H5Dopen2(file_id, path[i], H5P_DEFAULT);

      /* Get the dataspace of the dataset */
      sid1 = H5Dget_space(dset_id);

      /* Find the rank of the dataspace */
      nrank = H5Sget_simple_extent_ndims(sid1);
  
      /* Create references */

      /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reference */
      /*          1   2        n      1   2        n                          */

      start = (hsize_t *)malloc (sizeof (hsize_t) * nrank);
      count = (hsize_t *)malloc (sizeof (hsize_t) * nrank);

      for (j=0; j<nrank; j++) {
	start[j] = block_coord[nstart + j];
	count[j] = block_coord[nstart + j + nrank] - start[j] + 1;
      }
      nstart += 2*nrank;

      status = H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL);

      status = (int)H5Sget_select_npoints(sid1);

      /* Store dataset region */
      status = H5Rcreate(&buf[i], file_id, path[i], H5R_DATASET_REGION, sid1);

      /* Close */
      status = H5Dclose(dset_id);
      status = H5Sclose(sid1);
      
      free(start);
      free(count);
    }

  return SUCCEED;
}

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

herr_t H5LRmake_dataset(hid_t loc_id, const char *path, hid_t type_id, hid_t loc_id_ref, int buf_size, hdset_reg_ref_t *ref)
{
  hid_t ret_value = SUCCEED;          /* Return value */
  hid_t dset_ref = -1, sid_ref = -1;
  H5S_sel_type sel_type;
  hid_t dset_id;
  int nrank;
  int i,j;
  hsize_t *dims1;
  void *buf;
  herr_t status;
  hid_t  sid;
  size_t numelem;
  hsize_t start[2], end[2];
  hsize_t *bounds_coor;


  for (i=0; i<buf_size; i++) {

/*  status= H5LRread_region(loc_id_ref,
		       ref[i], 
		       type_id,
		       &numelem,
		       NULL ); */

    
     dset_ref = H5Rdereference(loc_id_ref, H5R_DATASET_REGION, ref[i]);
     /* Retrieve the dataspace with the specified region selected */
     sid_ref = H5Rget_region (dset_ref, H5R_DATASET_REGION, ref[i]);

    /* get the rank of the region reference */
     nrank = H5Sget_simple_extent_ndims(sid_ref);

     /* Allocate space for the dimension array */
     dims1 = (hsize_t *)malloc (sizeof (hsize_t) * nrank);

    /* get extents of the referenced data */
    
/*      nrank = H5Sget_simple_extent_dims(sid_ref, dims1, NULL  ); */

     bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2);
     /* a region reference is only allowed to reference one block */
     status = H5Sget_select_hyper_blocklist(sid_ref, (hsize_t)0, (hsize_t)1, bounds_coor  );

     for (j=0; j<nrank; j++) 
       dims1[j] = bounds_coor[nrank +j] - bounds_coor[j] + 1;

     numelem = H5Sget_select_npoints(sid_ref);
    
     buf = malloc(sizeof(type_id) * numelem);

     status= H5LRread_region(loc_id_ref,
			     (const hdset_reg_ref_t*)ref[i], 
			     type_id,
			     &numelem,
			     buf );


     status = H5Sget_select_bounds(sid_ref, start, end  );

    /*    dims1[0] = 6; */
     /* Create dataspace for datasets */
     sid = H5Screate_simple(nrank, dims1, NULL);
     dset_id = H5Dcreate2(loc_id, path, type_id, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT  );

     status = H5Dwrite(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

     status = H5Sclose(sid);
     status = H5Dclose(dset_id);
     status = H5Dclose(dset_ref);
     status = H5Sclose(sid_ref);

     free(dims1);
     free(bounds_coor);
     free(buf);
  }
/*   /\* Find the rank of the dataspace *\/ */
/*   ndim = H5Sget_simple_extent_ndims(sid1); */

/*   /\* Allocate space for the dimension array *\/ */
/*   dims1 = (hsize_t *)malloc (sizeof (hsize_t) * ndim); */

/*   /\* find the dimensions of each data space from the block coordinates *\/ */
/*   for (i=0; i<ndim; i++) */
/*     dims1[i] = block_coord[i+ndim] - block_coord[i] + 1; */

/*   /\* Create dataspace for reading buffer *\/ */
/*   sid2 = H5Screate_simple(ndim, dims1, NULL); */

/*   /\* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset *\/ */
/*   /\*          1   2        n      1   2        n                                       *\/ */

/*   start = (hsize_t *)malloc (sizeof (hsize_t) * ndim); */
/*   count = (hsize_t *)malloc (sizeof (hsize_t) * ndim); */
/*   for (i=0; i<ndim; i++) { */
/*     start[i] = block_coord[i]; */
/*     count[i] = dims1[i]; */
/*   } */

/*   status = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL); */

/*   status = H5Dread(dset_id, mem_type, sid2, sid1, H5P_DEFAULT, buf);  */

  return SUCCEED;


}

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

herr_t H5LRcopy_region(hid_t obj_id,
		       hdset_reg_ref_t *ref,
		       const char *file,
		       const char *path,
                       hsize_t *block_coord)
{
  hid_t ret_value = SUCCEED;          /* Return value */
  herr_t status;
  hsize_t  *dims1;
  hid_t sid1;
  hid_t sid2;
  hid_t sid_ref;
  hid_t dset_ref;
  hid_t file_id, type_id;
  hid_t dset_id;
  int ndim;
  void *buf;
  size_t numelem;
  int nrank;
  int i, j;
  hsize_t *start, *count;
  hsize_t *bounds_coor;
  hid_t dtype;
  

  /* Region reference data */
  dset_ref = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);

  /* Region reference space */
  sid_ref = H5Rget_region(dset_ref, H5R_DATASET_REGION, ref);

  /* Get the rank of the region reference */
  nrank = H5Sget_simple_extent_ndims(sid_ref);

  /* Allocate space for the dimension array */
  dims1 = (hsize_t *)malloc (sizeof (hsize_t) * nrank);

  bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2);
/* get the list of hyperslab blocks currently selected */
  status = H5Sget_select_hyper_blocklist(sid_ref, (hsize_t)0, (hsize_t)1, bounds_coor);

  numelem = 1;
  for (j=0; j<nrank; j++) {
    dims1[j] = bounds_coor[nrank +j] - bounds_coor[j] + 1;
    numelem = dims1[j]*numelem;
  }

  dtype = H5Dget_type(dset_ref);
  type_id = H5Tget_native_type(dtype , H5T_DIR_DEFAULT );

  buf = malloc(sizeof(type_id) * numelem);

  status= H5LRread_region(obj_id,
			  (const hdset_reg_ref_t*)ref, 
			  type_id,
			  &numelem,
			  buf );

/*   sel_type = H5Sget_select_type(sid); */
    
/*   dims1[0] = H5Sget_select_npoints(sid); */

/*   mem_space = H5Screate_simple (1, dims1, NULL); */

/*   Allocate space for the dimension array */
/*   dims1 = (hsize_t *)malloc (sizeof (hsize_t) * ndim);   */

/*   status = H5Dread (dset, mem_type, mem_space, sid, H5P_DEFAULT, buf); */

  
  status = H5Sclose(sid_ref);
  status = H5Dclose(dset_ref);


/*   Open the file */
   file_id = H5Fopen(file, H5F_ACC_RDWR,  H5P_DEFAULT);

/*   Open the dataset for a given the path */
   dset_id = H5Dopen2(file_id, path, H5P_DEFAULT);

/*   Get the dataspace of the dataset */
   sid1 = H5Dget_space(dset_id);

/*   Find the rank of the dataspace */
   ndim = H5Sget_simple_extent_ndims(sid1);

/*   /\* Allocate space for the dimension array *\/ */
/*   dims1 = (hsize_t *)malloc (sizeof (hsize_t) * ndim); */

/*   /\* find the dimensions of each data space from the block coordinates *\/ */
/*   for (i=0; i<ndim; i++) */
/*     dims1[i] = block_coord[i+ndim] - block_coord[i] + 1; */

   /* Create dataspace for writing the buffer */
   sid2 = H5Screate_simple(ndim, dims1, NULL);

/*   Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for writing memory dataset */
/*            1   2        n      1   2        n                                       */

   start = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
   count = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
/*    for (i=0; i<ndim; i++) { */
/*      start[i] = block_coord[i]; */
/*      count[i] = block_coord[i + ndim] - start[i] + 1; */
/*    } */

   for (i=0; i<ndim; i++) {
     start[i] = block_coord[i];
     count[i] = dims1[i];
   }

  status = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);

  status = H5Dwrite(dset_id, type_id, sid2, sid1, H5P_DEFAULT, buf);

/*   H5Sget_select_npoints(sid); */


/*   sel_type = H5Sget_select_type(sid); */

/*   dims1[0] = H5Sget_select_npoints(sid); */

/*   *numelem = dims1[0]; */

/*   mem_space = H5Screate_simple (1, dims1, NULL); */

/*   status = H5Dread (dset, mem_type, mem_space, sid, H5P_DEFAULT, buf); */

  free(start);
  free(count);
  free(buf);
  free(dims1);
  free(bounds_coor);

  status = H5Sclose(sid1);
  status = H5Sclose(sid2);
  status = H5Dclose(dset_id);
  status = H5Fclose(file_id);
  status = H5Tclose(dtype);
  status = H5Tclose(type_id);

/* close the data */

  return SUCCEED;

}

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

herr_t H5LRcopy_references(hid_t obj_id, hdset_reg_ref_t *ref, const char *file,
			  const char *path, const hsize_t *block_coord_dset, hdset_reg_ref_t *ref_new)
{
  hid_t ret_value = SUCCEED;          /* Return value */
  herr_t status;
  hsize_t  *dims1, *dims_src;
  hid_t sid1;
  hid_t sid2;
  hid_t  type_id, file_id;
  hid_t did_src, sid_src,dset_id;
  int ndim;
  void *buf;
  hsize_t  numelem_src;
  int nrank_src;
  H5S_sel_type sel_type;
  int i, j;
  hsize_t *start, *count;
  hsize_t *bounds_coor;
  hid_t dtype;

  /* Region reference data */
  did_src = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);

  /* Region reference space */
  sid_src = H5Rget_region (did_src, H5R_DATASET_REGION, ref);

  /* Determine the type of the dataspace selection */
  sel_type = H5Sget_select_type(sid_src);

  /* Find the rank of the dataspace */
  nrank_src = H5Sget_simple_extent_ndims(sid_src);

  /* Allocate space for the dimension array */
  dims_src = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src);

  bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src * 2);
/* get the list of hyperslab blocks currently selected */
  status = H5Sget_select_hyper_blocklist(sid_src, (hsize_t)0, (hsize_t)1, bounds_coor);

  numelem_src = 1;
  for (j=0; j<nrank_src; j++) {
    dims_src[j] = bounds_coor[nrank_src + j] - bounds_coor[j] + 1;
    numelem_src = dims_src[j]*numelem_src;
  }

  dtype = H5Dget_type(did_src);
  type_id = H5Tget_native_type(dtype , H5T_DIR_DEFAULT );

  buf = malloc(sizeof(type_id) * numelem_src);

  /* Create dataspace for reading buffer */
  sid2 = H5Screate_simple(nrank_src, dims_src, NULL);

  /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
  /*          1   2        n      1   2        n                                       */

  start = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src);
  count = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src);
  for (i=0; i<nrank_src; i++) {
    start[i] = bounds_coor[i];
    count[i] = dims_src[i];
  }

  status = H5Sselect_hyperslab(sid_src,H5S_SELECT_SET,start,NULL,count,NULL);

  status = H5Dread(did_src, type_id, sid2, sid_src, H5P_DEFAULT, buf);


  status = H5Dclose(did_src);
  status = H5Sclose(sid_src);
  status = H5Sclose(sid2);
  free(dims_src);

/* Open the file */
  file_id = H5Fopen(file, H5F_ACC_RDWR,  H5P_DEFAULT);

/* Open the dataset for a given the path */
  dset_id = H5Dopen2(file_id, path, H5P_DEFAULT);

/*   Get the dataspace of the dataset */
  sid1 = H5Dget_space(dset_id);

/*   Find the rank of the dataspace */
  ndim = H5Sget_simple_extent_ndims(sid1);

  /* Allocate space for the dimension array */
  dims1 = (hsize_t *)malloc (sizeof (hsize_t) * ndim);

  /* find the dimensions of each data space from the block coordinates */
  for (i=0; i<ndim; i++)
    dims1[i] = block_coord_dset[i+ndim] - block_coord_dset[i] + 1;

   /* Create dataspace for writing the buffer */
   sid2 = H5Screate_simple(ndim, dims1, NULL);

/*   Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for writing memory dataset */
/*            1   2        n      1   2        n                                       */

   start = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
   count = (hsize_t *)malloc (sizeof (hsize_t) * ndim);

   for (i=0; i<ndim; i++) {
     start[i] = block_coord_dset[i];
     count[i] = block_coord_dset[i + ndim] - start[i] + 1;
   }

  status = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);

  status = (int)H5Sget_select_npoints(sid1);

  status = H5Dwrite(dset_id, type_id, sid2, sid1, H5P_DEFAULT, buf);

  /* create reference */
  status = H5Rcreate(&ref_new, file_id, path, H5R_DATASET_REGION, sid1);

  /* close the data */

  status = H5Dclose(dset_id);
  status = H5Sclose(sid1);
  status = H5Sclose(sid2);
  status = H5Tclose(type_id);
  status = H5Fclose(file_id);
  free(dims1);
  free(buf);

  return SUCCEED;

}

