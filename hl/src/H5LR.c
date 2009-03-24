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


#include <string.h>
#include <stdlib.h>
#include <stdio.h>
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
   
herr_t H5LRget_region_info(hid_t obj_id,
			   const hdset_reg_ref_t *ref,  
			   size_t *len,
			   char *path,
			   int *rank, 
			   hid_t *dtype, 
			   H5S_sel_type *sel_type,
			   size_t *numelem,
			   hsize_t *buf )

{
  hid_t dset = -1, sid = -1;
  hid_t ret_value = SUCCEED;          /* Return value */
  herr_t status;
  
  FUNC_ENTER_API(H5LRget_region_info, FAIL)

  sid = H5Rget_region(obj_id, H5R_DATASET_REGION, ref);

  *sel_type = H5Sget_select_type(sid);

  if(*sel_type!=H5S_SEL_HYPERSLABS) printf("wrong select type\n");
    
    
    /* Try to open object */
  H5E_BEGIN_TRY {
    dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);
  } H5E_END_TRY;
  
  if(dset < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n INVALID OBJECT TYPE\n\t- Argument 1 \n");
  }

  /* Determine if the user only wants the size and rank returned */

  if( path == NULL) {

    /* Determine the size of the name buffer, with null character included */
    *len = (size_t)(1 + H5Iget_name (dset, NULL, 0));

    /* Determine the rank of the space */
    H5E_BEGIN_TRY {
      sid = H5Rget_region (obj_id,  H5R_DATASET_REGION, ref);
    } H5E_END_TRY;
    if(sid < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }

    H5E_BEGIN_TRY { 
      *rank = (int)H5Sget_simple_extent_ndims(sid);
    } H5E_END_TRY;
    if(*rank < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }

    H5E_BEGIN_TRY {
      *sel_type = H5Sget_select_type(sid);
    } H5E_END_TRY;

  /* get the number of elements */
    H5E_BEGIN_TRY {
      if(*sel_type==H5S_SEL_HYPERSLABS){
	*numelem = (size_t)H5Sget_select_hyper_nblocks(sid);
      } else if(*sel_type==H5S_SEL_POINTS){
	*numelem = (size_t)H5Sget_select_npoints(sid);
      }
	
    } H5E_END_TRY;


    H5E_BEGIN_TRY {
      status = H5Sclose(sid);
    } H5E_END_TRY;
    if(status < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }

    H5E_BEGIN_TRY {
      status = H5Dclose(dset);
    } H5E_END_TRY;
    if(status < 0){
      H5Eclear2(H5E_DEFAULT);
      HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
    }
    return SUCCEED;
  }

  /* Get the space identity region reference points to */
  H5E_BEGIN_TRY {
    sid = H5Rget_region (obj_id, H5R_DATASET_REGION, ref);
  } H5E_END_TRY;
  if(sid < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }
  
  /* Get the data set name the region reference points to */
  H5E_BEGIN_TRY {
    status = H5Iget_name (dset, path, *len);
  } H5E_END_TRY;
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }

  /* get the data type */
  H5E_BEGIN_TRY {
    *dtype = (hid_t)H5Dget_type(dset);
  } H5E_END_TRY;
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }

/*       if((native_type = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) */

  /* get the number of elements */
  H5E_BEGIN_TRY {
    if(*sel_type==H5S_SEL_HYPERSLABS){
      *numelem = (size_t)H5Sget_select_hyper_nblocks(sid);
    } else if(*sel_type==H5S_SEL_POINTS){
      *numelem = (size_t)H5Sget_select_npoints(sid);
    }
  } H5E_END_TRY;


  /* get the corner coordinates of the hyperslab */
  if(*sel_type == H5S_SEL_HYPERSLABS) {
    H5E_BEGIN_TRY {
      status = H5Sget_select_hyper_blocklist(sid, (hsize_t)0, (hsize_t)1, buf);
    } H5E_END_TRY;
  }    
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }

  H5E_BEGIN_TRY {
    status = H5Sclose(sid);
  } H5E_END_TRY;
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }
  
  H5E_BEGIN_TRY {
    status = H5Dclose(dset);
  } H5E_END_TRY;
  if(status < 0){
    H5Eclear2(H5E_DEFAULT);
    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "\n\n  INTERNAL ERROR \n\n   - Argument 1 \n");
  }
  return SUCCEED;

 done:
  FUNC_LEAVE_API(ret_value)
}

herr_t H5LRread_region(hid_t obj_id,
		       const hdset_reg_ref_t *ref, 
		       hid_t mem_type,
		       size_t *numelem,
		       void *buf )

{
  hid_t dset = -1, sid = -1;
  hid_t ret_value = SUCCEED;          /* Return value */
  H5S_sel_type sel_type;
  hid_t mem_space;
  herr_t status;
  hsize_t dims1[1];		
  
  FUNC_ENTER_API(H5LRread_region, FAIL)

  dset = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);

  sid = H5Rget_region (dset, H5R_DATASET_REGION, ref);

  sel_type = H5Sget_select_type(sid);

  dims1[0] = H5Sget_select_npoints(sid);

  *numelem = (size_t)dims1[0];	

  if( buf == NULL) return SUCCEED;

  mem_space = H5Screate_simple (1, dims1, NULL);


  status = H5Dread (dset, mem_type, mem_space, sid, H5P_DEFAULT, buf);

/* close the data */

  return SUCCEED;

 done:
  FUNC_LEAVE_API(ret_value)
}

herr_t H5LTread_region(const char *file,
		       const char *path,
                       hsize_t *block_coord,
		       hid_t mem_type,
		       void *buf )
{
  hid_t ret_value = SUCCEED;          /* Return value */
  herr_t status;
  H5S_sel_type sel_type;
  hsize_t  *dims1;
  hid_t file_id;
  hid_t dset_id;
  hid_t sid1;
  hid_t sid2;
  int ndim;
  int i;
  hsize_t *start, *count;
  
  FUNC_ENTER_API(H5LTread_region, FAIL)

  /* Open the  file */
  file_id = H5Fopen(file, H5F_ACC_RDONLY,  H5P_DEFAULT);

  /* Open the dataset for a given the path */
  dset_id = H5Dopen2(file_id, path, H5P_DEFAULT);

  /* Get the dataspace of the dataset */
  sid1 = H5Dget_space(dset_id);

  /* Find the rank of the dataspace */
  ndim = H5Sget_simple_extent_ndims(sid1);


  /* Allocate space for the dimension array */
  dims1 = (hsize_t *)malloc (sizeof (hsize_t) * ndim);

  /* find the dimensions of each data space from the block coordinates */
  for (i=0; i<ndim; i++)
    dims1[i] = block_coord[i+ndim] - block_coord[i] + 1;

  /* Create dataspace for reading buffer */
  sid2 = H5Screate_simple(ndim, dims1, NULL);

  /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
  /*          1   2        n      1   2        n                                       */

  start = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
  count = (hsize_t *)malloc (sizeof (hsize_t) * ndim);
  for (i=0; i<ndim; i++) {
    start[i] = block_coord[i];
    count[i] = dims1[i];
  }

  status = H5Sselect_hyperslab(sid1,H5S_SELECT_SET,start,NULL,count,NULL);

  status = H5Dread(dset_id, mem_type, sid2, sid1, H5P_DEFAULT, buf);

/*   H5Sget_select_npoints(hid_t space_id) */


/* close the data */

  return SUCCEED;

 done:
  FUNC_LEAVE_API(ret_value)
}


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


  FUNC_ENTER_API(H5LRcreate_region_references, FAIL)

    nstart = 0;
    for(i=0; i<num_elem; i++) {

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

 done:
  FUNC_LEAVE_API(ret_value)

}


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

  FUNC_ENTER_API(H5LRmake_dataset, FAIL)

  for (i=0; i<buf_size; i++) {

/*  status= H5LRread_region(loc_id_ref,
		       ref[i], 
		       type_id,
		       &numelem,
		       NULL ); */

    
     dset_ref = H5Rdereference(loc_id_ref, H5R_DATASET_REGION, ref[i]);
 
     sid_ref = H5Rget_region (dset_ref, H5R_DATASET_REGION, ref[i]);

    /* get the rank of the region reference */
     nrank = H5Sget_simple_extent_ndims(sid_ref);

     /* Allocate space for the dimension array */
      dims1 = (hsize_t *)malloc (sizeof (hsize_t) * nrank);

    /* get extents of the referenced data */
    
/*      nrank = H5Sget_simple_extent_dims(sid_ref, dims1, NULL  ); */

     bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2);
     /* a region reference is only allowed to reference one block */
     status = H5Sget_select_hyper_blocklist(sid_ref, 0, 1, bounds_coor  );

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

     sid = H5Screate_simple(nrank, dims1, NULL);
     dset_id = H5Dcreate2(loc_id, path, type_id, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT  );

     status = H5Dwrite(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

	status = H5Sclose(sid);
	status = H5Dclose(dset_id);
	
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

 done:
  FUNC_LEAVE_API(ret_value)

}

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
  
  FUNC_ENTER_API(H5LRcopy_region, FAIL)

  /* REGION REFERENCE DATA */
  dset_ref = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);

  /* REGION REFERENCE SPACE */
  sid_ref = H5Rget_region(dset_ref, H5R_DATASET_REGION, ref);

  /* GET THE RANK OF THE REGION REFERENCE */
  nrank = H5Sget_simple_extent_ndims(sid_ref);

  /* Allocate space for the dimension array */
  dims1 = (hsize_t *)malloc (sizeof (hsize_t) * nrank);

  bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2);
  status = H5Sget_select_hyper_blocklist(sid_ref, 0, 1, bounds_coor);

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



/* close the data */

    return SUCCEED;

 done:
  FUNC_LEAVE_API(ret_value)
}


herr_t H5LTcopy_region(const char *file_src,
		       const char *path_src,
		       const hsize_t *block_coord_src,
		       const char *file_dest,
		       const char *path_dest,
                       hsize_t *block_coord_dset)
{
  hid_t ret_value = SUCCEED;          /* Return value */
  herr_t status;
  H5S_sel_type sel_type;
  hsize_t  *dims1, *dims_src;
  hid_t sid1;
  hid_t sid2;
  hid_t fid_src, type_id, file_id;
  hid_t did_src, sid_src,dset_id;
  int ndim;
  void *buf;
  hsize_t  numelem_src;
  int  nrank_src;
  int i, j;
  hsize_t *start, *count;
  hid_t dtype;

  FUNC_ENTER_API(H5LTcopy_region, FAIL)

  /* Open the file */
  fid_src = H5Fopen(file_src, H5F_ACC_RDONLY,  H5P_DEFAULT);

  /* Open the dataset for a given the path */
  did_src = H5Dopen2(fid_src, path_src, H5P_DEFAULT);

  /* Get the dataspace of the dataset */
  sid_src = H5Dget_space(did_src);

  /* Find the rank of the dataspace */
  nrank_src = H5Sget_simple_extent_ndims(sid_src);

  /* Allocate space for the dimension array */
  dims_src = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src);

/*   bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank * 2); */
/*   status = H5Sget_select_hyper_blocklist(sid_ref, 0, 1, bounds_coor); */

  numelem_src = 1;
  for (j=0; j<nrank_src; j++) {
    dims_src[j] = block_coord_src[nrank_src + j] - block_coord_src[j] + 1;
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
    start[i] = block_coord_src[i];
    count[i] = dims_src[i];
  }

  status = H5Sselect_hyperslab(sid_src,H5S_SELECT_SET,start,NULL,count,NULL);

  status = H5Dread(did_src, type_id, sid2, sid_src, H5P_DEFAULT, buf);

  status = H5Dclose(did_src);
  status = H5Sclose(sid_src);
  status = H5Sclose(sid2);
  status = H5Fclose(fid_src);
  free(dims_src);

/*   Open the file */
   file_id = H5Fopen(file_dest, H5F_ACC_RDWR,  H5P_DEFAULT);

/*   Open the dataset for a given the path */
   dset_id = H5Dopen2(file_id, path_dest, H5P_DEFAULT);

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

  status = H5Dwrite(dset_id, type_id, sid2, sid1, H5P_DEFAULT, buf);

/* close the data */

  status = H5Dclose(dset_id);
  status = H5Sclose(sid1);
  status = H5Sclose(sid2);
  status = H5Tclose(type_id);
  status = H5Fclose(file_id);
  free(dims1);
  free(buf);

    return SUCCEED;

 done:
  FUNC_LEAVE_API(ret_value)
}

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

  FUNC_ENTER_API(H5LRcopy_references, FAIL)

  did_src = H5Rdereference(obj_id, H5R_DATASET_REGION, ref);

  sid_src = H5Rget_region (did_src, H5R_DATASET_REGION, ref);

  sel_type = H5Sget_select_type(sid_src);

  /* Find the rank of the dataspace */
  nrank_src = H5Sget_simple_extent_ndims(sid_src);

  /* Allocate space for the dimension array */
  dims_src = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src);

  bounds_coor = (hsize_t *)malloc (sizeof (hsize_t) * nrank_src * 2);
  status = H5Sget_select_hyper_blocklist(sid_src, 0, 1, bounds_coor);

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

 done:
  FUNC_LEAVE_API(ret_value)
}
