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

/*
   Tests hl region references and hyperslab APIs.
*/

#include <stdlib.h>
#include "h5hltest.h"
#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "FileA.h5"
#define dsetnamev "DS2"
#define dsetnamer "R1"

/* used by H5Lvisit_by_name */ 
static size_t  _regref_to_all_start;  /* starting index counter for placing data into 1D array, used by H5LRcreate_regref_to_all */ 
static H5R_type_t _regref_type;       /* type of data to create region references for */


/************************************************************

  Operator function:
     (1) Compares the number of elements in the region references
         to that of the dataset (operator_data). 

 ************************************************************/


static herr_t 
op_func (hid_t loc_id, const char *name, const H5O_info_t *info, void *operator_data)
{
    herr_t status; 
    hid_t dtype_id=-1, sid=-1;
    hid_t dtype_id2;
    hid_t sid2;
    hssize_t npoints, npoints_ref;
    hsize_t coord[1][1];
    hdset_reg_ref_t  reg_out[1];
    hobj_ref_t      obj_out[1];
    hid_t dataspace_id_reg;
    hsize_t dims[1];              /* array of the size of source dimension, reading into 1D array */
    hid_t mem_space = -1;
    hsize_t dims_out[1];
    ssize_t i;
    char buf[100];;
    char name2[100];
    if(_regref_type == H5R_DATASET_REGION) {

      if(info->type == H5O_TYPE_DATASET) {

	/* Open the dataset for a given path */
	if( ( dtype_id = H5Dopen2(loc_id, name, H5P_DEFAULT) ) < 0) goto out;
	
	/* Get the dataspace of the dataset */
	if( ( sid = H5Dget_space(dtype_id) ) < 0) goto out;
	
	/* Select the entire extent of the dataspace */
	if( H5Sselect_all( sid ) < 0 ) goto out;
	
	npoints = H5Sget_simple_extent_npoints(sid);
	
	if(H5Dclose(dtype_id) != 0) goto out;
	dtype_id = -1;
	if(H5Sclose(sid) != 0) goto out;
	sid = -1;

	/* Open the dataset for a given path */
	if( ( dtype_id2 = H5Dopen2(loc_id, "/RegRef", H5P_DEFAULT) ) < 0) goto out;
	
	/* Get the dataspace of the dataset */
	if( ( sid2 = H5Dget_space(dtype_id2) ) < 0) goto out;
	
	/* get the extent of the region reference array */
	if( (H5Sget_simple_extent_dims(sid2, dims_out, NULL  ) )<1) goto out;
	/* only inquire the region references that were created (i.e. not /RegRef itself) */
	if( _regref_to_all_start < dims_out[0] ) {
	  coord[0][0] = _regref_to_all_start;
	  status = H5Sselect_elements(sid2, H5S_SELECT_SET, 1, &coord[0][0]  );
	  if(status !=0) goto out;
	  
	  /* read the region reference */
	  dims[0] = 1;
	  mem_space = H5Screate_simple (1, dims, NULL);
	  status = H5Dread(dtype_id2, H5T_STD_REF_DSETREG,  mem_space, sid2, H5P_DEFAULT, reg_out);
	  if(status !=0) goto out;
	  
	  dataspace_id_reg = H5Rget_region( dtype_id2, H5R_DATASET_REGION, reg_out );
	  
	  npoints_ref = H5Sget_simple_extent_npoints(dataspace_id_reg);
	  
	  if((int)npoints != (int)npoints_ref) {
	    printf(" ERROR: incorrect number of elements referenced for region references\n");
	    goto out;
	  }

	  H5Dclose(dtype_id2);
	  H5Sclose(sid2);
	  
	  _regref_to_all_start += 1;
	}

      } /* if dataset */
    } else if(_regref_type == H5R_OBJECT) {

      /* Open the dataset for a given path */
      if( ( dtype_id2 = H5Dopen2(loc_id, "/ObjRef", H5P_DEFAULT) ) < 0) goto out;

      /* Get the dataspace of the dataset */
      if( ( sid2 = H5Dget_space(dtype_id2) ) < 0) goto out;

      /* get the extent of the region reference array */
      if( (H5Sget_simple_extent_dims(sid2, dims_out, NULL  ) )<1) goto out;
      /* only enquire the objects references that were created (i.e. not /ObjRef itself) */
      if( _regref_to_all_start < dims_out[0] && !(HDstrcmp(name, "ObjRef")==0) ) {
	coord[0][0] = _regref_to_all_start;
	status = H5Sselect_elements(sid2, H5S_SELECT_SET, 1, &coord[0][0]  );
	if(status !=0) goto out;

	/* read the region reference */
	  
	dims[0] = 1;
	mem_space = H5Screate_simple (1, dims, NULL);
	status = H5Dread(dtype_id2, H5T_STD_REF_OBJ,  mem_space, sid2, H5P_DEFAULT, obj_out);
	if(status !=0) goto out;

	*buf = '\0';
	i = H5Rget_name(dtype_id2, H5R_OBJECT, &obj_out[0], (char*)buf, sizeof(buf));

	sprintf(name2, "/%s", name);
	
 	if(!((HDstrcmp(buf, name2) == 0) )) goto out;
	_regref_to_all_start += 1;

      } /* if object */
      H5Dclose(dtype_id2);
      H5Sclose(sid2);
    }
    return 0;

 out:
    if(dtype_id > 0)
      H5Dclose(dtype_id);
    if(sid > 0)
      H5Sclose(sid);

    return -2;
}


static herr_t op_func_L (hid_t loc_id, const char *name, const H5L_info_t *info,
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
    if(status < 0) return -1;

    return op_func (loc_id, name, &infobuf, operator_data);
}

static int test_regref_hyper( void )
{
    hid_t file_id;        /* file identifier */
    hid_t space_id;       /* dataspace identifiers */
    hid_t space_id1;       /* dataspace identifiers */
    hid_t space_id3;       /* dataspace identifiers */
    hid_t spacer_id;
    hid_t group_id, group_id1, group_id3;
    hid_t dsetv_id;       /*dataset identifiers*/
    hid_t dsetr_id;
    hsize_t dims[2] =  {9,8};
    hsize_t dimsr[1] =  {4};
    int rank = 2;
    int rank1D = 1;
    int rank3D = 3;
    hsize_t dims1D[1] = {9};
    hsize_t dims3D[3] = {9,8,6};
    herr_t status;
    hdset_reg_ref_t ref[4];
    int data[9][8], data_read_2D[9][8];
    int data1D[9];
    int data3D[9][8][6];
    hsize_t start[2];
    hsize_t count[2];
    hsize_t start3D[3];
    hsize_t count3D[3];
    hsize_t coord[5] = {1,2,4,6,7};
    size_t num_points = 5;
    hsize_t i, ii, iii, j, jj, k, kk;
    size_t nlength;
    int rank_out[3];
    hid_t dtype;
    hsize_t *buf;
    char *name;
    size_t numelem[3];
    size_t numelem_size;
    H5S_sel_type sel_type;
    int *data_out;
    int data_out2[3][2];
    hsize_t block_coord_3D[6] ={ 0, 1, 2, 1, 2, 3};
    hsize_t block_coord[4] ={ 3, 3, 5, 4};
    hsize_t num_elem;
    char *path[2];
    hsize_t block_coord_6[8] ={ 3, 6, 5, 7, 0, 0, 2, 1};
    hsize_t block_coord_4[4] ={ 1, 3, 3, 4};
    hsize_t block_coord_2D_src[4] ={ 0, 0, 1, 2};
    hsize_t block_coord_2D_dest[4] ={5, 5, 7, 6 };
    hsize_t block_coord_2D_dest_a[4] ={0, 3, 2, 4 };
    hsize_t block_coord_3D_refblock[6] ={ 3, 3, 3, 4, 4, 4 };
    hdset_reg_ref_t ref6[2];
    hdset_reg_ref_t ref_new;
    hid_t file_id_array[2];

    num_elem = 2;
    TESTING("creating dataset for region reference"); 

    path[0] ="/Group_2D/DS2";
    path[1] ="/Group_2D/DS2";

    for (i=0; i<dims3D[0]; i++) {
      data1D[i] = i;
	for (j=0; j<dims3D[1]; j++) {
	data[i][j] =  10*(i+1)+j+1;
	  for (k=0; k<dims3D[2]; k++){
	    data3D[i][j][k] =  100*(i+1)+10*(k+1) + j + 1;
	  }
	}
    }

    /*
     * Create file with default file access and file creation properties.
     */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace for datasets.
     */
    space_id = H5Screate_simple(rank, dims, NULL);
    group_id = H5Gcreate (file_id, "/Group_2D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(group_id, dsetnamev, H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data);
    status = H5Dclose(dsetv_id);

    /*
     * Create dataspace for 1D datasets.
     */
    space_id1 = H5Screate_simple(rank1D, dims1D, NULL);
    group_id1 = H5Gcreate (file_id, "/Group_1D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(group_id1, "DS1", H5T_NATIVE_INT, space_id1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data1D);
    status = H5Dclose(dsetv_id);

    /*
     * Create dataspace for 3D datasets.
     */
    space_id3 = H5Screate_simple(rank3D, dims3D, NULL);
    group_id3 = H5Gcreate (file_id, "/Group_3D", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create integer dataset.
     */
    dsetv_id = H5Dcreate2(group_id3, "DS3", H5T_NATIVE_INT, space_id3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write data to the dataset.
     */
    status = H5Dwrite(dsetv_id, H5T_NATIVE_INT, H5S_ALL , H5S_ALL, H5P_DEFAULT,data3D);
    status = H5Dclose(dsetv_id);


    /*
     * Dataset with references.
     */
    spacer_id = H5Screate_simple(rank1D, dimsr, NULL);
    dsetr_id = H5Dcreate2(file_id, dsetnamer, H5T_STD_REF_DSETREG, spacer_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create a reference to elements selection.
     */
    status = H5Sselect_none(space_id1);
    status = H5Sselect_elements(space_id1, H5S_SELECT_SET, num_points, coord);
    status = H5Rcreate(&ref[0], group_id1, "DS1", H5R_DATASET_REGION, space_id1);

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 3;
    start[1] = 3;
    count[0] = 3;
    count[1] = 2;
    status = H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL);
    status = H5Rcreate(&ref[1], group_id, dsetnamev, H5R_DATASET_REGION, space_id);

    /*
     * Create a reference to the hyperslab.
     */
    start[0] = 0;
    start[1] = 5;
    count[0] = 9;
    count[1] = 2;
    status = H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, NULL);
    status = H5Rcreate(&ref[2], group_id, dsetnamev, H5R_DATASET_REGION, space_id);


    /*
     * Create a reference to the hyperslab.
     */
    start3D[0] = 3;
    start3D[1] = 3;
    start3D[2] = 3;
    count3D[0] = 2;
    count3D[1] = 2;
    count3D[2] = 2;
    status = H5Sselect_hyperslab(space_id3, H5S_SELECT_SET, start3D, NULL, count3D, NULL);
    status = H5Rcreate(&ref[3], group_id3, "DS3", H5R_DATASET_REGION, space_id3);


    /*
     * Write dataset with the references.
     */
    status = H5Dwrite(dsetr_id, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref);

    /*
     * Close all objects.
     */
    status = H5Sclose(space_id);
    status = H5Sclose(spacer_id);
    status = H5Dclose(dsetr_id);
    status = H5Fclose(file_id);
    status = H5Gclose(group_id);

    PASSED();

    /* START READING SECTION */

    TESTING("H5LRget_region_info with NULL entries, 1D dataset");
    /*
     * Reopen the file to read selections back.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
    if(file_id < 0)
      goto out;
    nlength = 0;
    rank_out[0] = 0;

    /* 1D element section region reference infomation */
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[0],
				 &nlength,
				 NULL,
				 &rank_out[0],
				 NULL,
				 &sel_type,
				 &numelem[0],
				 NULL);
    if(status < 0)
      goto out;
    if( nlength != 14)
      goto out;
    if( rank_out[0] != rank1D)
      goto out;
    if( sel_type != H5S_SEL_POINTS)
      goto out;
    if(numelem[0] != num_points)
      goto out;

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LRget_region_info test
     *-------------------------------------------------------------------------
     */

    TESTING("H5LRget_region_info with NULL entries, 3D dataset");
    /* 3D hyperslab section region reference  infomation*/
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[3],
				 &nlength,
				 NULL,
				 &rank_out[2],
				 NULL,
				 &sel_type,
				 &numelem[2],
				 NULL);
    if(status < 0)
      goto out;
    if( nlength != 14)
      goto out;
    if( sel_type != H5S_SEL_HYPERSLABS)
      goto out;
    if(numelem[2] != 1)
      goto out;
    PASSED();

    /*-------------------------------------------------------------------------
     * H5LRget_region_info test
     *-------------------------------------------------------------------------
     */
    TESTING("H5LRget_region_info with NULL entries, 2D dataset");
    rank_out[1] =0;
    /* 2D hyperslab section region reference infomation*/
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[1],
				 &nlength,
				 NULL,
				 &rank_out[1],
				 NULL,
				 &sel_type,
				 &numelem[1],
				 NULL);
    if(status < 0)
      goto out;
    if( nlength != 14)
      goto out;
    if( rank_out[1] != rank)
      goto out;
    if( sel_type != H5S_SEL_HYPERSLABS)
      goto out;
    if(numelem[1] != 1)
      goto out;
    PASSED();

    /* allocate the name */
    name = (char *) malloc (nlength);
    if(name == NULL)
      goto out;

    /* allocate space for hyperslab block
                            | NUMBLOCKS |   RANK    |  SIZEOF DATA TYPE | NUMBER COORDINATE POINTS | */
    buf = (hsize_t *)malloc(  numelem[1]   * rank_out[1]  *   sizeof(hsize_t) *          2                );
    if(buf == NULL)
      goto out;


    /*-------------------------------------------------------------------------
     * H5LRget_region_info test
     *-------------------------------------------------------------------------
     */

    /* Get region reference information */
    TESTING("H5LRget_region_info, 2D dataset");
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[1],
				 &nlength,
				 name,
				 &rank_out[1],
				 &dtype,
				 &sel_type,
				 &numelem[1],
				 buf );
    if(status < 0)
      goto out;
    if(strcmp(name, "/Group_2D/DS2"))
      goto out;
    /* check values */
    
    for(i=0; i<rank_out[1]*2*numelem[1];i++) {
      if(buf[i] != block_coord[i]) goto out;
    }

    free(buf);
    free(name);

    PASSED();

    /*-------------------------------------------------------------------------
     * H5LRget_region_info test
     *-------------------------------------------------------------------------
     */

    TESTING("H5LRget_region_info, 3D dataset");

    /* allocate the name */
    name = (char *) malloc (nlength);
    if(name == NULL)
      goto out;


    /* allocate space for hyperslab block
                            | NUMBLOCKS |   RANK    |  SIZEOF DATA TYPE | NUMBER COORDINATE POINTS | */
    buf = (hsize_t *)malloc(  numelem[2]   * rank_out[2]  *   sizeof(hsize_t) *          2                );
    if(buf == NULL)
      goto out;

    /* Get region reference information */

    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[3],
				 &nlength,
				 name,
				 &rank_out[2],
				 &dtype,
				 &sel_type,
				 &numelem[2],
				 buf );
    if(status < 0)
      goto out;
    if(strcmp(name, "/Group_3D/DS3"))
      goto out;
    /* check values */
    
    for(i=0; i<rank_out[2]*2*numelem[2];i++) {
      if(buf[i] != block_coord_3D_refblock[i]) goto out;
    }

    free(buf);
    free(name);

    PASSED();

    /*-------------------------------------------------------------------------
     *  H5LRget_region test
     *-------------------------------------------------------------------------
     */
    TESTING("H5LRget_region, 2D dataset");
    /* Read a region of the 2D data using a region reference */
    if ( H5LRread_region(file_id,
			 (const hdset_reg_ref_t*)ref[1],
			 H5T_NATIVE_INT,
			 &numelem_size,
			 NULL) < 0) goto out;

    /* check size of data from region reference */
    if( (hsize_t)numelem_size !=  (block_coord[2]-block_coord[0]+1) * (block_coord[3]-block_coord[1]+1) )
	goto out;

    data_out = (int *)malloc( numelem_size * sizeof(int));

    if ( H5LRread_region(file_id,
			 (const hdset_reg_ref_t*)ref[1],
			 H5T_NATIVE_INT,
			 &numelem_size,
			 data_out) < 0) goto out;


    ii = 0; jj = 0;
    for (i=0; i<numelem_size; i++){
      if( data_out[i] != data[ii+(int)block_coord[0]][jj+(int)block_coord[1]])
	goto out;
      if( jj+1 == ( (size_t)block_coord[3]-(size_t)block_coord[1] + 1) ) {
	ii = ii + 1;
	jj = 0;
      } else {
	jj = jj + 1;
      }
    }

    free(data_out);
    status = H5Fclose(file_id);

    PASSED();

    /*-------------------------------------------------------------------------
     *  H5LTget_region test
     *-------------------------------------------------------------------------
     */

    TESTING("H5LTread_region, 3D dataset");

    /* Read a region of the data using corner coordinates of a block */

    numelem_size = (block_coord_3D[3]-block_coord_3D[0]+1) * (block_coord_3D[4]-block_coord_3D[1]+1) * (block_coord_3D[5]-block_coord_3D[2]+1);

    data_out = (int *)malloc( numelem_size * sizeof(int));


    status = H5LTread_region(filename,
			     "/Group_3D/DS3",
	                     block_coord_3D,
 			     H5T_NATIVE_INT,
 			     data_out);
    if(status < 0) goto out;

    ii = 0; jj = 0; kk = 0;
    for (i=0; i<numelem_size; i++){
      if( data_out[i] != data3D[ii+(int)block_coord_3D[0]][jj+(int)block_coord_3D[1]][kk+(int)block_coord_3D[2]])
	goto out;
      if( ( jj +1 == (hsize_t)( block_coord_3D[4]-block_coord_3D[1] + 1) ) && ( kk+1 == (hsize_t)( block_coord_3D[5]-block_coord_3D[2] + 1 ) ) ) {
	ii = ii + 1;
	jj = 0;
	kk = 0;
      } else if( kk+1 == (hsize_t)( block_coord_3D[5]-block_coord_3D[2] + 1)) {
	jj = jj+1;
	kk = 0;
      }
      else {
	kk = kk + 1;
      }
    }

    free(data_out);
    PASSED();

    TESTING("H5LTcopy_region, 2D dataset");
    /* copy a region described by blocks to another region described by another block */
    status = H5LTcopy_region(filename,
 			     "/Group_2D/DS2",
 	                     block_coord_2D_src,
 			     filename,
  			     "/Group_2D/DS2",
  			     block_coord_2D_dest);
    if(status < 0) goto out;

    /* check the region was copied correctly */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
    status = H5LTread_dataset(file_id,"/Group_2D/DS2",H5T_NATIVE_INT,data_read_2D);

    status = H5Fclose(file_id);

    ii =  block_coord_2D_src[0];
    for (i=0; i< (hsize_t)(block_coord_2D_dest[2] - block_coord_2D_dest[0] + 1) ; i++) {
      jj =  block_coord_2D_src[1];
      for (j=0; j< (hsize_t)(block_coord_2D_dest[3] - block_coord_2D_dest[1] + 1)  ; j++) {
	if(data_read_2D[i][j] != data[ii][jj])
	  goto out;
	jj = jj + 1;
      }
      ii = ii + 1;
    }

    PASSED();

    TESTING("H5LRcopy_references, 2D dataset");

    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
    status =  H5LRcopy_references( file_id, &ref[1], filename,
				   "/Group_2D/DS2", block_coord_2D_dest_a, &ref_new);


    /* check the region was copied correctly and the region reference is correct */

    status = H5LTread_dataset(file_id,"/Group_2D/DS2",H5T_NATIVE_INT,data_read_2D);
    
    /* data_read_2D is the full dataset located at given path */

    ii =  block_coord[0];
    for (i=block_coord_2D_dest_a[0]; i< (hsize_t)block_coord_2D_dest_a[2] + 1; i++) {
      jj =  block_coord[1];
      for (j=block_coord_2D_dest_a[1]; j< (hsize_t)block_coord_2D_dest_a[3] + 1 ; j++) {
	if(data_read_2D[i][j] != data[ii][jj])
	  goto out;
	jj = jj + 1;
      }
      ii = ii + 1;
    }

    /* check the data pointed to by the new region reference */
    status = H5LRread_region(file_id,
  			     (const hdset_reg_ref_t*)ref_new,
  			     H5T_NATIVE_INT,
  			     &numelem_size,
  			     data_out2);

    if(status<0) goto out;

    ii = block_coord[0];
    for (i = 0 ; i < (hsize_t)(block_coord[2] - block_coord[0] + 1); i++) {
      jj = block_coord[1];
      for (j = 0; j < (hsize_t)(block_coord[3] - block_coord[1] + 1) ; j++) {
	if(data_out2[i][j] != data[ii][jj])
	  goto out;
	jj = jj + 1;
      }
      ii = ii + 1;
    }

    PASSED();

    TESTING("H5LRcopy_region, 2D dataset");

    /* copy the region reference into a block_coord */
    status = H5LRcopy_region(file_id, &ref[1], filename, "/Group_2D/DS2", block_coord_4);

    if(status<0) goto out;

    /* check the region was copied correctly */
    status = H5LTread_dataset(file_id,"/Group_2D/DS2",H5T_NATIVE_INT,data_read_2D);
    
    if(status<0) goto out;
    
    ii =  block_coord[0];
    for (i=block_coord_4[0]; i< (hsize_t)block_coord_4[2] + 1; i++) {
      jj =  block_coord[1];
      for (j=block_coord_4[1]; j< (hsize_t)block_coord_4[3] + 1 ; j++) {
	if(data_read_2D[i][j] != data[ii][jj])
	  goto out;
	jj = jj + 1;
      }
      ii = ii + 1;
    }

    PASSED();

    TESTING("H5LRcreate_region_references, 2D dataset");


    status = H5LRcreate_region_references(file_id,
					  num_elem,
					  (const char **)path,
					  block_coord_6,
					  ref6);
    if(status < 0) goto out;

    PASSED();
    for (i=0; i< num_elem; i++)
      file_id_array[i] = file_id;


    TESTING("H5LRmake_dataset, 2D dataset");
    status = H5LRmake_dataset(file_id, "/Group_2D/DS2a", H5T_NATIVE_INT, num_elem, file_id_array, ( const hdset_reg_ref_t *)ref6);

    /* check the regions and check they were made correctly */

    kk = 0;
    for (iii=0; iii< (hsize_t)num_elem; iii++) {
      status = H5LRread_region(file_id,
			       (const hdset_reg_ref_t*)ref6[iii],
			       H5T_NATIVE_INT,
			       &numelem_size,
			       data_out2);
      if(status < 0) goto out;

      /* check the values */
      ii =  block_coord_6[0+kk];
      for (i=0 ; i < (hsize_t)(block_coord_6[kk+2] - block_coord_6[0+kk] + 1); i++) {
	jj =  block_coord_6[1+kk];
	for (j=0 ; j < (hsize_t)(block_coord_6[kk+3] - block_coord_6[1+kk] + 1); j++) {
	  if(data_out2[i][j] != data_read_2D[ii][jj])
	  goto out;

	  jj = jj + 1;
	}
	ii = ii + 1;
      }
      kk = kk + 4;
    }

    PASSED();


    /*-------------------------------------------------------------------------
     *  Testing H5LRcreate_regref_to_all
     *-------------------------------------------------------------------------
     */

    TESTING("H5LRcreate_regref_to_all Region Reference Datatset");
    status = H5LRcreate_regref_to_all(file_id, "/","/RegRef", H5_INDEX_NAME, H5_ITER_INC, H5R_DATASET_REGION);

    if(status < 0) goto out;

    /* check results */
    _regref_to_all_start=0;
    _regref_type = H5R_DATASET_REGION;

    status = H5Lvisit_by_name(file_id, "/", H5_INDEX_NAME, H5_ITER_INC, op_func_L, NULL, H5P_DEFAULT );

    if(status < 0) goto out;

    PASSED();

    TESTING("H5LRcreate_regref_to_all Region Reference Object");
    status = H5LRcreate_regref_to_all(file_id, "/","/ObjRef", H5_INDEX_NAME, H5_ITER_INC, H5R_OBJECT);

    if(status < 0) goto out;

    /* check results */
    _regref_to_all_start=0;
    _regref_type = H5R_OBJECT;

    status = H5Lvisit_by_name(file_id, "/", H5_INDEX_NAME, H5_ITER_INC, op_func_L, NULL, H5P_DEFAULT );
    if(status < 0) goto out;

    PASSED();

    return 0;
out:
    H5_FAILED();
    return -1;
}

/*-------------------------------------------------------------------------
* the main program
*-------------------------------------------------------------------------
*/
int main( void )
{
    int  nerrors=0;

    /* test region region references and hyperslab selections */
    nerrors += test_regref_hyper();

    /* check for errors */
    if (nerrors)
        goto error;

    return 0;

error:
    return 1;

}

