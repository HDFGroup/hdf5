/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"
#include "h5test.h"
#include "h5repack.h"


/*-------------------------------------------------------------------------
 * Function: make_deflate
 *
 * Purpose: make a dataset using DEFLATE (GZIP) compression in FID
 *
 *-------------------------------------------------------------------------
 */
static int
make_deflate(hid_t fid)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    dsid; /* dataset ID */
 hid_t    sid;  /* dataspace ID */
 int      rank=2;
 hsize_t  dims[2]={4,2};
 hsize_t  chunk_dims[2]={2,1};
 int      buf[4][2]={{1,2},{3,4},{5,6},{7,8}};
 
 /* create a space */
 if((sid = H5Screate_simple(rank, dims, NULL))<0)
  TEST_ERROR;

 /* create the dataset creation property list */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  TEST_ERROR;
 
 /* set up for deflated data */
 if(H5Pset_chunk(dcpl, rank, chunk_dims)<0)
  TEST_ERROR;
 if(H5Pset_deflate(dcpl, 9)<0)
  TEST_ERROR;

 /* create the dataset */
 if((dsid = H5Dcreate (fid, "dset_gzip", H5T_NATIVE_INT, sid, dcpl))<0)
  TEST_ERROR;

 /* write the data to the dataset */
 if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
  TEST_ERROR;
 
 /* close */
 if(H5Dclose(dsid)<0)
  TEST_ERROR;
 if(H5Pclose(dcpl)<0)
  TEST_ERROR;
 if(H5Sclose(sid)<0)
  TEST_ERROR;
 
 return 0;                                                 
 
error:                                                       
 return 1;
}

/*-------------------------------------------------------------------------
 * Function: make_szip
 *
 * Purpose: make a dataset using SZIP compression in FID
 *
 *-------------------------------------------------------------------------
 */
static int
make_szip(hid_t fid)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    dsid; /* dataset ID */
 hid_t    sid;  /* dataspace ID */
 int      rank=2;
 hsize_t  dims[2]={4,2};
 hsize_t  chunk_dims[2]={2,1};
 int      buf[4][2]={{1,2},{3,4},{5,6},{7,8}};
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block;

  /* 
  pixels_per_block must be an even number, and <= pixels_per_scanline 
  and <= MAX_PIXELS_PER_BLOCK
  */
 szip_pixels_per_block=16;
 
 /* create a space */
 if((sid = H5Screate_simple(rank, dims, NULL))<0)
  TEST_ERROR;

 /* create the dataset creation property list */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  TEST_ERROR;
 
 /* set up for sziped data */
 if(H5Pset_chunk(dcpl, rank, chunk_dims)<0)
  TEST_ERROR;
 if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block)<0)
  TEST_ERROR;

 /* create the dataset */
 if((dsid = H5Dcreate (fid, "dset_szip", H5T_NATIVE_INT, sid, dcpl))<0)
  TEST_ERROR;

 /* write the data to the dataset */
 if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)<0)
  TEST_ERROR;
 
 /* close */
 if(H5Dclose(dsid)<0)
  TEST_ERROR;
 if(H5Pclose(dcpl)<0)
  TEST_ERROR;
 if(H5Sclose(sid)<0)
  TEST_ERROR;
 
 return 0;                                                 
 
error:                                                       
 return 1;
}


/*-------------------------------------------------------------------------
 * Function: make_testfiles
 *
 * Purpose: make a test file with all types of HDF5 objects, 
 *   datatypes and filters
 *
 *-------------------------------------------------------------------------
 */
int make_testfiles(void)
{
 hid_t  fid;  /* file ID */
 int    nerrors=0;

 TESTING("    generating datasets");

 /* create a file for general copy test */
 if((fid = H5Fcreate(FNAME1,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  TEST_ERROR;
 nerrors += make_all_objects(fid);
  /* close */
 if(H5Fclose(fid)<0)
  TEST_ERROR;

 /* create a file for attributes copy test */
 if((fid = H5Fcreate(FNAME2,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  TEST_ERROR;
 nerrors += make_attributes(fid);
  /* close */
 if(H5Fclose(fid)<0)
  TEST_ERROR;

 /* create a file for special items test */
 if((fid = H5Fcreate(FNAME3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  TEST_ERROR;
 nerrors += make_special_objects(fid);
 /* close */
 if(H5Fclose(fid)<0)
  TEST_ERROR;

 /* create a file for the filters test */
 if((fid = H5Fcreate(FNAME4,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  TEST_ERROR;
 nerrors += make_deflate(fid);
 nerrors += make_szip(fid);
 /* close */
 if(H5Fclose(fid)<0)
  TEST_ERROR;
 
 if (nerrors)
  goto error;

 PASSED();   
 return 0;                                                 
 
error:                                                       
 return 1;
}


/*-------------------------------------------------------------------------
 * Function: make_all_objects
 *
 * Purpose: make a test file with all types of HDF5 objects, datatypes 
 *
 *-------------------------------------------------------------------------
 */
int make_all_objects(hid_t fid)
{
 hid_t   dset_id;
 hid_t   group_id;
 hid_t   type_id;  
 hid_t   root_id;
 hid_t   space_id;
 hsize_t dims[1]={2};
 /* Compound datatype */
 typedef struct s_t 
 {
  int    a;
  float  b;
 } s_t;
 
/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */

 space_id = H5Screate_simple(1,dims,NULL);
 dset_id  = H5Dcreate(fid,"dset_ref",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */ 
 group_id  = H5Gcreate(fid,"g1",0);
 root_id   = H5Gopen(fid, "/");

/*-------------------------------------------------------------------------
 * H5G_TYPE
 *-------------------------------------------------------------------------
 */

 /* Create a memory compound datatype */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
 /* Commit compound datatype and close it */
 H5Tcommit(fid, "type", type_id);
 H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5G_LINK
 *-------------------------------------------------------------------------
 */
 
 H5Glink(fid, H5G_LINK_SOFT, "dset", "link");

/*-------------------------------------------------------------------------
 * write a series of datasetes on the group, and root group
 *-------------------------------------------------------------------------
 */

 write_dset_in(root_id,"dset_ref",fid,0);
 write_dset_in(group_id,"dset_ref",fid,0);


 /* Close */
 H5Dclose(dset_id);
 H5Gclose(group_id);
 H5Gclose(root_id);

 return 0;                                                 
 
}


/*-------------------------------------------------------------------------
 * Function: make_attributes
 *
 * Purpose: make a test file with all types of attributes 
 *
 *-------------------------------------------------------------------------
 */
int make_attributes(hid_t fid)
{
 hid_t   dset_id;
 hid_t   group_id;
 hid_t   root_id;
 hid_t   space_id;
 hsize_t dims[1]={2};
 
/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */

 space_id = H5Screate_simple(1,dims,NULL);
 dset_id  = H5Dcreate(fid,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */ 
 group_id  = H5Gcreate(fid,"g1",0);
 root_id   = H5Gopen(fid, "/");

/*-------------------------------------------------------------------------
 * write a series of attributes on the dataset, group, and root group
 *-------------------------------------------------------------------------
 */

 write_attr_in(dset_id,"dset",fid,0);
 write_attr_in(group_id,"dset",fid,0);
 write_attr_in(root_id,"dset",fid,0);

 /* Close */
 H5Dclose(dset_id);
 H5Gclose(group_id);
 H5Gclose(root_id);

 return 0;                                                 
 
}



/*-------------------------------------------------------------------------
 * Function: make_special_objects
 *
 * Purpose: make a test file with non common items  
 *
 *-------------------------------------------------------------------------
 */
int make_special_objects(hid_t loc_id)
{
 hid_t   group1_id;
 hid_t   group2_id;
 hid_t   group3_id;
 hsize_t dims[1]={2};
 int     buf[2]= {1,2};                 

 
/*-------------------------------------------------------------------------
 * create a dataset and some hard links to it
 *-------------------------------------------------------------------------
 */

 if (write_dset(loc_id,1,dims,"dset",H5T_NATIVE_INT,buf)<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link1 to dset")<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link2 to dset")<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "dset", "link3 to dset")<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a group and some hard links to it
 *-------------------------------------------------------------------------
 */ 

 if ((group1_id = H5Gcreate(loc_id,"g1",0))<0)
  return -1;
 if ((group2_id = H5Gcreate(group1_id,"g2",0))<0)
  return -1;
 if ((group3_id = H5Gcreate(group2_id,"g3",0))<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "g1", "link1 to g1")<0)
  return -1;
 if (H5Glink(loc_id, H5G_LINK_HARD, "g1", "link2 to g1")<0)
  return -1;


 H5Gclose(group1_id);
 H5Gclose(group2_id);

 return 0;                                                 
 
}

