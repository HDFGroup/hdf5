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

#define DIM1  40
#define DIM2  20
#define CDIM1 DIM1/2
#define CDIM2 DIM2/2
#define RANK  2

int make_all_objects(hid_t loc_id);
int make_attributes(hid_t loc_id);
int make_hlinks(hid_t loc_id);
int make_early(void);
int make_layout(hid_t loc_id);
#ifdef H5_HAVE_FILTER_SZIP
int make_szip(hid_t loc_id);
#endif /* H5_HAVE_FILTER_SZIP */
int make_deflate(hid_t loc_id);
int make_shuffle(hid_t loc_id);
int make_fletcher32(hid_t loc_id);
int make_nbit(hid_t loc_id);
int make_scaleoffset(hid_t loc_id);
int make_all(hid_t loc_id);
int make_fill(hid_t loc_id);


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
 hid_t  loc_id;  /* file ID */

 TESTING("    generating datasets");

/*-------------------------------------------------------------------------
 * create a file for general copy test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME0,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_fill(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create another file for general copy test (all datatypes)
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME1,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_all_objects(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a file for attributes copy test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME2,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_attributes(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;
/*-------------------------------------------------------------------------
 * create a file for hard links test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME3,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_hlinks(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;
/*-------------------------------------------------------------------------
 * create a file for layouts test
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME4,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_layout(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a file for the H5D_ALLOC_TIME_EARLY test
 *-------------------------------------------------------------------------
 */
 if (make_early()<0)
  goto out;

/*-------------------------------------------------------------------------
 * create a file with the SZIP filter
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_FILTER_SZIP
 if((loc_id = H5Fcreate(FNAME7,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_szip(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;
#endif /* H5_HAVE_FILTER_SZIP */

/*-------------------------------------------------------------------------
 * create a file with the deflate filter
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME8,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_deflate(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a file with the shuffle filter
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME9,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_shuffle(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a file with the fletcher32 filter
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME10,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_fletcher32(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;

/*-------------------------------------------------------------------------
 * create a file with the nbit filter
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME12,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_nbit(loc_id)<0)
  goto out;

/*-------------------------------------------------------------------------
 * create a file with the scaleoffset filter
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME13,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_scaleoffset(loc_id)<0)
  goto out;

/*-------------------------------------------------------------------------
 * create a file with the all filters
 *-------------------------------------------------------------------------
 */
 if((loc_id = H5Fcreate(FNAME11,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (make_all(loc_id)<0)
  goto out;
 if(H5Fclose(loc_id)<0)
  return -1;


 PASSED();
 return 0;

out:
  H5Fclose(loc_id);
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: make_all_objects
 *
 * Purpose: make a test file with all types of HDF5 objects, datatypes
 *
 *-------------------------------------------------------------------------
 */
int make_all_objects(hid_t loc_id)
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
 dset_id  = H5Dcreate(loc_id,"dset_referenced",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */
 group_id  = H5Gcreate(loc_id,"g1",0);
 root_id   = H5Gopen(loc_id, "/");

/*-------------------------------------------------------------------------
 * H5G_TYPE
 *-------------------------------------------------------------------------
 */

 /* Create a memory compound datatype */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
 /* Commit compound datatype and close it */
 H5Tcommit(loc_id, "type", type_id);
 H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5G_LINK
 *-------------------------------------------------------------------------
 */

 H5Glink(loc_id, H5G_LINK_SOFT, "dset", "link");

/*-------------------------------------------------------------------------
 * write a series of datasetes
 *-------------------------------------------------------------------------
 */

 write_dset_in(root_id,"dset_referenced",loc_id,0);

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
int make_attributes(hid_t loc_id)
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
 dset_id  = H5Dcreate(loc_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */
 group_id  = H5Gcreate(loc_id,"g1",0);
 root_id   = H5Gopen(loc_id, "/");

/*-------------------------------------------------------------------------
 * write a series of attributes on the dataset, group, and root group
 *-------------------------------------------------------------------------
 */

 write_attr_in(dset_id,"dset",loc_id,0);
 write_attr_in(group_id,"dset",loc_id,0);
 write_attr_in(root_id,"dset",loc_id,0);

 /* Close */
 H5Dclose(dset_id);
 H5Gclose(group_id);
 H5Gclose(root_id);

 return 0;

}

/*-------------------------------------------------------------------------
 * Function: make_hlinks
 *
 * Purpose: make a test file with hard links
 *
 *-------------------------------------------------------------------------
 */
int make_hlinks(hid_t loc_id)
{
 hid_t   group1_id;
 hid_t   group2_id;
 hid_t   group3_id;
 hsize_t dims[2]={3,2};
 int     buf[3][2]= {{1,1},{1,2},{2,2}};

/*-------------------------------------------------------------------------
 * create a dataset and some hard links to it
 *-------------------------------------------------------------------------
 */

 if (write_dset(loc_id,2,dims,"dset",H5T_NATIVE_INT,buf)<0)
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

 if (H5Glink2(loc_id, "g1", H5G_LINK_HARD, group2_id, "link1 to g1")<0)
  return -1;
 if (H5Glink2(group1_id, "g2", H5G_LINK_HARD, group3_id, "link1 to g2")<0)
  return -1;

 H5Gclose(group1_id);
 H5Gclose(group2_id);
 H5Gclose(group3_id);

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: make_szip
 *
 * Purpose: make a dataset with the SZIP filter
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_FILTER_SZIP
int make_szip(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block=8;
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;
 int szip_can_encode = 0;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dcpl */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;

/*-------------------------------------------------------------------------
 * SZIP
 *-------------------------------------------------------------------------
 */
 /* Make sure encoding is enabled */
if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
   szip_can_encode = 1;
}
if (szip_can_encode) {
 /* set szip data */
 if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
  goto out;
 if (make_dset(loc_id,"dset_szip",sid,dcpl,buf)<0)
  goto out;
} else {
	/* WARNING? SZIP is decoder only, can't generate test files */
}

 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}
#endif /* H5_HAVE_FILTER_SZIP */



/*-------------------------------------------------------------------------
 * Function: make_deflate
 *
 * Purpose: make a dataset with the deflate filter
 *
 *-------------------------------------------------------------------------
 */
int make_deflate(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }

 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dcpl */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;
/*-------------------------------------------------------------------------
 * GZIP
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_DEFLATE)
 /* set deflate data */
 if(H5Pset_deflate(dcpl, 9)<0)
  goto out;
 if (make_dset(loc_id,"dset_deflate",sid,dcpl,buf)<0)
  goto out;
#endif

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: make_shuffle
 *
 * Purpose: make a dataset with the shuffle filter
 *
 *-------------------------------------------------------------------------
 */
int make_shuffle(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dcpl */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;

/*-------------------------------------------------------------------------
 * shuffle
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_SHUFFLE)
 /* set the shuffle filter */
 if (H5Pset_shuffle(dcpl)<0)
  goto out;
 if (make_dset(loc_id,"dset_shuffle",sid,dcpl,buf)<0)
  goto out;
#endif

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}

/*-------------------------------------------------------------------------
 * Function: make_fletcher32
 *
 * Purpose: make a dataset with the fletcher32 filter
 *
 *-------------------------------------------------------------------------
 */
int make_fletcher32(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;


/*-------------------------------------------------------------------------
 * fletcher32
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_FLETCHER32)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
  goto out;
 /* set the checksum filter */
 if (H5Pset_fletcher32(dcpl)<0)
  goto out;
 if (make_dset(loc_id,"dset_fletcher32",sid,dcpl,buf)<0)
  goto out;
#endif

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: make_nbit
 *
 * Purpose: make a dataset with the nbit filter
 *
 *-------------------------------------------------------------------------
 */
int make_nbit(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hid_t    dtid;
 hid_t    dsid;
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;

 dtid = H5Tcopy(H5T_NATIVE_INT);
 if (H5Tset_precision(dtid,(H5Tget_precision(dtid) - 1)) < 0)
 {
  H5Tclose(dtid);
  goto out;
 }

#if defined (H5_HAVE_FILTER_NBIT)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if (H5Pset_nbit(dcpl)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if((dsid = H5Dcreate (loc_id,"dset_nbit",dtid,sid,dcpl))<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if(H5Dwrite(dsid,dtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 H5Dclose(dsid);

 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if((dsid = H5Dcreate (loc_id,"dset_int31",dtid,sid,dcpl))<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if(H5Dwrite(dsid,dtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 H5Dclose(dsid);
#endif

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: make_scaleoffset
 *
 * Purpose: make a dataset with the scaleoffset filter
 *
 *-------------------------------------------------------------------------
 */
int make_scaleoffset(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hid_t    dtid;
 hid_t    dsid;
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;

 dtid = H5Tcopy(H5T_NATIVE_INT);

#if defined (H5_HAVE_FILTER_NBIT)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if (H5Pset_scaleoffset(dcpl,31,2)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if((dsid = H5Dcreate (loc_id,"dset_scaleoffset",dtid,sid,dcpl))<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if(H5Dwrite(dsid,dtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 H5Dclose(dsid);
 if((dsid = H5Dcreate (loc_id,"dset_none",dtid,sid,H5P_DEFAULT))<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 if(H5Dwrite(dsid,dtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
 {
  H5Tclose(dtid);
  goto out;
 }
 H5Tclose(dtid);
 H5Dclose(dsid);
#endif

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}


/*-------------------------------------------------------------------------
 * Function: make_all
 *
 * Purpose: make a file with all filters
 *
 *-------------------------------------------------------------------------
 */
int make_all(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hid_t    dtid;
 hid_t    dsid;
#if defined (H5_HAVE_FILTER_SZIP)
 unsigned szip_options_mask=H5_SZIP_ALLOW_K13_OPTION_MASK|H5_SZIP_NN_OPTION_MASK;
 unsigned szip_pixels_per_block=8;
#endif /* H5_HAVE_FILTER_SZIP */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;
#if defined (H5_HAVE_FILTER_SZIP)
 int szip_can_encode = 0;
#endif

 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 /* set up chunk */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;

#if defined (H5_HAVE_FILTER_SHUFFLE)
 /* set the shuffle filter */
 if (H5Pset_shuffle(dcpl)<0)
  goto out;
#endif

#if defined (H5_HAVE_FILTER_FLETCHER32)
 /* set the checksum filter */
 if (H5Pset_fletcher32(dcpl)<0)
  goto out;
#endif

#if defined (H5_HAVE_FILTER_SZIP)
if (h5tools_can_encode(H5Z_FILTER_SZIP) == 1) {
   szip_can_encode = 1;
}
if (szip_can_encode) {
 /* set szip data */
 if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
  goto out;
} else {
	/* WARNING? SZIP is decoder only, can't generate test data using szip */
}
#endif

#if defined (H5_HAVE_FILTER_DEFLATE)
 /* set deflate data */
 if(H5Pset_deflate(dcpl, 9)<0)
  goto out;
#endif

 if (make_dset(loc_id,"dset_all",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * fletcher32
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_FLETCHER32)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
  goto out;
 /* set the checksum filter */
 if (H5Pset_fletcher32(dcpl)<0)
  goto out;
 if (make_dset(loc_id,"dset_fletcher32",sid,dcpl,buf)<0)
  goto out;
#endif

/*-------------------------------------------------------------------------
 * SZIP
 *-------------------------------------------------------------------------
 */
 /* Make sure encoding is enabled */
#if defined (H5_HAVE_FILTER_SZIP)
if (szip_can_encode) {
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
  goto out;
 /* set szip data */
 if(H5Pset_szip (dcpl,szip_options_mask,szip_pixels_per_block)<0)
  goto out;
 if (make_dset(loc_id,"dset_szip",sid,dcpl,buf)<0)
  goto out;
} else {
	/* WARNING? SZIP is decoder only, can't generate test dataset */
}
#endif

/*-------------------------------------------------------------------------
 * shuffle
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_SHUFFLE)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
  goto out;
 /* set the shuffle filter */
 if (H5Pset_shuffle(dcpl)<0)
  goto out;
 if (make_dset(loc_id,"dset_shuffle",sid,dcpl,buf)<0)
  goto out;
#endif

/*-------------------------------------------------------------------------
 * GZIP
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_DEFLATE)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
  goto out;
 /* set deflate data */
 if(H5Pset_deflate(dcpl, 1)<0)
  goto out;
 if (make_dset(loc_id,"dset_deflate",sid,dcpl,buf)<0)
  goto out;
#endif


/*-------------------------------------------------------------------------
 * nbit
 *-------------------------------------------------------------------------
 */
#if defined (H5_HAVE_FILTER_NBIT)
 /* remove the filters from the dcpl */
 if (H5Premove_filter(dcpl,H5Z_FILTER_ALL)<0)
  goto out;
 /* set the shuffle filter */
 if (H5Pset_nbit(dcpl)<0)
  goto out;
 dtid = H5Tcopy(H5T_NATIVE_INT);
 H5Tset_precision(dtid,(H5Tget_precision(dtid)-1));
 if((dsid = H5Dcreate (loc_id,"dset_nbit",dtid,sid,dcpl))<0)
  goto out;
 if(H5Dwrite(dsid,dtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
  goto out;

 /* close */
 if(H5Tclose(dtid)<0)
  return -1;
 if(H5Dclose(dsid)<0)
  return -1;
#endif

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: make_early
 *
 * Purpose: create a file for the H5D_ALLOC_TIME_EARLY test
 *
 *-------------------------------------------------------------------------
 */
int make_early(void)
{
 hsize_t dims[1] ={3000};
 hsize_t cdims[1]={30};
 hid_t   fid=-1;
 hid_t   dset_id=-1;
 hid_t   sid=-1;
 hid_t   tid=-1;
 hid_t   dcpl=-1;
 int     i;
 char    name[10];
 int     iter=100;

 if ((fid = H5Fcreate(FNAME5,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;
 if (H5Fclose(fid)<0)
  goto out;

 if ((sid = H5Screate_simple(1, dims, NULL))<0)
  goto out;
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;
 if (H5Pset_chunk(dcpl,1,cdims)<0)
  goto out;
 if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY)<0)
  goto out;

 for (i=0; i<iter; i++)
 {
  if ((fid = H5Fopen(FNAME5,H5F_ACC_RDWR,H5P_DEFAULT))<0)
   goto out;
  if ((dset_id = H5Dcreate(fid,"early",H5T_NATIVE_DOUBLE,sid,dcpl))<0)
   goto out;
  if ((tid = H5Tcopy(H5T_NATIVE_DOUBLE))<0)
   goto out;
  sprintf(name,"%d", i);
  if ((H5Tcommit(fid,name,tid))<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
  if (H5Dclose(dset_id)<0)
   goto out;
  if (H5Gunlink(fid,"early")<0)
   goto out;
  if (H5Fclose(fid)<0)
   goto out;
 }

/*-------------------------------------------------------------------------
 * do the same without close/opening the file and creating the dataset
 *-------------------------------------------------------------------------
 */

 if ((fid = H5Fcreate(FNAME6,H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  return -1;

 for (i=0; i<iter; i++)
 {
  if ((tid = H5Tcopy(H5T_NATIVE_DOUBLE))<0)
   goto out;
  sprintf(name,"%d", i);
  if ((H5Tcommit(fid,name,tid))<0)
   goto out;
  if (H5Tclose(tid)<0)
   goto out;
 }

 if (H5Sclose(sid)<0)
  goto out;
 if (H5Pclose(dcpl)<0)
  goto out;
 if (H5Fclose(fid)<0)
  goto out;


 return 0;

out:
 H5E_BEGIN_TRY {
  H5Tclose(tid);
  H5Pclose(dcpl);
  H5Sclose(sid);
  H5Dclose(dset_id);
  H5Fclose(fid);
 } H5E_END_TRY;
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: make_layout
 *
 * Purpose: make several datasets with several layouts in location LOC_ID
 *
 *-------------------------------------------------------------------------
 */
int make_layout(hid_t loc_id)
{
 hid_t    dcpl; /* dataset creation property list */
 hid_t    sid;  /* dataspace ID */
 hsize_t  dims[RANK]={DIM1,DIM2};
 hsize_t  chunk_dims[RANK]={CDIM1,CDIM2};
 int      buf[DIM1][DIM2];
 int      i, j, n;
 char     name[6];


 for (i=n=0; i<DIM1; i++){
  for (j=0; j<DIM2; j++){
   buf[i][j]=n++;
  }
 }

/*-------------------------------------------------------------------------
 * make several dataset with no filters
 *-------------------------------------------------------------------------
 */
 for (i=0; i<4; i++)
 {
  sprintf(name,"dset%d",i+1);
  if (write_dset(loc_id,RANK,dims,name,H5T_NATIVE_INT,buf)<0)
   return -1;
 }


/*-------------------------------------------------------------------------
 * make several dataset with several layout options
 *-------------------------------------------------------------------------
 */
 /* create a space */
 if((sid = H5Screate_simple(RANK, dims, NULL))<0)
  return -1;
 /* create a dataset creation property list; the same DCPL is used for all dsets */
 if ((dcpl = H5Pcreate(H5P_DATASET_CREATE))<0)
  goto out;

/*-------------------------------------------------------------------------
 * H5D_COMPACT
 *-------------------------------------------------------------------------
 */
 if(H5Pset_layout (dcpl,H5D_COMPACT)<0)
  goto out;
 if (make_dset(loc_id,"dset_compact",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * H5D_COMPACT
 *-------------------------------------------------------------------------
 */
 if(H5Pset_layout (dcpl,H5D_CONTIGUOUS)<0)
  goto out;
 if (make_dset(loc_id,"dset_contiguous",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * H5D_CHUNKED
 *-------------------------------------------------------------------------
 */
 if(H5Pset_chunk(dcpl, RANK, chunk_dims)<0)
  goto out;
 if (make_dset(loc_id,"dset_chunk",sid,dcpl,buf)<0)
  goto out;

/*-------------------------------------------------------------------------
 * close space and dcpl
 *-------------------------------------------------------------------------
 */
 if(H5Sclose(sid)<0)
  goto out;
 if(H5Pclose(dcpl)<0)
  goto out;

 return 0;

out:
 H5E_BEGIN_TRY {
  H5Pclose(dcpl);
  H5Sclose(sid);
 } H5E_END_TRY;
 return -1;
}





/*-------------------------------------------------------------------------
 * Function: make a file with an integer dataset with a fill value
 *
 * Purpose: test copy of fill values
 *
 *-------------------------------------------------------------------------
 */
int make_fill(hid_t loc_id)
{
 hid_t   did;
 hid_t   sid;
 hid_t   dcpl;
 hsize_t dims[2]={3,2};
 int     buf[3][2]= {{1,1},{1,2},{2,2}};
 int     fillvalue=2;

/*-------------------------------------------------------------------------
 * H5T_INTEGER, write a fill value
 *-------------------------------------------------------------------------
 */

 dcpl = H5Pcreate(H5P_DATASET_CREATE);
	H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue);
	sid = H5Screate_simple(2,dims,NULL);
	did = H5Dcreate(loc_id,"dset_fill",H5T_NATIVE_INT,sid,dcpl);
	H5Dwrite(did,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf);
	H5Pclose(dcpl);
	H5Dclose(did);
 H5Sclose(sid);

 return 0;

}

