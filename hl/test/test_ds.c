
/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING f.                                                           *
 *                                                                          *
 ****************************************************************************/



#include "H5DS.h"
#include "H5LT.h"
#include <stdlib.h>



/*-------------------------------------------------------------------------
 * DS API test
 *
 * Functions tested:
 *
 * H5DSattach_scale
 *
 *-------------------------------------------------------------------------
 */

#define RANK      2
#define DIM_DATA  12
#define DIM1_SIZE 3
#define DIM2_SIZE 4



/*-------------------------------------------------------------------------
 * the main program
 *-------------------------------------------------------------------------
 */

int main(void)
{
 hid_t   fid;                                              /* file ID */
 hid_t   did;                                              /* dataset ID */
 hid_t   dsid;                                             /* DS dataset ID */
 int     rank     = RANK;                                  /* rank of data dataset */
 int     rankds   = 1;                                     /* rank of DS dataset */
 hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};              /* size of data dataset */
 int     buf[DIM_DATA] = {1,2,3,4,5,6,7,8,9,10,11,12};     /* data of data dataset */
 hsize_t s1_dim[1]  = {DIM1_SIZE};                         /* size of DS 1 dataset */
 hsize_t s2_dim[1]  = {DIM2_SIZE};                         /* size of DS 2 dataset */
 char    sname[30];                                        /* scale name */
 int     s1_wbuf[DIM1_SIZE] = {10,20,30};                  /* data of DS 1 dataset */
 int     s2_wbuf[DIM2_SIZE] = {100,200,300,400};           /* data of DS 2 dataset */
 char    s1_label[16];                                     /* read label for DS 1 */
 char    s2_label[16];                                     /* read label for DS 1 */
	
/*-------------------------------------------------------------------------
 * create a file for the test
 *-------------------------------------------------------------------------
 */  

 /* create a file using default properties */
 if ((fid=H5Fcreate("test_ds.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  goto out;
	
/*-------------------------------------------------------------------------
 * create 6 datasets: two "data" datasets and 4 dimension scales
 *-------------------------------------------------------------------------
 */  

 /* make a dataset */
 if (H5LTmake_dataset_int(fid,"dset_a",rank,dims,buf)<0)
  goto out;

 /* make a dataset */
 if (H5LTmake_dataset_int(fid,"dset_b",rank,dims,buf)<0)
  goto out;

 /* make a DS dataset for the first dimension */
 if (H5LTmake_dataset_int(fid,"ds1",rankds,s1_dim,s1_wbuf)<0)
  goto out;

 /* make a DS dataset for the second dimension */
 if (H5LTmake_dataset_int(fid,"ds2",rankds,s2_dim,s2_wbuf)<0)
  goto out;

 /* make a DS dataset with an alternate scale for the 2nd dimension  */
 if (H5LTmake_dataset_int(fid,"ds21",rankds,s2_dim,s2_wbuf)<0)
  goto out;

 /* make a DS dataset with an alternate scale for the 2nd dimension  */
 if (H5LTmake_dataset_int(fid,"ds22",rankds,s2_dim,s2_wbuf)<0)
  goto out;

	
/*-------------------------------------------------------------------------
 * test 1: attach scale
 *-------------------------------------------------------------------------
 */  

 TESTING("attach scales");

 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

/*-------------------------------------------------------------------------
 * attach the "ds1" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */  

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds1"))<0)
  goto out;

 /* attach the "ds1" dimension scale to "dset_a" as the 1st dimension  */
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * attach the "ds2" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */  

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds2"))<0)
  goto out;

 /* attach the "ds2" dimension scale to "dset_a" as the 2nd dimension  */
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;
 
/*-------------------------------------------------------------------------
 *  attach the "ds21" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */ 

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds21"))<0)
  goto out;

 /* attach the "ds21" dimension scale to "dset_a" as the 2nd dimension  */
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;

  /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

/*-------------------------------------------------------------------------
 *  attach the "ds22" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */ 

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds22"))<0)
  goto out;

  /* attach the "ds22" dimension scale to "dset_a" as the 2nd dimension  */
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;

  /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close dataset ID of "dset_a" */
 if (H5Dclose(did)<0)
  goto out;
 
/*-------------------------------------------------------------------------
 *  attach the "ds1" dimension scale to "dset_b"
 *-------------------------------------------------------------------------
 */ 

 /* get the dataset id for "dset_b" */
 if ((did = H5Dopen(fid,"dset_b"))<0)
  goto out;

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds1"))<0)
  goto out;

 /* attach the "ds1" dimension scale to "dset_b" as the 1st dimension  */
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close dataset ID of "dset_b" */
 if (H5Dclose(did)<0)
  goto out;
 
/*-------------------------------------------------------------------------
 *  verify if the dimension scales are valid 
 *-------------------------------------------------------------------------
 */  

 if ((did = H5Dopen(fid,"ds1"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;

 if ((did = H5Dopen(fid,"ds2"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;

 if ((did = H5Dopen(fid,"ds21"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;

 if ((did = H5Dopen(fid,"ds22"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;


 PASSED();



/*-------------------------------------------------------------------------
 * test 2: has scales
 *-------------------------------------------------------------------------
 */  

 TESTING("has scales");

/*-------------------------------------------------------------------------
 * verify that "dset_a" has dimension scales
 *-------------------------------------------------------------------------
 */  
  
 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 /* verify that "dset_a" has dimension scales   */
 if ((H5DShas_scale(did))==0)
  goto out;

 /* close dataset ID of "dset_a" */
 if (H5Dclose(did)<0)
  goto out;

 
/*-------------------------------------------------------------------------
 * verify if "dset_b" has dimension scales
 *-------------------------------------------------------------------------
 */  

 /* get the dataset id for "dset_b" */
 if ((did = H5Dopen(fid,"dset_b"))<0)
  goto out;

 /* verify that "dset_b" does not have a complete definition of dimension scales   */
 if ((H5DShas_scale(did))==1)
  goto out;

 /* close dataset ID of "dset_b" */
 if (H5Dclose(did)<0)
  goto out;

 PASSED();



/*-------------------------------------------------------------------------
 * test 3: detach scales
 *-------------------------------------------------------------------------
 */ 
 
 TESTING("detach scales ");

 
/*-------------------------------------------------------------------------
 * detach the "ds21" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */ 

 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds21"))<0)
  goto out;

 /* detach the "ds21" dimension scale to "dset_a" at index 1  */
 if (H5DSdetach_scale(did,dsid,1)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close dataset ID of "dset_a" */
 if (H5Dclose(did)<0)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * test 4: set/get label
 *-------------------------------------------------------------------------
 */ 
 
 TESTING("set/get label");

 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 if (H5DSset_label(did,"DY",0)<0)
  goto out;

 if (H5DSset_label(did,"DX",1)<0)
  goto out;

 if (H5DSget_label(did,s1_label,0)<0)
  goto out;

 if (H5DSget_label(did,s2_label,1)<0)
  goto out;

 if (strcmp("DY",s1_label)!=0)
  goto out;
 if (strcmp("DX",s2_label)!=0)
  goto out;

 if (H5Dclose(did))
  goto out;

 PASSED();
 
/*-------------------------------------------------------------------------
 * test 5: set scale/get scale name
 *-------------------------------------------------------------------------
 */ 
 

 TESTING("set scale/get scale name");

/*-------------------------------------------------------------------------
 * make a dataset named "ds3" and convert it to a DS dataset
 *-------------------------------------------------------------------------
 */ 
 if (H5LTmake_dataset_int(fid,"ds3",rankds,s1_dim,s1_wbuf)<0)
  goto out;

 if ((did = H5Dopen(fid,"ds3"))<0)
  goto out;

 if (H5DSset_scale(did,"scale 1")<0)
  goto out;

 /* verify that "ds3" is a dimension scale dataset  */
 if ((H5DSis_scale(did))==0)
  goto out;

/*-------------------------------------------------------------------------
 * get its scale name
 *-------------------------------------------------------------------------
 */ 

 if (H5DSget_scale_name(did,sname)<0)
  goto out;

 if (H5Dclose(did))
  goto out;

 if (strcmp("scale 1",sname)!=0)
  goto out;

 PASSED();


	
/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */  

 /* close */
 H5Fclose(fid);



 return 0;
 
  
 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Dclose(did);
  H5Fclose(fid);
 } H5E_END_TRY;
 H5_FAILED();
 return 1;
}

