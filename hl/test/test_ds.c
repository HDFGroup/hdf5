
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

/* operator functions */
static herr_t verifiy_scale(hid_t dset, unsigned dim, hid_t scale, void *visitor_data);
static herr_t read_scale(hid_t dset, unsigned dim, hid_t scale, void *visitor_data);
static herr_t match_dim_scale(hid_t did, unsigned dim, hid_t dsid, void *visitor_data);

/* prototypes */
static int test_simple(void);
static int test_errors(void);


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
 int	nerrors=0;

 nerrors += test_simple()<0 	?1:0;
 nerrors += test_errors()<0 	?1:0;

 if (nerrors) goto error;
 printf("All dimension scales tests passed.\n");
 return 0;
 
error:
 printf("***** %d DIMENSION SCALES TEST%s FAILED! *****\n",nerrors, 1 == nerrors ? "" : "S");
 return 1;
}


/*-------------------------------------------------------------------------
 * DS API test
 *
 * Functions tested:
 *
 * H5DSattach_scale
 * H5DSdetach_scale
 * H5DSset_label
 * H5DSget_label
 * H5DSset_scale
 * H5DSget_scale_name
 * H5DSis_scale
 *
 *-------------------------------------------------------------------------
 */


static int test_simple(void)
{
 hid_t   fid;                                              /* file ID */
 hid_t   did;                                              /* dataset ID */
 hid_t   dsid;                                             /* DS dataset ID */
 hid_t   sid;                                              /* space ID */
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
 char    s2_label[16];                                     /* read label for DS 2 */
 unsigned int dim;                                         /* dataset dimension index */
 int     scale_idx;                                        /* scale index */
 int     nscales;                                          /* number of scales in DIM */
 
  
 printf("Testing API functions\n");
 	
/*-------------------------------------------------------------------------
 * create a file for the test
 *-------------------------------------------------------------------------
 */  

 /* create a file using default properties */
 if ((fid=H5Fcreate("test_ds1.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  goto out;
	
/*-------------------------------------------------------------------------
 * create datasets: 1 "data" dataset and 4 dimension scales
 *-------------------------------------------------------------------------
 */  

 /* make a dataset */
 if (H5LTmake_dataset_int(fid,"dset_a",rank,dims,buf)<0)
  goto out;

 /* make a DS dataset for the first dimension */
 if (H5LTmake_dataset_int(fid,"ds_a_1",rankds,s1_dim,s1_wbuf)<0)
  goto out;

 /* make a DS dataset for the second dimension */
 if (H5LTmake_dataset_int(fid,"ds_a_2",rankds,s2_dim,s2_wbuf)<0)
  goto out;

 /* make a DS dataset with an alternate scale for the 2nd dimension  */
 if (H5LTmake_dataset_int(fid,"ds_a_21",rankds,s2_dim,s2_wbuf)<0)
  goto out;

 /* make a DS dataset with an alternate scale for the 2nd dimension  */
 if (H5LTmake_dataset_int(fid,"ds_a_22",rankds,s2_dim,s2_wbuf)<0)
  goto out;

	
/*-------------------------------------------------------------------------
 * test 1: attach scale
 *-------------------------------------------------------------------------
 */  

 TESTING2("attach scales");

 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

/*-------------------------------------------------------------------------
 * attach the "ds1" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */  

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_a_1"))<0)
  goto out;

 /* attach the "ds_a_1" dimension scale to "dset_a" as the 1st dimension  */
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

/*-------------------------------------------------------------------------
 * attach the "ds_a_2" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */  

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_a_2"))<0)
  goto out;

 /* attach the "ds2" dimension scale to "dset_a" as the 2nd dimension  */
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;
 
/*-------------------------------------------------------------------------
 *  attach the "ds_a_21" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */ 

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_a_21"))<0)
  goto out;

 /* attach the "ds_a_21" dimension scale to "dset_a" as the 2nd dimension  */
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;

  /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

/*-------------------------------------------------------------------------
 *  attach the "ds_a_22" dimension scale to "dset_a"
 *-------------------------------------------------------------------------
 */ 

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_a_22"))<0)
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
 *  verify if the dimension scales are valid 
 *-------------------------------------------------------------------------
 */  

 if ((did = H5Dopen(fid,"ds_a_1"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;

 if ((did = H5Dopen(fid,"ds_a_2"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;

 if ((did = H5Dopen(fid,"ds_a_21"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;

 if ((did = H5Dopen(fid,"ds_a_22"))<0)
  goto out;
 if ((H5DSis_scale(did))<=0)
  goto out;
 if (H5Dclose(did))
  goto out;


 PASSED();


/*-------------------------------------------------------------------------
 * test 2: get number of scales
 *-------------------------------------------------------------------------
 */  

 TESTING2("get number of scales");

/*-------------------------------------------------------------------------
 * verify that "dset_a" has dimension scales
 *-------------------------------------------------------------------------
 */  
  
 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 /* verify that "dset_a" has 1 dimension scale at DIM 0   */
 if (H5DSget_nscales(did,0,&nscales)<0)
  goto out;
 if (nscales!=1)
  goto out;

 /* verify that "dset_a" has 3 dimension scales at DIM 1   */
 if (H5DSget_nscales(did,1,&nscales)<0)
  goto out;
 if (nscales!=3)
  goto out;

 /* close dataset ID of "dset_a" */
 if (H5Dclose(did)<0)
  goto out;

 
/*-------------------------------------------------------------------------
 * create datasets: 1 "data" dataset and 1 dimension scale
 *-------------------------------------------------------------------------
 */  

 /* make a dataset */
 if (H5LTmake_dataset_int(fid,"dset_b",rank,dims,buf)<0)
  goto out;

 /* make a DS dataset for the first dimension */
 if (H5LTmake_dataset_int(fid,"ds_b_1",rankds,s1_dim,s1_wbuf)<0)
  goto out;

/*-------------------------------------------------------------------------
 *  attach the scale to "dset_b"
 *-------------------------------------------------------------------------
 */ 

 if ((did = H5Dopen(fid,"dset_b"))<0)
  goto out;
 if ((dsid = H5Dopen(fid,"ds_b_1"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;
 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * verify if "dset_b" has dimension scales
 *-------------------------------------------------------------------------
 */  

 /* get the dataset id for "dset_b" */
 if ((did = H5Dopen(fid,"dset_b"))<0)
  goto out;

 /* verify that "dset_b" has 1 dimension scale at DIM 0   */
 if (H5DSget_nscales(did,0,&nscales)<0)
  goto out;
 if (nscales!=1)
  goto out;

 /* verify that "dset_b" has 0 dimension scales at DIM 1   */
 if (H5DSget_nscales(did,1,&nscales)<0)
  goto out;
 if (nscales!=0)
  goto out;

 /* close dataset ID of "dset_b" */
 if (H5Dclose(did)<0)
  goto out;

 PASSED();



/*-------------------------------------------------------------------------
 * test 3: detach scales
 *-------------------------------------------------------------------------
 */ 

 TESTING2("detach scales ");


/*-------------------------------------------------------------------------
 * create datasets: one "data" dataset and 4 dimension scales
 *-------------------------------------------------------------------------
 */  

 /* make a dataset */
 if (H5LTmake_dataset_int(fid,"dset_c",rank,dims,buf)<0)
  goto out;

 /* make a DS dataset for the first dimension */
 if (H5LTmake_dataset_int(fid,"ds_c_1",rankds,s1_dim,s1_wbuf)<0)
  goto out;

 /* make a DS dataset for the second dimension */
 if (H5LTmake_dataset_int(fid,"ds_c_2",rankds,s2_dim,s2_wbuf)<0)
  goto out;

 /* make a DS dataset with an alternate scale for the 2nd dimension  */
 if (H5LTmake_dataset_int(fid,"ds_c_21",rankds,s2_dim,s2_wbuf)<0)
  goto out;

 /* make a DS dataset with an alternate scale for the 2nd dimension  */
 if (H5LTmake_dataset_int(fid,"ds_c_22",rankds,s2_dim,s2_wbuf)<0)
  goto out;


/*-------------------------------------------------------------------------
 *  attach the scales to "dset_c"
 *-------------------------------------------------------------------------
 */ 

 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;

 if ((dsid = H5Dopen(fid,"ds_c_1"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;

 if ((dsid = H5Dopen(fid,"ds_c_2"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;

 if ((dsid = H5Dopen(fid,"ds_c_21"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;

 if ((dsid = H5Dopen(fid,"ds_c_22"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;

 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * verify if "dset_c" has dimension scales
 *-------------------------------------------------------------------------
 */  

 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;
 /* verify that "dset_c" has 1 dimension scale at DIM 0   */
 if (H5DSget_nscales(did,0,&nscales)<0)
  goto out;
 if (nscales!=1)
  goto out;
 /* verify that "dset_c" has 3 dimension scales at DIM 1   */
 if (H5DSget_nscales(did,1,&nscales)<0)
  goto out;
 if (nscales!=3)
  goto out;
 if (H5Dclose(did)<0)
  goto out;
  
/*-------------------------------------------------------------------------
 * detach the "ds_c_21" dimension scale to "dset_c"
 *-------------------------------------------------------------------------
 */ 

 /* get the dataset id for "dset_c" */
 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_c_21"))<0)
  goto out;

 /* detach the "ds_c_21" dimension scale to "dset_c" in DIM 1  */
 if (H5DSdetach_scale(did,dsid,1)<0) 
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close dataset ID of "dset_c" */
 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * "dset_c" must have now 2 dimension scales at DIM 1
 *-------------------------------------------------------------------------
 */  

 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;
  /* verify that "dset_c" has 2 dimension scales at DIM 1  */
 if (H5DSget_nscales(did,1,&nscales)<0)
  goto out;
 if (nscales!=2)
  goto out;
 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * detach the "ds_c_22" dimension scale to "dset_c"
 *-------------------------------------------------------------------------
 */ 

 /* get the dataset id for "dset_c" */
 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_c_22"))<0)
  goto out;

 /* detach the "ds_c_22" dimension scale to "dset_c" in DIM 1  */
 if (H5DSdetach_scale(did,dsid,1)<0) 
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close dataset ID of "dset_c" */
 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * "dset_c" must have now 1 dimension scale at DIM 1
 *-------------------------------------------------------------------------
 */  

 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;
  /* verify that "dset_c" has 1 dimension scale at DIM 1  */
 if (H5DSget_nscales(did,1,&nscales)<0)
  goto out;
 if (nscales!=1)
  goto out;
 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * detach the "ds_c_2" dimension scale to "dset_c"
 *-------------------------------------------------------------------------
 */ 

 /* get the dataset id for "dset_c" */
 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;

 /* get the DS dataset id */
 if ((dsid = H5Dopen(fid,"ds_c_2"))<0)
  goto out;

 /* detach the "ds_c_2" dimension scale to "dset_c" in DIM 1  */
 if (H5DSdetach_scale(did,dsid,1)<0) 
  goto out;

 /* close DS id */
 if (H5Dclose(dsid)<0)
  goto out;

 /* close dataset ID of "dset_c" */
 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * "dset_c" must have now 0 dimension scales at DIM 1
 *-------------------------------------------------------------------------
 */  

 if ((did = H5Dopen(fid,"dset_c"))<0)
  goto out;
  /* verify that "dset_c" has 1 dimension scale at DIM 1  */
 if (H5DSget_nscales(did,1,&nscales)<0)
  goto out;
 if (nscales!=0)
  goto out;
 if (H5Dclose(did)<0)
  goto out;


 PASSED();

/*-------------------------------------------------------------------------
 * test 4: set/get label
 *-------------------------------------------------------------------------
 */ 
 
 TESTING2("set/get label");

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
 

 TESTING2("set scale/get scale name");

/*-------------------------------------------------------------------------
 * make a dataset named "ds_1" and convert it to a DS dataset
 *-------------------------------------------------------------------------
 */ 
 if (H5LTmake_dataset_int(fid,"ds_1",rankds,s1_dim,s1_wbuf)<0)
  goto out;

 if ((did = H5Dopen(fid,"ds_1"))<0)
  goto out;

 if (H5DSset_scale(did,"scale 1")<0)
  goto out;

 /* verify that "ds5_1" is a dimension scale dataset  */
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
 * test 6: test iterate scales with a function verifiy_scale
 *-------------------------------------------------------------------------
 */ 
 TESTING2("iterate scales (verify scale)");

 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 dim = 0;

 /* iterate trough the 1st dimension of "dset_a" and verify that its DS is valid  */
 if (H5DSiterate_scales(did,dim,NULL,verifiy_scale,NULL)<0)
  goto out;

 /* iterate trough the 2nd dimension of "dset_a" and verify that its DS is valid
    start at DS index 2 */
 dim = 1;
 scale_idx = 2;

 if (H5DSiterate_scales(did,dim,&scale_idx,verifiy_scale,NULL)<0)
  goto out;

 /* close dataset ID of "dset_a" */
 if (H5Dclose(did)<0)
  goto out;

 PASSED();


/*-------------------------------------------------------------------------
 * test 7: test iterate scales with a function read_scale
 *-------------------------------------------------------------------------
 */ 
 TESTING2("iterate scales (read scale values)");


 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 dim = 0;

 /* iterate trough the 1st dimension of "dset_a" and read the DS  */
 if (H5DSiterate_scales(did,dim,NULL,read_scale,s1_wbuf)<0)
  goto out;

 /* iterate trough the 2nd dimension of "dset_a" and read the DS
    start at DS index 2 */
 dim = 1;
 scale_idx = 2;

 if (H5DSiterate_scales(did,dim,&scale_idx,read_scale,s2_wbuf)<0)
  goto out;

 /* close dataset ID of "dset_a" */
 if (H5Dclose(did)<0)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * test 8: test iterate scales with a function match_dim_scale
 *-------------------------------------------------------------------------
 */ 
 TESTING2("iterate scales (verify the scale sizes match)");

 /* get the dataset id for "dset_a" */
 if ((did = H5Dopen(fid,"dset_a"))<0)
  goto out;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* get dimensions of dataset */
 if (H5Sget_simple_extent_dims(sid,dims,NULL)<0)
  goto out;

 {
  int match_size; /* does this scale size matches the dataset DIM size */
  int idx=0;      /* scale index to start iterating, on return, index where iterator stoped */
  
  /* iterate trough all the dimensions  */
  for(dim=0; dim<(unsigned)rank; dim++)
  {
   if ((match_size=H5DSiterate_scales(did,dim,&idx,match_dim_scale,NULL))<0)
    goto out;
   
   /* "dset_a" was defined with all dimension scales size matching the size of its dimensions */
   if (match_size==0)
    goto out;

   /* both "ds_a_1" and "ds_a_2" are the on the first index */
   if (idx!=0)
    goto out;
  }
 }

 
 /* close */
 if (H5Dclose(did)<0)
  goto out;
 if (H5Sclose(sid)<0)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * test 9: test iterate scales with a function match_dim_scale
 *-------------------------------------------------------------------------
 */ 
 TESTING2("iterate scales (verify the scale sizes do not match)");

/*-------------------------------------------------------------------------
 * create 3 datasets: 1 "data" dataset and dimension scales (some are empty)
 *-------------------------------------------------------------------------
 */  
 if (H5LTmake_dataset_int(fid,"dset_d",rank,dims,buf)<0)
  goto out;
 if (H5LTmake_dataset_int(fid,"ds_d_1",rankds,s1_dim,NULL)<0)
  goto out;
 if (H5LTmake_dataset_int(fid,"ds_d_11",rankds,s1_dim,s1_wbuf)<0)
  goto out;
 if (H5LTmake_dataset_int(fid,"ds_d_2",rankds,s2_dim,NULL)<0)
  goto out;

/*-------------------------------------------------------------------------
 * attach them
 *-------------------------------------------------------------------------
 */  
 if ((did = H5Dopen(fid,"dset_d"))<0)
  goto out;

 if ((dsid = H5Dopen(fid,"ds_d_1"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;
 if ((dsid = H5Dopen(fid,"ds_d_11"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,0)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;
 if ((dsid = H5Dopen(fid,"ds_d_2"))<0)
  goto out;
 if (H5DSattach_scale(did,dsid,1)<0)
  goto out;
 if (H5Dclose(dsid)<0)
  goto out;

 if (H5Dclose(did)<0)
  goto out;

/*-------------------------------------------------------------------------
 * verify match
 *-------------------------------------------------------------------------
 */  
 /* get the dataset id for "dset_d" */
 if ((did = H5Dopen(fid,"dset_d"))<0)
  goto out;

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* get dimensions of dataset */
 if (H5Sget_simple_extent_dims(sid,dims,NULL)<0)
  goto out;

 {
  int match_size; /* does this scale size matches the dataset DIM size */
  int idx;        /* scale index to start iterating, on return, index where iterator stoped */
  
  /* iterate trough all the dimensions  */
  for(dim=0; dim<(unsigned)rank; dim++)
  {
   /* always start at 1st scale */
   idx=0;

   if ((match_size=H5DSiterate_scales(did,dim,&idx,match_dim_scale,NULL))<0)
    goto out;
   
    /* "dset_d" was defined with :
        dim 0: 2 scales, first is empty
        dim 1: 1 scale, empty */
   switch(dim)
   {
   case 0: /* for DIM 0, we get a valid scale at IDX 1 */
    if (match_size!=1 && idx!=1)
     goto out;
    break;
   case 1: /* for DIM 1, we get no valid scales */
    if (match_size!=0 && idx!=0)
     goto out;
   }/*switch*/
  }/*for*/
 }
 
 /* close */
 if (H5Dclose(did)<0)
  goto out;
 if (H5Sclose(sid)<0)
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
 return FAIL;
}


/*-------------------------------------------------------------------------
 * test error conditions
 *-------------------------------------------------------------------------
 */

static int test_errors(void)
{
 hid_t   fid;                                              /* file ID */
 int     rank     = RANK;                                  /* rank of data dataset */
 int     rankds   = 1;                                     /* rank of DS dataset */
 hsize_t dims[RANK]  = {DIM1_SIZE,DIM2_SIZE};              /* size of data dataset */
 hsize_t s1_dim[1]  = {DIM1_SIZE};                         /* size of DS 1 dataset */
 hid_t   dset;                                             /* dataset ID */
 hid_t   scale;                                            /* scale ID */
 hid_t   gid;                                              /* group ID */
 hid_t   sid;                                              /* space ID */
 hid_t   sidds; 
 
 printf("Testing error conditions\n");

/*-------------------------------------------------------------------------
 * create a file, spaces, dataset and group ids
 *-------------------------------------------------------------------------
 */  

 /* create a file using default properties */
 if ((fid=H5Fcreate("test_ds2.h5",H5F_ACC_TRUNC,H5P_DEFAULT,H5P_DEFAULT))<0)
  goto out;
 /* create a group */
 if ((gid=H5Gcreate(fid,"grp",0))<0) 
  goto out;
 /* create the data space for the dataset */
 if ((sid=H5Screate_simple(rank,dims,NULL))<0)
  goto out;
 /* create the data space for the scale */
 if ((sidds=H5Screate_simple(rankds,s1_dim,NULL))<0)
  goto out;
 /* create a dataset */
 if ((dset=H5Dcreate(fid,"dset_1",H5T_NATIVE_INT,sid,H5P_DEFAULT))<0)
  goto out;
 /* create a dataset for the scale */
 if ((scale=H5Dcreate(fid,"scale_1",H5T_NATIVE_INT,sidds,H5P_DEFAULT))<0)
  goto out;


/*-------------------------------------------------------------------------
 * attempt to attach a dataset to itself, it should fail
 *-------------------------------------------------------------------------
 */ 
 TESTING2("attach a dataset to itself");

 if (H5DSattach_scale(dset,dset,0)==SUCCESS)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * attempt to attach a group with a dataset, it should fail
 *-------------------------------------------------------------------------
 */ 
 TESTING2("attach a group with a dataset");

 if (H5DSattach_scale(gid,scale,0)==SUCCESS)
  goto out;

 PASSED();

/*-------------------------------------------------------------------------
 * attempt to attach a dataset with a group, it should fail
 *-------------------------------------------------------------------------
 */ 
 TESTING2("attach a dataset with a group");

 if (H5DSattach_scale(dset,gid,0)==SUCCESS)
  goto out;

 PASSED();



/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */  

 /* close */
 if (H5Dclose(scale)<0)
  goto out;
 if (H5Dclose(dset)<0)
  goto out;
 if (H5Sclose(sid)<0)
  goto out;
 if (H5Sclose(sidds)<0)
  goto out;
 if (H5Gclose(gid)<0)
  goto out;
 if (H5Fclose(fid)<0)
  goto out;

 return 0;
  
 /* error zone, gracefully close */
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Sclose(sidds);
  H5Dclose(dset);
  H5Dclose(scale);
  H5Gclose(gid);
  H5Fclose(fid);
 } H5E_END_TRY;
 H5_FAILED();
 return FAIL;
}


/*-------------------------------------------------------------------------
 * Function: verifiy_scale
 *
 * Purpose: example operator function used by H5DSiterate_scales, used
 *  to verify that SCALE_ID refers to a valid DS dataset
 *
 * Return: 
 * The return values from an operator are: 
 * Zero causes the iterator to continue, returning zero when all group members have been processed. 
 * Positive causes the iterator to immediately return that positive value, indicating 
 *  short-circuit success. The iterator can be restarted at the next group member. 
 * Negative causes the iterator to immediately return that value, indicating failure. 
 *  The iterator can be restarted at the next group member. 
 *
 *-------------------------------------------------------------------------
 */

static herr_t verifiy_scale(hid_t dset, unsigned dim, hid_t scale_id, void *visitor_data)
{
 /* define a default zero value for return. This will cause the iterator to continue */
 int ret = 0;  
  
 /* unused */
 dset=dset;
 dim=dim;
 visitor_data=visitor_data;

 /* define a positive value for return value. This will cause the iterator to 
    immediately return that positive value, indicating short-circuit success 
  */

 /* the parameter DS dataset must be a valid DS dataset */
 if ((H5DSis_scale(scale_id))==1)
 {
  ret = 1;
 }
 
 return ret;
} 


/*-------------------------------------------------------------------------
 * Function: read_scale
 *
 * Purpose: example operator function used by H5DSiterate_scales, used
 *  to read data from DS dataset. compare read and write buffers
 *
 * Return: 
 * The return values from an operator are: 
 * Zero causes the iterator to continue, returning zero when all group members have been processed. 
 * Positive causes the iterator to immediately return that positive value, indicating 
 *  short-circuit success. The iterator can be restarted at the next group member. 
 * Negative causes the iterator to immediately return that value, indicating failure. 
 *  The iterator can be restarted at the next group member 
 *
 *
 *-------------------------------------------------------------------------
 */

static herr_t read_scale(hid_t dset, unsigned dim, hid_t scale_id, void *visitor_data)
{
 int      ret = 0;   /* define a default zero value for return. This will cause the iterator to continue */
 hid_t    sid;       /* space ID */
 hid_t    tid;       /* file type ID */
 hid_t    mtid;      /* memory type ID */
 hssize_t nelmts;    /* number of data elements */
 char     *buf=NULL; /* data buffer */
 size_t   size;
 int      i;
 char     *data=visitor_data;

  /* unused */
 dset=dset;
 dim=dim;

 /* get space */
 if ((sid = H5Dget_space(scale_id))<0)
  goto out;
 /* get type */
 if ((tid = H5Dget_type(scale_id))<0)
   goto out;
 /* get size of the DS array */
 if ((nelmts = H5Sget_simple_extent_npoints(sid))<0)
  goto out;
 /* get type */
 if ((mtid=H5Tget_native_type(tid,H5T_DIR_DEFAULT))<0)
  goto out;
 /* get type size */
 if ((size=H5Tget_size(mtid))==0)
  goto out;
 
 if (nelmts)
 {
  buf=(char *) malloc((unsigned)(nelmts*size));
  if ( buf==NULL)
   goto out;
  if (H5Dread(scale_id,mtid,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
   goto out;
  
  for(i=0; i<nelmts; i++)
  {
   if (buf[i] != data[i])
   {
    printf("read and write buffers differ\n");
    goto out;
   }
  }
  
 } /* if */
 
 if (H5Sclose(sid)<0)
  goto out;
 if (H5Tclose(tid)<0) 
  goto out;
 if (H5Tclose(mtid)<0) 
  goto out;
 if (buf)
  free(buf);
 
 
 return ret;

 /* error zone, gracefully close */
 out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
  H5Tclose(tid);
  H5Tclose(mtid);
  if (buf)
   free(buf);
 } H5E_END_TRY;

  return FAIL;
} 


/*-------------------------------------------------------------------------
 * Function: match_dim_scale
 *
 * Purpose: example operator function used by H5DSiterate_scales, used
 *  to verify the the DSID scale size matches the dataset DIM size
 *
 * Return: 
 * The return values from an operator are: 
 * Zero causes the iterator to continue, returning zero when all group members have been processed. 
 * Positive causes the iterator to immediately return that positive value, indicating 
 *  short-circuit success. The iterator can be restarted at the next group member. 
 * Negative causes the iterator to immediately return that value, indicating failure. 
 *  The iterator can be restarted at the next group member. 
 *
 *-------------------------------------------------------------------------
 */

static herr_t match_dim_scale(hid_t did, unsigned dim, hid_t dsid, void *visitor_data)
{
 int       ret = 0;              /* define a default zero value for return. This will cause the iterator to continue */
 hid_t     sid;                  /* space ID */
 hssize_t  nelmts;               /* size of a dimension scale array */
 int       rank;                 /* rank of dataset */
 hsize_t   dims[H5S_MAX_RANK];   /* dimensions of dataset */
 hsize_t   storage_size;

/*-------------------------------------------------------------------------
 * get DID (dataset) space info
 *-------------------------------------------------------------------------
 */ 

 /* get dataset space */
 if ((sid = H5Dget_space(did))<0)
  goto out;
 
 /* get rank */
 if ((rank=H5Sget_simple_extent_ndims(sid))<0)
  goto out;

 /* get dimensions of dataset */
 if (H5Sget_simple_extent_dims(sid,dims,NULL)<0)
  goto out;

 /* close the dataspace id */
 if (H5Sclose(sid)<0)
  goto out;
 
/*-------------------------------------------------------------------------
 * get DSID (scale) space info
 *-------------------------------------------------------------------------
 */ 
 
 /* get the space for the scale */
 if ((sid = H5Dget_space(dsid))<0)
  goto out;
 
 /* get size of the DS array */
 if ((nelmts = H5Sget_simple_extent_npoints(sid))<0)
  goto out;

 /* close */
 if (H5Sclose(sid)<0)
  goto out;
 
 /* the size of the DS array must match the dimension of the dataset */
 if (nelmts == (hssize_t)dims[dim])
  ret = 1;

 /* if the scale is empty assume it cannot be used */
 if ((storage_size=H5Dget_storage_size(dsid))<0)
  goto out;

 if (storage_size==0)
  ret = 0;

 return ret;
 
out:
 H5E_BEGIN_TRY {
  H5Sclose(sid);
 } H5E_END_TRY;
 return FAIL;
} 