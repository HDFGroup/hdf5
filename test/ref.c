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

#include "h5test.h"

/* The file structure tested is as follows:
 *  /
 *  	Group1
 *  		Dataset1
 *  		Dataset2
 *  		Group2
 *  			Dataset4
 *  			Hard link to group1
 *  				Datatset5
 *  	Datatype1
 *
 *  	The purpose of the hard link to make sure that the reference lookup code
 *  	does not infinite loop when looking for Dataset3.
 */

#define FILE1   "trefer1.h5"
#define FILE2   "trefer2.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	1
#define SPACE1_DIM1	8

/* 2-D dataset with fixed dimensions */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	2
#define SPACE2_DIM1	10
#define SPACE2_DIM2	10

int 
main(void) {
    hid_t		fid1, fid2;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group, group2;      /* Group ID             */
    hid_t		sid1, sid2;       /* Dataspace ID			*/
    hid_t		tid1;       /* Datatype ID			*/
    hid_t		ref;
    hsize_t		dims1[] = {SPACE1_DIM1};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t	start[SPACE2_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE2_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE2_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE2_RANK];     /* Block size of hyperslab */

    hobj_ref_t      *wbuf;      /* buffer to write to disk */
    int       *tu32;      /* Temporary pointer to int data */
    int        i;          /* counting variables */
    const char *write_comment="Foo!"; /* Comments for group */
    herr_t		ret;		/* Generic return value		*/
    char buf[100] = {0};

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

    /* Allocate write buffers */
    wbuf=(hobj_ref_t *)malloc(sizeof(hobj_ref_t)*SPACE1_DIM1);
    tu32=malloc(sizeof(int)*SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    fid2 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);

    /* Create a group */
    group=H5Gcreate(fid1,"Group1",-1);

    /* Set group's comment */
    ret=H5Gset_comment(group,".",write_comment);

    /* Create a single dataset inside the second file, which will be mounted
     * and used to mask objects in the first file */
    dataset=H5Dcreate(fid2,"Dataset1",H5T_STD_U32LE,sid1,H5P_DEFAULT);
    ret = H5Dclose(dataset);
    
    /* Create a dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset1",H5T_STD_U32LE,sid1,H5P_DEFAULT);

    for(i=0; i < SPACE1_DIM1; i++)
        tu32[i] = i*3;

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,tu32);

    /* Close Dataset */
    ret = H5Dclose(dataset);

    /* Create another dataset (inside Group1) */
    dataset=H5Dcreate(group,"Dataset2",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
 
    /* Close Dataset */
    ret = H5Dclose(dataset);
 
    /* Create a datatype to refer to */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));

    /* Insert fields */
    ret=H5Tinsert (tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);

    ret=H5Tinsert (tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT);

    ret=H5Tinsert (tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT);

    /* Save datatype for later */
    ret=H5Tcommit (group, "Datatype1", tid1);

    /* Close datatype */
    ret = H5Tclose(tid1);

    /* Create a new group in group1 */
    group2=H5Gcreate(group,"Group2",-1);
 
    /* Create a hard link to group1 in group2 */
    if(H5Glink(fid1, H5G_LINK_HARD, "/Group1", "/Group1/Group2/Link")<0)
	H5_FAILED();
 
    /* Create dataset in that group */
    dataset=H5Dcreate(group2,"Dataset4",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
  
    /* Close Dataset */
    ret = H5Dclose(dataset);
  
    /* Close group */
    ret = H5Gclose(group);
    ret = H5Gclose(group2);

    /* Open up that hard link and make a new dataset there */
    group = H5Gopen(fid1, "/Group1/Group2/Link");
    dataset=H5Dcreate(group,"Dataset5",H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);

    ret = H5Dclose(dataset);
    ret = H5Gclose(group);

    /* Open up that hard link and make a new dataset region there */
    group = H5Gopen(fid1, "/Group1/Group2/Link");
    dataset=H5Dcreate(group,"Dataset6",H5T_STD_REF_DSETREG,sid1,H5P_DEFAULT);
    start[0]=2; start[1]=2;
    stride[0]=1; stride[1]=1;
    count[0]=6; count[1]=6;
    block[0]=1; block[1]=1;
    ret = H5Sselect_hyperslab(sid2,H5S_SELECT_SET,start,stride,count,block);


    ret = H5Dclose(dataset);
    ret = H5Gclose(group);


    
    /* Create a dataset to store references */
    dataset=H5Dcreate(fid1,"Dataset3",H5T_STD_REF_OBJ,sid1,H5P_DEFAULT);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[0],fid1,"/Dataset3",H5R_OBJECT,-1);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf[1],fid1,"/Group1/Dataset2",H5R_OBJECT,-1);

    /* Create reference to group */
    ret = H5Rcreate(&wbuf[2],fid1,"/Group1",H5R_OBJECT,-1);

    /* Create reference to named datatype */
    ret = H5Rcreate(&wbuf[3],fid1,"/Group1/Datatype1",H5R_OBJECT,-1);
    
    ret = H5Rcreate(&wbuf[4],fid1,"/Group1/Group2/Dataset4",H5R_OBJECT,-1);
    ret = H5Rcreate(&wbuf[5],fid1,"/Group1/Group2",H5R_OBJECT,-1);
    ret = H5Rcreate(&wbuf[6],fid1,"/Group1/Group2/Link/Dataset5",H5R_OBJECT,-1);
    ret = H5Rcreate(&wbuf[7],fid1,"/Group1/Group2/Link/Dataset6",H5R_DATASET_REGION,sid2);

    /* Write selection to disk */
    ret=H5Dwrite(dataset,H5T_STD_REF_OBJ,H5S_ALL,H5S_ALL,H5P_DEFAULT,wbuf);

    TESTING("Getting path to normal dataset in root group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[0]), (char*)buf, 100);
    if( (strcmp(buf, "/Dataset3")== 0) && (i == 10) )
	PASSED()
    else
	H5_FAILED()

    memset(buf, 0, 100);
    TESTING("Getting path to dataset in /Group1"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[1]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1/Dataset2")== 0) && (i == 17) )
        PASSED()
    else
	H5_FAILED()

    memset(buf, 0, 100);
    TESTING("Getting path to /Group1"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[2]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1")== 0) && (i == 8) )
        PASSED()
    else
	H5_FAILED()

    memset(buf, 0, 100);
    TESTING("Getting path to datatype in /Group1"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[3]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1/Datatype1")== 0) && (i == 18) )
        PASSED()
    else
	H5_FAILED()

    memset(buf, 0, 100);
    TESTING("Getting path to dataset in nested group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[4]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1/Group2/Dataset4")== 0) && (i == 24) )
        PASSED()
    else
	H5_FAILED()

    memset(buf, 0, 100);
    TESTING("Getting path to nested group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[5]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1/Group2")== 0) && (i == 15) )
        PASSED()
    else
	H5_FAILED()
    memset(buf, 0, 100);
 
    TESTING("Getting path to dataset created via hard link"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[6]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1/Dataset5")== 0) && (i == 17) )
        PASSED()
    else
	H5_FAILED()

    TESTING("Getting path to dataset region"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_DATASET_REGION , &wbuf[7]), (char*)buf, 100);
    if( (strcmp(buf, "/Group1/Dataset6")== 0) && (i == 17) )
        PASSED()
    else
	H5_FAILED()

    /* Now we mount fid2 at /Group2 and look for dataset4.  It shouldn't be found */
    H5Fmount(fid1, "/Group1/Group2", fid2, H5P_DEFAULT);

    TESTING("Getting path to dataset hidden by a mounted file");
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[4]), (char*)buf, 100);
    if(i==0)
        PASSED()
    else
	H5_FAILED()

    /* Now we try unlinking dataset2 from the file and searching for it.  It shouldn't be found */
    ref = H5Rdereference(dataset, H5R_OBJECT , &wbuf[1]);
    H5Gunlink(fid1, "/Group1/Dataset2");
 
    TESTING("Getting path to a dataset that has been unlinked"); 
    i = H5Iget_name(ref, (char*)buf, 100);
    if(i==0)
        PASSED()
    else
	H5_FAILED()

    
    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    ret = H5Sclose(sid2);
    
    /* Close Dataset */
    ret = H5Dclose(dataset);
 
    /* Close file */
    ret = H5Fclose(fid1);
    ret = H5Fclose(fid2);
    free(wbuf);
    free(tu32);
    return 0;
}
