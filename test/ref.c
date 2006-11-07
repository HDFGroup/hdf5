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
 *  				Dataset5
 *  	Datatype1
 *
 *  	The purpose of the hard link to make sure that the reference lookup code
 *  	does not infinite loop when looking for Dataset3.
 */

#define FILE1   "trefer1.h5"
#define FILE2   "trefer2.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_RANK	1
#define SPACE1_DIM1	8

int 
main(void)
{
    hid_t	fid1, fid2;		/* HDF5 File IDs		*/
    hid_t	dataset;	        /* Dataset ID			*/
    hid_t	group, group2;          /* Group ID                     */
    hid_t	sid1;                   /* Dataspace ID			*/
    hid_t	tid1;                   /* Datatype ID			*/
    hid_t	ref;
    hsize_t	dims1[] = {SPACE1_DIM1};
    hobj_ref_t  wbuf[SPACE1_DIM1];      /* Buffer to write to disk */
    int         tu32[SPACE1_DIM1];      /* Int data */
    int         i;                      /* counting variables */
    char buf[100];

    /* Compound datatype */
    typedef struct s1_t {
        unsigned int a;
        unsigned int b;
        float c;
    } s1_t;

    /* Create files */
    if((fid1 = H5Fcreate(FILE1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((fid2 = H5Fcreate(FILE2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create dataspace for datasets */
    if((sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL)) < 0)
        TEST_ERROR

    /* Create a group */
    if((group = H5Gcreate(fid1, "Group1", (size_t)0)) < 0)
        TEST_ERROR

    /* Create a single dataset inside the second file, which will be mounted
     * and used to mask objects in the first file */
    if((dataset = H5Dcreate(fid2, "Dataset1", H5T_STD_U32LE, sid1, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR
    
    /* Create a dataset (inside Group1) */
    if((dataset = H5Dcreate(group, "Dataset1", H5T_STD_U32LE, sid1, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Initialize data buffer */
    for(i = 0; i < SPACE1_DIM1; i++)
        tu32[i] = i * 3;

    /* Write selection to disk */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, tu32) < 0)
        TEST_ERROR

    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Create another dataset (inside Group1) */
    if((dataset = H5Dcreate(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT)) < 0)
        TEST_ERROR
 
    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR
 
    /* Create a datatype to refer to */
    if((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR

    /* Insert fields */
    if(H5Tinsert(tid1, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT) < 0)
        TEST_ERROR
    if(H5Tinsert(tid1, "b", HOFFSET(s1_t,b), H5T_NATIVE_INT) < 0)
        TEST_ERROR
    if(H5Tinsert(tid1, "c", HOFFSET(s1_t,c), H5T_NATIVE_FLOAT) < 0)
        TEST_ERROR

    /* Save datatype for later */
    if(H5Tcommit(group, "Datatype1", tid1) < 0)
        TEST_ERROR

    /* Close datatype */
    if(H5Tclose(tid1) < 0)
        TEST_ERROR

    /* Create a new group in group1 */
    if((group2 = H5Gcreate(group, "Group2", (size_t)0)) < 0)
        TEST_ERROR
 
    /* Create a hard link to group1 in group2 */
    if(H5Glink(fid1, H5G_LINK_HARD, "/Group1", "/Group1/Group2/Link") < 0)
        TEST_ERROR
 
    /* Create dataset in that group */
    if((dataset = H5Dcreate(group2, "Dataset4", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT)) < 0)
        TEST_ERROR
  
    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR
  
    /* Close group */
    if(H5Gclose(group) < 0)
        TEST_ERROR
    if(H5Gclose(group2) < 0)
        TEST_ERROR

    /* Open up that hard link and make a new dataset there */
    if((group = H5Gopen(fid1, "/Group1/Group2/Link")) < 0)
        TEST_ERROR
    if((dataset = H5Dcreate(group, "Dataset5", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if(H5Dclose(dataset) < 0)
        TEST_ERROR
    if(H5Gclose(group) < 0)
        TEST_ERROR


    /* Create a dataset to store references */
    if((dataset = H5Dcreate(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create reference to dataset */
    if(H5Rcreate(&wbuf[0], fid1, "/Dataset3", H5R_OBJECT, -1) < 0)
        TEST_ERROR

    /* Create reference to dataset */
    if(H5Rcreate(&wbuf[1], fid1, "/Group1/Dataset2", H5R_OBJECT, -1) < 0)
        TEST_ERROR

    /* Create reference to group */
    if(H5Rcreate(&wbuf[2], fid1, "/Group1", H5R_OBJECT, -1) < 0)
        TEST_ERROR

    /* Create reference to named datatype */
    if(H5Rcreate(&wbuf[3], fid1, "/Group1/Datatype1", H5R_OBJECT, -1) < 0)
        TEST_ERROR
    
    if(H5Rcreate(&wbuf[4], fid1, "/Group1/Group2/Dataset4", H5R_OBJECT, -1) < 0)
        TEST_ERROR
    if(H5Rcreate(&wbuf[5], fid1, "/Group1/Group2", H5R_OBJECT, -1) < 0)
        TEST_ERROR
    if(H5Rcreate(&wbuf[6], fid1, "/Group1/Group2/Link/Dataset5", H5R_OBJECT, -1) < 0)
        TEST_ERROR

    /* Create reference to root group */
    if(H5Rcreate(&wbuf[7], fid1, "/", H5R_OBJECT, -1) < 0)
        TEST_ERROR

    /* Write selection to disk */
    if(H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR

    TESTING("getting path to normal dataset in root group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[0]), (char*)buf, (size_t)100);
    if((HDstrcmp(buf, "/Dataset3") == 0) && (i == 10))
	PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, (size_t)100);
    TESTING("getting path to dataset in /Group1"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[1]), (char*)buf, (size_t)100);
    if((HDstrcmp(buf, "/Group1/Dataset2") == 0) && (i == 17))
        PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, (size_t)100);
    TESTING("getting path to /Group1"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[2]), (char*)buf, 100);
    if((HDstrcmp(buf, "/Group1") == 0) && (i == 8))
        PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, 100);
    TESTING("getting path to datatype in /Group1"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[3]), (char*)buf, 100);
    if((HDstrcmp(buf, "/Group1/Datatype1") == 0) && (i == 18))
        PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, 100);
    TESTING("getting path to dataset in nested group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[4]), (char*)buf, 100);
    if((HDstrcmp(buf, "/Group1/Group2/Dataset4") == 0) && (i == 24))
        PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, 100);
    TESTING("getting path to nested group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[5]), (char*)buf, 100);
    if((HDstrcmp(buf, "/Group1/Group2") == 0) && (i == 15))
        PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, 100);
    TESTING("getting path to dataset created via hard link"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[6]), (char*)buf, 100);
    if((HDstrcmp(buf, "/Group1/Dataset5") == 0) && (i == 17))
        PASSED()
    else
	TEST_ERROR

    HDmemset(buf, 0, 100);
    TESTING("getting path to root group"); 
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[7]), (char*)buf, 100);
    if((HDstrcmp(buf, "/") == 0) && (i == 2))
        PASSED()
    else
	TEST_ERROR

    /* Now we mount fid2 at /Group2 and look for dataset4.  It shouldn't be found */
    if(H5Fmount(fid1, "/Group1/Group2", fid2, H5P_DEFAULT) < 0)
        TEST_ERROR

    TESTING("getting path to dataset hidden by a mounted file");
    i = H5Iget_name(H5Rdereference(dataset, H5R_OBJECT , &wbuf[4]), (char*)buf, 100);
    if(i == 0)
        PASSED()
    else
	TEST_ERROR

    /* Now we try unlinking dataset2 from the file and searching for it.  It shouldn't be found */
    if((ref = H5Rdereference(dataset, H5R_OBJECT , &wbuf[1])) < 0)
        TEST_ERROR
    if(H5Gunlink(fid1, "/Group1/Dataset2") < 0)
        TEST_ERROR
 
    TESTING("getting path to dataset that has been unlinked"); 
    i = H5Iget_name(ref, (char*)buf, 100);
    if(i == 0)
        PASSED()
    else
	TEST_ERROR
    
    /* Close disk dataspace */
    if(H5Sclose(sid1) < 0)
        TEST_ERROR
    
    /* Close Dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR
 
    /* Close file */
    if(H5Fclose(fid1) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    return 0;

error:
    return 1;
}

