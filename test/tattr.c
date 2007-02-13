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

/***********************************************************
*
* Test program:	 tattr
*
* Test the attribute functionality
*
*************************************************************/

#include "testhdf5.h"
#include "h5test.h"
#include "hdf5.h"

/*
 * This file needs to access private information from the H5O package.
 * This file also needs to access the object header testing code.
 */
#define H5O_PACKAGE
#define H5O_TESTING
#include "H5Opkg.h"		/* Object headers 			*/

/*
 * This file needs to access private information from the H5A package.
 * This file also needs to access the attribute testing code.
 */
#define H5A_PACKAGE
#define H5A_TESTING
#include "H5Apkg.h"		/* Attributes	 			*/

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file testing code.
 */
#define H5F_PACKAGE
#define H5F_TESTING
#include "H5Fpkg.h"		/* File access	 			*/

#define FILENAME   "tattr.h5"
#define NAME_BUF_SIZE   1024
#define ATTR_NAME_LEN   16
#define ATTR_MAX_DIMS   7
#define ATTR_TMP_NAME   "a really long temp_name"

/* 3-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	3
#define SPACE1_DIM1	3
#define SPACE1_DIM2	15
#define SPACE1_DIM3	13

/* Dataset Information */
#define DSET1_NAME "Dataset1"
#define DSET2_NAME "Dataset2"
#define DSET3_NAME "Dataset3"
#define NUM_DSETS       3

/* Group Information */
#define GROUP1_NAME "/Group1"

/* Named Datatype Information */
#define TYPE1_NAME "/Type"

/* Attribute Rank & Dimensions */
#define ATTR1_NAME  "Attr1"
#define ATTR1_RANK	1
#define ATTR1_DIM1	3
int attr_data1[ATTR1_DIM1]={512,-234,98123}; /* Test data for 1st attribute */

/* rank & dimensions for another attribute */
#define ATTR1A_NAME  "Attr1_a"
int attr_data1a[ATTR1_DIM1]={256,11945,-22107};

#define ATTR2_NAME  "Attr2"
#define ATTR2_RANK	2
#define ATTR2_DIM1	2
#define ATTR2_DIM2	2
int attr_data2[ATTR2_DIM1][ATTR2_DIM2]={{7614,-416},{197814,-3}}; /* Test data for 2nd attribute */

#define ATTR3_NAME  "Attr3"
#define ATTR3_RANK	3
#define ATTR3_DIM1	2
#define ATTR3_DIM2	2
#define ATTR3_DIM3	2
double attr_data3[ATTR3_DIM1][ATTR3_DIM2][ATTR3_DIM3]={{{2.3,-26.1},{0.123,-10.0}},{{981724.2,-0.91827},{2.0,23.0}}}; /* Test data for 3rd attribute */

#define ATTR4_NAME  "Attr4"
#define ATTR4_RANK	2
#define ATTR4_DIM1	2
#define ATTR4_DIM2	2
#define ATTR4_FIELDNAME1	"i"
#define ATTR4_FIELDNAME2	"d"
#define ATTR4_FIELDNAME3	"c"
size_t attr4_field1_off=0;
size_t attr4_field2_off=0;
size_t attr4_field3_off=0;
struct attr4_struct {
    int i;
    double d;
    char c;
 } attr_data4[ATTR4_DIM1][ATTR4_DIM2]={{{3,-26.1,'d'},{-100000, 0.123,'3'}},
    {{-23,981724.2,'Q'},{0,2.0,'\n'}}}; /* Test data for 4th attribute */

#define ATTR5_NAME  "Attr5"
#define ATTR5_RANK	0
float attr_data5=(float)-5.123;        /* Test data for 5th attribute */

herr_t attr_op1(hid_t loc_id, const char *name, void *op_data);


/****************************************************************
**
**  test_attr_basic_write(): Test basic H5A (attribute) code.
**      Tests integer attributes on both datasets and groups
**
****************************************************************/
static void
test_attr_basic_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;	    /* Group ID			    */
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr, attr2;	    /* Attribute ID		*/
    hsize_t             attr_size;  /* storage size for attribute       */
    ssize_t             attr_name_size; /* size of attribute name       */
    char                *attr_name=NULL;    /* name of attribute        */
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    hsize_t		dims3[] = {ATTR2_DIM1,ATTR2_DIM2};
    int       read_data1[ATTR1_DIM1]={0}; /* Buffer for reading 1st attribute */
    int         i;
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Scalar Attribute Writing Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,DSET1_NAME,H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");


    /* Try to create an attribute on the file (should create an attribute on root group) */
    attr = H5Acreate(fid1, ATTR1_NAME, H5T_NATIVE_INT, sid2, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the root group */
    group = H5Gopen(fid1, "/");
    CHECK(group, FAIL, "H5Gopen");

    /* Open attribute again */
    attr = H5Aopen_name(group, ATTR1_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close root group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");


    /* Create an attribute for the dataset */
    attr=H5Acreate(dataset,ATTR1_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(dataset,ATTR1_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write attribute information */
    ret=H5Awrite(attr,H5T_NATIVE_INT,attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Create an another attribute for the dataset */
    attr2=H5Acreate(dataset,ATTR1A_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Write attribute information */
    ret=H5Awrite(attr2,H5T_NATIVE_INT,attr_data1a);
    CHECK(ret, FAIL, "H5Awrite");

    /* Check storage size for attribute */
    attr_size=H5Aget_storage_size(attr);
    VERIFY(attr_size, (ATTR1_DIM1*sizeof(int)), "H5A_get_storage_size");

    /* Read attribute information immediately, without closing attribute */
    ret=H5Aread(attr,H5T_NATIVE_INT,read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR1_DIM1; i++)
        if(attr_data1[i]!=read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute */
    ret=H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    /* change attribute name */
    ret=H5Arename(dataset, ATTR1_NAME, ATTR_TMP_NAME);
    CHECK(ret, FAIL, "H5Arename");

    /* Open attribute again */
    attr=H5Aopen_name(dataset, ATTR_TMP_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Verify new attribute name */
    attr_name_size = H5Aget_name(attr, (size_t)0, NULL);
    CHECK(attr_name_size, FAIL, "H5Aget_name");

    if(attr_name_size>0)
        attr_name = (char*)HDcalloc((size_t)(attr_name_size+1), sizeof(char));

    ret=(herr_t)H5Aget_name(attr, (size_t)(attr_name_size+1), attr_name);
    CHECK(ret, FAIL, "H5Aget_name");
    ret=HDstrcmp(attr_name, ATTR_TMP_NAME);
    VERIFY(ret, 0, "HDstrcmp");

    if(attr_name)
        HDfree(attr_name);

    /* Read attribute information immediately, without closing attribute */
    ret=H5Aread(attr,H5T_NATIVE_INT,read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR1_DIM1; i++)
        if(attr_data1[i]!=read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the second attribute again */
    attr2=H5Aopen_name(dataset, ATTR1A_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Verify new attribute name */
    attr_name_size = H5Aget_name(attr2, (size_t)0, NULL);
    CHECK(attr_name_size, FAIL, "H5Aget_name");

    if(attr_name_size>0)
        attr_name = (char*)HDcalloc((size_t)(attr_name_size+1), sizeof(char));

    ret=(herr_t)H5Aget_name(attr2, (size_t)(attr_name_size+1), attr_name);
    CHECK(ret, FAIL, "H5Aget_name");
    ret=HDstrcmp(attr_name, ATTR1A_NAME);
    VERIFY(ret, 0, "HDstrcmp");

    if(attr_name)
        HDfree(attr_name);

    /* Read attribute information immediately, without closing attribute */
    ret=H5Aread(attr2,H5T_NATIVE_INT,read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR1_DIM1; i++)
        if(attr_data1a[i]!=read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1a[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1a[i],i,read_data1[i]);

    /* Close attribute */
    ret=H5Aclose(attr2);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create group */
    group = H5Gcreate(fid1, GROUP1_NAME, (size_t)0);
    CHECK(group, FAIL, "H5Gcreate");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR2_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create an attribute for the group */
    attr=H5Acreate(group,ATTR2_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Check storage size for attribute */
    attr_size=H5Aget_storage_size(attr);
    VERIFY(attr_size, (ATTR2_DIM1*ATTR2_DIM2*sizeof(int)), "H5Aget_storage_size");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(group,ATTR2_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write attribute information */
    ret=H5Awrite(attr,H5T_NATIVE_INT,attr_data2);
    CHECK(ret, FAIL, "H5Awrite");

    /* Check storage size for attribute */
    attr_size=H5Aget_storage_size(attr);
    VERIFY(attr_size, (ATTR2_DIM1*ATTR2_DIM2*sizeof(int)), "H5A_get_storage_size");

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Attribute dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_basic_write() */

/****************************************************************
**
**  test_attr_basic_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void
test_attr_basic_read(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		group;	    /* Group ID			    */
    hid_t		attr;	    /* Attribute ID			*/
    int       read_data1[ATTR1_DIM1]={0}; /* Buffer for reading 1st attribute */
    int       read_data2[ATTR2_DIM1][ATTR2_DIM2]={{0}}; /* Buffer for reading 2nd attribute */
    int         i,j;
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 2, "H5Aget_num_attrs");

    /* Open first attribute for the dataset */
    attr=H5Aopen_name(dataset, ATTR_TMP_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Read attribute information */
    ret=H5Aread(attr,H5T_NATIVE_INT,read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR1_DIM1; i++)
        if(attr_data1[i]!=read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open the group */
    group = H5Gopen(fid1,GROUP1_NAME);
    CHECK(group, FAIL, "H5Gopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(group);
    VERIFY(ret, 1, "H5Aget_num_attrs");

    /* Open the attribute for the group */
    attr=H5Aopen_name(group,ATTR2_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Read attribute information */
    ret=H5Aread(attr,H5T_NATIVE_INT,read_data2);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR2_DIM1; i++)
        for(j=0; j<ATTR2_DIM2; j++)
            if(attr_data2[i][j]!=read_data2[i][j])
                TestErrPrintf("%d: attribute data different: attr_data2[%d][%d]=%d, read_data2[%d][%d]=%d\n",__LINE__, i,j,attr_data2[i][j],i,j,read_data1[i]);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_basic_read() */

/****************************************************************
**
**  test_attr_flush(): Test H5A (attribute) code for performing
**                      I/O when H5Fflush is used.
**
****************************************************************/
static void
test_attr_flush(hid_t fapl)
{
    hid_t fil,          /* File ID */
        att,            /* Attribute ID */
        spc,            /* Dataspace ID */
        set;            /* Dataset ID */
    double wdata=3.14159;       /* Data to write */
    double rdata;       /* Data read in */
    herr_t ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Flushing\n"));

    fil = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fil, FAIL, "H5Fcreate");

    spc = H5Screate(H5S_SCALAR);
    CHECK(spc, FAIL, "H5Screate");

    set = H5Dcreate(fil, DSET1_NAME, H5T_NATIVE_DOUBLE, spc, H5P_DEFAULT);
    CHECK(set, FAIL, "H5Dcreate");

    att = H5Acreate(set, ATTR1_NAME, H5T_NATIVE_DOUBLE, spc, H5P_DEFAULT);
    CHECK(att, FAIL, "H5Acreate");

    ret=H5Aread(att, H5T_NATIVE_DOUBLE, &rdata);
    CHECK(ret, FAIL, "H5Awrite");

    if(rdata!=0.0)
        TestErrPrintf("attribute value wrong: rdata=%f, should be %f\n",rdata,0.0);

    ret=H5Fflush(fil, H5F_SCOPE_GLOBAL);
    CHECK(ret, FAIL, "H5Fflush");

    ret=H5Aread(att, H5T_NATIVE_DOUBLE, &rdata);
    CHECK(ret, FAIL, "H5Awrite");

    if(rdata!=0.0)
        TestErrPrintf("attribute value wrong: rdata=%f, should be %f\n",rdata,0.0);

    ret=H5Awrite(att, H5T_NATIVE_DOUBLE, &wdata);
    CHECK(ret, FAIL, "H5Awrite");

    ret=H5Aread(att, H5T_NATIVE_DOUBLE, &rdata);
    CHECK(ret, FAIL, "H5Awrite");

    if(rdata!=wdata)
        TestErrPrintf("attribute value wrong: rdata=%f, should be %f\n",rdata,wdata);

    ret=H5Sclose(spc);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Aclose(att);
    CHECK(ret, FAIL, "H5Aclose");
    ret=H5Dclose(set);
    CHECK(ret, FAIL, "H5Dclose");
    ret=H5Fclose(fil);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_flush() */

/****************************************************************
**
**  test_attr_plist(): Test Attribute Creation Property Lists
**
****************************************************************/
static void
test_attr_plist(hid_t fapl)
{
    hid_t		fid1;           /* HDF5 File IDs		*/
    hid_t		dataset;        /* Dataset ID			*/
    hid_t		sid1,sid2;      /* Dataspace ID			*/
    hid_t		attr;	        /* Attribute ID		*/
    hid_t               plist;          /* Property list ID             */
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    H5T_cset_t          cset;           /* Character set for attributes */
    herr_t		ret;            /* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Property Lists\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,DSET1_NAME,H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create default property list for attribute */
    plist = H5Pcreate(H5P_ATTRIBUTE_CREATE);
    CHECK(plist, FAIL, "H5Pcreate");

    /* Get the character encoding and ensure that it is the default (ASCII) */
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_ASCII, "H5Pget_char_encoding");

    /* Create an attribute for the dataset using the property list */
    attr=H5Acreate(dataset,ATTR1_NAME,H5T_NATIVE_INT,sid2,plist);
    CHECK(attr, FAIL, "H5Acreate");

    /* Close the property list, and get the attribute's property list */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    plist = H5Aget_create_plist(attr);
    CHECK(plist, FAIL, "H5Aget_create_plist");

    /* Get the character encoding and ensure that it is the default (ASCII) */
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_ASCII, "H5Pget_char_encoding");

    /* Close the property list and attribute */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a new property list and modify it to use a different encoding */
    plist = H5Pcreate(H5P_ATTRIBUTE_CREATE);
    CHECK(plist, FAIL, "H5Pcreate");
    ret=H5Pset_char_encoding(plist, H5T_CSET_UTF8);
    CHECK(ret, FAIL, "H5Pset_char_encoding");

    /* Get the character encoding and ensure that it has been changed */
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_UTF8, "H5Pget_char_encoding");

    /* Create an attribute for the dataset using the modified property list */
    attr=H5Acreate(dataset,ATTR2_NAME,H5T_NATIVE_INT,sid2,plist);
    CHECK(attr, FAIL, "H5Acreate");

    /* Close the property list and attribute */
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Re-open the second attribute and ensure that its character encoding is correct */
    attr = H5Aopen_name(dataset, ATTR2_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");
    plist = H5Aget_create_plist(attr);
    CHECK(plist, FAIL, "H5Aget_create_plist");
    ret = H5Pget_char_encoding(plist, &cset);
    CHECK(ret, FAIL, "H5Pget_char_encoding");
    VERIFY(cset, H5T_CSET_UTF8, "H5Pget_char_encoding");

    /* Close everything */
    ret=H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret=H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(plist);
    CHECK(ret, FAIL, "H5Pclose");
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");
    ret=H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
    ret=H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}  /* test_attr_plist() */

/****************************************************************
**
**  test_attr_compound_write(): Test H5A (attribute) code.
**      Tests compound datatype attributes
**
****************************************************************/
static void
test_attr_compound_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t       tid1;       /* Attribute datatype ID */
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR4_DIM1,ATTR4_DIM2};
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Multiple Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,DSET1_NAME,H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Close dataset's dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create the attribute datatype.  */
    tid1 = H5Tcreate (H5T_COMPOUND, sizeof(struct attr4_struct));
    CHECK(tid1, FAIL, "H5Tcreate");
    attr4_field1_off=HOFFSET(struct attr4_struct, i);
    ret = H5Tinsert(tid1, ATTR4_FIELDNAME1, attr4_field1_off, H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");
    attr4_field2_off=HOFFSET(struct attr4_struct, d);
    ret = H5Tinsert(tid1, ATTR4_FIELDNAME2, attr4_field2_off, H5T_NATIVE_DOUBLE);
    CHECK(ret, FAIL, "H5Tinsert");
    attr4_field3_off=HOFFSET(struct attr4_struct, c);
    ret = H5Tinsert(tid1, ATTR4_FIELDNAME3, attr4_field3_off,
		    H5T_NATIVE_SCHAR);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create dataspace for 1st attribute */
    sid2 = H5Screate_simple(ATTR4_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create complex attribute for the dataset */
    attr=H5Acreate(dataset,ATTR4_NAME,tid1,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(dataset,ATTR4_NAME,tid1,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write complex attribute data */
    ret=H5Awrite(attr,tid1,attr_data4);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close attribute's datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_compound_write() */

/****************************************************************
**
**  test_attr_compound_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void
test_attr_compound_read(hid_t fapl)
{
    hid_t   fid1;		/* HDF5 File IDs		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   space;      /* Attribute dataspace  */
    hid_t   type;       /* Attribute datatype   */
    hid_t   attr;	    /* Attribute ID			*/
    char    attr_name[ATTR_NAME_LEN]; /* Buffer for attribute names */
    int     rank;       /* Attribute rank */
    hsize_t dims[ATTR_MAX_DIMS];    /* Attribute dimensions */
    H5T_class_t t_class;  /* Attribute datatype class */
    H5T_order_t order;  /* Attribute datatype order */
    size_t      size;   /* Attribute datatype size as stored in file */
    int     fields;     /* # of Attribute datatype fields */
    char *fieldname;    /* Name of a field */
    size_t      offset; /* Attribute datatype field offset */
    hid_t   field;      /* Attribute field datatype */
    struct attr4_struct read_data4[ATTR4_DIM1][ATTR4_DIM2]; /* Buffer for reading 4th attribute */
    int     i,j;
    size_t  name_len;   /* Length of attribute name */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 1, "H5Aget_num_attrs");

    /* Open 1st attribute for the dataset */
    attr=H5Aopen_idx(dataset,0);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Dataspace */
    space=H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank=H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR4_RANK, "H5Sget_simple_extent_ndims");
    ret=H5Sget_simple_extent_dims(space,dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0]!=ATTR4_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n",(int)dims[0],ATTR4_DIM1);
    if(dims[1]!=ATTR4_DIM2)
        TestErrPrintf("attribute dimensions different: dims[1]=%d, should be %d\n",(int)dims[1],ATTR4_DIM2);
    H5Sclose(space);

    /* Verify Datatype */
    type=H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class=H5Tget_class(type);
    VERIFY(t_class, H5T_COMPOUND, "H5Tget_class");
    fields=H5Tget_nmembers(type);
    VERIFY(fields, 3, "H5Tget_nmembers");
    for(i=0; i<fields; i++) {
        fieldname=H5Tget_member_name(type,(unsigned)i);
        if(!(HDstrcmp(fieldname,ATTR4_FIELDNAME1) ||
                HDstrcmp(fieldname,ATTR4_FIELDNAME2) ||
                HDstrcmp(fieldname,ATTR4_FIELDNAME3)))
            TestErrPrintf("invalid field name for field #%d: %s\n",i,fieldname);
        free(fieldname);
      } /* end for */
    offset=H5Tget_member_offset(type,0);
    VERIFY(offset, attr4_field1_off, "H5Tget_member_offset");
    offset=H5Tget_member_offset(type,1);
    VERIFY(offset, attr4_field2_off, "H5Tget_member_offset");
    offset=H5Tget_member_offset(type,2);
    VERIFY(offset, attr4_field3_off, "H5Tget_member_offset");

    /* Verify each field's type, class & size */
    field=H5Tget_member_type(type,0);
    CHECK(field, FAIL, "H5Tget_member_type");
    t_class=H5Tget_class(field);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order=H5Tget_order(field);
    VERIFY(order, H5Tget_order(H5T_NATIVE_INT), "H5Tget_order");
    size=H5Tget_size(field);
    VERIFY(size, H5Tget_size(H5T_NATIVE_INT), "H5Tget_size");
    H5Tclose(field);
    field=H5Tget_member_type(type,1);
    CHECK(field, FAIL, "H5Tget_member_type");
    t_class=H5Tget_class(field);
    VERIFY(t_class, H5T_FLOAT, "H5Tget_class");
    order=H5Tget_order(field);
    VERIFY(order, H5Tget_order(H5T_NATIVE_DOUBLE), "H5Tget_order");
    size=H5Tget_size(field);
    VERIFY(size, H5Tget_size(H5T_NATIVE_DOUBLE), "H5Tget_size");
    H5Tclose(field);
    field=H5Tget_member_type(type,2);
    CHECK(field, FAIL, "H5Tget_member_type");
    t_class=H5Tget_class(field);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order=H5Tget_order(field);
    VERIFY(order, H5Tget_order(H5T_NATIVE_SCHAR), "H5Tget_order");
    size=H5Tget_size(field);
    VERIFY(size, H5Tget_size(H5T_NATIVE_SCHAR), "H5Tget_size");
    H5Tclose(field);

    /* Read attribute information */
    ret=H5Aread(attr,type,read_data4);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR4_DIM1; i++)
        for(j=0; j<ATTR4_DIM2; j++)
            if(HDmemcmp(&attr_data4[i][j],&read_data4[i][j],sizeof(struct attr4_struct))) {
                printf("%d: attribute data different: attr_data4[%d][%d].i=%d, read_data4[%d][%d].i=%d\n",__LINE__,i,j,attr_data4[i][j].i,i,j,read_data4[i][j].i);
                printf("%d: attribute data different: attr_data4[%d][%d].d=%f, read_data4[%d][%d].d=%f\n",__LINE__,i,j,attr_data4[i][j].d,i,j,read_data4[i][j].d);
                TestErrPrintf("%d: attribute data different: attr_data4[%d][%d].c=%c, read_data4[%d][%d].c=%c\n",__LINE__,i,j,attr_data4[i][j].c,i,j,read_data4[i][j].c);
             } /* end if */

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR4_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR4_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR4_NAME);

    /* Close attribute datatype */
    ret=H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_compound_read() */

/****************************************************************
**
**  test_attr_scalar_write(): Test scalar H5A (attribute) writing code.
**
****************************************************************/
static void
test_attr_scalar_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,DSET1_NAME,H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Create dataspace for attribute */
    sid2 = H5Screate_simple(ATTR5_RANK, NULL, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create an attribute for the dataset */
    attr=H5Acreate(dataset,ATTR5_NAME,H5T_NATIVE_FLOAT,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(dataset,ATTR5_NAME,H5T_NATIVE_FLOAT,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write attribute information */
    ret=H5Awrite(attr,H5T_NATIVE_FLOAT,&attr_data5);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_scalar_write() */

/****************************************************************
**
**  test_attr_scalar_read(): Test scalar H5A (attribute) reading code.
**
****************************************************************/
static void
test_attr_scalar_read(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid;	        /* Dataspace ID			*/
    hid_t		attr;	        /* Attribute ID			*/
    H5S_class_t         stype;          /* Dataspace class              */
    float       rdata=0.0;  /* Buffer for reading 1st attribute */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Scalar Attribute Reading Functions\n"));

    /* Create file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 1, "H5Aget_num_attrs");

    /* Open an attribute for the dataset */
    attr=H5Aopen_name(dataset,ATTR5_NAME);
    CHECK(attr, FAIL, "H5Aopen_name");

    /* Read attribute information */
    ret=H5Aread(attr,H5T_NATIVE_FLOAT,&rdata);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(rdata, attr_data5, "H5Aread");

    /* Get the attribute's dataspace */
    sid = H5Aget_space(attr);
    CHECK(sid, FAIL, "H5Aget_space");

    /* Make certain the dataspace is scalar */
    stype = H5Sget_simple_extent_type (sid);
    VERIFY(stype, H5S_SCALAR, "H5Sget_simple_extent_type");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_scalar_read() */

/****************************************************************
**
**  test_attr_mult_write(): Test basic H5A (attribute) code.
**      Tests integer attributes on both datasets and groups
**
****************************************************************/
static void
test_attr_mult_write(hid_t fapl)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1,sid2;	/* Dataspace ID			*/
    hid_t		attr;	    /* Attribute ID			*/
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {ATTR1_DIM1};
    hsize_t		dims3[] = {ATTR2_DIM1,ATTR2_DIM2};
    hsize_t		dims4[] = {ATTR3_DIM1,ATTR3_DIM2,ATTR3_DIM3};
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Multiple Attribute Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a dataset */
    dataset=H5Dcreate(fid1,DSET1_NAME,H5T_NATIVE_UCHAR,sid1,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Close dataset's dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for 1st attribute */
    sid2 = H5Screate_simple(ATTR1_RANK, dims2, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create 1st attribute for the dataset */
    attr=H5Acreate(dataset,ATTR1_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(dataset,ATTR1_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write 1st attribute data */
    ret=H5Awrite(attr,H5T_NATIVE_INT,attr_data1);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close 1st attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for 2nd attribute */
    sid2 = H5Screate_simple(ATTR2_RANK, dims3, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create 2nd attribute for the dataset */
    attr=H5Acreate(dataset,ATTR2_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(dataset,ATTR2_NAME,H5T_NATIVE_INT,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write 2nd attribute information */
    ret=H5Awrite(attr,H5T_NATIVE_INT,attr_data2);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close 2nd attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close 2nd attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create dataspace for 3rd attribute */
    sid2 = H5Screate_simple(ATTR3_RANK, dims4, NULL);
    CHECK(sid2, FAIL, "H5Screate_simple");

    /* Create 3rd attribute for the dataset */
    attr=H5Acreate(dataset,ATTR3_NAME,H5T_NATIVE_DOUBLE,sid2,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Try to create the same attribute again (should fail) */
    ret=H5Acreate(dataset,ATTR3_NAME,H5T_NATIVE_DOUBLE,sid2,H5P_DEFAULT);
    VERIFY(ret, FAIL, "H5Acreate");

    /* Write 3rd attribute information */
    ret=H5Awrite(attr,H5T_NATIVE_DOUBLE,attr_data3);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close 3rd attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close 3rd attribute's dataspace */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_mult_write() */

/****************************************************************
**
**  test_attr_mult_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void
test_attr_mult_read(hid_t fapl)
{
    hid_t   fid1;		/* HDF5 File IDs		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   space;      /* Attribute dataspace  */
    hid_t   type;       /* Attribute datatype   */
    hid_t   attr;	    /* Attribute ID			*/
    char    attr_name[ATTR_NAME_LEN]; /* Buffer for attribute names */
    char    temp_name[ATTR_NAME_LEN]; /* Buffer for mangling attribute names */
    int     rank;       /* Attribute rank */
    hsize_t dims[ATTR_MAX_DIMS];    /* Attribute dimensions */
    H5T_class_t t_class;              /* Attribute datatype class */
    H5T_order_t order;              /* Attribute datatype order */
    size_t      size;               /* Attribute datatype size as stored in file */
    int   read_data1[ATTR1_DIM1]={0}; /* Buffer for reading 1st attribute */
    int   read_data2[ATTR2_DIM1][ATTR2_DIM2]={{0}}; /* Buffer for reading 2nd attribute */
    double  read_data3[ATTR3_DIM1][ATTR3_DIM2][ATTR3_DIM3]={{{0}}}; /* Buffer for reading 3rd attribute */
    int     i,j,k;
    size_t  name_len;   /* Length of attribute name */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 3, "H5Aget_num_attrs");

    /* Open 1st attribute for the dataset */
    attr=H5Aopen_idx(dataset,0);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Dataspace */
    space=H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank=H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR1_RANK, "H5Sget_simple_extent_ndims");
    ret=H5Sget_simple_extent_dims(space,dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0]!=ATTR1_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n",(int)dims[0],ATTR1_DIM1);
    H5Sclose(space);

    /* Verify Datatype */
    type=H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class=H5Tget_class(type);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order=H5Tget_order(type);
    VERIFY(order, H5Tget_order(H5T_NATIVE_INT), "H5Tget_order");
    size=H5Tget_size(type);
    VERIFY(size, H5Tget_size(H5T_NATIVE_INT), "H5Tget_size");
    H5Tclose(type);

    /* Read attribute information */
    ret=H5Aread(attr,H5T_NATIVE_INT,read_data1);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR1_DIM1; i++)
        if(attr_data1[i]!=read_data1[i])
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR1_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR1_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR1_NAME);

    /* Verify Name with too small of a buffer */
    name_len=H5Aget_name(attr,HDstrlen(ATTR1_NAME), attr_name);
    VERIFY(name_len, HDstrlen(ATTR1_NAME), "H5Aget_name");
    HDstrcpy(temp_name,ATTR1_NAME);     /* make a copy of the name */
    temp_name[HDstrlen(ATTR1_NAME)-1]='\0';   /* truncate it to match the one retrieved */
    if(HDstrcmp(attr_name,temp_name))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,temp_name);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open 2nd attribute for the dataset */
    attr=H5Aopen_idx(dataset,1);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Dataspace */
    space=H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank=H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR2_RANK, "H5Sget_simple_extent_ndims");
    ret=H5Sget_simple_extent_dims(space,dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0]!=ATTR2_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n",(int)dims[0],ATTR2_DIM1);
    if(dims[1]!=ATTR2_DIM2)
        TestErrPrintf("attribute dimensions different: dims[1]=%d, should be %d\n",(int)dims[1],ATTR2_DIM2);
    H5Sclose(space);

    /* Verify Datatype */
    type=H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class=H5Tget_class(type);
    VERIFY(t_class, H5T_INTEGER, "H5Tget_class");
    order=H5Tget_order(type);
    VERIFY(order, H5Tget_order(H5T_NATIVE_INT), "H5Tget_order");
    size=H5Tget_size(type);
    VERIFY(size, H5Tget_size(H5T_NATIVE_INT), "H5Tget_size");
    H5Tclose(type);

    /* Read attribute information */
    ret=H5Aread(attr,H5T_NATIVE_INT,read_data2);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR2_DIM1; i++)
        for(j=0; j<ATTR2_DIM2; j++)
            if(attr_data2[i][j]!=read_data2[i][j])
                TestErrPrintf("%d: attribute data different: attr_data2[%d][%d]=%d, read_data2[%d][%d]=%d\n",__LINE__,i,j,attr_data2[i][j],i,j,read_data2[i][j]);

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR2_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR2_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR2_NAME);

    /* Verify Name with too small of a buffer */
    name_len=H5Aget_name(attr, HDstrlen(ATTR2_NAME), attr_name);
    VERIFY(name_len, HDstrlen(ATTR2_NAME), "H5Aget_name");
    HDstrcpy(temp_name,ATTR2_NAME);     /* make a copy of the name */
    temp_name[HDstrlen(ATTR2_NAME)-1]='\0';   /* truncate it to match the one retrieved */
    if(HDstrcmp(attr_name,temp_name))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,temp_name);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open 2nd attribute for the dataset */
    attr=H5Aopen_idx(dataset,2);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Dataspace */
    space=H5Aget_space(attr);
    CHECK(space, FAIL, "H5Aget_space");
    rank=H5Sget_simple_extent_ndims(space);
    VERIFY(rank, ATTR3_RANK, "H5Sget_simple_extent_ndims");
    ret=H5Sget_simple_extent_dims(space,dims, NULL);
    CHECK(ret, FAIL, "H5Sget_simple_extent_dims");
    if(dims[0]!=ATTR3_DIM1)
        TestErrPrintf("attribute dimensions different: dims[0]=%d, should be %d\n",(int)dims[0],ATTR3_DIM1);
    if(dims[1]!=ATTR3_DIM2)
        TestErrPrintf("attribute dimensions different: dims[1]=%d, should be %d\n",(int)dims[1],ATTR3_DIM2);
    if(dims[2]!=ATTR3_DIM3)
        TestErrPrintf("attribute dimensions different: dims[2]=%d, should be %d\n",(int)dims[2],ATTR3_DIM3);
    H5Sclose(space);

    /* Verify Datatype */
    type=H5Aget_type(attr);
    CHECK(type, FAIL, "H5Aget_type");
    t_class=H5Tget_class(type);
    VERIFY(t_class, H5T_FLOAT, "H5Tget_class");
    order=H5Tget_order(type);
    VERIFY(order, H5Tget_order(H5T_NATIVE_DOUBLE), "H5Tget_order");
    size=H5Tget_size(type);
    VERIFY(size, H5Tget_size(H5T_NATIVE_DOUBLE), "H5Tget_size");
    H5Tclose(type);

    /* Read attribute information */
    ret=H5Aread(attr,H5T_NATIVE_DOUBLE,read_data3);
    CHECK(ret, FAIL, "H5Aread");

    /* Verify values read in */
    for(i=0; i<ATTR3_DIM1; i++)
        for(j=0; j<ATTR3_DIM2; j++)
            for(k=0; k<ATTR3_DIM3; k++)
                if(attr_data3[i][j][k]!=read_data3[i][j][k])
                    TestErrPrintf("%d: attribute data different: attr_data3[%d][%d][%d]=%f, read_data3[%d][%d][%d]=%f\n",__LINE__,i,j,k,attr_data3[i][j][k],i,j,k,read_data3[i][j][k]);

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR3_NAME);

    /* Verify Name with too small of a buffer */
    name_len=H5Aget_name(attr, HDstrlen(ATTR3_NAME), attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    HDstrcpy(temp_name,ATTR3_NAME);     /* make a copy of the name */
    temp_name[HDstrlen(ATTR3_NAME)-1]='\0';   /* truncate it to match the one retrieved */
    if(HDstrcmp(attr_name,temp_name))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,temp_name);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_mult_read() */

/****************************************************************
**
**  attr_op1(): Attribute operator
**
****************************************************************/
herr_t attr_op1(hid_t UNUSED loc_id, const char *name, void *op_data)
{
    int *count=(int *)op_data;
    herr_t ret=0;

    switch(*count) {
        case 0:
            if(HDstrcmp(name,ATTR1_NAME))
                TestErrPrintf("attribute name different: name=%s, should be %s\n",name,ATTR1_NAME);
             (*count)++;
             break;

        case 1:
            if(HDstrcmp(name,ATTR2_NAME))
                TestErrPrintf("attribute name different: name=%s, should be %s\n",name,ATTR2_NAME);
             (*count)++;
             break;

        case 2:
            if(HDstrcmp(name,ATTR3_NAME))
                TestErrPrintf("attribute name different: name=%s, should be %s\n",name,ATTR3_NAME);
             (*count)++;
             break;

        default:
            ret=-1;
            break;
    }  /* end switch() */

    return(ret);
} /* end attr_op1() */

/****************************************************************
**
**  test_attr_iterate(): Test H5A (attribute) iterator code.
**
****************************************************************/
static void
test_attr_iterate(hid_t fapl)
{
    hid_t   file;		/* HDF5 File ID 		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   sid;	/* Dataspace ID			*/
    unsigned start;     /* Starting attribute to look up */
    int     count;      /* operator data for the iterator */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(file, FAIL, "H5Fopen");

    /* Create a dataspace */
    sid=H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a new dataset */
    dataset=H5Dcreate(file,DSET2_NAME,H5T_NATIVE_INT,sid,H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 0, "H5Aget_num_attrs");

    /* Iterate over attributes on dataset */
    start = 0;
    count = 0;
    ret = H5Aiterate(dataset, &start, attr_op1, &count);
    VERIFY(ret, 0, "H5Aiterate");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open existing dataset w/attributes */
    dataset=H5Dopen(file,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 3, "H5Aget_num_attrs");

    /* Iterate over attributes on dataset */
    start=0;
    count=0;
    ret = H5Aiterate(dataset,&start,attr_op1,&count);
    VERIFY(ret, 0, "H5Aiterate");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_iterate() */

/****************************************************************
**
**  test_attr_delete(): Test H5A (attribute) code for deleting objects.
**
****************************************************************/
static void
test_attr_delete(hid_t fapl)
{
    hid_t   fid1;		/* HDF5 File ID 		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   attr;	    /* Attribute ID			*/
    char    attr_name[ATTR_NAME_LEN]; /* Buffer for attribute names */
    size_t  name_len;   /* Length of attribute name */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 3, "H5Aget_num_attrs");

    /* Try to delete bogus attribute */
    ret=H5Adelete(dataset,"Bogus");
    VERIFY(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 3, "H5Aget_num_attrs");

    /* Delete middle (2nd) attribute */
    ret=H5Adelete(dataset,ATTR2_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 2, "H5Aget_num_attrs");

    /* Open 1st attribute for the dataset */
    attr=H5Aopen_idx(dataset,0);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN,attr_name);
    VERIFY(name_len, HDstrlen(ATTR1_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR1_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR1_NAME);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open last (formally 3rd) attribute for the dataset */
    attr=H5Aopen_idx(dataset,1);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR3_NAME);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete first attribute */
    ret=H5Adelete(dataset,ATTR1_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 1, "H5Aget_num_attrs");

    /* Open last (formally 3rd) attribute for the dataset */
    attr=H5Aopen_idx(dataset,0);
    CHECK(attr, FAIL, "H5Aopen_idx");

    /* Verify Name */
    name_len=H5Aget_name(attr, (size_t)ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR3_NAME);

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete first attribute */
    ret=H5Adelete(dataset,ATTR3_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 0, "H5Aget_num_attrs");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_delete() */

/****************************************************************
**
**  test_attr_dtype_shared(): Test H5A (attribute) code for using
**                              shared datatypes in attributes.
**
****************************************************************/
static void
test_attr_dtype_shared(hid_t fapl)
{
    hid_t file_id;              /* File ID */
    hid_t dset_id;              /* Dataset ID */
    hid_t space_id;             /* Dataspace ID for dataset & attribute */
    hid_t type_id;              /* Datatype ID for named datatype */
    hid_t attr_id;              /* Attribute ID */
    int data=8;                 /* Data to write */
    int rdata=0;                /* Read read in */
    H5G_stat_t statbuf;         /* Object's information */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Shared Datatypes with Attributes\n"));

    /* Create a file */
    file_id=H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Close file */
    ret=H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize=h5_get_file_size(FILENAME);
    if(empty_filesize<0)
        TestErrPrintf("Line %d: file size wrong!\n",__LINE__);


    /* Re-open file */
    file_id=H5Fopen(FILENAME,H5F_ACC_RDWR,fapl);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Create a datatype to commit and use */
    type_id=H5Tcopy(H5T_NATIVE_INT);
    CHECK(type_id, FAIL, "H5Tcopy");

    /* Commit datatype to file */
    ret=H5Tcommit(file_id,TYPE1_NAME,type_id);
    CHECK(ret, FAIL, "H5Tcommit");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 1, "H5Tcommit");

    /* Create dataspace for dataset */
    space_id=H5Screate(H5S_SCALAR);
    CHECK(space_id, FAIL, "H5Screate");

    /* Create dataset */
    dset_id=H5Dcreate(file_id,DSET1_NAME,type_id,space_id,H5P_DEFAULT);
    CHECK(dset_id, FAIL, "H5Dcreate");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 2, "H5Dcreate");

    /* Create attribute on dataset */
    attr_id=H5Acreate(dset_id,ATTR1_NAME,type_id,space_id,H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 3, "H5Acreate");

    /* Close attribute */
    ret=H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Delete attribute */
    ret=H5Adelete(dset_id,ATTR1_NAME);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 2, "H5Adelete");

    /* Create attribute on dataset */
    attr_id=H5Acreate(dset_id,ATTR1_NAME,type_id,space_id,H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 3, "H5Acreate");

    /* Write data into the attribute */
    ret=H5Awrite(attr_id,H5T_NATIVE_INT,&data);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret=H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret=H5Dclose(dset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret=H5Sclose(space_id);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close datatype */
    ret=H5Tclose(type_id);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close file */
    ret=H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    file_id=H5Fopen(FILENAME,H5F_ACC_RDWR,fapl);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Open dataset */
    dset_id=H5Dopen(file_id,DSET1_NAME);
    CHECK(dset_id, FAIL, "H5Dopen");

    /* Open attribute */
    attr_id=H5Aopen_name(dset_id,ATTR1_NAME);
    CHECK(attr_id, FAIL, "H5Aopen_name");

    /* Read data from the attribute */
    ret=H5Aread(attr_id,H5T_NATIVE_INT,&rdata);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(data, rdata, "H5Aread");

    /* Close attribute */
    ret=H5Aclose(attr_id);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret=H5Dclose(dset_id);
    CHECK(ret, FAIL, "H5Dclose");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 3, "H5Gget_objinfo");

    /* Unlink the dataset */
    ret=H5Gunlink(file_id,DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 1, "H5Gget_objinfo");

    /* Unlink the named datatype */
    ret=H5Gunlink(file_id,TYPE1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret=H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize=h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dtype_shared() */

/****************************************************************
**
**  test_attr_dense_verify(): Test basic H5A (attribute) code.
**      Verify attributes on object
**
****************************************************************/
static void
test_attr_dense_verify(hid_t loc_id, unsigned max_attr)
{
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    hid_t	attr;	        /* Attribute ID	*/
    unsigned    value;          /* Attribute value */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value	*/

    /* Re-open all the attributes by name and verify the data */
    for(u = 0; u < max_attr; u++) {
        /* Open attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Aopen_name(loc_id, attrname);
        CHECK(attr, FAIL, "H5Aopen_name");

        /* Read data from the attribute */
        ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Re-open all the attributes by index and verify the data */
    for(u = 0; u < max_attr; u++) {
        size_t name_len;                /* Length of attribute name */
        char check_name[ATTR_NAME_LEN]; /* Buffer for checking attribute names */

        /* Open attribute */
        attr = H5Aopen_idx(loc_id, u);
        CHECK(attr, FAIL, "H5Aopen_idx");

        /* Verify Name */
        sprintf(attrname, "attr %02u", u);
        name_len = H5Aget_name(attr, (size_t)ATTR_NAME_LEN, check_name);
        VERIFY(name_len, HDstrlen(attrname), "H5Aget_name");
        if(HDstrcmp(check_name, attrname))
            TestErrPrintf("attribute name different: attr_name = '%s', should be '%s'\n", check_name, attrname);

        /* Read data from the attribute */
        ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */
}   /* test_attr_dense_verify() */

/****************************************************************
**
**  test_attr_dense_create(): Test basic H5A (attribute) code.
**      Tests "dense" attribute storage creation
**
****************************************************************/
static void
test_attr_dense_create(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dense Attribute Storage Creation\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until just before converting to dense storage */
    for(u = 0; u < max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add one more attribute, to push into "dense" storage */
    /* Create attribute */
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Attempt to add attribute again, which should fail */
    attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
    VERIFY(attr, FAIL, "H5Acreate");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Gunlink(fid, DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_create() */

/****************************************************************
**
**  test_attr_dense_open(): Test basic H5A (attribute) code.
**      Tests opening attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_open(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Opening Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until just before converting to dense storage */
    for(u = 0; u < max_compact; u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Verify attributes written so far */
        test_attr_dense_verify(dataset, u);
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add one more attribute, to push into "dense" storage */
    /* Create attribute */
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Verify all the attributes written */
    test_attr_dense_verify(dataset, (u + 1));

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Gunlink(fid, DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_open() */

/****************************************************************
**
**  test_attr_dense_delete(): Test basic H5A (attribute) code.
**      Tests deleting attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_delete(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    int         attr_count;     /* # of attributes */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deleting Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until well into dense storage */
    for(u = 0; u < (max_compact * 2); u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Check # of attributes */
        attr_count = H5Aget_num_attrs(dataset);
        CHECK(attr_count, FAIL, "H5Aget_num_attrs");
        VERIFY(attr_count, (int)(u + 1), "H5Aget_num_attrs");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset */
    dataset = H5Dopen(fid, DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Delete attributes until the attributes revert to compact storage again */
    for(u--; u >= min_dense; u--) {
        /* Delete attribute */
        sprintf(attrname, "attr %02u", u);
        ret = H5Adelete(dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Verify attributes still left */
        test_attr_dense_verify(dataset, u);
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Delete one more attribute, which should cause reversion to compact storage */
    sprintf(attrname, "attr %02u", u);
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Verify attributes still left */
    test_attr_dense_verify(dataset, (u - 1));

    /* Delete another attribute, to verify deletion in compact storage */
    sprintf(attrname, "attr %02u", (u - 1));
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Verify attributes still left */
    test_attr_dense_verify(dataset, (u - 2));

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Gunlink(fid, DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_delete() */

/****************************************************************
**
**  test_attr_dense_rename(): Test basic H5A (attribute) code.
**      Tests renaming attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_rename(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    char	new_attrname[NAME_BUF_SIZE];    /* New name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    int         attr_count;     /* # of attributes */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Renaming Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until well into dense storage */
    for(u = 0; u < (max_compact * 2); u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Rename attribute */
        sprintf(new_attrname, "new attr %02u", u);

        /* Rename attribute */
        ret = H5Arename(dataset, attrname, new_attrname);
        CHECK(ret, FAIL, "H5Arename");

        /* Check # of attributes */
        attr_count = H5Aget_num_attrs(dataset);
        CHECK(attr_count, FAIL, "H5Aget_num_attrs");
        VERIFY(attr_count, (int)(u + 1), "H5Aget_num_attrs");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset */
    dataset = H5Dopen(fid, DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify renamed attributes */
    for(u = 0; u < (max_compact * 2); u++) {
        unsigned    value;          /* Attribute value */

        /* Create attribute */
        sprintf(attrname, "new attr %02u", u);
        attr = H5Aopen_name(dataset, attrname);
        CHECK(attr, FAIL, "H5Aopen_name");

        /* Read data from the attribute */
        ret = H5Aread(attr, H5T_NATIVE_UINT, &value);
        CHECK(ret, FAIL, "H5Aread");
        VERIFY(value, u, "H5Aread");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");
    } /* end for */

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset with attributes */
    ret = H5Gunlink(fid, DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_rename() */

/****************************************************************
**
**  test_attr_dense_unlink(): Test basic H5A (attribute) code.
**      Tests unlinking object with attributes in "dense" storage
**
****************************************************************/
static void
test_attr_dense_unlink(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    int         attr_count;     /* # of attributes */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Unlinking Object with Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Add attributes, until well into dense storage */
    for(u = 0; u < (max_compact * 2); u++) {
        /* Create attribute */
        sprintf(attrname, "attr %02u", u);
        attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Check # of attributes */
        attr_count = H5Aget_num_attrs(dataset);
        CHECK(attr_count, FAIL, "H5Aget_num_attrs");
        VERIFY(attr_count, (int)(u + 1), "H5Aget_num_attrs");
    } /* end for */

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Unlink dataset */
    ret = H5Gunlink(fid, DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Check on dataset's attribute storage status */
    ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
    CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
    VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_unlink() */

/****************************************************************
**
**  test_attr_dense_limits(): Test basic H5A (attribute) code.
**      Tests attribute in "dense" storage limits
**
****************************************************************/
static void
test_attr_dense_limits(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact, rmax_compact;      /* Maximum # of attributes to store compactly */
    unsigned    min_dense, rmin_dense;          /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Phase Change Limits For Attributes in Dense Storage\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize = h5_get_file_size(FILENAME);
    if(empty_filesize < 0)
        TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Query the group creation properties */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Change limits on compact/dense attribute storage */
    max_compact = 0;
    min_dense = 0;
    ret = H5Pset_attr_phase_change(dcpl, max_compact, min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Retrieve limits for compact/dense attribute storage */
    ret = H5Pget_attr_phase_change(dcpl, &rmax_compact, &rmin_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");
    VERIFY(rmax_compact, max_compact, "H5Pget_attr_phase_change");
    VERIFY(rmin_dense, min_dense, "H5Pget_attr_phase_change");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");


    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Add first attribute, which should be immediately in dense storage */

    /* Create attribute */
    u = 0;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


    /* Add second attribute, to allow deletions to be checked easily */

    /* Create attribute */
    u = 1;
    sprintf(attrname, "attr %02u", u);
    attr = H5Acreate(dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate");

    /* Write data into the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


    /* Delete second attribute, attributes should still be stored densely */

    /* Delete attribute */
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


    /* Delete first attribute, attributes should not be stored densely */

    /* Delete attribute */
    u = 0;
    sprintf(attrname, "attr %02u", u);
    ret = H5Adelete(dataset, attrname);
    CHECK(ret, FAIL, "H5Adelete");

    /* Check on dataset's attribute storage status */
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");


    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Unlink dataset */
    ret = H5Gunlink(fid, DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize = h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "h5_get_file_size");
}   /* test_attr_dense_limits() */

/****************************************************************
**
**  test_attr_corder_create_empty(): Test basic H5A (attribute) code.
**      Tests basic code to create objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_create_basic(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dataset;	/* Dataset ID			*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    crt_order_flags;/* Creation order flags */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Code for Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Get creation order indexing on object */
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, 0, "H5Pget_attr_creation_order");

    /* Setting invalid combination of a attribute order creation order indexing on should fail */
    ret = H5Pset_attr_creation_order(dcpl, H5P_CRT_ORDER_INDEXED);
    VERIFY(ret, FAIL, "H5Pset_attr_creation_order");
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, 0, "H5Pget_attr_creation_order");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_attr_creation_order");
    VERIFY(crt_order_flags, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) , "H5Pget_attr_creation_order");

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create a dataset */
    dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dataset, FAIL, "H5Dcreate");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open dataset created */
    dataset = H5Dopen(fid, DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Check on dataset's attribute storage status */
    is_empty = H5O_is_attr_empty_test(dataset);
    VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
    is_dense = H5O_is_attr_dense_test(dataset);
    VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

    /* Retrieve dataset creation property list for group */
    dcpl = H5Dget_create_plist(dataset);
    CHECK(dcpl, FAIL, "H5Dget_create_plist");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_creation_order(dcpl, &crt_order_flags);
    CHECK(ret, FAIL, "H5Pget_link_creation_order");
    VERIFY(crt_order_flags, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) , "H5Pget_attr_creation_order");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_create_basic() */

/****************************************************************
**
**  test_attr_corder_create_compact(): Test basic H5A (attribute) code.
**      Tests compact attribute storage on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_create_compact(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Compact Storage of Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create datasets */
    dset1 = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset1, FAIL, "H5Dcreate");
    dset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset2, FAIL, "H5Dcreate");
    dset3 = H5Dcreate(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset3, FAIL, "H5Dcreate");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Create several attributes, but keep storage in compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (u + 1), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        } /* end for */
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen(fid, DSET1_NAME);
    CHECK(dset1, FAIL, "H5Dopen");
    dset2 = H5Dopen(fid, DSET2_NAME);
    CHECK(dset2, FAIL, "H5Dopen");
    dset3 = H5Dopen(fid, DSET3_NAME);
    CHECK(dset3, FAIL, "H5Dopen");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Loop through attributes, checking their creation order values */
        /* (the name index is used, but the creation order value is in the same order) */
        for(u = 0; u < max_compact; u++) {
            H5A_info_t ainfo;           /* Attribute information */

            /* Retrieve information for attribute */
            sprintf(attrname, "attr %02u", u);
            ret = H5Aget_info(my_dataset, attrname, &ainfo);
            CHECK(ret, FAIL, "H5Aget_info");

            /* Verify creation order of attribute */
            VERIFY(ainfo.corder_valid, TRUE, "H5Aget_info");
            VERIFY(ainfo.corder, u, "H5Aget_info");
        } /* end for */
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_create_compact() */

/****************************************************************
**
**  test_attr_corder_create_dense(): Test basic H5A (attribute) code.
**      Tests dense attribute storage on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_create_dense(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dense Storage of Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create datasets */
    dset1 = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset1, FAIL, "H5Dcreate");
    dset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset2, FAIL, "H5Dcreate");
    dset3 = H5Dcreate(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset3, FAIL, "H5Dcreate");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Create several attributes, but keep storage in compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (u + 1), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        } /* end for */

        /* Create another attribute, to push into dense storage */
        sprintf(attrname, "attr %02u", max_compact);
        attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen(fid, DSET1_NAME);
    CHECK(dset1, FAIL, "H5Dopen");
    dset2 = H5Dopen(fid, DSET2_NAME);
    CHECK(dset2, FAIL, "H5Dopen");
    dset3 = H5Dopen(fid, DSET3_NAME);
    CHECK(dset3, FAIL, "H5Dopen");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Loop through attributes, checking their creation order values */
        /* (the name index is used, but the creation order value is in the same order) */
        for(u = 0; u < (max_compact + 1); u++) {
            H5A_info_t ainfo;           /* Attribute information */

            /* Retrieve information for attribute */
            sprintf(attrname, "attr %02u", u);
            ret = H5Aget_info(my_dataset, attrname, &ainfo);
            CHECK(ret, FAIL, "H5Aget_info");

            /* Verify creation order of attribute */
            VERIFY(ainfo.corder_valid, TRUE, "H5Aget_info");
            VERIFY(ainfo.corder, u, "H5Aget_info");
        } /* end for */
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_create_dense() */

/****************************************************************
**
**  test_attr_corder_transition(): Test basic H5A (attribute) code.
**      Tests attribute storage transitions on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_transition(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Storage Transitions of Attributes with Creation Order Info\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

/* XXX: Try to find a way to resize dataset's object header so that the object
 *      header can have one chunk, then retrieve "empty" file size and check
 *      that size after everything is deleted -QAK
 */
    /* Create datasets */
    dset1 = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset1, FAIL, "H5Dcreate");
    dset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset2, FAIL, "H5Dcreate");
    dset3 = H5Dcreate(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
    CHECK(dset3, FAIL, "H5Dcreate");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen(fid, DSET1_NAME);
    CHECK(dset1, FAIL, "H5Dopen");
    dset2 = H5Dopen(fid, DSET2_NAME);
    CHECK(dset2, FAIL, "H5Dopen");
    dset3 = H5Dopen(fid, DSET3_NAME);
    CHECK(dset3, FAIL, "H5Dopen");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Create several attributes, but keep storage in compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (u + 1), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        } /* end for */

        /* Create another attribute, to push into dense storage */
        sprintf(attrname, "attr %02u", max_compact);
        attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate");

        /* Write data into the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
        CHECK(ret, FAIL, "H5Awrite");

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");

        /* Delete several attributes from object, until attribute storage resumes compact form */
        for(u = max_compact; u >= min_dense; u--) {
            sprintf(attrname, "attr %02u", u);
            ret = H5Adelete(my_dataset, attrname);
            CHECK(ret, FAIL, "H5Adelete");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, u, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
        } /* end for */

        /* Delete another attribute, to push attribute storage into compact form */
        sprintf(attrname, "attr %02u", (min_dense - 1));
        ret = H5Adelete(my_dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (min_dense - 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Re-add attributes to get back into dense form */
        for(u = (min_dense - 1); u < (max_compact + 1); u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");


    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open datasets created */
    dset1 = H5Dopen(fid, DSET1_NAME);
    CHECK(dset1, FAIL, "H5Dopen");
    dset2 = H5Dopen(fid, DSET2_NAME);
    CHECK(dset2, FAIL, "H5Dopen");
    dset3 = H5Dopen(fid, DSET3_NAME);
    CHECK(dset3, FAIL, "H5Dopen");

    /* Work on all the datasets */
    for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
        switch(curr_dset) {
            case 0:
                my_dataset = dset1;
                break;

            case 1:
                my_dataset = dset2;
                break;

            case 2:
                my_dataset = dset3;
                break;

            default:
                HDassert(0 && "Too many datasets!");
        } /* end switch */

        /* Check on dataset's attribute storage status */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");

        /* Delete several attributes from object, until attribute storage resumes compact form */
        for(u = max_compact; u >= min_dense; u--) {
            sprintf(attrname, "attr %02u", u);
            ret = H5Adelete(my_dataset, attrname);
            CHECK(ret, FAIL, "H5Adelete");

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, u, "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
        } /* end for */

        /* Delete another attribute, to push attribute storage into compact form */
        sprintf(attrname, "attr %02u", (min_dense - 1));
        ret = H5Adelete(my_dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (min_dense - 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Re-add attributes to get back into dense form */
        for(u = (min_dense - 1); u < (max_compact + 1); u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact + 1), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Retrieve & verify # of records in the name & creation order indices */
        ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
        CHECK(ret, FAIL, "H5O_attr_dense_info_test");
        VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");

        /* Delete all attributes */
        for(u = max_compact; u > 0; u--) {
            sprintf(attrname, "attr %02u", u);
            ret = H5Adelete(my_dataset, attrname);
            CHECK(ret, FAIL, "H5Adelete");
        } /* end for */
        sprintf(attrname, "attr %02u", 0);
        ret = H5Adelete(my_dataset, attrname);
        CHECK(ret, FAIL, "H5Adelete");
    } /* end for */

    /* Close Datasets */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_corder_transition() */

/****************************************************************
**
**  test_attr_corder_delete(): Test basic H5A (attribute) code.
**      Tests deleting object w/dense attribute storage on objects with attribute creation order info
**
****************************************************************/
static void
test_attr_corder_delete(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    hbool_t     reopen_file;            /* Whether to re-open the file before deleting group */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
#ifdef LATER
    h5_stat_size_t empty_size;  /* Size of empty file */
    h5_stat_size_t file_size;   /* Size of file after operating on it */
#endif /* LATER */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deleting Object w/Dense Attribute Storage and Creation Order Info\n"));

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(dcpl, FAIL, "H5Pcreate");

    /* Set attribute creation order tracking & indexing for object */
    ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED));
    CHECK(ret, FAIL, "H5Pset_attr_creation_order");

    /* Query the attribute creation properties */
    ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
    CHECK(ret, FAIL, "H5Pget_attr_phase_change");


/* XXX: Try to find a way to resize dataset's object header so that the object
 *      header can have one chunk, then retrieve "empty" file size and check
 *      that size after everything is deleted -QAK
 */
#ifdef LATER
    /* Create empty file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get the size of an empty file */
    empty_size = h5_get_file_size(FILENAME);
    CHECK(empty_size, FAIL, "h5_get_file_size");
#endif /* LATER */


    /* Loop to leave file open when deleting dataset, or to close & re-open file
     *  before deleting dataset */
    for(reopen_file = FALSE; reopen_file <= TRUE; reopen_file++) {
        /* Create test file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Create datasets */
        dset1 = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dset1, FAIL, "H5Dcreate");
        dset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dset2, FAIL, "H5Dcreate");
        dset3 = H5Dcreate(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dset3, FAIL, "H5Dcreate");

        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    break;

                case 1:
                    my_dataset = dset2;
                    break;

                case 2:
                    my_dataset = dset3;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Check on dataset's attribute storage status */
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Create attributes, until attribute storage is in dense form */
            for(u = 0; u < max_compact * 2; u++) {
                /* Create attribute */
                sprintf(attrname, "attr %02u", u);
                attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Write data into the attribute */
                ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
                CHECK(ret, FAIL, "H5Awrite");

                /* Close attribute */
                ret = H5Aclose(attr);
                CHECK(ret, FAIL, "H5Aclose");
            } /* end for */

            /* Verify state of object */
            ret = H5O_num_attrs_test(my_dataset, &nattrs);
            CHECK(ret, FAIL, "H5O_num_attrs_test");
            VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check for deleting datasets without re-opening file */
        if(!reopen_file) {
            ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET3_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check for deleting dataset after re-opening file */
        if(reopen_file) {
            /* Re-open file */
            fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
            CHECK(fid, FAIL, "H5Fopen");

            /* Delete the datasets */
            ret = H5Ldelete(fid, DSET1_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET2_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");
            ret = H5Ldelete(fid, DSET3_NAME, H5P_DEFAULT);
            CHECK(ret, FAIL, "H5Ldelete");

            /* Close file */
            ret = H5Fclose(fid);
            CHECK(ret, FAIL, "H5Fclose");
        } /* end if */

#ifdef LATER
        /* Get the size of the file now */
        file_size = h5_get_file_size(FILENAME);
        CHECK(file_size, FAIL, "h5_get_file_size");
        VERIFY(file_size, empty_size, "h5_get_file_size");
#endif /* LATER */
    } /* end for */

    /* Close property list */
    ret = H5Pclose(dcpl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_corder_delete() */


/*-------------------------------------------------------------------------
 * Function:    attr_info_by_idx_check
 *
 * Purpose:     Support routine for attr_info_by_idx, to verify the attribute
 *              info is correct for a attribute
 *
 * Note:	This routine assumes that the attributes have been added to the
 *              object in alphabetical order.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, Februrary 13, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
attr_info_by_idx_check(hid_t obj_id, const char *attrname, hsize_t n,
    hbool_t use_index)
{
    char tmpname[NAME_BUF_SIZE];        /* Temporary attribute name */
    H5A_info_t  ainfo;                  /* Attribute info struct */
    int         old_nerrs;              /* Number of errors when entering this check */
    herr_t	ret;		        /* Generic return value */

    /* Retrieve the current # of reported errors */
    old_nerrs = GetTestNumErrs();

    /* Verify the information for first attribute, in increasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, n, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);


    /* Don't test "native" order if there is no creation order index, since
     *  there's not a good way to easily predict the attribute's order in the name
     *  index.
     */
    if(use_index) {
        /* Verify the information for first attribute, in native creation order */
        HDmemset(&ainfo, 0, sizeof(ainfo));
        ret = H5Aget_info_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, (hsize_t)0, &ainfo);
        CHECK(ret, FAIL, "H5Aget_info_by_idx");
        VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

        /* Verify the information for new attribute, in native creation order */
        HDmemset(&ainfo, 0, sizeof(ainfo));
        ret = H5Aget_info_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, &ainfo);
        CHECK(ret, FAIL, "H5Aget_info_by_idx");
        VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

        /* Verify the name for new link, in increasing native order */
        HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
        ret = H5Aget_name_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, n, tmpname, (size_t)NAME_BUF_SIZE);
        CHECK(ret, FAIL, "H5Aget_name_by_idx");
        if(HDstrcmp(attrname, tmpname))
            TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);
    } /* end if */


    /* Verify the information for first attribute, in decreasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, n, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing creation order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing creation order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);


    /* Verify the information for first attribute, in increasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_NAME, H5_ITER_INC, n, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, H5_INDEX_NAME, H5_ITER_INC, n, tmpname, (size_t)NAME_BUF_SIZE);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);


    /* Don't test "native" order queries on link name order, since there's not
     *  a good way to easily predict the order of the links in the name index.
     */


    /* Verify the information for first attribute, in decreasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_NAME, H5_ITER_DEC, n, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, 0, "H5Aget_info_by_idx");

    /* Verify the information for new attribute, in increasing name order */
    HDmemset(&ainfo, 0, sizeof(ainfo));
    ret = H5Aget_info_by_idx(obj_id, H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, &ainfo);
    CHECK(ret, FAIL, "H5Aget_info_by_idx");
    VERIFY(ainfo.corder, n, "H5Aget_info_by_idx");

    /* Verify the name for new link, in increasing name order */
    HDmemset(tmpname, 0, (size_t)NAME_BUF_SIZE);
    ret = H5Aget_name_by_idx(obj_id, H5_INDEX_NAME, H5_ITER_DEC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE);
    CHECK(ret, FAIL, "H5Aget_name_by_idx");
    if(HDstrcmp(attrname, tmpname))
        TestErrPrintf("Line %d: attribute name size wrong!\n", __LINE__);

    /* Retrieve current # of errors */
    if(old_nerrs == GetTestNumErrs())
        return(0);
    else
        return(-1);
} /* end attr_info_by_idx_check() */

/****************************************************************
**
**  test_attr_info_by_idx(): Test basic H5A (attribute) code.
**      Tests querying attribute info by index
**
****************************************************************/
static void
test_attr_info_by_idx(hbool_t new_format, hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	dset1, dset2, dset3;	/* Dataset IDs			*/
    hid_t	my_dataset;	/* Current dataset ID		*/
    hid_t	sid;	        /* Dataspace ID			*/
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    H5A_info_t  ainfo;          /* Attribute information */
    unsigned    max_compact;    /* Maximum # of links to store in group compactly */
    unsigned    min_dense;      /* Minimum # of links to store in group "densely" */
    htri_t	is_empty;	/* Are there any attributes? */
    htri_t	is_dense;	/* Are attributes stored densely? */
    hsize_t     nattrs;         /* Number of attributes on object */
    hsize_t     name_count;     /* # of records in name index */
    hsize_t     corder_count;   /* # of records in creation order index */
    hbool_t     use_index;      /* Use index on creation order values */
    char	attrname[NAME_BUF_SIZE];    /* Name of attribute */
    char        tmpname[NAME_BUF_SIZE];     /* Temporary attribute name */
    unsigned    curr_dset;      /* Current dataset to work on */
    unsigned    u;              /* Local index variable */
    herr_t	ret;		/* Generic return value		*/

    /* Create dataspace for dataset & attributes */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Loop over using index for creation order value */
    for(use_index = FALSE; use_index <= TRUE; use_index++) {
        /* Output message about test being performed */
        if(use_index)
            MESSAGE(5, ("Testing Querying Attribute Info By Index w/Creation Order Index\n"))
        else
            MESSAGE(5, ("Testing Querying Attribute Info By Index w/o Creation Order Index\n"))

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Create dataset creation property list */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Set attribute creation order tracking & indexing for object */
        ret = H5Pset_attr_creation_order(dcpl, (H5P_CRT_ORDER_TRACKED | (use_index ? H5P_CRT_ORDER_INDEXED : (unsigned)0)));
        CHECK(ret, FAIL, "H5Pset_attr_creation_order");

        /* Query the attribute creation properties */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Create datasets */
        dset1 = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dset1, FAIL, "H5Dcreate");
        dset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dset2, FAIL, "H5Dcreate");
        dset3 = H5Dcreate(fid, DSET3_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dset3, FAIL, "H5Dcreate");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Work on all the datasets */
        for(curr_dset = 0; curr_dset < NUM_DSETS; curr_dset++) {
            switch(curr_dset) {
                case 0:
                    my_dataset = dset1;
                    break;

                case 1:
                    my_dataset = dset2;
                    break;

                case 2:
                    my_dataset = dset3;
                    break;

                default:
                    HDassert(0 && "Too many datasets!");
            } /* end switch */

            /* Check on dataset's attribute storage status */
            is_empty = H5O_is_attr_empty_test(my_dataset);
            VERIFY(is_empty, TRUE, "H5O_is_attr_empty_test");
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

            /* Check for query on non-existant attribute */
            ret = H5Aget_info_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, &ainfo);
            VERIFY(ret, FAIL, "H5Aget_info_by_idx");
            ret = H5Aget_name_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)0, tmpname, (size_t)NAME_BUF_SIZE);
            VERIFY(ret, FAIL, "H5Aget_name_by_idx");
        } /* end for */

        /* Create attributes, up to limit of compact form */
        for(u = 0; u < max_compact; u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify information for new attribute */
            ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
            CHECK(ret, FAIL, "attr_info_by_idx_check");
        } /* end for */

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, max_compact, "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Check for out of bound offset queries */
        ret = H5Aget_info_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &ainfo);
        VERIFY(ret, FAIL, "H5Aget_info_by_idx");
        ret = H5Aget_info_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &ainfo);
        VERIFY(ret, FAIL, "H5Aget_info_by_idx");
        ret = H5Aget_name_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE);
        VERIFY(ret, FAIL, "H5Aget_name_by_idx");

        /* Create more attributes, to push into dense form */
        for(; u < (max_compact * 2); u++) {
            /* Create attribute */
            sprintf(attrname, "attr %02u", u);
            attr = H5Acreate(my_dataset, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT);
            CHECK(attr, FAIL, "H5Acreate");

            /* Write data into the attribute */
            ret = H5Awrite(attr, H5T_NATIVE_UINT, &u);
            CHECK(ret, FAIL, "H5Awrite");

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Verify state of object */
            is_dense = H5O_is_attr_dense_test(my_dataset);
            VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

            /* Verify information for new attribute */
            ret = attr_info_by_idx_check(my_dataset, attrname, (hsize_t)u, use_index);
            CHECK(ret, FAIL, "attr_info_by_idx_check");
        } /* end for */

        /* Verify state of object */
        ret = H5O_num_attrs_test(my_dataset, &nattrs);
        CHECK(ret, FAIL, "H5O_num_attrs_test");
        VERIFY(nattrs, (max_compact * 2), "H5O_num_attrs_test");
        is_empty = H5O_is_attr_empty_test(my_dataset);
        VERIFY(is_empty, FALSE, "H5O_is_attr_empty_test");
        is_dense = H5O_is_attr_dense_test(my_dataset);
        VERIFY(is_dense, (new_format ? TRUE : FALSE), "H5O_is_attr_dense_test");

        if(new_format) {
            /* Retrieve & verify # of records in the name & creation order indices */
            ret = H5O_attr_dense_info_test(my_dataset, &name_count, &corder_count);
            CHECK(ret, FAIL, "H5O_attr_dense_info_test");
            if(use_index)
                VERIFY(name_count, corder_count, "H5O_attr_dense_info_test");
            VERIFY(name_count, (max_compact * 2), "H5O_attr_dense_info_test");
        } /* end if */

        /* Check for out of bound offset queries */
        ret = H5Aget_info_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, &ainfo);
        VERIFY(ret, FAIL, "H5Aget_info_by_idx");
        ret = H5Aget_info_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_DEC, (hsize_t)u, &ainfo);
        VERIFY(ret, FAIL, "H5Aget_info_by_idx");
        ret = H5Aget_name_by_idx(my_dataset, H5_INDEX_CRT_ORDER, H5_ITER_INC, (hsize_t)u, tmpname, (size_t)NAME_BUF_SIZE);
        VERIFY(ret, FAIL, "H5Aget_name_by_idx");

        /* Close Datasets */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */
}   /* test_attr_info_by_idx() */

/****************************************************************
**
**  test_attr_shared_write(): Test basic H5A (attribute) code.
**      Tests writing mix of shared & un-shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_write(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset IDs			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Writing Shared & Unshared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_MESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_MESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit(fid, TYPE1_NAME, attr_tid);
            CHECK(ret, FAIL, "H5Tcommit");

            /* Close attribute's datatype */
            ret = H5Tclose(attr_tid);
            CHECK(ret, FAIL, "H5Tclose");
        } /* end switch */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Re-open attribute datatype as necessary */
        if(test_shared == 2) {
            attr_tid = H5Topen(fid, TYPE1_NAME);
            CHECK(attr_tid, FAIL, "H5Topen");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset, FAIL, "H5Dcreate");
        dataset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset2, FAIL, "H5Dcreate");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Dataset's datatypes are immutable and shouldn't be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Dataset's dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
        } /* end for */

        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check on shared message status now */
        if(test_shared != 0) {
            if(test_shared == 1) {
                /* Check on datatype storage status */
                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
                VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
            } /* end if */

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Unlink datasets with attributes */
        ret = H5Gunlink(fid, DSET1_NAME);
        CHECK(ret, FAIL, "H5Gunlink");
        ret = H5Gunlink(fid, DSET2_NAME);
        CHECK(ret, FAIL, "H5Gunlink");

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_write() */

/****************************************************************
**
**  test_attr_shared_rename(): Test basic H5A (attribute) code.
**      Tests renaming shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_rename(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* HDF5 File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset ID2			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute on first dataset */
    char	attrname2[NAME_BUF_SIZE];       /* Name of attribute on second dataset */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Renaming Shared & Unshared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_MESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_MESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit(fid, TYPE1_NAME, attr_tid);
            CHECK(ret, FAIL, "H5Tcommit");

            /* Close attribute's datatype */
            ret = H5Tclose(attr_tid);
            CHECK(ret, FAIL, "H5Tclose");
        } /* end switch */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Re-open attribute datatype as necessary */
        if(test_shared == 2) {
            attr_tid = H5Topen(fid, TYPE1_NAME);
            CHECK(attr_tid, FAIL, "H5Topen");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset, FAIL, "H5Dcreate");
        dataset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset2, FAIL, "H5Dcreate");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Dataset's datatypes are immutable and shouldn't be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Dataset's dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Create new attribute name */
            sprintf(attrname2, "new attr %02u", u);

            /* Change second dataset's attribute's name */
            ret = H5Arename(dataset2, attrname, attrname2);
            CHECK(ret, FAIL, "H5Arename");


            /* Check refcount on attributes now */

            /* Check refcount on renamed attribute */
            attr = H5Aopen_name(dataset2, attrname2);
            CHECK(attr, FAIL, "H5Aopen_name");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check refcount on original attribute */
            attr = H5Aopen_name(dataset, attrname);
            CHECK(attr, FAIL, "H5Aopen_name");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");


            /* Change second dataset's attribute's name back to original */
            ret = H5Arename(dataset2, attrname2, attrname);
            CHECK(ret, FAIL, "H5Arename");


            /* Check refcount on attributes now */

            /* Check refcount on renamed attribute */
            attr = H5Aopen_name(dataset2, attrname);
            CHECK(attr, FAIL, "H5Aopen_name");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check refcount on original attribute */
            attr = H5Aopen_name(dataset, attrname);
            CHECK(attr, FAIL, "H5Aopen_name");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */


        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check on shared message status now */
        if(test_shared != 0) {
            if(test_shared == 1) {
                /* Check on datatype storage status */
                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
                VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
            } /* end if */

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Unlink datasets with attributes */
        ret = H5Gunlink(fid, DSET1_NAME);
        CHECK(ret, FAIL, "H5Gunlink");
        ret = H5Gunlink(fid, DSET2_NAME);
        CHECK(ret, FAIL, "H5Gunlink");

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_rename() */

/****************************************************************
**
**  test_attr_shared_delete(): Test basic H5A (attribute) code.
**      Tests deleting shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_delete(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset IDs			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute on first dataset */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deleting Shared & Unshared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_MESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_MESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit(fid, TYPE1_NAME, attr_tid);
            CHECK(ret, FAIL, "H5Tcommit");

            /* Close attribute's datatype */
            ret = H5Tclose(attr_tid);
            CHECK(ret, FAIL, "H5Tclose");
        } /* end switch */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Re-open attribute datatype as necessary */
        if(test_shared == 2) {
            attr_tid = H5Topen(fid, TYPE1_NAME);
            CHECK(attr_tid, FAIL, "H5Topen");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset, FAIL, "H5Dcreate");
        dataset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset2, FAIL, "H5Dcreate");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Dataset's datatypes are immutable and shouldn't be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Dataset's dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
        } /* end for */


        /* Delete attributes from second dataset */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Delete second dataset's attribute */
            ret = H5Adelete(dataset2, attrname);
            CHECK(ret, FAIL, "H5Adelete");


            /* Check refcount on attributes now */

            /* Check refcount on first dataset's attribute */
            attr = H5Aopen_name(dataset, attrname);
            CHECK(attr, FAIL, "H5Aopen_name");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */


        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Check on shared message status now */
        if(test_shared != 0) {
            if(test_shared == 1) {
                /* Check on datatype storage status */
                ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
                CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
                VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
            } /* end if */

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 2, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Unlink datasets with attributes */
        ret = H5Gunlink(fid, DSET1_NAME);
        CHECK(ret, FAIL, "H5Gunlink");
        ret = H5Gunlink(fid, DSET2_NAME);
        CHECK(ret, FAIL, "H5Gunlink");

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_delete() */

/****************************************************************
**
**  test_attr_shared_unlink(): Test basic H5A (attribute) code.
**      Tests unlinking object with  shared attributes in "compact" & "dense" storage
**
****************************************************************/
static void
test_attr_shared_unlink(hid_t fcpl, hid_t fapl)
{
    hid_t	fid;		/* File ID			*/
    hid_t	my_fcpl;	/* File creation property list ID */
    hid_t	dataset, dataset2;	/* Dataset IDs			*/
    hid_t	attr_tid;	/* Attribute's datatype ID	*/
    hid_t	sid, big_sid;	/* Dataspace IDs		*/
    hsize_t	big_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};   /* Dimensions for "big" attribute */
    hid_t	attr;	        /* Attribute ID			*/
    hid_t	dcpl;	        /* Dataset creation property list ID */
    char	attrname[NAME_BUF_SIZE];        /* Name of attribute on first dataset */
    unsigned    max_compact;    /* Maximum # of attributes to store compactly */
    unsigned    min_dense;      /* Minimum # of attributes to store "densely" */
    htri_t	is_dense;	/* Are attributes stored densely? */
    htri_t	is_shared;	/* Is attributes shared? */
    hsize_t     shared_refcount;        /* Reference count of shared attribute */
    unsigned    attr_value;     /* Attribute value */
    unsigned	big_value[SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3];   /* Data for "big" attribute */
    size_t      mesg_count;     /* # of shared messages */
    unsigned    test_shared;    /* Index over shared component type */
    unsigned    u;              /* Local index variable */
    h5_stat_size_t empty_filesize;       /* Size of empty file */
    h5_stat_size_t filesize;             /* Size of file after modifications */
    herr_t	ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Unlinking Object with Shared Attributes in Compact & Dense Storage\n"));

    /* Initialize "big" attribute data */
    HDmemset(big_value, 1, sizeof(big_value));

    /* Create dataspace for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create "big" dataspace for "large" attributes */
    big_sid = H5Screate_simple(SPACE1_RANK, big_dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Loop over type of shared components */
    for(test_shared = 0; test_shared < 3; test_shared++) {
        /* Make copy of file creation property list */
        my_fcpl = H5Pcopy(fcpl);
        CHECK(my_fcpl, FAIL, "H5Pcopy");

        /* Set up datatype for attributes */
        attr_tid = H5Tcopy(H5T_NATIVE_UINT);
        CHECK(attr_tid, FAIL, "H5Tcopy");

        /* Special setup for each type of shared components */
        if(test_shared == 0) {
            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);
        } /* end if */
        else {
            /* Set up copy of file creation property list */

            ret = H5Pset_shared_mesg_nindexes(my_fcpl, (unsigned)3);
            CHECK_I(ret, "H5Pset_shared_mesg_nindexes");

            /* Make attributes > 500 bytes shared */
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)500);

            /* Make datatypes & dataspaces > 1 byte shared (i.e. all of them :-) */
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)1, H5O_MESG_DTYPE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
            ret = H5Pset_shared_mesg_index(my_fcpl, (unsigned)2, H5O_MESG_SDSPACE_FLAG, (unsigned)1);
            CHECK_I(ret, "H5Pset_shared_mesg_index");
        } /* end else */

        /* Create file */
        fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, my_fcpl, fapl);
        CHECK(fid, FAIL, "H5Fcreate");

        /* Close FCPL copy */
        ret = H5Pclose(my_fcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Commit datatype to file */
        if(test_shared == 2) {
            ret = H5Tcommit(fid, TYPE1_NAME, attr_tid);
            CHECK(ret, FAIL, "H5Tcommit");

            /* Close attribute's datatype */
            ret = H5Tclose(attr_tid);
            CHECK(ret, FAIL, "H5Tclose");
        } /* end switch */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Get size of file */
        empty_filesize = h5_get_file_size(FILENAME);
        if(empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);


        /* Re-open file */
        fid = H5Fopen(FILENAME, H5F_ACC_RDWR, fapl);
        CHECK(fid, FAIL, "H5Fopen");

        /* Re-open attribute datatype as necessary */
        if(test_shared == 2) {
            attr_tid = H5Topen(fid, TYPE1_NAME);
            CHECK(attr_tid, FAIL, "H5Topen");
        } /* end if */

        /* Set up to query the object creation properties */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(dcpl, FAIL, "H5Pcreate");

        /* Create datasets */
        dataset = H5Dcreate(fid, DSET1_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset, FAIL, "H5Dcreate");
        dataset2 = H5Dcreate(fid, DSET2_NAME, H5T_NATIVE_UCHAR, sid, dcpl);
        CHECK(dataset2, FAIL, "H5Dcreate");

        /* Check on dataset's message storage status */
        if(test_shared != 0) {
            /* Dataset's datatypes are immutable and shouldn't be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Dataset's dataspace can be shared */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 1, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Retrieve limits for compact/dense attribute storage */
        ret = H5Pget_attr_phase_change(dcpl, &max_compact, &min_dense);
        CHECK(ret, FAIL, "H5Pget_attr_phase_change");

        /* Close property list */
        ret = H5Pclose(dcpl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Check on datasets' attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
        is_dense = H5O_is_attr_dense_test(dataset2);
        VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");

        /* Add attributes to each dataset, until after converting to dense storage */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on first dataset */
                attr = H5Acreate(dataset, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");


            /* Alternate between creating "small" & "big" attributes */
            if(u % 2) {
                /* Create "small" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");

                /* Write data into the attribute */
                attr_value = u + 1;
                ret = H5Awrite(attr, attr_tid, &attr_value);
                CHECK(ret, FAIL, "H5Awrite");
            } /* end if */
            else {
                /* Create "big" attribute on second dataset */
                attr = H5Acreate(dataset2, attrname, attr_tid, big_sid, H5P_DEFAULT);
                CHECK(attr, FAIL, "H5Acreate");

                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");

                /* Write data into the attribute */
                big_value[0] = u + 1;
                ret = H5Awrite(attr, attr_tid, big_value);
                CHECK(ret, FAIL, "H5Awrite");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 2, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");

            /* Check on dataset's attribute storage status */
            is_dense = H5O_is_attr_dense_test(dataset2);
            if(u < max_compact)
                VERIFY(is_dense, FALSE, "H5O_is_attr_dense_test");
            else
                VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");
        } /* end for */


        /* Close attribute's datatype */
        ret = H5Tclose(attr_tid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Close second dataset */
        ret = H5Dclose(dataset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Unlink second dataset */
        ret = H5Gunlink(fid, DSET2_NAME);
        CHECK(ret, FAIL, "H5Gunlink");


        /* Check on first dataset's attribute storage status */
        is_dense = H5O_is_attr_dense_test(dataset);
        VERIFY(is_dense, TRUE, "H5O_is_attr_dense_test");

        /* Check ref count on  attributes of first dataset */
        for(u = 0; u < max_compact * 2; u++) {
            /* Create attribute name */
            sprintf(attrname, "attr %02u", u);

            /* Open attribute on first dataset */
            attr = H5Aopen_name(dataset, attrname);
            CHECK(attr, FAIL, "H5Acreate");

            if(u % 2) {
                /* Check that attribute is not shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, FALSE, "H5A_is_shared_test");
            } /* end if */
            else {
                /* Check that attribute is shared */
                is_shared = H5A_is_shared_test(attr);
                VERIFY(is_shared, TRUE, "H5A_is_shared_test");

                /* Check refcount for attribute */
                ret = H5A_get_shared_rc_test(attr, &shared_refcount);
                CHECK(ret, FAIL, "H5A_get_shared_rc_test");
                VERIFY(shared_refcount, 1, "H5A_get_shared_rc_test");
            } /* end else */

            /* Close attribute */
            ret = H5Aclose(attr);
            CHECK(ret, FAIL, "H5Aclose");
        } /* end for */

        /* Close Datasets */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");

        /* Unlink first dataset */
        ret = H5Gunlink(fid, DSET1_NAME);
        CHECK(ret, FAIL, "H5Gunlink");

        /* Check on attribute storage status */
        ret = H5F_get_sohm_mesg_count_test(fid, H5O_ATTR_ID, &mesg_count);
        CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
        VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

        if(test_shared != 0) {
            /* Check on datatype storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_DTYPE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");

            /* Check on dataspace storage status */
            ret = H5F_get_sohm_mesg_count_test(fid, H5O_SDSPACE_ID, &mesg_count);
            CHECK(ret, FAIL, "H5F_get_sohm_mesg_count_test");
            VERIFY(mesg_count, 0, "H5F_get_sohm_mesg_count_test");
        } /* end if */

        /* Close file */
        ret = H5Fclose(fid);
        CHECK(ret, FAIL, "H5Fclose");

        /* Check size of file */
        filesize = h5_get_file_size(FILENAME);
        VERIFY(filesize, empty_filesize, "h5_get_file_size");
    } /* end for */

    /* Close dataspaces */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(big_sid);
    CHECK(ret, FAIL, "H5Sclose");
}   /* test_attr_shared_unlink() */

/****************************************************************
**
**  test_attr(): Main H5A (attribute) testing routine.
**
****************************************************************/
void
test_attr(void)
{
    hid_t	fapl = (-1), fapl2 = (-1);    /* File access property lists */
    hid_t	fcpl = (-1), fcpl2 = (-1);    /* File creation property lists */
    hbool_t new_format;         /* Whether to use the new format or not */
    hbool_t use_shared;         /* Whether to use shared attributes or not */
    herr_t ret;                 /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attributes\n"));

    /* Create a default file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Copy the file access property list */
    fapl2 = H5Pcopy(fapl);
    CHECK(fapl2, FAIL, "H5Pcopy");

    /* Set the "use the latest version of the format" flag for creating objects in the file */
    ret = H5Pset_latest_format(fapl2, TRUE);
    CHECK(ret, FAIL, "H5Pset_latest_format");

    /* Create a default file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    CHECK(fcpl, FAIL, "H5Pcreate");

    /* Copy the file creation property list */
    fcpl2 = H5Pcopy(fcpl);
    CHECK(fcpl2, FAIL, "H5Pcopy");

    /* Make attributes > 1 byte shared (i.e. all of them :-) */
    ret = H5Pset_shared_mesg_nindexes(fcpl2, (unsigned)1);
    CHECK_I(ret, "H5Pset_shared_mesg_nindexes");
    ret = H5Pset_shared_mesg_index(fcpl2, (unsigned)0, H5O_MESG_ATTR_FLAG, (unsigned)1);
    CHECK_I(ret, "H5Pset_shared_mesg_index");

    /* Loop over using new group format */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            MESSAGE(7, ("testing with new file format\n"));
            my_fapl = fapl2;
        } /* end if */
        else {
            MESSAGE(7, ("testing with old file format\n"));
            my_fapl = fapl;
        } /* end else */

        /* These next two tests use the same file information */
        test_attr_basic_write(my_fapl);    /* Test basic H5A writing code */
        test_attr_basic_read(my_fapl);     /* Test basic H5A reading code */

        /* These next two tests use their own file information */
        test_attr_flush(my_fapl);          /* Test H5A I/O in the presence of H5Fflush calls */
        test_attr_plist(my_fapl);          /* Test attribute property lists */

        /* These next two tests use the same file information */
        test_attr_compound_write(my_fapl);  /* Test complex datatype H5A writing code */
        test_attr_compound_read(my_fapl);   /* Test complex datatype H5A reading code */

        /* These next two tests use the same file information */
        test_attr_scalar_write(my_fapl);  /* Test scalar dataspace H5A writing code */
        test_attr_scalar_read(my_fapl);   /* Test scalar dataspace H5A reading code */

        /* These next four tests use the same file information */
        test_attr_mult_write(my_fapl);     /* Test H5A writing code for multiple attributes */
        test_attr_mult_read(my_fapl);      /* Test H5A reading code for multiple attributes */
        test_attr_iterate(my_fapl);        /* Test H5A iterator code */
        test_attr_delete(my_fapl);         /* Test H5A code for deleting attributes */

        /* This next test uses its own file information */
        test_attr_dtype_shared(my_fapl);   /* Test using shared dataypes in attributes */

        /* Tests on "new format" attribute storage */
        if(new_format == TRUE) {
            /* Loop over using shared attributes */
            for(use_shared = FALSE; use_shared <= TRUE; use_shared++) {
                hid_t my_fcpl;

                /* Set the FCPL for shared or not */
                if(use_shared) {
                    MESSAGE(7, ("testing with shared attributes\n"));
                    my_fcpl = fcpl2;
                } /* end if */
                else {
                    MESSAGE(7, ("testing without shared attributes\n"));
                    my_fcpl = fcpl;
                } /* end else */

                /* General attribute tests */
                test_attr_dense_create(my_fcpl, my_fapl);       /* Test dense attribute storage creation */
                test_attr_dense_open(my_fcpl, my_fapl);         /* Test opening attributes in dense storage */
                test_attr_dense_delete(my_fcpl, my_fapl);       /* Test deleting attributes in dense storage */
                test_attr_dense_rename(my_fcpl, my_fapl);       /* Test renaming attributes in dense storage */
                test_attr_dense_unlink(my_fcpl, my_fapl);       /* Test unlinking object with attributes in dense storage */
                test_attr_dense_limits(my_fcpl, my_fapl);       /* Test dense attribute storage limits */

                /* Attribute creation order tests */
                test_attr_corder_create_basic(my_fcpl, my_fapl);/* Test creating an object w/attribute creation order info */
/* XXX: when creation order indexing is fully working, go back and add checks
 *      to these tests to make certain that the creation order values are
 *      correct.
 */
                test_attr_corder_create_compact(my_fcpl, my_fapl);  /* Test compact attribute storage on an object w/attribute creation order info */
                test_attr_corder_create_dense(my_fcpl, my_fapl);/* Test dense attribute storage on an object w/attribute creation order info */
                test_attr_corder_transition(my_fcpl, my_fapl);  /* Test attribute storage transitions on an object w/attribute creation order info */
                test_attr_corder_delete(my_fcpl, my_fapl);      /* Test deleting object using dense storage w/attribute creation order info */

                /* New attribute API routine tests */
                test_attr_info_by_idx(new_format, my_fcpl, my_fapl);        /* Test querying attribute info by index */

                /* More complex tests with both "new format" and "shared" attributes */
                if(use_shared == TRUE) {
                    test_attr_shared_write(my_fcpl, my_fapl);   /* Test writing to shared attributes in compact & dense storage */
                    test_attr_shared_rename(my_fcpl, my_fapl);  /* Test renaming shared attributes in compact & dense storage */
                    test_attr_shared_delete(my_fcpl, my_fapl);  /* Test deleting shared attributes in compact & dense storage */
                    test_attr_shared_unlink(my_fcpl, my_fapl);  /* Test unlinking object with shared attributes in compact & dense storage */
                } /* end if */
            } /* end for */
        } /* end if */
        else {
            /* New attribute API routine tests, on old-format storage */
            test_attr_info_by_idx(new_format, fcpl, my_fapl);        /* Test querying attribute info by index */
        } /* end else */
    } /* end for */

    /* Close  FCPLs */
    ret = H5Pclose(fcpl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fcpl2);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close  FAPLs */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Pclose(fapl2);
    CHECK(ret, FAIL, "H5Pclose");
}   /* test_attr() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_attr
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_attr(void)
{
    remove(FILENAME);
}

