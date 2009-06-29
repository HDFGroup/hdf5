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

#define FILENAME   "tattr.h5"
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
test_attr_basic_write(void)
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
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
    attr_name_size = H5Aget_name(attr, 0, NULL);
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
    attr_name_size = H5Aget_name(attr2, 0, NULL);
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
            TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

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
    group = H5Gcreate(fid1, GROUP1_NAME, 0);
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
test_attr_basic_read(void)
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
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset=H5Dopen(fid1,DSET1_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(dataset);
    VERIFY(ret, 2, "H5Aget_num_attrs");

    /* Open an attribute for the dataset */
    attr = H5Aopen_name(dataset, ATTR_TMP_NAME);
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
    group=H5Gopen(fid1,GROUP1_NAME);

    /* Verify the correct number of attributes */
    ret=H5Aget_num_attrs(group);
    VERIFY(ret, 1, "H5Aget_num_attrs");

    /* Open an attribute for the dataset */
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
test_attr_flush(void)
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

    fil = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
}   /* test_attr_basic_flush() */

/****************************************************************
**
**  test_attr_compound_write(): Test H5A (attribute) code.
**      Tests compound datatype attributes
**
****************************************************************/
static void
test_attr_compound_write(void)
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
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
test_attr_compound_read(void)
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
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
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
    name_len=H5Aget_name(attr,ATTR_NAME_LEN, attr_name);
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
test_attr_scalar_write(void)
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
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
test_attr_scalar_read(void)
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
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
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
test_attr_mult_write(void)
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
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
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
test_attr_mult_read(void)
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
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
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
    name_len=H5Aget_name(attr, ATTR_NAME_LEN, attr_name);
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
    name_len=H5Aget_name(attr,ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR2_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR2_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR2_NAME);

    /* Verify Name with too small of a buffer */
    name_len=H5Aget_name(attr,HDstrlen(ATTR2_NAME), attr_name);
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
    name_len=H5Aget_name(attr,ATTR_NAME_LEN, attr_name);
    VERIFY(name_len, HDstrlen(ATTR3_NAME), "H5Aget_name");
    if(HDstrcmp(attr_name,ATTR3_NAME))
        TestErrPrintf("attribute name different: attr_name=%s, should be %s\n",attr_name,ATTR3_NAME);

    /* Verify Name with too small of a buffer */
    name_len=H5Aget_name(attr,HDstrlen(ATTR3_NAME), attr_name);
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
test_attr_iterate(void)
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
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
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
    start=0;
    count=0;
    ret = H5Aiterate(dataset,&start,attr_op1,&count);
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
test_attr_delete(void)
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
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
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
    name_len=H5Aget_name(attr,ATTR_NAME_LEN,attr_name);
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
    name_len=H5Aget_name(attr,ATTR_NAME_LEN, attr_name);
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
    name_len=H5Aget_name(attr,ATTR_NAME_LEN, attr_name);
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
**  test_attr_compat(): Test H5A (attribute) version
**                      compatibility macros.
**
****************************************************************/
static void
test_attr_compat(void)
{
    hid_t   file;		/* HDF5 File ID 		*/
    hid_t   dataset;	/* Dataset ID			*/
    hid_t   sid;	/* Dataspace ID			*/
    hid_t   attr;       /* Attribute ID             */
    H5A_operator1_t op; /* Operator function; test H5A_operator1_1 */
    unsigned start;     /* Starting attribute to look up */
    int     count;      /* operator data for the iterator */
    herr_t  ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Basic Attribute Functions\n"));

    /* Open file */
    file = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(file, FAIL, "H5Fopen");

    /* Open existing dataset */
    dataset=H5Dopen(file,DSET2_NAME);
    CHECK(dataset, FAIL, "H5Dopen");

    /* Create dataspace for attribute */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, FAIL, "H5Screate");

    /* Create attribute for the dataset (test H5Acreate1) */
    attr=H5Acreate1(dataset,ATTR1_NAME,H5T_NATIVE_INT,sid,H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate1");

    /* Close attribute */
    ret=H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close attribute's dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Iterate over attributes on dataset (test H5Aiterate1) */
    start=0;
    count=0;
    op = &attr_op1;
    ret = H5Aiterate1(dataset,&start,op,&count);
    VERIFY(ret, 0, "H5Aiterate1");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
}   /* test_attr_iterate() */

/****************************************************************
**
**  test_attr_empty_read(): Test H5A (attribute) code for reading
**                          an empty attribute for bug #1513.
**                          v1.6 library works correctly because
**                          fill value is written to the attribute
**                          during H5A_close if no data is written.
**                          v1.8 and later leaves it empty.
****************************************************************/
test_attr_empty_read(void)
{
    hid_t   fid;            /* File ID */
    hid_t   gid;            /* Group ID */
    hid_t   aid1, aid2;     /* Attribute IDs */
    hid_t   sid;            /* Dataspace ID */
    hsize_t dims[ATTR1_RANK] = {ATTR1_DIM1};  /* Attribute dimensions */
    int     intar[ATTR1_DIM1];       /* Data reading buffer */
    herr_t  ret;            /* Generic return status */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing that empty attribute can be read\n"));

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Open root group */
    gid = H5Gopen(fid, "/");
    CHECK(gid, FAIL, "H5Gopen");

    /* Create dataspace */
    sid = H5Screate_simple(1, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create attribute on group */
    aid1 = H5Acreate(gid, ATTR1_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    CHECK(aid1, FAIL, "H5Acreate2");

    ret = H5Aclose(aid1);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open the attribute again */
    aid2 = H5Aopen_name(gid, ATTR1_NAME);
    CHECK(aid2, FAIL, "H5Aopen_name");

    ret = H5Aread(aid2, H5T_NATIVE_INT, intar);
    CHECK(ret, FAIL, "H5Aread");

    /* Close IDs */
    ret = H5Aclose(aid2);
    CHECK(ret, FAIL, "H5Aclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
}

/****************************************************************
**
**  test_attr_dtype_shared(): Test H5A (attribute) code for using
**                              shared datatypes in attributes.
**
****************************************************************/
static void
test_attr_dtype_shared(void)
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
    file_id=H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(file_id, FAIL, "H5Fopen");

    /* Close file */
    ret=H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Get size of file */
    empty_filesize=h5_get_file_size(FILENAME);
    if(empty_filesize<0)
        TestErrPrintf("Line %d: file size wrong!\n",__LINE__);

    /* Re-open file */
    file_id=H5Fopen(FILENAME,H5F_ACC_RDWR,H5P_DEFAULT);
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
    file_id=H5Fopen(FILENAME,H5F_ACC_RDWR,H5P_DEFAULT);
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
    VERIFY(statbuf.nlink, 3, "H5Aopen_name");

    /* Unlink the dataset */
    ret=H5Gunlink(file_id,DSET1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Check reference count on named datatype */
    ret=H5Gget_objinfo(file_id,TYPE1_NAME,0,&statbuf);
    CHECK(ret, FAIL, "H5Gget_objinfo");
    VERIFY(statbuf.nlink, 1, "H5Gunlink");

    /* Unlink the named datatype */
    ret=H5Gunlink(file_id,TYPE1_NAME);
    CHECK(ret, FAIL, "H5Gunlink");

    /* Close file */
    ret=H5Fclose(file_id);
    CHECK(ret, FAIL, "H5Fclose");

    /* Check size of file */
    filesize=h5_get_file_size(FILENAME);
    VERIFY(filesize, empty_filesize, "H5Fclose");
}   /* test_attr_dtype_shared() */

/****************************************************************
**
**  test_attr(): Main H5A (attribute) testing routine.
**
****************************************************************/
void
test_attr(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attributes\n"));

    /* These next two tests use the same file information */
    test_attr_basic_write();    /* Test basic H5A writing code */
    test_attr_basic_read();     /* Test basic H5A reading code */
    test_attr_flush();          /* Test H5A I/O in the presence of H5Fflush calls */

    /* These next two tests use the same file information */
    test_attr_compound_write();  /* Test complex datatype H5A writing code */
    test_attr_compound_read();   /* Test complex datatype H5A reading code */

    /* These next two tests use the same file information */
    test_attr_scalar_write();  /* Test scalar dataspace H5A writing code */
    test_attr_scalar_read();   /* Test scalar dataspace H5A reading code */

    test_attr_empty_read();     /* Test H5A reading code for an empty attribute */

    /* These next five tests use the same file information */
    test_attr_mult_write();     /* Test H5A writing code for multiple attributes */
    test_attr_mult_read();      /* Test H5A reading code for multiple attributes */
    test_attr_iterate();        /* Test H5A iterator code */
    test_attr_delete();         /* Test H5A code for deleting attributes */
    test_attr_compat();         /* Test H5A version compatibility macros */

    /* This next test use the same file information */
    test_attr_dtype_shared();   /* Test using shared dataypes in attributes */
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

