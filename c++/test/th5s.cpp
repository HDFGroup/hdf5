/*
 * Copyright (C) 2001 National Center for Supercomputing Applications
 *                    All rights reserved.
 *
 */

/***********************************************************
*
* Test program:	 th5s
*
* Test the dataspace functionality
*
*************************************************************/

#include <iostream>
#include "H5Cpp.h"
#include "testhdf5.h"

#ifndef H5_NO_NAMESPACE
using namespace H5;
#endif  /* !H5_NO_NAMESPACE */

#define TESTFILE   "th5s.h5"
#define FILE   "th5s1.h5"

/* 3-D dataset with fixed dimensions */
#define SPACE1_NAME  "Space1"
#define SPACE1_RANK	3
#define SPACE1_DIM1	3
#define SPACE1_DIM2	15
#define SPACE1_DIM3	13

/* 4-D dataset with one unlimited dimension */
#define SPACE2_NAME  "Space2"
#define SPACE2_RANK	4
#define SPACE2_DIM1	0
#define SPACE2_DIM2	15
#define SPACE2_DIM3	13
#define SPACE2_DIM4	23
#define SPACE2_MAX1	H5S_UNLIMITED
#define SPACE2_MAX2	15
#define SPACE2_MAX3	13
#define SPACE2_MAX4	23

/* Scalar dataset with simple datatype */
#define SPACE3_NAME  "Scalar1"
#define SPACE3_RANK	0
unsigned space3_data=65;

/* Scalar dataset with compound datatype */
#define SPACE4_NAME  "Scalar2"
#define SPACE4_RANK	0
#define SPACE4_FIELDNAME1	"c1"
#define SPACE4_FIELDNAME2	"u"
#define SPACE4_FIELDNAME3	"f"
#define SPACE4_FIELDNAME4	"c2"
size_t space4_field1_off=0;
size_t space4_field2_off=0;
size_t space4_field3_off=0;
size_t space4_field4_off=0;
struct space4_struct {
    char c1;
    unsigned u;
    float f;
    char c2;
 } space4_data={'v',987123,-3.14,'g'}; /* Test data for 4th dataspace */

/****************************************************************
**
**  test_h5s_basic(): Test basic H5S (dataspace) code.
** 
****************************************************************/
static void 
test_h5s_basic(void)
{
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2, SPACE2_DIM3,
				   SPACE2_DIM4};
    hsize_t		dims3[H5S_MAX_RANK+1];
    hsize_t		tmax[4];

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace Manipulation\n"));

    try 
    { // beginning of first try block

	/* Create file - removed this since the following operations don't
	need the file to be opened */

	// Create simple dataspace sid1
	DataSpace sid1 (SPACE1_RANK, dims1 );

	// Get simple extent npoints of the dataspace sid1 and verify it
	hssize_t	n;	 	/* Number of dataspace elements */
	n = sid1.getSimpleExtentNpoints();
	VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3,
	   "H5Sget_simple_extent_npoints");

	// Get the logical rank of dataspace sid1 and verify it
	int	rank;		/* Logical rank of dataspace	*/
	rank = sid1.getSimpleExtentNdims();
	VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");

	// Retrieves dimension size of dataspace sid1 and verify it
	int ndims;		/* Number of dimensions		*/
	hsize_t	tdims[4];	/* Dimension array to test with */
	ndims = sid1.getSimpleExtentDims( tdims );
	VERIFY(HDmemcmp(tdims, dims1, SPACE1_RANK * sizeof(unsigned)), 0,
	   "H5Sget_simple_extent_dims");

	// Create simple dataspace sid2
	hsize_t	max2[] = {SPACE2_MAX1, SPACE2_MAX2, SPACE2_MAX3, SPACE2_MAX4};
	DataSpace sid2 (SPACE2_RANK, dims2, max2);

	// Get simple extent npoints of dataspace sid2 and verify it
	n = sid2.getSimpleExtentNpoints();
	VERIFY(n, SPACE2_DIM1 * SPACE2_DIM2 * SPACE2_DIM3 * SPACE2_DIM4,
	   "H5Sget_simple_extent_npoints");

	// Get the logical rank of dataspace sid2 and verify it
	rank = sid2.getSimpleExtentNdims();
	VERIFY(rank, SPACE2_RANK, "H5Sget_simple_extent_ndims");

	// Retrieves dimension size and max size of dataspace sid2 and 
	// verify them
	ndims = sid2.getSimpleExtentDims( tdims, tmax );
	VERIFY(HDmemcmp(tdims, dims2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "H5Sget_simple_extent_dims");
	VERIFY(HDmemcmp(tmax, max2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "H5Sget_simple_extent_dims");
    }	// end of first try block
    catch( DataSpaceIException error ) 
    {
        CHECK(FAIL, FAIL, error.getCFuncName()); 
    }

    /*
    * Check to be sure we can't create a simple data space that has too many
    * dimensions.
    */
    try { 
	DataSpace manydims_ds(H5S_MAX_RANK+1, dims3, NULL);

	// Should FAIL but didn't - BMR (Note 1): a new macro that skips 
	// the comparison b/w the 1st & 2nd args would be more appropriate, 
	// but VERIFY will still do - Mar 12, 01
	VERIFY(manydims_ds.getId(), FAIL, "DataSpace constructor");
    }
    catch( DataSpaceIException error ) {} // do nothing, FAIL expected

    /*
     * Try reading a file that has been prepared that has a dataset with a
     * higher dimensionality than what the library can handle.
     *
     * If this test fails and the H5S_MAX_RANK variable has changed, follow
     * the instructions in space_overflow.c for regenating the th5s.h5 file.
     */
    //{
    char testfile[512]="";
    char *srcdir = getenv("srcdir");
    if (srcdir && ((strlen(srcdir) + strlen(TESTFILE) + 1) < sizeof(testfile))){
	strcpy(testfile, srcdir);
	strcat(testfile, "/");
    }
    strcat(testfile, TESTFILE);
    try {  // try block for testing higher dimensionality

	// Create file
	H5File fid1(testfile, H5F_ACC_RDONLY);

	// Try to open the dataset that has higher dimensionality than 
	// what the library can handle and this operation should fail.
	try {
	    DataSet dset1 = fid1.openDataSet( "dset" ); 
	    VERIFY( dset1.getId(), FAIL, "H5File::openDataSet");
	}
	catch( FileIException error ) { } // do nothing, FAIL expected
    }	// end of try block for testing higher dimensionality

    // catch exception thrown by H5File constructor
    catch( FileIException error ) {
    	CHECK_I(FAIL, error.getCFuncName());
	cout << "***cannot open the pre-created H5S_MAX_RANK test file" <<
	    testfile << endl;
    }

    // CHECK_I(ret, "H5Fclose");  // leave this here, later, fake a failure
			// in the p_close see how this will handle it. ???

    /* Verify that incorrect dimensions don't work */
    dims1[0] = 0;
    try {
	DataSpace wrongdim_ds (SPACE1_RANK, dims1); 
	VERIFY(wrongdim_ds.getId(), FAIL, "DataSpace constructor");
    }
    catch( DataSpaceIException error ) {} // do nothing; FAIL expected 

    // Create a simple dataspace
    DataSpace sid3 (H5S_SIMPLE);

    // Attempts to use incorrect dimensions, should fail
    try { sid3.setExtentSimple( SPACE1_RANK, dims1 ); }
    catch( DataSpaceIException error )
    {
	// ret value is already < 0 for an exception to be thrown;
	// also see Note 1 above
	VERIFY(FAIL, FAIL, error.getCFuncName()); 
    }
}				/* test_h5s_basic() */

/****************************************************************
**
**  test_h5s_scalar_write(): Test scalar H5S (dataspace) writing code.
** 
****************************************************************/
static void 
test_h5s_scalar_write(void)
{

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Writing\n"));

    try
    {
	// Create file
	H5File fid1(FILE, H5F_ACC_TRUNC);
	//    CHECK(fid1, FAIL, "H5Fcreate");

	/* Create scalar dataspace */
	//sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
	DataSpace sid1(SPACE3_RANK, NULL);

	//n = H5Sget_simple_extent_npoints(sid1);
	hssize_t	n;	 	/* Number of dataspace elements */
	n = sid1.getSimpleExtentNpoints();
	VERIFY(n, 1, "DataSpace::getSimpleExtentNpoints");
	//VERIFY(n, 1, "H5Sget_simple_extent_npoints");

	int	rank;		/* Logical rank of dataspace	*/
	rank = sid1.getSimpleExtentNdims();
	//VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");
	VERIFY(rank, SPACE3_RANK, "DataSpace::getSimpleExtentNdims");

	// Retrieves dimension size of dataspace sid1 and verify it
	int ndims;		/* Number of dimensions		*/
	hsize_t	tdims[4];	/* Dimension array to test with */
	ndims = sid1.getSimpleExtentDims( tdims );
	//VERIFY(ndims, 0, "H5Sget_simple_extent_dims");
	VERIFY(ndims, 0, "DataSpace::getSimpleExtentDims");

	//    rank = H5Sget_simple_extent_ndims(sid1);
	//VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

	//    ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
	//    VERIFY(ret, 0, "H5Sget_simple_extent_dims");

	/* Verify extent type */
	//ext_type = H5Sget_simple_extent_type(sid1);
	//VERIFY(ext_type, H5S_SCALAR, "H5Sget_simple_extent_type");
	H5S_class_t ext_type;   /* Extent type */
	ext_type = sid1.getSimpleExtentType();
	VERIFY(ext_type, H5S_SCALAR, "DataSpace::getSimpleExtentType");

	/* Create a dataset */
	DataSet dataset = fid1.createDataSet("Dataset1", PredType::NATIVE_UINT,sid1);

	dataset.write(&space3_data, PredType::NATIVE_UINT);
    } // end of try block
    catch (Exception error)
    {
	CHECK(FAIL, FAIL, error.getCFuncName());
    }
}				/* test_h5s_scalar_write() */

/****************************************************************
**
**  test_h5s_scalar_read(): Test scalar H5S (dataspace) reading code.
** 
****************************************************************/
static void 
test_h5s_scalar_read(void)
{
    hsize_t		tdims[4];	/* Dimension array to test with */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Reading\n"));

    try
    {
	/* Create file */
	H5File fid1(FILE, H5F_ACC_RDWR);
	//CHECK(fid1, FAIL, "H5Fopen");

	/* Create a dataset */
	DataSet dataset = fid1.openDataSet("Dataset1");
	//CHECK(dataset, FAIL, "H5Dopen");

	DataSpace sid1 = dataset.getSpace();
	//CHECK(sid1, FAIL, "H5Dget_space");

	// Get the number of dataspace elements
	hssize_t n = sid1.getSimpleExtentNpoints();
	//hssize_t n = H5Sget_simple_extent_npoints(sid1);
	//CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
	VERIFY(n, 1, "H5Sget_simple_extent_npoints");

	// Get the logical rank of the dataspace
	int ndims = sid1.getSimpleExtentNdims();
	//CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
	VERIFY(ndims, SPACE3_RANK, "H5Sget_simple_extent_ndims");

	ndims = sid1.getSimpleExtentDims(tdims);
	//ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
	VERIFY(ndims, 0, "H5Sget_simple_extent_dims");

	unsigned      	rdata;      	/* Scalar data read in 		*/
	dataset.read(&rdata, PredType::NATIVE_UINT);
	//CHECK(ret, FAIL, "H5Dread");
	VERIFY(rdata, space3_data, "H5Dread");
    }   // end of try block
    catch (Exception error)
    {
	// all the exceptions caused by negative returned values by C APIs
	CHECK(FAIL, FAIL, error.getCFuncName());
    }
    
}				/* test_h5s_scalar_read() */

/****************************************************************
**
**  test_h5s_compound_scalar_write(): Test scalar H5S (dataspace) writing for
**          compound datatypes.
** 
****************************************************************/
static void 
test_h5s_compound_scalar_write(void)
{

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Compound Dataspace Writing\n"));

    try
    {
	/* Create file */
	//fid1 = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	H5File fid1(FILE, H5F_ACC_TRUNC);
	//CHECK(fid1, FAIL, "H5Fcreate");

	/* Create the compound datatype.  */
	CompType tid1(sizeof(struct space4_struct));
	//tid1 = H5Tcreate (H5T_COMPOUND, sizeof(struct space4_struct));
	//CHECK(tid1, FAIL, "H5Tcreate");
	space4_field1_off=HOFFSET(struct space4_struct, c1);
	//ret = H5Tinsert(tid1, SPACE4_FIELDNAME1, space4_field1_off,
		    //H5T_NATIVE_SCHAR);
	tid1.insertMember(SPACE4_FIELDNAME1, space4_field1_off,
		    PredType::NATIVE_SCHAR);
	//CHECK(ret, FAIL, "H5Tinsert");
	space4_field2_off=HOFFSET(struct space4_struct, u);
	//ret = H5Tinsert(tid1, SPACE4_FIELDNAME2, space4_field2_off,
	//	    H5T_NATIVE_UINT);
	tid1.insertMember(SPACE4_FIELDNAME2, space4_field2_off,
		    PredType::NATIVE_UINT);
	//CHECK(ret, FAIL, "H5Tinsert");
	space4_field3_off=HOFFSET(struct space4_struct, f);
	tid1.insertMember(SPACE4_FIELDNAME3, space4_field3_off,
		    PredType::NATIVE_FLOAT);
	//CHECK(ret, FAIL, "H5Tinsert");
	space4_field4_off=HOFFSET(struct space4_struct, c2);
	tid1.insertMember(SPACE4_FIELDNAME4, space4_field4_off,
		    PredType::NATIVE_SCHAR);
	//CHECK(ret, FAIL, "H5Tinsert");

	/* Create scalar dataspace */
	DataSpace sid1(SPACE3_RANK, NULL);
	//sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
	//CHECK(sid1, FAIL, "H5Screate_simple");

	// Get the number of dataspace elements
	hssize_t n = sid1.getSimpleExtentNpoints();
	//hssize_t n = H5Sget_simple_extent_npoints(sid1);
	//CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
	VERIFY(n, 1, "H5Sget_simple_extent_npoints");

	// Get the logical rank of the dataspace
	int ndims = sid1.getSimpleExtentNdims();
	//CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
	VERIFY(ndims, SPACE3_RANK, "H5Sget_simple_extent_ndims");

	hsize_t		tdims[4];	/* Dimension array to test with */
	ndims = sid1.getSimpleExtentDims(tdims);
	//ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
	VERIFY(ndims, 0, "H5Sget_simple_extent_dims");

	/* Create a dataset */
	DataSet dataset = fid1.createDataSet("Dataset1", tid1, sid1);
	//dataset=H5Dcreate(fid1,"Dataset1",tid1,sid1,H5P_DEFAULT);
	//CHECK(dataset, FAIL, "H5Dcreate");

	dataset.write(&space4_data, tid1);
	//ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &space4_data);
	//CHECK(ret, FAIL, "H5Dwrite");
    }	// end of try block
    catch (Exception error)
    {
	// all the exceptions caused by negative returned values by C APIs
	CHECK(FAIL, FAIL, error.getCFuncName());
    }

}				/* test_h5s_compound_scalar_write() */

/****************************************************************
**
**  test_h5s_compound_scalar_read(): Test scalar H5S (dataspace) reading for
**          compound datatypes.
** 
****************************************************************/
static void 
test_h5s_compound_scalar_read(void)
{
    hid_t		fid1;		/* HDF5 File IDs		*/
    hid_t		dataset;	/* Dataset ID			*/
    hid_t		sid1;	    	/* Dataspace ID			*/
    hid_t       	type;       	/* Datatype             	*/
    unsigned		rank;		/* Logical rank of dataspace	*/
    hsize_t		tdims[4];	/* Dimension array to test with */
    size_t		n;	 	/* Number of dataspace elements */
    herr_t		ret;		/* Generic return value		*/

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Compound Dataspace Reading\n"));
    try
    {
	/* Create file */
	H5File fid1(FILE, H5F_ACC_RDWR);
	//CHECK(fid1, FAIL, "H5Fopen");

	/* Create a dataset */
	DataSet dataset = fid1.openDataSet("Dataset1");
	//CHECK(dataset, FAIL, "H5Dopen");

	DataSpace sid1 = dataset.getSpace();
	//CHECK(sid1, FAIL, "H5Dget_space");

	// Get the number of dataspace elements
	hssize_t n = sid1.getSimpleExtentNpoints();
	//hssize_t n = H5Sget_simple_extent_npoints(sid1);
	//CHECK(n, UFAIL, "H5Sget_simple_extent_npoints");
	VERIFY(n, 1, "H5Sget_simple_extent_npoints");

	// Get the logical rank of the dataspace
	int ndims = sid1.getSimpleExtentNdims();
	//CHECK(rank, UFAIL, "H5Sget_simple_extent_ndims");
	VERIFY(ndims, SPACE3_RANK, "H5Sget_simple_extent_ndims");

	ndims = sid1.getSimpleExtentDims(tdims);
	//ret = H5Sget_simple_extent_dims(sid1, tdims, NULL);
	VERIFY(ndims, 0, "H5Sget_simple_extent_dims");

	// Get the datatype of this dataset.
	CompType type(dataset);
    //type=H5Dget_type(dataset);
    //CHECK(type, FAIL, "H5Dget_type");
     
    struct space4_struct rdata; 	/* Scalar data read in 		*/
	dataset.read(&rdata, type);
	//CHECK(ret, FAIL, "H5Dread");
    //ret = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);

	// Verify read data
	if(HDmemcmp(&space4_data,&rdata,sizeof(struct space4_struct)))
	{
            cout << "scalar data different: space4_data.c1=" 
		<< space4_data.c1 << ", read_data4.c1=" << rdata.c1 << endl;
        //printf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n",space4_data.c1,rdata.c1);
            cout << "scalar data different: space4_data.u="
		<< space4_data.u << ", read_data4.u=" << rdata.u << endl;
        //printf("scalar data different: space4_data.u=%u, read_data4.u=%u\n",space4_data.u,rdata.u);
            cout << "scalar data different: space4_data.f="
		<< space4_data.f << ", read_data4.f=" << rdata.f << endl;
        //printf("scalar data different: space4_data.f=%f, read_data4.f=%f\n",space4_data.f,rdata.f);
            cout << "scalar data different: space4_data.c1="
		<< space4_data.c1 << ", read_data4.c1=" << rdata.c2 << endl;
        //printf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n",space4_data.c1,rdata.c2);
        num_errs++;
	} /* end if */
    }   // end of try block
    catch (Exception error)
    {
	// all the exceptions caused by negative returned values by C APIs
	CHECK(FAIL, FAIL, error.getCFuncName());
    }
}				/* test_h5s_compound_scalar_read() */

/****************************************************************
**
**  test_h5s(): Main H5S (dataspace) testing routine.
** 
****************************************************************/
void 
test_h5s(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspaces\n"));

    test_h5s_basic();		/* Test basic H5S code */
    test_h5s_scalar_write();	/* Test scalar H5S writing code */
    test_h5s_scalar_read();		/* Test scalar H5S reading code */
    test_h5s_compound_scalar_write();	/* Test compound datatype scalar H5S writing code */
    test_h5s_compound_scalar_read();	/* Test compound datatype scalar H5S reading code */
} /* test_h5s() */


/*-------------------------------------------------------------------------
 * Function:	cleanup_h5s
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
cleanup_h5s(void)
{
    remove(FILE);
}

