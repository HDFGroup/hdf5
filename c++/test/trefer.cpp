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

/*****************************************************************************
   FILE
   trefer.cpp - HDF5 C++ testing the functionalities associated with the C
		Reference interface (H5R)

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#include "H5Cpp.h"      // C++ API header file
using namespace H5;

#include "h5cpputil.h"  // C++ utilility header file

// File names
const H5std_string      FILE1("trefer1.h5");
const H5std_string      FILE2("trefer2.h5");

// Dataset and datatype names
const H5std_string      DSET1_NAME("Dataset1");
const H5std_string      DSET2_NAME("Dataset2");
const H5std_string      DSET3_NAME("Dataset3");
const H5std_string      DTYPE_NAME("Datatype1");

// Compound type member names
const H5std_string MEMBER1( "a_name" );
const H5std_string MEMBER2( "b_name" );
const H5std_string MEMBER3( "c_name" );

// 1-D dataset with fixed dimensions
const int SPACE1_RANK = 1;
const int SPACE1_DIM1 = 4;

/* Larger 1-D dataset with fixed dimensions */
const int SPACE3_RANK = 1;
const int SPACE3_DIM1 = 100;

/* Element selection information */
const int POINT1_NPOINTS = 10;

// Compound datatype
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

/****************************************************************
**
**  test_reference_params(): Test basic H5R (reference) parameters
**                           for correct processing
**
****************************************************************/
static void
test_reference_params(void)
{
    const char *write_comment = "Foo!"; /* Comments for group */

    // Output message about test being performed
    SUBTEST("Object Reference Parameters");

    H5File* file1 = NULL;
    try {
	hobj_ref_t *wbuf,      // buffer to write to disk
		   *rbuf,      // buffer read from disk
		   *tbuf;      // temp. buffer read from disk

	// Allocate write & read buffers
	int temp_size = MAX(sizeof(unsigned),sizeof(hobj_ref_t));
	wbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);
	rbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);
	tbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);

        // Create file FILE1
        file1 = new H5File (FILE1, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	hsize_t	dims1[] = {SPACE1_DIM1};
	DataSpace sid1(SPACE1_RANK, dims1);

	// Create a group
	Group group = file1->createGroup("Group1");

	// Set group's comment
 	group.setComment(".", write_comment);

	// Create a dataset (inside /Group1)
	DataSet dataset = group.createDataSet(DSET1_NAME, PredType::NATIVE_UINT, sid1);

	unsigned *tu32;      // Temporary pointer to uint32 data
	int i;
	for (tu32=(unsigned *)wbuf, i=0; i<SPACE1_DIM1; i++)
	    *tu32++=i*3; // from C test

	// Write selection to disk
	dataset.write(wbuf, PredType::NATIVE_UINT);

	// Close Dataset
	dataset.close();

	// Create another dataset (inside /Group1)
	dataset = group.createDataSet(DSET2_NAME, PredType::NATIVE_UCHAR, sid1);

	// Close Dataset
	dataset.close();

	// Create a datatype to refer to
	CompType dtype1(sizeof(s1_t));

	// Insert fields
	dtype1.insertMember(MEMBER1, HOFFSET(s1_t, a), PredType::NATIVE_INT);
	dtype1.insertMember(MEMBER2, HOFFSET(s1_t, b), PredType::NATIVE_INT);
	dtype1.insertMember(MEMBER3, HOFFSET(s1_t, c), PredType::NATIVE_FLOAT);

	// Save datatype for later
	dtype1.commit(group, DTYPE_NAME);

	// Close datatype and group
	dtype1.close();
	group.close();

	// Create a dataset
	dataset = file1->createDataSet(DSET3_NAME, PredType::STD_REF_OBJ, sid1);

	/* Test parameters to H5Location::reference */
	try {
	    file1->reference(NULL, "/Group1/Dataset1");
	} catch (ReferenceException& E) {} // We expect this to fail
	try {
	    file1->reference(&wbuf[0], NULL);
	} catch (ReferenceException& E) {} // We expect this to fail
	try {
	    file1->reference(&wbuf[0], "");
	} catch (ReferenceException& E) {} // We expect this to fail
	try {
	    file1->reference(&wbuf[0], "/Group1/Dataset1", H5R_MAXTYPE);
	} catch (ReferenceException& E) {} // We expect this to fail
	try {
	    file1->reference(&wbuf[0], "/Group1/Dataset1", H5R_DATASET_REGION);
	} catch (ReferenceException& E) {} // We expect this to fail

	// Close resources
	dataset.close();
	file1->close();
	// Let sid1 go out of scope

	// Free memory buffers
	HDfree(wbuf);
	HDfree(rbuf);
	HDfree(tbuf);

	PASSED();
    } // end try
    catch (Exception& E)
    {
	issue_fail_msg("test_reference_param()",__LINE__,__FILE__,
			E.getCFuncName(), E.getCDetailMsg());
    }

    if(file1)
        delete file1;
}   /* test_reference_param() */

/****************************************************************
**
**  test_reference_obj(): Test basic object reference functions
**			  to various kinds of objects
**
****************************************************************/
static void test_reference_obj(void)
{
    int    i;          // counting variables
    const  H5std_string write_comment="Foo!"; // Comments for group

    // Output message about test being performed
    SUBTEST("Object Reference Functions");

    H5File* file1 = NULL;
    try {
	hobj_ref_t *wbuf,      // buffer to write to disk
		   *rbuf,      // buffer read from disk
		   *tbuf;      // temp. buffer read from disk

	// Allocate write & read buffers
	int temp_size = MAX(sizeof(unsigned),sizeof(hobj_ref_t));
	wbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);
	rbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);
	tbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);

        // Create file FILE1
        file1 = new H5File (FILE1, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	hsize_t	dims1[] = {SPACE1_DIM1};
	DataSpace sid1(SPACE1_RANK, dims1);

	// Create a group
	Group group = file1->createGroup("Group1");

	// Set group's comment
	group.setComment(".", write_comment);

	// Create a dataset (inside /Group1)
	DataSet dataset = group.createDataSet(DSET1_NAME, PredType::NATIVE_UINT, sid1);

	unsigned *tu32;      // Temporary pointer to uint32 data
	for (tu32 = (unsigned *)wbuf, i = 0; i < SPACE1_DIM1; i++)
	    *tu32++=i*3; // from C test

	// Write selection to disk
	dataset.write(wbuf, PredType::NATIVE_UINT);

	// Close Dataset
	dataset.close();

	// Create another dataset (inside /Group1)
	dataset = group.createDataSet(DSET2_NAME, PredType::NATIVE_UCHAR, sid1);

	// Close Dataset
	dataset.close();

	// Create a datatype to refer to
	CompType dtype1(sizeof(s1_t));

	// Insert fields
	dtype1.insertMember(MEMBER1, HOFFSET(s1_t, a), PredType::NATIVE_INT);
	dtype1.insertMember(MEMBER2, HOFFSET(s1_t, b), PredType::NATIVE_INT);
	dtype1.insertMember(MEMBER3, HOFFSET(s1_t, c), PredType::NATIVE_FLOAT);

	// Save datatype for later
	dtype1.commit(group, DTYPE_NAME);

	// Close datatype and group
	dtype1.close();
	group.close();

	// Create a dataset
	dataset = file1->createDataSet(DSET3_NAME, PredType::STD_REF_OBJ, sid1);

	// Create reference to dataset and test getRefObjType
	file1->reference(&wbuf[0], "/Group1/Dataset1");
	H5O_type_t refobj_type = dataset.getRefObjType(&wbuf[0], H5R_OBJECT);
	verify_val(refobj_type, H5O_TYPE_DATASET, "DataSet::getRefObjType",__LINE__,__FILE__);

	// Create reference to dataset and test getRefObjType
	file1->reference(&wbuf[1], "/Group1/Dataset2");
	refobj_type = dataset.getRefObjType(&wbuf[1], H5R_OBJECT);
	verify_val(refobj_type, H5O_TYPE_DATASET, "DataSet::getRefObjType",__LINE__,__FILE__);

	// Create reference to group
	file1->reference(&wbuf[2], "/Group1");
	refobj_type = dataset.getRefObjType(&wbuf[2], H5R_OBJECT);
	verify_val(refobj_type, H5O_TYPE_GROUP, "DataSet::getRefObjType",__LINE__,__FILE__);

	// Create reference to named datatype
	file1->reference(&wbuf[3], "/Group1/Datatype1");
	refobj_type = dataset.getRefObjType(&wbuf[3], H5R_OBJECT);
	verify_val(refobj_type, H5O_TYPE_NAMED_DATATYPE, "DataSet::getRefObjType",__LINE__,__FILE__);

	// Write selection to disk
	dataset.write(wbuf, PredType::STD_REF_OBJ);

	// Close disk dataspace, dataset, and file
	sid1.close();
	dataset.close();
	delete file1;

	// Re-open the file
	file1 = new H5File(FILE1, H5F_ACC_RDWR);

	// Open the dataset
	dataset = file1->openDataSet(DSET3_NAME);

	// Read selection from disk
	dataset.read(rbuf, PredType::STD_REF_OBJ);

	// Dereference dataset object by ctor, from the location where
	// 'dataset' is located
	DataSet dset2(dataset, &rbuf[0], H5R_OBJECT);

	// Check information in the referenced dataset
	sid1 = dset2.getSpace();
	hssize_t n_elements = sid1.getSimpleExtentNpoints();
	verify_val((long)n_elements, 4, "DataSpace::getSimpleExtentNpoints",__LINE__,__FILE__);

	// Read from disk
	dset2.read(tbuf, PredType::NATIVE_UINT);

	for(tu32 = (unsigned *)tbuf, i = 0; i < SPACE1_DIM1; i++, tu32++)
	   verify_val(*tu32, (uint32_t)(i*3), "DataSpace::getSimpleExtentNpoints",__LINE__,__FILE__);

	// Close dereferenced dataset
	dset2.close();

	// Dereference group object from the location where 'dataset' is located
	group.dereference(dataset, &rbuf[2]);

	// Get group's comment using
	// H5std_string getComment(const char* name, <buf_size=0 by default>)
	H5std_string read_comment1 = group.getComment(".", 10);
	verify_val(read_comment1.c_str(), write_comment, "Group::getComment",__LINE__,__FILE__);

	// Test with the old default value
	read_comment1 = group.getComment(".", 256);
	verify_val(read_comment1.c_str(), write_comment, "Group::getComment",__LINE__,__FILE__);

	// Test that getComment handles failures gracefully, using
	// H5std_string getComment(const char* name, <buf_size=0 by default>)
	try {
	    H5std_string read_comment_tmp = group.getComment(NULL);
	}
	catch (Exception& E) {} // We expect this to fail

	// Close group
	group.close();

	/*
	 * Verify correct referenced datatype
	 */
	// Open datatype object
	dtype1.dereference(dataset, &rbuf[3]);

	// Verify correct datatype
        H5T_class_t tclass;

        tclass = dtype1.getClass();
	verify_val(tclass, H5T_COMPOUND, "DataType::getClass",__LINE__,__FILE__);
	int n_members = dtype1.getNmembers();
        verify_val(n_members, 3, "CompType::getNmembers",__LINE__,__FILE__);

	// Close all objects and file
	dtype1.close();
	dataset.close();
	file1->close();

	// Free allocated buffers
	HDfree(wbuf);
	HDfree(rbuf);
	HDfree(tbuf);

	PASSED();
    } // end try
    catch (Exception& E)
    {
	issue_fail_msg("test_reference_obj()",__LINE__,__FILE__,
			E.getCFuncName(), E.getCDetailMsg());
    }

    if(file1)
        delete file1;
}   // test_reference_obj()


/****************************************************************
**
**  test_reference_group(): Test object reference functionality
**      Tests for correct behavior of various routines on
**	dereferenced group
**
****************************************************************/
#define GROUPNAME       "/group"
#define GROUPNAME2      "group2"
#define GROUPNAME3      "group3"
#define DSETNAME        "/dset"
#define DSETNAME2       "dset2"
#define NAME_SIZE       16

static void
test_reference_group(void)
{
    hobj_ref_t wref;	/* Reference to write */
    hobj_ref_t rref;	/* Reference to read */
    const  H5std_string write_comment="Foo!"; // Comments for group

    // Output message about test being performed
    SUBTEST("Object Reference to Group");

    H5File* file1 = NULL;
    try {
	/*
	 * Create file with a group and a dataset containing an object
	 *  reference to the group
	 */

        // Create file FILE1
        file1 = new H5File (FILE1, H5F_ACC_TRUNC);

	// Create scalar dataspace
	DataSpace sid1;

	// Create a group
	Group group = file1->createGroup(GROUPNAME);

	/* Create nested groups */
	Group group2 = group.createGroup(GROUPNAME2);
	group2.close();
	group2 = group.createGroup(GROUPNAME3);
	group2.close();

	// Create bottom dataset
	DataSet dset1 = group.createDataSet(DSETNAME2, PredType::NATIVE_INT, sid1);
	dset1.close();

	// Close group 1
	group.close();

	// Create dataset
	DataSet dset2 = file1->createDataSet(DSETNAME, PredType::STD_REF_OBJ, sid1);

	file1->reference(&wref, GROUPNAME);

	// Write selection to disk
	dset2.write(&wref, PredType::STD_REF_OBJ);

	// Close resources
	dset2.close();
	sid1.close();
	file1->close();

	/*
	 * Re-open the file and test deferencing group
	 */

	// Re-open file
        file1->openFile(FILE1, H5F_ACC_RDWR);

	// Re-open dataset
	dset1 = file1->openDataSet(DSETNAME);

	// Read in the reference
	dset1.read(&rref, PredType::STD_REF_OBJ);

	// Dereference to get the group
	Group refgroup(dset1, &rref);

	// Dereference group object the other way
	group.dereference(dset1, &rref);

	/*
	 * Various queries on the group opened
	 */

	// Check number of objects in the group dereferenced by constructor
	hsize_t nobjs = refgroup.getNumObjs();
	verify_val(nobjs, (hsize_t)3, "H5Group::getNumObjs",__LINE__,__FILE__);

	// Check number of objects in the group dereferenced by ::reference
	nobjs = group.getNumObjs();
	verify_val(nobjs, (hsize_t)3, "H5Group::getNumObjs",__LINE__,__FILE__);

	// Check getting file name given the group dereferenced via constructor
	H5std_string fname = refgroup.getFileName();
	verify_val(fname, FILE1, "H5Group::getFileName",__LINE__,__FILE__);
    
	// Check getting file name given the group dereferenced by ::reference
	fname = group.getFileName();
	verify_val(fname, FILE1, "H5Group::getFileName",__LINE__,__FILE__);

	// Unlink one of the objects in the dereferenced group, and re-check
	refgroup.unlink(GROUPNAME2);
	nobjs = refgroup.getNumObjs();
	verify_val(nobjs, (hsize_t)2, "H5Group::getNumObjs",__LINE__,__FILE__);

	// Close resources
	group.close();
	refgroup.close();
	dset1.close();
	file1->close();

	PASSED();
    } // end try
    catch (Exception& E)
    {
	issue_fail_msg("test_reference_group()",__LINE__,__FILE__,
			E.getCFuncName(), E.getCDetailMsg());
    }

    if(file1)
        delete file1;
}   /* test_reference_group() */

/****************************************************************
**
**  test_reference_region_1D(): Test 1-D reference functionality
**      Tests 1-D references to various kinds of objects
**
****************************************************************/
static void
test_reference_region_1D(void)
{
    hsize_t	start[SPACE3_RANK];     /* Starting location of hyperslab */
    hsize_t	stride[SPACE3_RANK];    /* Stride of hyperslab */
    hsize_t	count[SPACE3_RANK];     /* Element count of hyperslab */
    hsize_t	block[SPACE3_RANK];     /* Block size of hyperslab */
    hsize_t	coord1[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    hsize_t *   coords;                 /* Coordinate buffer */
    hsize_t	low[SPACE3_RANK];       /* Selection bounds */
    hsize_t	high[SPACE3_RANK];      /* Selection bounds */
    int         i;      /* counting variables */

    // Output message about test being performed
    SUBTEST("1-D Dataset Region Reference Functions");

    try {
	hdset_reg_ref_t *wbuf,	// buffer to write to disk
		   *rbuf;	// buffer read from disk
	uint8_t    *dwbuf,	// Buffer for writing numeric data to disk
		   *drbuf;	// Buffer for reading numeric data from disk

	// Allocate write & read buffers
	wbuf = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), (size_t)SPACE1_DIM1);
	rbuf = (hdset_reg_ref_t *)HDmalloc(sizeof(hdset_reg_ref_t) * SPACE1_DIM1);
	dwbuf = (uint8_t *)HDmalloc(sizeof(uint8_t) * SPACE3_DIM1);
	drbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)SPACE3_DIM1);

        // Create file FILE1
        H5File file1(FILE2, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	hsize_t	dims3[] = {SPACE3_DIM1};
	DataSpace sid3(SPACE3_RANK, dims3);

	// Create a dataset
	DataSet dset3 = file1.createDataSet(DSET2_NAME, PredType::STD_U8LE, sid3);

	uint8_t *tu8;      // Temporary pointer to uint8 data
	for (tu8 = dwbuf, i = 0; i < SPACE3_DIM1; i++)
	    *tu8++ = i * 3; // from C test

	// Write selection to disk
	dset3.write(dwbuf, PredType::STD_U8LE);

	// Close Dataset
	dset3.close();

	// Create dataspace for datasets
	hsize_t	dims1[] = {SPACE1_DIM1};
	DataSpace sid1(SPACE1_RANK, dims1);

	// Create a dataset
	DataSet dset1 = file1.createDataSet(DSET1_NAME, PredType::STD_REF_DSETREG, sid1);

	/*
	 * Create references and prepare for testing
	 */

	/* Select 15 2x1 hyperslabs for first reference */
	start[0] = 2;
	stride[0] = 5;
	count[0] = 15;
	block[0] = 2;

	// Select a hyperslab region to add to the current selected region
	sid3.selectHyperslab(H5S_SELECT_SET, count, start, stride, block);

	// Get and verify the number of elements in a dataspace selection
	hssize_t nelms = sid3.getSelectNpoints();
	verify_val(nelms, 30, "DataSet::getRefObjType",__LINE__,__FILE__);

	// Store first dataset region
	file1.reference(&wbuf[0], "/Dataset2", sid3);

	// Get and verify object type
	H5O_type_t obj_type = dset1.getRefObjType(&wbuf[0], H5R_DATASET_REGION);
	verify_val(obj_type, H5O_TYPE_DATASET, "DataSet::getRefObjType",__LINE__,__FILE__);

	/* Select sequence of ten points for second reference */
	coord1[0][0] = 16;
	coord1[1][0] = 22;
	coord1[2][0] = 38;
	coord1[3][0] = 41;
	coord1[4][0] = 52;
	coord1[5][0] = 63;
	coord1[6][0] = 70;
	coord1[7][0] = 89;
	coord1[8][0] = 97;
	coord1[9][0] = 3;

	// Selects array elements to be included in the selection for sid3
	sid3.selectElements(H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);

	// Get and verify the number of elements in a dataspace selection
	nelms = sid3.getSelectNpoints();
	verify_val(nelms, 10, "DataSet::getRefObjType",__LINE__,__FILE__);

	// Store first dataset region
	file1.reference(&wbuf[1], "/Dataset2", sid3);

	// Write selection to disk
	dset1.write(wbuf, PredType::STD_REF_DSETREG);

	// Close disk dataspace, dataset, and file
	sid1.close();
	dset1.close();
	sid3.close();
	file1.close();

	/*
	 * Testing various dereference functions
	 */

	// Re-open the file
	file1.openFile(FILE2, H5F_ACC_RDWR);

	// Open the dataset
	dset1 = file1.openDataSet("/Dataset1");

	// Read selection from disk
	dset1.read(rbuf, PredType::STD_REF_DSETREG);

	{ // Test DataSet::dereference
	    dset3.dereference(dset1, &rbuf[0], H5R_DATASET_REGION);

	    // Get and verify object type
	    obj_type = dset1.getRefObjType(&rbuf[0], H5R_DATASET_REGION);
	    verify_val(obj_type, H5O_TYPE_DATASET, "DataSet::getRefObjType",__LINE__,__FILE__);

	    // Get dataspace of dset3 the verify number of elements
	    sid1 = dset3.getSpace();
	    nelms = sid1.getSimpleExtentNpoints();
	    verify_val((long)nelms, 100, "DataSpace::getSimpleExtentNpoints",__LINE__,__FILE__);
	} // End of test DataSet::dereference

	{ // Test DataSet constructor -by dereference
	    // Dereference dataset object by ctor, from the location where
	    // 'dset1' is located
	    DataSet newds(dset1, &rbuf[0], H5R_DATASET_REGION);

	    // Get dataspace of newds then verify number of elements
	    sid1 = newds.getSpace();
	    nelms = sid1.getSimpleExtentNpoints();
	    verify_val((long)nelms, 100, "DataSpace::getSimpleExtentNpoints",__LINE__,__FILE__);

	    // Close objects for this mini test
	    newds.close();
	    sid1.close();
	} // End of test DataSet constructor -by dereference

	// Read from disk
	dset3.read(drbuf, PredType::STD_U8LE);

	for(tu8 = (uint8_t *)drbuf, i = 0; i < SPACE3_DIM1; i++, tu8++)
	   verify_val(*tu8, (uint8_t)(i * 3), "DataSpace::getSimpleExtentNpoints",__LINE__,__FILE__);

	/*
	 * Test getting the referenced region
	 */

	// Get region
	DataSpace reg_sp = dset1.getRegion(&rbuf[0]);

	// Get and verify number of elements in a dataspace selection
	nelms = reg_sp.getSelectNpoints();
	verify_val((long)nelms, 30, "DataSpace::getSelectNpoints",__LINE__,__FILE__);

	// Get and verify number of hyperslab blocks
	nelms = reg_sp.getSelectHyperNblocks();
	verify_val((long)nelms, 15, "DataSpace::getSelectNpoints",__LINE__,__FILE__);

	/* Allocate space for the hyperslab blocks */
	coords = (hsize_t *)HDmalloc(nelms * SPACE3_RANK * sizeof(hsize_t) * 2);

	// Get the list of hyperslab blocks currently selected
	reg_sp.getSelectHyperBlocklist((hsize_t)0, (hsize_t)nelms, coords);

	// Verify values in the list
	verify_val(coords[0], (hsize_t)2, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[1], (hsize_t)3, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[2], (hsize_t)7, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[3], (hsize_t)8, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[4],(hsize_t)12, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[5],(hsize_t)13, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[6],(hsize_t)17, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[7],(hsize_t)18, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[8],(hsize_t)22, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[9],(hsize_t)23, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[10],(hsize_t)27, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[11],(hsize_t)28, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[12],(hsize_t)32, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[13],(hsize_t)33, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[14],(hsize_t)37, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[15],(hsize_t)38, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[16],(hsize_t)42, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[17],(hsize_t)43, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[18],(hsize_t)47, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[19],(hsize_t)48, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[20],(hsize_t)52, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[21],(hsize_t)53, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[22],(hsize_t)57, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[23],(hsize_t)58, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[24],(hsize_t)62, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[25],(hsize_t)63, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[26],(hsize_t)67, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[27],(hsize_t)68, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[28],(hsize_t)72, "Hyperslab Coordinates",__LINE__,__FILE__);
	verify_val(coords[29],(hsize_t)73, "Hyperslab Coordinates",__LINE__,__FILE__);

	HDfree(coords);

	// Check boundaries
	reg_sp.getSelectBounds(low, high);
	verify_val(low[0],(hsize_t)2, "DataSpace::getSelectBounds",__LINE__,__FILE__);
	verify_val(high[0],(hsize_t)73, "DataSpace::getSelectBounds",__LINE__,__FILE__);

	/* Close region space */
	reg_sp.close();

	/*
	 * Another test on getting the referenced region
	 */

	// Get region
	DataSpace elm_sp = dset1.getRegion(&rbuf[1]);

	// Get and verify number of element points in the current selection
	hssize_t nelmspts = elm_sp.getSelectElemNpoints();
	verify_val((long)nelmspts, 10, "DataSpace::getSelectNpoints",__LINE__,__FILE__);

	/* Allocate space for the hyperslab blocks */
	coords = (hsize_t *)HDmalloc(nelmspts * SPACE3_RANK * sizeof(hsize_t));

	// Get the list of element points currently selected
	elm_sp.getSelectElemPointlist((hsize_t)0, (hsize_t)nelmspts, coords);

	// Verify points
	verify_val(coords[0], coord1[0][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[1], coord1[1][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[2], coord1[2][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[3], coord1[3][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[4], coord1[4][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[5], coord1[5][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[6], coord1[6][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[7], coord1[7][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[8], coord1[8][0], "Element Coordinates",__LINE__,__FILE__);
	verify_val(coords[9], coord1[9][0], "Element Coordinates",__LINE__,__FILE__);

	HDfree(coords);

	// Check boundaries
	elm_sp.getSelectBounds(low, high);
	verify_val(low[0],(hsize_t)3, "DataSpace::getSelectBounds",__LINE__,__FILE__);
	verify_val(high[0],(hsize_t)97, "DataSpace::getSelectBounds",__LINE__,__FILE__);

	// Close element space
	elm_sp.close();

	// Close resources
	sid1.close();
	dset3.close();
	dset1.close();
	file1.close();

	// Free memory buffers
	HDfree(wbuf);
	HDfree(rbuf);
	HDfree(dwbuf);
	HDfree(drbuf);

	PASSED();
    } // end try
    catch (Exception& E)
    {
	issue_fail_msg("test_reference_region_1D()",__LINE__,__FILE__,
			E.getCFuncName(), E.getCDetailMsg());
    }
}   /* test_reference_region_1D() */


/****************************************************************
**
**  test_reference_compat(): Test basic object reference functionality.
**      Tests references to various kinds of objects using deprecated API.
**
****************************************************************/
static void test_reference_compat(void)
{
   // Not yet
}   // test_reference_compat()


/****************************************************************
**
**  test_reference(): Main reference testing routine.
**
****************************************************************/
extern "C"
void test_reference(void)
{
    // Output message about test being performed
    //MESSAGE("Testing References\n");
    MESSAGE(5, ("Testing References\n"));

    test_reference_params();    // Test basic parameters of reference functionality
    test_reference_obj();       // Test basic object reference functionality
    test_reference_group();     // Test group reference functionality
    test_reference_region_1D(); // Test 1-D reference functionality
    test_reference_compat();    // Tests deprecated reference routines (not yet)

}   // test_reference()


/****************************************************************
** Function:	cleanup_reference
** Purpose:	Cleanup temporary test files
** Return:	none
****************************************************************/
extern "C"
void cleanup_reference(void)
{
    HDremove(FILE1.c_str());
    HDremove(FILE2.c_str());
}

