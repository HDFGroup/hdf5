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
   tarray.cpp - HDF5 C++ testing the array datatype functionality

 ***************************************************************************/
#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h"      // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h"  // C++ utilility header file

const H5std_string    FILENAME("tarray.h5");
const H5std_string    ARRAYTYPE_NAME("/Array type 1");
const int SPACE1_RANK = 1;
const hsize_t SPACE1_DIM1 = 4;
const int ARRAY1_RANK = 1;
const hsize_t ARRAY1_DIM1 = 4;

typedef enum flt_t {
    FLT_FLOAT, FLT_DOUBLE, FLT_LDOUBLE, FLT_OTHER
} flt_t;

typedef enum int_t {
    INT_CHAR, INT_UCHAR, INT_SHORT, INT_USHORT, INT_INT, INT_UINT,
    INT_LONG, INT_ULONG, INT_LLONG, INT_ULLONG, INT_OTHER
} int_t;


/*-------------------------------------------------------------------------
 * Function:    test_array_compound_array
 *
 * Purpose:     Tests 1-D array of compound datatypes (with array fields)
 *
 * Return:      None.
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *		January, 2016
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_array_compound_array()
{
    SUBTEST("ArrayType::getArrayNDims & ArrayType::getArrayDims");
    typedef struct {        // Typedef for compound datatype */
        int i;
        float f[ARRAY1_DIM1];
    } s1_t;
    s1_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   // Information to write
    s1_t rdata[SPACE1_DIM1][ARRAY1_DIM1];   // Information read in
    hsize_t sdims1[] = {SPACE1_DIM1};
    hsize_t tdims1[] = {ARRAY1_DIM1};
    int     nmemb;      // Number of compound members
    int     ii;		// counting variables
    hsize_t idxi, idxj, idxk; // dimension indicing variables
    H5T_class_t mclass; // Datatype class for field

    // Initialize array data to write
    for (idxi =0; idxi < SPACE1_DIM1; idxi++)
        for (idxj = 0; idxj < ARRAY1_DIM1; idxj++) {
            wdata[idxi][idxj].i = idxi * 10 + idxj;
            for(idxk = 0; idxk < ARRAY1_DIM1; idxk++)
	    {
                float temp = idxi * 10.0 + idxj * 2.5 + idxk;
                wdata[idxi][idxj].f[idxk] = temp;
	    }
        } // end for

    try {
	// Create File
	H5File file1(FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	DataSpace space(SPACE1_RANK, sdims1, NULL);

	/*
	 * Create an array datatype of compounds, arrtype.  Each compound
	 * datatype, comptype, contains an integer and an array of floats,
	 * arrfltype.
	 */

	// Create a compound datatype
	CompType comptype(sizeof(s1_t));

	// Insert integer field
	comptype.insertMember("i", HOFFSET(s1_t, i), PredType::NATIVE_INT);

	// Create an array of floats datatype
	ArrayType arrfltype(PredType::NATIVE_FLOAT, ARRAY1_RANK, tdims1);

	// Insert float array field
	comptype.insertMember("f", HOFFSET(s1_t, f), arrfltype);

	// Close array of floats field datatype
	arrfltype.close();

	// Create an array datatype of the compound datatype
	ArrayType arrtype(comptype, ARRAY1_RANK, tdims1);

	// Close compound datatype comptype
	comptype.close();

	// Create a dataset
	DataSet dataset = file1.createDataSet("Dataset1", arrtype, space);

	// Write dataset to disk
	dataset.write(wdata, arrtype);

	// Test opening ArrayType with opening constructor (Dec 2016)

	// Commit the arrtype to give it a name
	arrtype.commit(file1, ARRAYTYPE_NAME);

	// Close it, then re-open with the opening constructor
	arrtype.close();
	ArrayType named_type(file1, ARRAYTYPE_NAME);

	// Get and verify the type's name
	H5std_string type_name = named_type.getObjName();
	verify_val(type_name, ARRAYTYPE_NAME, "DataType::getObjName tests constructor", __LINE__, __FILE__);
	named_type.close();

	// Close all
	dataset.close();
	space.close();
	file1.close();

	// Re-open file
	file1.openFile(FILENAME, H5F_ACC_RDONLY);

	// Open the dataset
	dataset = file1.openDataSet("Dataset1");

	/*
	 * Check the datatype array of compounds
	 */

	// Verify that it is an array of compounds
	DataType dstype = dataset.getDataType();
	mclass = dstype.getClass();
	verify_val(mclass==H5T_ARRAY, true, "f2_type.getClass", __LINE__, __FILE__);

	dstype.close();

	// Get the array datatype to check
	ArrayType atype_check = dataset.getArrayType();

	// Check the array rank
	int ndims = atype_check.getArrayNDims();
	verify_val(ndims, ARRAY1_RANK, "atype_check.getArrayNDims", __LINE__, __FILE__);

	// Get the array dimensions
	hsize_t rdims1[H5S_MAX_RANK];
	atype_check.getArrayDims(rdims1);

	// Check the array dimensions
	for (ii =0; ii <ndims; ii++)
	    if (rdims1[ii]!=tdims1[ii]) {
		TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%zd, tdims1[%d]=z%d\n", ii, rdims1[ii], ii, tdims1[ii]);
            continue;
        } // end if

	// Test ArrayType::ArrayType(const hid_t existing_id)
	ArrayType new_arrtype(atype_check.getId());

	// Check the array rank
	ndims = new_arrtype.getArrayNDims();
	verify_val(ndims, ARRAY1_RANK, "new_arrtype.getArrayNDims", __LINE__, __FILE__);

	// Get the array dimensions
	new_arrtype.getArrayDims(rdims1);

	// Check the array dimensions
	for (ii = 0; ii < ndims; ii++)
	    if (rdims1[ii] != tdims1[ii]) {
		TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%zd, tdims1[%d]=%zd\n", ii, rdims1[ii], ii, tdims1[ii]);
            continue;
        } // end if

	/*
	 * Check the compound datatype and the array of floats datatype
	 * in the compound.
	 */
	// Get the compound datatype, which is the base datatype of the
	// array datatype atype_check.
	DataType base_type = atype_check.getSuper();
	mclass = base_type.getClass();
	verify_val(mclass==H5T_COMPOUND, true, "atype_check.getClass", __LINE__, __FILE__);

	// Verify the compound datatype info
	CompType ctype_check(base_type.getId());
	base_type.close();
	
	// Check the number of members
	nmemb = ctype_check.getNmembers();
	verify_val(nmemb, 2, "ctype_check.getNmembers", __LINE__, __FILE__);

	// Check the 2nd field's name
	H5std_string field2_name = ctype_check.getMemberName(1);
	if (HDstrcmp(field2_name.c_str(),"f") != 0)
	    TestErrPrintf("Compound field name doesn't match!, field2_name=%s\n",field2_name.c_str());

	// Get the 2nd field's datatype
	DataType f2_type = ctype_check.getMemberDataType(1);

	// Get the 2nd field's class, this 2nd field should have an array type
	mclass = f2_type.getClass();
	verify_val(mclass==H5T_ARRAY, true, "f2_type.getClass", __LINE__, __FILE__);
	f2_type.close();

	// Get the 2nd field, array of floats datatype, to check
	ArrayType f2_atype_check = ctype_check.getMemberArrayType(1);

	// Check the array rank
	ndims = f2_atype_check.getArrayNDims();
	verify_val(ndims, ARRAY1_RANK, "f2_atype_check.getArrayNDims", __LINE__, __FILE__);

	// Get the array dimensions
	HDmemset(rdims1, 0, H5S_MAX_RANK);
	f2_atype_check.getArrayDims(rdims1);

	// Check the array dimensions
	for (ii = 0; ii < ndims; ii++)
	    if (rdims1[ii] != tdims1[ii]) {
		TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%zd, tdims1[%d]=%zd\n", ii, rdims1[ii], ii, tdims1[ii]);
		continue;
	    } // end if

	// Close done datatypes
	f2_atype_check.close();
	ctype_check.close();

	// Read dataset from disk
	dataset.read(rdata, atype_check);

	// Compare data read in
	for (idxi = 0; idxi < SPACE1_DIM1; idxi++) {
	    for (idxj = 0; idxj < ARRAY1_DIM1; idxj++) {
		if (wdata[idxi][idxj].i != rdata[idxi][idxj].i) {
		    TestErrPrintf("Array data information doesn't match!, wdata[%d][%d].i=%d, rdata[%d][%d].i=%d\n",idxi,idxj,wdata[idxi][idxj].i,idxi,idxj,rdata[idxi][idxj].i);
		    continue;
		} // end if
	    } // end for
	} // end for

	// Close all
	atype_check.close();
	dataset.close();
	file1.close();
        PASSED();
    }   // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_array_compound_array", __LINE__, __FILE__, E.getCDetailMsg());
    }

} // end test_array_compound_array()


/*-------------------------------------------------------------------------
 * Function:    test_array_assignment
 *
 * Purpose:     Tests the operator=
 *
 * Return:      None.
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *		March, 2016
 *
 * Description:
 *		Used user's sample code in HDFFV-9562
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
/*
 * Helper routine to demonstrate the issue in HDFFV-9562
 */
H5::DataType getArr()
{
    hsize_t *dims = new hsize_t; 
    *dims = 5; 
    H5::ArrayType ret; 
    ret = H5::ArrayType(H5::PredType::NATIVE_INT, 1, dims); 
    delete[] dims; 
    return ret; }

static void test_array_assignment()
{
    hsize_t sdims1[] = {SPACE1_DIM1};
    SUBTEST("ArrayType::operator=");

    try {
	// Create File
	H5File file1(FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	DataSpace space(SPACE1_RANK, sdims1, NULL);

	/*
	 * Create an array datatype of compounds, arrtype.  Each compound
	 * datatype, comptype, contains an integer and an array of floats,
	 * arrfltype.
	 */

	// Create a compound datatype
	CompType comptype(static_cast<size_t>(24));

	// Insert integer field
	comptype.insertMember("i", 0, PredType::NATIVE_INT);

	// Insert float array field
	comptype.insertMember("a", 4, getArr());

	// Create a dataset
	DataSet dataset = file1.createDataSet("Dataset1", comptype, space);

	// Close all
	dataset.close();
	comptype.close();
	space.close();
	file1.close();

        PASSED();
    }   // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_array_assignment", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // end test_array_assignment()


/*-------------------------------------------------------------------------
 * Function:    test_array_info
 *
 * Purpose:     Tests getting array information using the const methods.
 *
 * Return:      None.
 *
 * Programmer:  Binh-Minh Ribler
 *		April, 2016
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_array_info()
{
    SUBTEST("ArrayType Const Methods");
    typedef struct {        // Typedef for compound datatype */
        int i;
        float f[ARRAY1_DIM1];
    } s1_t;
    s1_t wdata[SPACE1_DIM1][ARRAY1_DIM1];   // Information to write
    s1_t rdata[SPACE1_DIM1][ARRAY1_DIM1];   // Information read in
    hsize_t sdims1[] = {SPACE1_DIM1};
    hsize_t tdims1[] = {ARRAY1_DIM1};
    int     nmemb;      // Number of compound members
    int     ii;		// counting variables
    hsize_t idxi, idxj, idxk; // dimension indicing variables
    H5T_class_t mclass; // Datatype class for field

    // Initialize array data to write
    for (idxi =0; idxi < SPACE1_DIM1; idxi++)
        for (idxj = 0; idxj < ARRAY1_DIM1; idxj++) {
            wdata[idxi][idxj].i = idxi * 10 + idxj;
            for(idxk = 0; idxk < ARRAY1_DIM1; idxk++)
	    {
                float temp = idxi * 10.0 + idxj * 2.5 + idxk;
                wdata[idxi][idxj].f[idxk] = temp;
	    }
        } // end for

    try {
	// Create File
	H5File file1(FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	DataSpace space(SPACE1_RANK, sdims1, NULL);

	/*
	 * Create some array datatypes, then close the file.
	 */

	// Create an array of floats datatype
	ArrayType arrfltype(PredType::NATIVE_FLOAT, ARRAY1_RANK, tdims1);

	// Create an array datatype of the compound datatype
	ArrayType arrtype(PredType::NATIVE_UINT, ARRAY1_RANK, tdims1);

	// Create a dataset
	DataSet dataset = file1.createDataSet("Dataset1", arrtype, space);

	// Write dataset to disk
	dataset.write(wdata, arrtype);

	// Close array of floats field datatype
	arrfltype.close();

	// Close all
	dataset.close();
	arrtype.close();
	space.close();
	file1.close();

	// Re-open file
	file1.openFile(FILENAME, H5F_ACC_RDONLY);

	// Open the dataset
	dataset = file1.openDataSet("Dataset1");

	/*
	 * Check the datatype array of compounds
	 */

	// Verify that it is an array of compounds
	DataType dstype = dataset.getDataType();
	mclass = dstype.getClass();
	verify_val(mclass==H5T_ARRAY, true, "f2_type.getClass", __LINE__, __FILE__);

	dstype.close();

	{ // Let atype_check go out of scope
	// Get the array datatype, declared as const
	const ArrayType atype_check = dataset.getArrayType();

	// Check the array rank with the const method
	int ndims = atype_check.getArrayNDims();
	verify_val(ndims, ARRAY1_RANK, "atype_check.getArrayNDims", __LINE__, __FILE__);

	// Get the array dimensions with the const method
	hsize_t rdims1[H5S_MAX_RANK];
	atype_check.getArrayDims(rdims1);

	// Check the array dimensions
	for (ii =0; ii <ndims; ii++)
	    if (rdims1[ii]!=tdims1[ii]) {
		TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%zd, tdims1[%d]=z%d\n", ii, rdims1[ii], ii, tdims1[ii]);
            continue;
        } // end if
	}

	// Close all
	dataset.close();
	file1.close();
        PASSED();
    }   // end of try block
    catch (Exception& E)
    {
        issue_fail_msg("test_array_info", __LINE__, __FILE__, E.getCDetailMsg());
    }

} // end test_array_info()


/****************************************************************
**
**  test_array(): Main datatypes testing routine.
**
****************************************************************/
extern "C"
void test_array()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing Array Datatypes\n"));

    // Test array of compounds with array field
    test_array_compound_array();

    // Test operator= (HDFFV-9562)
    test_array_assignment();

    // Test const functions (HDFFV-9725)
    test_array_info();

}   // test_array()


/*-------------------------------------------------------------------------
 * Function:	cleanup_array
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *		January, 2016
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
extern "C"
void cleanup_array()
{
    HDremove(FILENAME.c_str());
}  // cleanup_array
