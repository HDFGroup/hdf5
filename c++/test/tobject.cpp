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
   tobject.cpp - HDF5 C++ testing object related functionality

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"  // C++ utilility header file

const H5std_string	FILE_OBJECTS("tobjects.h5");
const H5std_string	GROUP1("Top Group");
const H5std_string	GROUP1_PATH("/Top Group");
const H5std_string	GROUP1_1("Sub-Group 1.1");
const H5std_string	GROUP1_1_PATH("/Top Group/Sub-Group 1.1");
const H5std_string	GROUP1_2("Sub-Group 1.2");
const H5std_string	GROUP1_2_PATH("/Top Group/Sub-Group 1.2");
const H5std_string	DSET_DEFAULT_NAME("default");
const H5std_string	DSET_IN_FILE("Dataset in File");
const H5std_string	DSET_IN_FILE_PATH("/Dataset in File");
const H5std_string	DSET_IN_GRP1("Dataset in Group 1");
const H5std_string	DSET_IN_GRP1_PATH("/Top Group/Dataset in Group 1");
const H5std_string	DSET_IN_GRP1_2("Dataset in Group 1.2");
const H5std_string	DSET_IN_GRP1_2_PATH("/Top Group/Sub-Group 1.2/Dataset in Group 1.2");

/*-------------------------------------------------------------------------
 * Function:	test_get_objname
 *
 * Purpose:	Tests getting object name of groups and datasets.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler
 *		Friday, March 4, 2014
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_get_objname()
{
    SUBTEST("H5Object::getObjName on Groups and Datasets");

    try {
	// Create file
	H5File file(FILE_OBJECTS, H5F_ACC_TRUNC);

	// Create a top group and 2 subgroups
	Group grp1 = file.createGroup(GROUP1, 0);
	Group grp1_1 = grp1.createGroup(GROUP1_1, 0);
	Group grp1_2 = grp1.createGroup(GROUP1_2, 0);

	// Get part of the group's name, random length using
	// ssize_t getObjName(char* comment, size_t buf_size)

	// Get the length of the group's name first
	ssize_t name_len = grp1.getObjName(NULL);

	// Random length is 4
	if (name_len > 4)
	{
	    char* grp1_name = new char[5];
	    name_len = grp1.getObjName(grp1_name, 5);
	    verify_val((const char*)grp1_name, "/Top", "Group::getObjName", __LINE__, __FILE__);
	    delete []grp1_name;
	}

	// Create a data space
	hsize_t     dims[2];
	dims[0] = 2;
	dims[1] = 5;
	DataSpace space (2, dims, NULL);

	// Create a dataset in the file
	DataSet dsinfile = file.createDataSet(DSET_IN_FILE,
			 PredType::NATIVE_DOUBLE, space);

	// Create a dataset in the group
	DataSet dsingrp = grp1.createDataSet(DSET_IN_GRP1,
			 PredType::NATIVE_INT, space);

	// Get and verify the name of each dataset, using
	// H5std_string getObjName() and
	// ssize_t getObjName(H5std_string& obj_name, size_t len = 0)
	H5std_string ds_name = dsinfile.getObjName();
	verify_val(ds_name, DSET_IN_FILE_PATH, "DataSet::getObjName", __LINE__, __FILE__);

	name_len = dsingrp.getObjName(ds_name); // default len
	verify_val(ds_name, DSET_IN_GRP1_PATH, "DataSet::getObjName", __LINE__, __FILE__);

	// Close dataset
	dsingrp.close();

	// Create a dataset in sub-group 1.2
	dsingrp = grp1_2.createDataSet(DSET_IN_GRP1_2, PredType::NATIVE_INT, space);

	// Get and verify the name of the dataset that belongs to subgroup
	// 1.2, using H5std_string getObjName()
	ds_name = dsingrp.getObjName();
	verify_val(ds_name, DSET_IN_GRP1_2_PATH, "DataSet::getObjName", __LINE__, __FILE__);

	// Close dataset
	dsingrp.close();

	// Reopen that same dataset then check the name again with another
	// overload: ssize_t getObjName(H5std_string& obj_name, size_t len = 0)
	dsingrp = grp1_2.openDataSet(DSET_IN_GRP1_2);
	name_len = dsingrp.getObjName(ds_name);
	verify_val(ds_name, DSET_IN_GRP1_2_PATH, "DataSet::getObjName", __LINE__, __FILE__);

	// Everything will be closed as they go out of scope

	PASSED();
    }	// try block

    // catch all other exceptions
    catch (Exception& E)
    {
	issue_fail_msg("test_get_objname", __LINE__, __FILE__);
    }
}   // test_get_objname

/*-------------------------------------------------------------------------
 * Function:	test_get_objname_ontypes
 *
 * Purpose:	Test getting object name from various committed types.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:  Binh-Minh Ribler
 *		March 4, 2014
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_get_objname_ontypes()
{
    SUBTEST("H5Object::getObjName on Committed Datatypes");

    try {
	// Create a file with default prop lists
	H5File file(FILE_OBJECTS, H5F_ACC_RDWR);

	// Create a group
	Group grp = file.createGroup ("typetests");

	// Create a datatype and save it
	IntType inttype(PredType::STD_B8LE);
	inttype.commit(file, "INT type of STD_B8LE");

	// Close the type then open it again to test getting its name
	inttype.close();
	inttype = file.openIntType("INT type of STD_B8LE");

	// Get and verify its name
	H5std_string inttype_name = inttype.getObjName();
	verify_val(inttype_name, "/INT type of STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

	// Make copy of a predefined type and save it
	DataType dtype(PredType::STD_B8LE);
	dtype.commit(file, "STD_B8LE");

	// Close the data type and file
	dtype.close();
	file.close();

	// Re-open the file and the data type to test getting its name
	file.openFile(FILE_OBJECTS, H5F_ACC_RDWR);
	dtype = file.openDataType("STD_B8LE");

	// Get and verify its name
	H5std_string type_name = dtype.getObjName();
	verify_val(type_name, "/STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

	// Test getting type's name from copied type
	DataType copied_type;
	copied_type.copy(dtype);
	copied_type.commit(file, "copy of STD_B8LE");
	type_name = copied_type.getObjName();
	verify_val(type_name, "/copy of STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

	// Test copying an integer predefined type
	IntType new_int_type(PredType::NATIVE_INT);

	// Name this datatype
	new_int_type.commit(grp, "IntType NATIVE_INT");
	ssize_t name_len = new_int_type.getObjName(type_name); // default len
	verify_val(name_len, (ssize_t)HDstrlen("/typetests/IntType NATIVE_INT"), "DataType::getObjName", __LINE__, __FILE__);
	verify_val(type_name, "/typetests/IntType NATIVE_INT", "DataType::getObjName", __LINE__, __FILE__);

	// Close everything or they can be closed when objects go out of scope
	dtype.close();
	copied_type.close();
	new_int_type.close();
	grp.close();

	PASSED();
    } // end top try block

    catch (Exception& E)
    {
	issue_fail_msg("test_get_objname_ontypes", __LINE__, __FILE__);
    }
}   // test_get_objname_ontypes

/*-------------------------------------------------------------------------
 * Function:	test_get_objtype
 *
 * Purpose:	Tests getting object type
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Binh-Minh Ribler
 *		Friday, March 4, 2014
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_get_objtype()
{
    SUBTEST("H5File::childObjType and H5Group::childObjType");

    try {
	// Open file
	H5File file(FILE_OBJECTS, H5F_ACC_RDWR);

	// Open the top group
	Group grp1 = file.openGroup(GROUP1);

	// Create a datatype and save it
	DataType dtype(PredType::STD_I32LE);
	dtype.commit(grp1, "STD_I32LE");

	// Get and verify object type with
	// H5O_type_t childObjType(const H5std_string& objname)
	H5O_type_t objtype = file.childObjType(DSET_IN_FILE);
	verify_val(objtype, H5O_TYPE_DATASET, "DataSet::childObjType", __LINE__, __FILE__);

	// Get and verify object type with
	// H5O_type_t childObjType(const char* objname)
	objtype = grp1.childObjType(GROUP1_1.c_str());
	verify_val(objtype, H5O_TYPE_GROUP, "DataSet::childObjType", __LINE__, __FILE__);

	// Get and verify object type with
	// H5O_type_t childObjType(hsize_t index, H5_index_t index_type,
	// H5_iter_order_t order, const char* objname=".")
	objtype = grp1.childObjType((hsize_t)1, H5_INDEX_NAME, H5_ITER_INC);
	verify_val(objtype, H5O_TYPE_NAMED_DATATYPE, "DataSet::childObjType", __LINE__, __FILE__);

	// Get and verify object type with
	// H5O_type_t childObjType(hsize_t index,
	// H5_index_t index_type=H5_INDEX_NAME,
	// H5_iter_order_t order=H5_ITER_INC, const char* objname=".")
	objtype = grp1.childObjType((hsize_t)2);
	verify_val(objtype, H5O_TYPE_GROUP, "DataSet::childObjType", __LINE__, __FILE__);

	// Everything will be closed as they go out of scope

	PASSED();
    }	// try block

    // catch all other exceptions
    catch (Exception& E)
    {
	issue_fail_msg("test_get_objtype", __LINE__, __FILE__);
    }
}   // test_get_objtype

/*-------------------------------------------------------------------------
 * Function:	test_objects
 *
 * Purpose:	Tests HDF5 object related functionality
 *
 * Return:	Success: 0
 *		Failure: -1
 *
 * Programmer:	Binh-Minh Ribler
 *		Friday, Mar 4, 2014
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_object()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing Object Functions\n"));

    test_get_objname();    // Test get object name from groups/datasets
    test_get_objname_ontypes();	// Test get object name from types
    test_get_objtype();    // Test get object type

}   // test_objects

/*-------------------------------------------------------------------------
 * Function:    cleanup_objects
 *
 * Purpose:     Cleanup temporary test files
 *
 * Return:      none
 *
 * Programmer:  (use C version)
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_object()
{
    HDremove(FILE_OBJECTS.c_str());
} // cleanup_objects
