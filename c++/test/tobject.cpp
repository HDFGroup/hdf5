/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
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
#include "H5Cpp.h"      // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h"  // C++ utilility header file

const H5std_string        FILE_OBJECTS("tobjects.h5");
const H5std_string        FILE_OBJHDR("tobject_header.h5");
const H5std_string        GROUP1("Top Group");
const H5std_string        GROUP1_PATH("/Top Group");
const H5std_string        GROUP1_1("Sub-Group 1.1");
const H5std_string        GROUP1_1_PATH("/Top Group/Sub-Group 1.1");
const H5std_string        GROUP1_2("Sub-Group 1.2");
const H5std_string        GROUP1_2_PATH("/Top Group/Sub-Group 1.2");
const H5std_string        DSET_DEFAULT_NAME("default");
const H5std_string        DSET_IN_FILE("Dataset in File");
const H5std_string        DSET_IN_FILE_PATH("/Dataset in File");
const H5std_string        DSET_IN_GRP1("Dataset_in_Group_1");
const H5std_string        DSET_IN_GRP1_PATH("/Top Group/Dataset_in_Group_1");
const H5std_string        DSET_IN_GRP1_2("Dataset_in_Group_1.2");
const H5std_string        DSET_IN_GRP1_2_PATH("/Top Group/Sub-Group 1.2/Dataset_in_Group_1.2");


/*-------------------------------------------------------------------------
 * Function:    test_get_objname
 *
 * Purpose      Tests getting object name of groups and datasets.
 *
 * Description:
 *        File structure:
 *              GROUP1
 *                  GROUP1_1
 *                  GROUP1_2
 *                      DSET_IN_GRP1_2
 *                  DSET_IN_GRP1
 *              DSET_IN_FILE
 *
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler
 *              Friday, March 4, 2014
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
    }        // try block

    // catch all other exceptions
    catch (Exception& E)
    {
        issue_fail_msg("test_get_objname", __LINE__, __FILE__);
    }
}   // test_get_objname


/*-------------------------------------------------------------------------
 * Function:    test_existance
 *
 * Purpose      Tests getting object name of groups and datasets.
 *
 * Description:
 *        File structure:
 *              GROUP1
 *                  GROUP1_1
 *                  GROUP1_2
 *                      DSET_IN_GRP1_2
 *                  DSET_IN_GRP1
 *              DSET_IN_FILE
 *
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler
 *              Friday, March 4, 2014
 *-------------------------------------------------------------------------
 */
static void test_existance()
{
    SUBTEST("H5File::exists and Group::exists");

    try {
        // Open file
        H5File file(FILE_OBJECTS, H5F_ACC_RDONLY);

        // Check if GROUP1 exists in the file
        bool exists = file.nameExists(GROUP1);
        verify_val(exists, TRUE, "Group::nameExists GROUP1_1", __LINE__, __FILE__);
        // Deprecated
        exists = file.exists(GROUP1);
        verify_val(exists, TRUE, "Group::exists GROUP1_1", __LINE__, __FILE__);

        // Open GROUP1
        Group grp1 = file.openGroup(GROUP1);

        // Check if GROUP1_1 and GROUP1_2 exist in GROUP1
        exists = grp1.nameExists(GROUP1_1);
        verify_val(exists, TRUE, "Group::nameExists GROUP1_1", __LINE__, __FILE__);
        exists = grp1.nameExists(GROUP1_2);
        verify_val(exists, TRUE, "Group::nameExists GROUP1_2", __LINE__, __FILE__);
        // Deprecated
        exists = grp1.exists(GROUP1_1);
        verify_val(exists, TRUE, "Group::exists GROUP1_1", __LINE__, __FILE__);
        exists = grp1.exists(GROUP1_2);
        verify_val(exists, TRUE, "Group::exists GROUP1_2", __LINE__, __FILE__);

        // Check if DSET_IN_GRP1 exists in GROUP1
        exists = grp1.nameExists(DSET_IN_GRP1);
        verify_val(exists, TRUE, "Group::nameExists DSET_IN_GRP1", __LINE__, __FILE__);
        // Deprecated
        exists = grp1.exists(DSET_IN_GRP1);
        verify_val(exists, TRUE, "Group::exists DSET_IN_GRP1", __LINE__, __FILE__);

        // Open GROUP1_2
        Group grp1_2 = grp1.openGroup(GROUP1_2);

        // Check if DSET_IN_GRP1_2 exists in GROUP1_2
        exists = grp1_2.nameExists(DSET_IN_GRP1_2);
        verify_val(exists, TRUE, "Group::nameExists DSET_IN_GRP1_2", __LINE__, __FILE__);
        // Deprecated
        exists = grp1_2.exists(DSET_IN_GRP1_2);
        verify_val(exists, TRUE, "Group::exists DSET_IN_GRP1_2", __LINE__, __FILE__);

        // Check if a dataset exists given dataset as location with full path name
        DataSet dset1 = file.openDataSet(DSET_IN_FILE);
        exists = dset1.nameExists("/Top Group/Dataset_in_Group_1");
        verify_val(exists, TRUE, "Group::nameExists given dataset with full path name", __LINE__, __FILE__);

        exists = grp1_2.nameExists(DSET_IN_GRP1);
        verify_val(exists, FALSE, "Group::nameExists DSET_IN_GRP1", __LINE__, __FILE__);
        // Deprecated
        exists = dset1.exists("/Top Group/Dataset_in_Group_1");
        verify_val(exists, TRUE, "Group::exists given dataset with full path name", __LINE__, __FILE__);
        exists = grp1_2.exists(DSET_IN_GRP1);
        verify_val(exists, FALSE, "Group::exists DSET_IN_GRP1", __LINE__, __FILE__);

        // Everything will be closed as they go out of scope

        PASSED();
    }        // try block

    // catch all other exceptions
    catch (Exception& E)
    {
        issue_fail_msg("test_existance", __LINE__, __FILE__);
    }
}   // test_existance


/*-------------------------------------------------------------------------
 * Function:    test_get_objname_ontypes
 *
 * Purpose      Test getting object name from various committed types.
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler
 *              March 4, 2014
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
        inttype = file.openIntType("INT type of STD_B8LE"); // deprecated

        // Get and verify its name
        H5std_string inttype_name = inttype.getObjName();
        verify_val(inttype_name, "/INT type of STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

        // Close the type then open it again to test getting its name, but
        // with the constructor this time
        inttype.close();
        IntType std_b8le(file, "INT type of STD_B8LE");

        // Get and verify its name
        H5std_string std_b8le_name = std_b8le.getObjName();
        verify_val(std_b8le_name, "/INT type of STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

        // Make copy of a predefined type and save it
        DataType dtype(PredType::STD_B8LE);
        dtype.commit(file, "STD_B8LE");

        // Close the data type and file
        dtype.close();
        file.close();

        // Re-open the file and the data type to test getting its name
        file.openFile(FILE_OBJECTS, H5F_ACC_RDWR);
        dtype = file.openDataType("STD_B8LE"); // deprecated

        // Get and verify its name
        H5std_string type_name = dtype.getObjName();
        verify_val(type_name, "/STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

        // Close the type and open it again with the constructor then test
        // getting its name
        dtype.close();
        DataType dtype2(file, "STD_B8LE");
        type_name = dtype2.getObjName();
        verify_val(type_name, "/STD_B8LE", "DataType::getObjName", __LINE__, __FILE__);

        // Test getting type's name from copied type
        DataType copied_type;
        copied_type.copy(dtype2);
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
        dtype2.close();
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
 * Function:    test_get_objtype
 *
 * Purpose      Tests getting object type
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * Programmer   Binh-Minh Ribler
 *              Friday, March 4, 2014
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
    }        // try block

    // catch all other exceptions
    catch (Exception& E)
    {
        issue_fail_msg("test_get_objtype", __LINE__, __FILE__);
    }
}   // test_get_objtype


/*-------------------------------------------------------------------------
 * Function:    test_open_object_header
 *
 * Purpose      Test Group::getObjId function.
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              March, 2017
 *-------------------------------------------------------------------------
 */
const H5std_string GROUPNAME("group");
const H5std_string DTYPENAME("group/datatype");
const H5std_string DTYPENAME_INGRP("datatype");
const H5std_string DSETNAME("dataset");
#define RANK 2
#define DIM0 5
#define DIM1 10

static void test_open_object_header()
{
    hsize_t     dims[2];
    H5G_info_t  ginfo;                      /* Group info struct */

    // Output message about test being performed
    SUBTEST("Group::getObjId");

    try {
        // Create file FILE1
        H5File file1(FILE_OBJHDR, H5F_ACC_TRUNC);
        /* Create a group, dataset, and committed datatype within the file */

        // Create a group in the root group
        Group grp(file1.createGroup(GROUPNAME));
        grp.close();

        // Commit the type inside the file
        IntType dtype(PredType::NATIVE_INT);
        dtype.commit(file1, DTYPENAME);
        dtype.close();

        // Create a new dataset
        dims[0] = DIM0;
        dims[1] = DIM1;
        DataSpace dspace(RANK, dims);
        DataSet dset(file1.createDataSet(DSETNAME, PredType::NATIVE_INT, dspace));

        // Close dataset and dataspace
        dset.close();
        dspace.close();

        // Now make sure that getObjId can open all three types of objects
        hid_t obj_grp = file1.getObjId(GROUPNAME);
        hid_t obj_dtype = file1.getObjId(DTYPENAME);
        hid_t obj_dset = file1.getObjId(DSETNAME);

        // Make sure that each is the right kind of ID
        H5I_type_t id_type = IdComponent::getHDFObjType(obj_grp);
        verify_val(id_type, H5I_GROUP, "H5Iget_type for group ID", __LINE__, __FILE__);
        id_type = IdComponent::getHDFObjType(obj_dtype);
        verify_val(id_type, H5I_DATATYPE, "H5Iget_type for datatype ID", __LINE__, __FILE__);
        id_type = IdComponent::getHDFObjType(obj_dset);
        verify_val(id_type, H5I_DATASET, "H5Iget_type for dataset ID", __LINE__, __FILE__);

        /* Do something more complex with each of the IDs to make sure */

        Group grp2(obj_grp);
        hsize_t num_objs = grp2.getNumObjs();
        verify_val(num_objs, 1, "H5Gget_info", __LINE__, __FILE__);
        // There should be one object, the datatype

        // Close datatype object opened from the file
        file1.closeObjId(obj_dtype);

        dset.setId(obj_dset);
        dspace = dset.getSpace();
        bool is_simple = dspace.isSimple();
        dspace.close();

        // Open datatype object from the group
        obj_dtype = grp2.getObjId(DTYPENAME_INGRP);

        dtype.setId(obj_dtype);
        H5T_class_t type_class = dtype.getClass();
        verify_val(type_class, H5T_INTEGER, "H5Tget_class", __LINE__, __FILE__);
        dtype.close();

        // Close datatype object
        grp2.closeObjId(obj_dtype);

        // Close the group object
        file1.closeObjId(obj_grp);

        // Try doing something with group, the ID should still work
        num_objs = grp2.getNumObjs();
        verify_val(num_objs, 1, "H5Gget_info", __LINE__, __FILE__);

        // Close the cloned group
        grp2.close();

        PASSED();
    }   // end of try block
    // catch invalid action exception
    catch (InvalidActionException& E)
    {
        cerr << " in InvalidActionException" << endl;
        cerr << " *FAILED*" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;
    }
    // catch all other exceptions
    catch (Exception& E)
    {
        cerr << " in Exception" << endl;
        issue_fail_msg("test_file_name()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_open_object_header


/*-------------------------------------------------------------------------
 * Function:    test_getobjectinfo_same_file
 *
 * Purpose      Test that querying the object info for objects in the same
 *              file will return the same file "number".
 *
 * Return       None
 *
 * July, 2018
 *-------------------------------------------------------------------------
 */
const H5std_string FILE_OBJINFO("tobject_getinfo.h5");
const H5std_string GROUP1NAME("group1");
const H5std_string GROUP2NAME("group2");
static void test_getobjectinfo_same_file()
{
    H5O_info_t	oinfo1, oinfo2;         /* Object info structs */

    // Output message about test being performed
    SUBTEST("Group::getObjinfo");

    try {
        // Create a new HDF5 file
        H5File file1(FILE_OBJINFO, H5F_ACC_TRUNC);

        // Create two groups in the file
        Group grp1(file1.createGroup(GROUP1NAME));
        Group grp2(file1.createGroup(GROUP2NAME));

        // Reset object info
        HDmemset(&oinfo1, 0, sizeof(oinfo1));
        HDmemset(&oinfo2, 0, sizeof(oinfo2));

        // Query the info of two groups and verify that they have the same
        // file number
        grp1.getObjinfo(oinfo1);
        grp2.getObjinfo(oinfo2);
        verify_val(oinfo1.fileno, oinfo2.fileno, "file number from getObjinfo", __LINE__, __FILE__);

        // Close groups and file
        grp1.close();
        grp2.close();
        file1.close();

        // Open the file twice
        file1.openFile(FILE_OBJINFO, H5F_ACC_RDWR);
        H5File file2(FILE_OBJINFO, H5F_ACC_RDWR);

        // Create two groups in the file
        grp1 = file1.openGroup(GROUP1NAME);
        grp2 = file2.openGroup(GROUP2NAME);

        // Reset object info
        HDmemset(&oinfo1, 0, sizeof(oinfo1));
        HDmemset(&oinfo2, 0, sizeof(oinfo2));

        // Query the info of two groups and verify that they have the same
        // file number
        grp1.getObjinfo(oinfo1);
        grp2.getObjinfo(oinfo2);
        verify_val(oinfo1.fileno, oinfo2.fileno, "file number from getObjinfo", __LINE__, __FILE__);


        // Reset object info
        HDmemset(&oinfo1, 0, sizeof(oinfo1));
        HDmemset(&oinfo2, 0, sizeof(oinfo2));

        file1.getObjinfo(GROUP1NAME, oinfo1);
        file1.getObjinfo(GROUP2NAME, oinfo2);
        verify_val(oinfo1.fileno, oinfo2.fileno, "file number from getObjectInfo", __LINE__, __FILE__);

        // Close groups and files
        grp1.close();
        grp2.close();
        file1.close();
        file2.close();

        PASSED();
    }   // end of try block
    // catch all other exceptions
    catch (Exception& E)
    {
        cerr << " in Exception " << E.getCFuncName() << "detail: " << E.getCDetailMsg() << endl;
        issue_fail_msg("test_getobjectinfo_same_file()", __LINE__, __FILE__, E.getCDetailMsg());
    }

}   // test_getobjectinfo_same_file

/*-------------------------------------------------------------------------
 * Function:    test_intermediate_groups
 *
 * Purpose      Test that intermediate groups are created as specified by
 *              the property setting.
 *
 * Return       None
 *
 * April, 2019
 *-------------------------------------------------------------------------
 */
const H5std_string FILE_INTERGRPS("tobject_intergrps.h5");
const H5std_string GROUP10NAME("/group10");
const H5std_string GROUP11NAME("/group10/group11");
const H5std_string GROUP12NAME("/group10/group11/group12");
const H5std_string GROUP13NAME("/group10/group11/group12/group13");
const H5std_string GROUP14NAME("/group10/group11/group12/group13/group14");
const H5std_string GROUP14FROM13NAME("group14");
const H5std_string GROUP20NAME("/group20");
const H5std_string GROUP21NAME("/group20/group21");
const H5std_string GROUP22NAME("group21/group22");
const H5std_string GROUP22FULLNAME("/group20/group21/group22");
static void test_intermediate_groups()
{
    // Output message about test being performed
    SUBTEST("Group::set/getCreateIntermediateGroup");

    try {
        // Create a new HDF5 file
        H5File file(FILE_INTERGRPS, H5F_ACC_TRUNC);

        // Create a link create property list and set the "create
        // intermediate groups" flag
        LinkCreatPropList lcpl;
        lcpl.setCreateIntermediateGroup(true);

        // Verify value of create missing groups flag
        bool crt_int_grps = lcpl.getCreateIntermediateGroup();
        verify_val(crt_int_grps, true, "LinkCreatPropList::getCreateIntermediateGroup", __LINE__, __FILE__);

        // Create GROUP12NAME with creating missing groups
        Group grp12(file.createGroup(GROUP12NAME, lcpl));

        // Missing groups: GROUP10NAME and GROUP11NAME

        // Create GROUP14NAME without the use of link create plist, should
        // fail because group GROUP13NAME is missing
        try {
            Group grp14_nopl(file.createGroup(GROUP14NAME));
        } catch (FileIException& expected1) {} // Failure is ignored

        // Create GROUP14NAME with the flag to create missing groups set
        // to FALSE, should fail because group GROUP13NAME is missing

        // Reset flag to not create missing groups
        lcpl.setCreateIntermediateGroup(false);

        // Verify value of create missing groups flag
        crt_int_grps = lcpl.getCreateIntermediateGroup();
        verify_val(crt_int_grps, false, "LinkCreatPropList::getCreateIntermediateGroup", __LINE__, __FILE__);

        try {
            Group grp14_false(file.createGroup(GROUP14NAME, lcpl));
        } catch (FileIException& expected2) {} // Failure is ignored

        // Set the flag to create missing groups set to TRUE
        lcpl.setCreateIntermediateGroup(true);
        crt_int_grps = lcpl.getCreateIntermediateGroup();
        verify_val(crt_int_grps, true, "LinkCreatPropList::getCreateIntermediateGroup", __LINE__, __FILE__);


        // Create GROUP14NAME with the use of link create plist
        Group grp14(file.createGroup(GROUP14NAME, lcpl));

        // Missing groups: GROUP13NAME

        // Create group GROUP20NAME
        Group grp20(file.createGroup(GROUP20NAME));

        // Create group GROUP22NAME with missing group GROUP21NAME
        Group grp22(grp20.createGroup(GROUP22NAME, lcpl));

        // Close groups and file
        grp12.close();
        grp14.close();
        grp20.close();
        grp22.close();
        file.close();

        // Reopen the file
        file.openFile(FILE_INTERGRPS, H5F_ACC_RDWR);

        // Open the missing groups and various combinations
        Group grp10(file.openGroup(GROUP10NAME));
        Group grp11(file.openGroup(GROUP11NAME));
        Group grp13(file.openGroup(GROUP13NAME));
        Group grp14from13(grp13.openGroup(GROUP14FROM13NAME));
        Group grp21(file.openGroup(GROUP21NAME));
        Group grp22fromfile(file.openGroup(GROUP22FULLNAME));

        PASSED();
    }   // end of try block
    // catch all other exceptions
    catch (Exception& E)
    {
        cerr << " in Exception " << E.getCFuncName() << "detail: " << E.getCDetailMsg() << endl;
        issue_fail_msg("test_intermediate_groups()", __LINE__, __FILE__, E.getCDetailMsg());
    }

}   // test_intermediate_groups

/*-------------------------------------------------------------------------
 * Function:    test_object
 *
 * Purpose      Tests HDF5 object related functionality
 *
 * Return       Success: 0
 *              Failure: -1
 *
 * March 4, 2014
 *-------------------------------------------------------------------------
 */
extern "C"
void test_object()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing Object Functions\n"));

    test_get_objname();             // Test get object name from groups/datasets
    test_existance();               // Test check for object existance
    test_get_objname_ontypes();     // Test get object name from types
    test_get_objtype();             // Test get object type
    test_open_object_header();      // Test object header functions (H5O)
    test_getobjectinfo_same_file(); // Test object info in same file
    test_intermediate_groups();     // Test intermediate group property

}   // test_object


/*-------------------------------------------------------------------------
 * Function:    cleanup_objects
 *
 * Purpose      Cleanup temporary test files
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
extern "C"
void cleanup_object()
{
    HDremove(FILE_OBJECTS.c_str());
} // cleanup_objects
