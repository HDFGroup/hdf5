/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*****************************************************************************
   FILE
   tfile.cpp - HDF5 C++ testing the file I/O features

   EXTERNAL ROUTINES/VARIABLES:
     These routines are in the test directory of the C library:
        h5_fileaccess() -- in h5test.c, returns a file access template

 ***************************************************************************/
#include <iostream>
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h" // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h" // C++ utilility header file

const hsize_t      F1_USERBLOCK_SIZE = (hsize_t)0;
const size_t       F1_OFFSET_SIZE    = sizeof(haddr_t);
const size_t       F1_LENGTH_SIZE    = sizeof(hsize_t);
const unsigned     F1_SYM_LEAF_K     = 4;
const unsigned     F1_SYM_INTERN_K   = 16;
const H5std_string FILE1("tfile1.h5");

const hsize_t      F2_USERBLOCK_SIZE = (hsize_t)512;
const size_t       F2_OFFSET_SIZE    = 8;
const size_t       F2_LENGTH_SIZE    = 8;
const unsigned     F2_SYM_LEAF_K     = 8;
const unsigned     F2_SYM_INTERN_K   = 32;
const unsigned     F2_ISTORE         = 64;
const H5std_string FILE2("tfile2.h5");

const hsize_t      F3_USERBLOCK_SIZE = (hsize_t)0;
const size_t       F3_OFFSET_SIZE    = F2_OFFSET_SIZE;
const size_t       F3_LENGTH_SIZE    = F2_LENGTH_SIZE;
const unsigned     F3_SYM_LEAF_K     = F2_SYM_LEAF_K;
const unsigned     F3_SYM_INTERN_K   = F2_SYM_INTERN_K;
const H5std_string FILE3("tfile3.h5");

const int          KB = 1024;
const H5std_string FILE4("tfile4.h5");

/*-------------------------------------------------------------------------
 * Function:    test_file_create
 *
 * Purpose      Test file and template creations
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              January, 2001
 *
 * Modifications:
 *        January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *                     cases.  Since there are no operator<< for 'long long'
 *                     or int64 in VS C++ ostream, I casted the hsize_t values
 *                     passed to verify_val to 'long' as well.  If problems
 *                     arises later, this will have to be specifically handled
 *                     with a special routine.
 *-------------------------------------------------------------------------
 */
static void
test_file_create()
{
    // Output message about test being performed
    SUBTEST("File Creation I/O");

    // Test create with various sequences of H5F_ACC_EXCL and
    // H5F_ACC_TRUNC flags

    // Create with H5F_ACC_EXCL
    // First ensure the file does not exist
    remove(FILE1.c_str());

    // Setting this to NULL for cleaning up in failure situations
    H5File *file1 = NULL;
    try {
        // Create file FILE1
        file1 = new H5File(FILE1, H5F_ACC_EXCL);

        // Try to create the same file with H5F_ACC_TRUNC. This should fail
        // because file1 is the same file and is currently open.
        try {
            H5File file2(FILE1, H5F_ACC_TRUNC); // should throw E

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5File constructor", "Attempted to create an existing file.");
        }
        catch (FileIException &E) // catch truncating existing file
        {
        } // do nothing, FAIL expected

        // Close file1
        delete file1;
        file1 = NULL;

        // Try again with H5F_ACC_EXCL. This should fail because the file
        // already exists from the previous steps.
        try {
            H5File file2(FILE1, H5F_ACC_EXCL); // should throw E

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5File constructor", "File already exists.");
        }
        catch (FileIException &E) // catching creating existing file
        {
        } // do nothing, FAIL expected

        // Test create with H5F_ACC_TRUNC. This will truncate the existing file.
        file1 = new H5File(FILE1, H5F_ACC_TRUNC);

        // Try to create first file again. This should fail because file1
        // is the same file and is currently open.
        try {
            H5File file2(FILE1, H5F_ACC_TRUNC); // should throw E

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5File constructor", "H5F_ACC_TRUNC attempt on an opened file.");
        }
        catch (FileIException &E) // catching truncating opened file
        {
        } // do nothing, FAIL expected

        // Try with H5F_ACC_EXCL. This should fail too because the file already
        // exists.
        try {
            H5File file3(FILE1, H5F_ACC_EXCL); // should throw E

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5File constructor", "H5F_ACC_EXCL attempt on an existing file.");
        }
        catch (FileIException &E) // catching H5F_ACC_EXCL on existing file
        {
        } // do nothing, FAIL expected

        // Get the file-creation template
        FileCreatPropList tmpl1 = file1->getCreatePlist();

        hsize_t ublock = tmpl1.getUserblock();
        verify_val((long)ublock, (long)F1_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__,
                   __FILE__);

        size_t parm1, parm2; // file-creation parameters
        tmpl1.getSizes(parm1, parm2);
        verify_val(parm1, F1_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
        verify_val(parm2, F1_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned iparm1, iparm2; // file-creation parameters
        tmpl1.getSymk(iparm1, iparm2);
        verify_val(iparm1, F1_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F1_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

        // tmpl1 is automatically closed; if error occurs, it'll be
        // caught in the catch block

        // Close first file
        delete file1;
    }
    catch (InvalidActionException &E) {
        cerr << " *FAILED*" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;
        // clean up
        delete file1;
    }
    // catch all other exceptions
    catch (Exception &E) {
        issue_fail_msg("test_file_create()", __LINE__, __FILE__, E.getCDetailMsg());
        // clean up
        delete file1;
    }

    // Setting this to NULL for cleaning up in failure situations
    FileCreatPropList *tmpl1 = NULL;
    try {
        // Create a new file with a non-standard file-creation template
        tmpl1 = new FileCreatPropList;

        // Set the new file-creation parameters
        tmpl1->setUserblock(F2_USERBLOCK_SIZE);
        tmpl1->setSizes(F2_OFFSET_SIZE, F2_LENGTH_SIZE);
        tmpl1->setSymk(F2_SYM_INTERN_K, F2_SYM_LEAF_K);

        // Try to create second file, with non-standard file-creation template
        // params.
        H5File file2(FILE2, H5F_ACC_TRUNC, *tmpl1);

        // Release file-creation template
        delete tmpl1;
        tmpl1 = NULL;

        // Get the file-creation template
        tmpl1 = new FileCreatPropList(file2.getCreatePlist());

        // Get the file-creation parameters
        hsize_t ublock = tmpl1->getUserblock();
        verify_val((long)ublock, (long)F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__,
                   __FILE__);

        size_t parm1, parm2; // file-creation parameters
        tmpl1->getSizes(parm1, parm2);
        verify_val(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
        verify_val(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned iparm1, iparm2; // file-creation parameters
        tmpl1->getSymk(iparm1, iparm2);
        verify_val(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

        // Clone the file-creation template
        FileCreatPropList tmpl2;
        tmpl2.copy(*tmpl1);

        // Release file-creation template
        delete tmpl1;
        tmpl1 = NULL;

        // Set the new file-creation parameter
        tmpl2.setUserblock(F3_USERBLOCK_SIZE);

        // Try to create second file, with non-standard file-creation template
        // params
        H5File file3(FILE3, H5F_ACC_TRUNC, tmpl2);

        // Get the file-creation template
        tmpl1 = new FileCreatPropList(file3.getCreatePlist());

        // Get the file-creation parameters
        ublock = tmpl1->getUserblock();
        verify_val((long)ublock, (long)F3_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__,
                   __FILE__);

        tmpl1->getSizes(parm1, parm2);
        verify_val(parm1, F3_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
        verify_val(parm2, F3_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        tmpl1->getSymk(iparm1, iparm2);
        verify_val(iparm1, F3_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F3_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

        // Release file-creation template
        delete tmpl1;
        PASSED();
    }
    // catch all exceptions
    catch (Exception &E) {
        issue_fail_msg("test_file_create()", __LINE__, __FILE__, E.getCDetailMsg());
        // clean up
        delete tmpl1;
    }
} // test_file_create()

/*-------------------------------------------------------------------------
 * Function:    test_file_open
 *
 * Purpose      Test file accesses
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              January, 2001
 *
 * Modifications:
 *        January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *                     cases.  Since there are no operator<< for 'long long'
 *                     or int64 in VS C++ ostream, I casted the hsize_t values
 *                     passed to verify_val to 'long' as well.  If problems
 *                     arises later, this will have to be specifically handled
 *                     with a special routine.
 *-------------------------------------------------------------------------
 */
static void
test_file_open()
{
    // Output message about test being performed
    SUBTEST("File Opening I/O");

    try {

        // Open first file
        H5File file1(FILE2, H5F_ACC_RDWR);

        // Get the file-creation template
        FileCreatPropList tmpl1 = file1.getCreatePlist();

        // Get the file-creation parameters
        hsize_t ublock = tmpl1.getUserblock();
        verify_val((long)ublock, (long)F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__,
                   __FILE__);

        size_t parm1, parm2; // file-creation parameters
        tmpl1.getSizes(parm1, parm2);
        verify_val(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
        verify_val(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned iparm1, iparm2; // file-creation parameters
        tmpl1.getSymk(iparm1, iparm2);
        verify_val(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

        // Test H5File constructor with existing file id
        H5File file2(file1.getId());
        file1.close();

        // Try truncating the file, and it should fail because the file is
        // still opened with file2.
        try {
            H5File file3(FILE2, H5F_ACC_TRUNC); // should throw E

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5File constructor", "Attempt truncating an opened file.");
        }
        catch (FileIException &E) // catching H5F_ACC_TRUNC on opened file
        {
        } // do nothing, FAIL expected

        // Now, really close the file.
        file2.close();

        // Truncating should succeed now.
        H5File file3(FILE2, H5F_ACC_TRUNC);

        // Opening another file to file3 object, FILE2 should be closed, so
        // the next attempt to truncate FILE2 should succeed.
        file3.openFile(FILE1, H5F_ACC_RDONLY);
        H5File file4(FILE2, H5F_ACC_TRUNC);

        PASSED();
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_file_open()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_file_open()

/*-------------------------------------------------------------------------
 * Function:    test_file_size
 *
 * Purpose      Test file size.
 *
 * Return       None
 *
 * Programmer   Raymond Lu
 *              June, 2004
 *-------------------------------------------------------------------------
 */
static void
test_file_size()
{
    // Output message about test being performed
    SUBTEST("File Size");

    hid_t fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template

    try {
        // Use the file access template id to create a file access prop.
        // list object to pass in H5File::H5File
        FileAccPropList fapl(fapl_id);

        // Set to sec2 driver.  Do we want to test other file drivers?
        // They're not tested in C++.
        // File drivers seem not implemented.
        // fapl.setSec2();

        // Create a file
        H5File file4(FILE4, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Get file size
        hsize_t file_size = file4.getFileSize();

        // Check if file size is reasonable.  It's supposed to be 2KB now.
        if (file_size < 1 * KB || file_size > 4 * KB)
            issue_fail_msg("test_file_size()", __LINE__, __FILE__,
                           "getFileSize() returned unreasonable value");

        // Get the amount of free space in the file
        hssize_t free_space = file4.getFreeSpace();

        // Check if it's reasonable.  It's 0 now.
        if (free_space < 0 || free_space > 4 * KB)
            issue_fail_msg("test_file_size()", __LINE__, __FILE__,
                           "getFreeSpace returned unreasonable value");

        PASSED();
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_file_size()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    // use C test utility routine to close property list.
    herr_t ret = H5Pclose(fapl_id);
    if (ret < 0)
        issue_fail_msg("test_file_size()", __LINE__, __FILE__, "H5Pclose failed");

} // test_file_size()

/*-------------------------------------------------------------------------
 * Function:    test_file_num
 *
 * Purpose      Test file number.
 *
 * Return       None
 *
 * Programmer   Quincey Koziol
 *              April, 2019
 *-------------------------------------------------------------------------
 */
static void
test_file_num()
{
    // Output message about test being performed
    SUBTEST("File Number");

    hid_t fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template

    try {
        // Use the file access template id to create a file access prop.
        // list object to pass in H5File::H5File
        FileAccPropList fapl(fapl_id);

        // Create two files
        H5File file1(FILE1, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);
        H5File file2(FILE2, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Open the first file again
        H5File file3(FILE1, H5F_ACC_RDWR);

        // Get file numbers
        unsigned long file_num1 = file1.getFileNum();
        unsigned long file_num2 = file2.getFileNum();
        unsigned long file_num3 = file3.getFileNum();

        // Check file numbers
        if (file_num1 == file_num2)
            issue_fail_msg("test_file_num()", __LINE__, __FILE__, "getFileNum() returned wrong value");
        if (file_num1 != file_num3)
            issue_fail_msg("test_file_num()", __LINE__, __FILE__, "getFileNum() returned wrong value");

        PASSED();
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_file_num()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    // use C test utility routine to close property list.
    herr_t ret = H5Pclose(fapl_id);
    if (ret < 0)
        issue_fail_msg("test_file_num()", __LINE__, __FILE__, "H5Pclose failed");

} // test_file_num()

/*-------------------------------------------------------------------------
 * Function:    test_file_name
 *
 * Purpose      Test getting file's name.
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler
 *              July, 2004
 *-------------------------------------------------------------------------
 */
const int          RANK = 2;
const int          NX   = 4;
const int          NY   = 5;
const H5std_string GROUPNAME("group");
const H5std_string DSETNAME("dataset");
const H5std_string DATTRNAME("dataset attribute");
const H5std_string FATTRNAME("file attribute");
const H5std_string DTYPENAME("compound");

// Compound datatype
typedef struct s1_t {
    unsigned int a;
    float        b;
} s1_t;

static void
test_file_name()
{
    // Output message about test being performed.
    SUBTEST("File Name");

    H5std_string file_name;
    try {
        // Create a file using default properties.
        H5File file4(FILE4, H5F_ACC_TRUNC);

        // Get file name from the file instance.
        file_name = file4.getFileName();
        verify_val(file_name, FILE4, "H5File::getFileName", __LINE__, __FILE__);

        // Create a group in the root group.
        Group group(file4.createGroup(GROUPNAME, 0));

        // Get and verify file name via a group.
        file_name = group.getFileName();
        verify_val(file_name, FILE4, "Group::getFileName", __LINE__, __FILE__);

        // Create the data space.
        hsize_t   dims[RANK] = {NX, NY};
        DataSpace space(RANK, dims);

        // Create a new dataset.
        DataSet dataset(file4.createDataSet(DSETNAME, PredType::NATIVE_INT, space));

        // Get and verify file name via a dataset.
        file_name = dataset.getFileName();
        verify_val(file_name, FILE4, "DataSet::getFileName", __LINE__, __FILE__);

        // Create an attribute for the dataset.
        Attribute attr(dataset.createAttribute(DATTRNAME, PredType::NATIVE_INT, space));

        // Get and verify file name via an attribute.
        file_name = attr.getFileName();
        verify_val(file_name, FILE4, "Attribute::getFileName", __LINE__, __FILE__);

        // Create a compound datatype.
        CompType comp_type(sizeof(s1_t));

        // Insert fields.
        comp_type.insertMember("a", HOFFSET(s1_t, a), PredType::NATIVE_INT);
        comp_type.insertMember("b", HOFFSET(s1_t, b), PredType::NATIVE_FLOAT);

        // Save it on file.
        comp_type.commit(file4, DTYPENAME);

        // Get and verify file name via a committed datatype.
        comp_type.getFileName();
        verify_val(file_name, FILE4, "CompType::getFileName", __LINE__, __FILE__);
        PASSED();
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_file_name()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_file_name()

/*-------------------------------------------------------------------------
 *
 * Function:    test_file_attribute
 *
 * Purpose      Test file attributes
 *
 * Return       None
 *-------------------------------------------------------------------------
 */
const int          RANK1      = 1;
const int          ATTR1_DIM1 = 3;
const H5std_string FILE5("tfattrs.h5");
const H5std_string FATTR1_NAME("file attribute 1");
const H5std_string FATTR2_NAME("file attribute 2");
int                fattr_data[ATTR1_DIM1] = {512, -234, 98123}; // Test data for file attribute
int                dattr_data[ATTR1_DIM1] = {256, -123, 1000};  // Test data for dataset attribute

static void
test_file_attribute()
{
    int rdata[ATTR1_DIM1];
    int i;

    // Output message about test being performed
    SUBTEST("File Attribute");

    H5std_string file_name;
    try {
        // Create a file using default properties.
        H5File file5(FILE5, H5F_ACC_TRUNC);

        // Create the data space
        hsize_t   dims[RANK1] = {ATTR1_DIM1};
        DataSpace space(RANK1, dims);

        // Create two attributes for the file
        Attribute fattr1(file5.createAttribute(FATTR1_NAME, PredType::NATIVE_FLOAT, space));
        Attribute fattr2(file5.createAttribute(FATTR2_NAME, PredType::NATIVE_INT, space));

        fattr2.write(PredType::NATIVE_INT, fattr_data);

        try {
            // Try to create the same attribute again (should fail)
            Attribute fattr_dup(file5.createAttribute(FATTR2_NAME, PredType::NATIVE_INT, space));
            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5File createAttribute",
                                         "Attempted to create an existing attribute.");
        }
        catch (AttributeIException &E) // catch creating existing attribute
        {
        } // do nothing, FAIL expected

        // Create a new dataset
        DataSet dataset(file5.createDataSet(DSETNAME, PredType::NATIVE_INT, space));

        // Create an attribute for the dataset
        Attribute dattr(dataset.createAttribute(DATTRNAME, PredType::NATIVE_INT, space));

        // Write data to the second file attribute
        dattr.write(PredType::NATIVE_INT, dattr_data);

        // Test flushing out the data from the attribute object
        dattr.flush(H5F_SCOPE_GLOBAL);

        // Get and verify the number of all objects in the file
        // Current: 1 file, 2 file attr, 1 ds, and 1 ds attr.
        ssize_t num_objs = file5.getObjCount(H5F_OBJ_ALL);
        verify_val(num_objs, 5, "H5File::getObjCount", __LINE__, __FILE__);

        num_objs = file5.getObjCount(H5F_OBJ_GROUP);
        verify_val(num_objs, 0, "H5File::getObjCount(H5F_OBJ_GROUP)", __LINE__, __FILE__);
        num_objs = file5.getObjCount(H5F_OBJ_DATASET);
        verify_val(num_objs, 1, "H5File::getObjCount(H5F_OBJ_DATASET)", __LINE__, __FILE__);
        num_objs = file5.getObjCount(H5F_OBJ_ATTR);
        verify_val(num_objs, 3, "H5File::getObjCount(H5F_OBJ_ATTR)", __LINE__, __FILE__);
        num_objs = file5.getObjCount(H5F_OBJ_DATATYPE);
        verify_val(num_objs, 0, "H5File::getObjCount(H5F_OBJ_DATATYPE)", __LINE__, __FILE__);
        num_objs = file5.getObjCount(H5F_OBJ_FILE);
        verify_val(num_objs, 1, "H5File::getObjCount(H5F_OBJ_FILE)", __LINE__, __FILE__);

        // Get the file name using the attributes
        H5std_string fname = fattr1.getFileName();
        verify_val(fname, FILE5, "H5File::getFileName()", __LINE__, __FILE__);

        fname.clear();
        fname = dattr.getFileName();
        verify_val(fname, FILE5, "H5File::getFileName()", __LINE__, __FILE__);

        // Get the class of a file attribute's datatype
        H5T_class_t atclass = fattr1.getTypeClass();
        verify_val(atclass, H5T_FLOAT, "Attribute::getTypeClass()", __LINE__, __FILE__);

        // Get and verify the number of attributes attached to a file
        int n_attrs = file5.getNumAttrs();
        verify_val(n_attrs, 2, "H5File::getNumAttrs()", __LINE__, __FILE__);

        // Get and verify the number of attributes attached to a dataset
        n_attrs = 0;
        n_attrs = dataset.getNumAttrs();
        verify_val(n_attrs, 1, "DataSet::getNumAttrs()", __LINE__, __FILE__);

        // Read back attribute's data
        HDmemset(rdata, 0, sizeof(rdata));
        dattr.read(PredType::NATIVE_INT, rdata);
        /* Check results */
        for (i = 0; i < ATTR1_DIM1; i++) {
            if (rdata[i] != dattr_data[i]) {
                H5_FAILED();
                cerr << endl;
                cerr << "element [" << i << "] is " << rdata[i] << "but should have been " << dattr_data[i]
                     << endl;
            }
        }
        PASSED();
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_file_attribute()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_file_attribute()

/*-------------------------------------------------------------------------
 * Function:    test_libver_bounds_real
 *
 * Purpose      Verify that a file created and modified with the
 *              specified libver bounds has the specified object header
 *              versions for the right objects.
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              March, 2015
 *-------------------------------------------------------------------------
 */
const H5std_string FILE6("tfile5.h5");
const H5std_string ROOTGROUP("/");
const H5std_string GROUP1("/G1");
const H5std_string SUBGROUP3("/G1/G3");

static void
test_libver_bounds_real(H5F_libver_t libver_create, unsigned oh_vers_create, H5F_libver_t libver_mod,
                        unsigned oh_vers_mod)
{
    try {

        /*
         * Create a new file using the default creation property and access property
         * with latest library version.
         */
        FileAccPropList fapl;
        fapl.setLibverBounds(libver_create, H5F_LIBVER_LATEST);
        H5File file(FILE6, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        /*
         * Make sure the root group has the correct object header version
         */
        unsigned obj_version = file.childObjVersion(ROOTGROUP);
        verify_val(obj_version, oh_vers_create, "H5File::childObjVersion", __LINE__, __FILE__);

        // Verify object header version another way
        H5O_native_info_t ninfo;
        HDmemset(&ninfo, 0, sizeof(ninfo));
        file.getNativeObjinfo(ninfo, H5O_NATIVE_INFO_HDR);
        verify_val(ninfo.hdr.version, oh_vers_create, "H5File::getNativeObjinfo", __LINE__, __FILE__);

        /*
         * Reopen the file and make sure the root group still has the correct
         * version
         */
        file.close();

        fapl.setLibverBounds(libver_mod, H5F_LIBVER_LATEST);

        file.openFile(FILE6, H5F_ACC_RDWR, fapl);

        obj_version = file.childObjVersion(ROOTGROUP);
        verify_val(obj_version, oh_vers_create, "H5File::childObjVersion", __LINE__, __FILE__);

        /*
         * Create a group named "/G1" in the file, and make sure it has the correct
         * object header version
         */
        Group group = file.createGroup(GROUP1);

        obj_version = group.objVersion();
        verify_val(obj_version, oh_vers_mod, "Group::objVersion", __LINE__, __FILE__);

        // Verify object header version another way
        HDmemset(&ninfo, 0, sizeof(ninfo));
        group.getNativeObjinfo(ninfo, H5O_NATIVE_INFO_HDR);
        verify_val(ninfo.hdr.version, oh_vers_mod, "Group::getNativeObjinfo", __LINE__, __FILE__);

        group.close(); // close "/G1"

        /*
         * Create a group named "/G1/G3" in the file, and make sure it has the
         * correct object header version
         */
        group = file.createGroup(SUBGROUP3);

        obj_version = group.objVersion();
        verify_val(obj_version, oh_vers_mod, "Group::objVersion", __LINE__, __FILE__);

        group.close(); // close "/G1/G3"

        /*
         * Make sure the root group still has the correct object header version
         */
        obj_version = file.childObjVersion(ROOTGROUP);
        verify_val(obj_version, oh_vers_create, "H5File::childObjVersion", __LINE__, __FILE__);

        // Everything should be closed as they go out of scope
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_libver_bounds_real()", __LINE__, __FILE__, E.getCDetailMsg());
    }

} /* end test_libver_bounds_real() */

/*-------------------------------------------------------------------------
 *
 * Function:    test_libver_bounds
 *
 * Purpose      Verify that a file created and modified with various
 *              libver bounds is handled correctly.
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              March 2015
 *-------------------------------------------------------------------------
 */
static void
test_libver_bounds()
{
    // Output message about test being performed
    SUBTEST("Setting library version bounds");

    /* Run the tests */
    test_libver_bounds_real(H5F_LIBVER_EARLIEST, H5O_VERSION_1, H5F_LIBVER_LATEST, H5O_VERSION_2);
    test_libver_bounds_real(H5F_LIBVER_LATEST, H5O_VERSION_2, H5F_LIBVER_EARLIEST, H5O_VERSION_2);
    PASSED();
} /* end test_libver_bounds() */

/*-------------------------------------------------------------------------
 * Function:    test_commonfg
 *
 * Purpose      Verify that H5File works as a root group.
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              March, 2015
 *-------------------------------------------------------------------------
 */
static void
test_commonfg()
{
    // Output message about test being performed
    SUBTEST("Root group");

    try {
        // Create a file using default properties.
        H5File file4(FILE4, H5F_ACC_TRUNC);

        // Try opening the root group.
        Group rootgroup(file4.openGroup(ROOTGROUP));

        // Create a group in the root group.
        Group group(rootgroup.createGroup(GROUPNAME, 0));

        // Create the data space.
        hsize_t   dims[RANK] = {NX, NY};
        DataSpace space(RANK, dims);

        // Create a new dataset.
        DataSet dataset(group.createDataSet(DSETNAME, PredType::NATIVE_INT, space));

        // Get and verify file name via a dataset.
        H5std_string file_name = dataset.getFileName();
        verify_val(file_name, FILE4, "DataSet::getFileName", __LINE__, __FILE__);

        // Create an attribute for the dataset.
        Attribute attr(dataset.createAttribute(DATTRNAME, PredType::NATIVE_INT, space));

        // Get and verify file name via an attribute.
        file_name = attr.getFileName();
        verify_val(file_name, FILE4, "Attribute::getFileName", __LINE__, __FILE__);

        // Create an attribute for the file via root group.
        Attribute rootg_attr(rootgroup.createAttribute(FATTRNAME, PredType::NATIVE_INT, space));

        // Get and verify file name via an attribute.
        file_name = attr.getFileName();
        verify_val(file_name, FILE4, "Attribute::getFileName", __LINE__, __FILE__);

        PASSED();
    } // end of try block

    catch (Exception &E) {
        issue_fail_msg("test_commonfg()", __LINE__, __FILE__, E.getCDetailMsg());
    }

} /* end test_commonfg() */

/*-------------------------------------------------------------------------
 * Function:    test_file_info
 *
 * Purpose      Verify that various properties in a file creation property
 *              lists are stored correctly in the file and can be retrieved
 *              when the file is re-opened.
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler
 *              February, 2017
 *-------------------------------------------------------------------------
 */
const H5std_string FILE7("tfile7.h5");
const hsize_t      FSP_SIZE_DEF = 4096;
const hsize_t      FSP_SIZE512  = 512;

static void
test_file_info()
{
    // Output message about test being performed
    SUBTEST("File general information");

    hsize_t out_threshold = 0;     // Free space section threshold to get
    hbool_t out_persist   = FALSE; // Persist free-space read
    // File space handling strategy
    H5F_fspace_strategy_t out_strategy = H5F_FSPACE_STRATEGY_FSM_AGGR;

    try {
        // Create a file using default properties.
        H5File tempfile(FILE7, H5F_ACC_TRUNC);

        // Get the file's version information.
        H5F_info2_t finfo;
        tempfile.getFileInfo(finfo);
        verify_val(finfo.super.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(finfo.free.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(finfo.sohm.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);

        // Close the file.
        tempfile.close();

        // Create file creation property list.
        FileCreatPropList fcpl;

        // Retrieve file space information.
        fcpl.getFileSpaceStrategy(out_strategy, out_persist, out_threshold);

        // Verify file space information.
        verify_val(out_strategy, H5F_FSPACE_STRATEGY_FSM_AGGR, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(out_persist, FALSE, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(out_threshold, 1, "H5File::getFileInfo", __LINE__, __FILE__);

        /* Retrieve file space page size */
        hsize_t out_fsp_psize = fcpl.getFileSpacePagesize();
        verify_val(out_fsp_psize, FSP_SIZE_DEF, "FileCreatPropList::getFileSpacePagesize", __LINE__,
                   __FILE__);

        // Set various file information.
        fcpl.setUserblock(F2_USERBLOCK_SIZE);
        fcpl.setSizes(F2_OFFSET_SIZE, F2_LENGTH_SIZE);
        fcpl.setSymk(F2_SYM_INTERN_K, F2_SYM_LEAF_K);
        fcpl.setIstorek(F2_ISTORE);

        hsize_t               threshold = 5;    // Free space section threshold to set
        hbool_t               persist   = TRUE; // Persist free-space to set
        H5F_fspace_strategy_t strategy  = H5F_FSPACE_STRATEGY_PAGE;

        fcpl.setFileSpaceStrategy(strategy, persist, threshold);
        fcpl.setFileSpacePagesize(FSP_SIZE512);

        // Creating a file with the non-default file creation property list
        // should create a version 1 superblock

        // Create file with custom file creation property list.
        H5File file7(FILE7, H5F_ACC_TRUNC, fcpl);

        // Close the file creation property list.
        fcpl.close();

        // Get the file's version information.
        file7.getFileInfo(finfo);
        verify_val(finfo.super.version, 2, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(finfo.free.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(finfo.sohm.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);

        // Close the file.
        file7.close();

        // Re-open the file.
        file7.openFile(FILE7, H5F_ACC_RDONLY);

        // Get the file's creation property list.
        FileCreatPropList fcpl2 = file7.getCreatePlist();

        // Get the file's version information.
        file7.getFileInfo(finfo);
        verify_val(finfo.super.version, 2, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(finfo.free.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);
        verify_val(finfo.sohm.version, 0, "H5File::getFileInfo", __LINE__, __FILE__);

        // Retrieve the property values & check them.
        hsize_t userblock = fcpl2.getUserblock();
        verify_val(userblock, F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__, __FILE__);

        size_t off_size = 0, len_size = 0;
        fcpl2.getSizes(off_size, len_size);
        verify_val(off_size, F2_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
        verify_val(len_size, F2_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned sym_ik = 0, sym_lk = 0;
        fcpl2.getSymk(sym_ik, sym_lk);
        verify_val(sym_ik, F2_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(sym_lk, F2_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

        unsigned istore_ik = fcpl2.getIstorek();
        verify_val(istore_ik, F2_ISTORE, "FileCreatPropList::getIstorek", __LINE__, __FILE__);

        /*  ret=H5Pget_shared_mesg_nindexes(fcpl2,&nindexes);
        CHECK(ret, FAIL, "H5Pget_shared_mesg_nindexes");
        VERIFY(nindexes, MISC11_NINDEXES, "H5Pget_shared_mesg_nindexes");
     */

        // Get and verify the file space info from the creation property list */
        fcpl2.getFileSpaceStrategy(out_strategy, out_persist, out_threshold);
        verify_val(out_strategy, strategy, "FileCreatPropList::getFileSpaceStrategy", __LINE__, __FILE__);
        verify_val(out_persist, persist, "FileCreatPropList::getFileSpaceStrategy", __LINE__, __FILE__);
        verify_val(out_threshold, threshold, "FileCreatPropList::getFileSpaceStrategy", __LINE__, __FILE__);

        out_fsp_psize = fcpl2.getFileSpacePagesize();
        verify_val(out_fsp_psize, FSP_SIZE512, "FileCreatPropList::getFileSpacePagesize", __LINE__, __FILE__);

        PASSED();
    } // end of try block
    catch (Exception &E) {
        issue_fail_msg("test_filespace_info()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} /* test_file_info() */

/*-------------------------------------------------------------------------
 * Function:    test_file
 *
 * Purpose      Main file testing routine
 *
 * Return       None
 *
 * Programmer   Binh-Minh Ribler (use C version)
 *              January 2001
 *-------------------------------------------------------------------------
 */
extern "C" void
test_file()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing File I/O Operations\n"));

    test_file_create();    // Test file creation (also creation templates)
    test_file_open();      // Test file opening
    test_file_size();      // Test file size
    test_file_num();       // Test file number
    test_file_name();      // Test getting file's name
    test_file_attribute(); // Test file attribute feature
    test_libver_bounds();  // Test format version
    test_commonfg();       // Test H5File as a root group
    test_file_info();      // Test various file info
} // test_file()

/*-------------------------------------------------------------------------
 * Function:    cleanup_file
 *
 * Purpose      Cleanup temporary test files
 *
 * Return       none
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
    void
    cleanup_file()
{
    HDremove(FILE1.c_str());
    HDremove(FILE2.c_str());
    HDremove(FILE3.c_str());
    HDremove(FILE4.c_str());
    HDremove(FILE5.c_str());
    HDremove(FILE6.c_str());
    HDremove(FILE7.c_str());
} // cleanup_file
