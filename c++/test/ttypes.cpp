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
   ttypes.cpp - HDF5 C++ testing the general datatype functionality

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
using std::cerr;
using std::endl;

#include <string>
#include "H5Cpp.h" // C++ API header file
using namespace H5;

#include "h5test.h"
#include "h5cpputil.h" // C++ utilility header file

/*
 * Offset from alinged memory returned by malloc().  This can be used to test
 * that type conversions handle non-aligned buffers correctly.
 */
#define ALIGNMENT 1

/*
 * Define if you want to test alignment code on a machine that doesn't
 * normally require alignment. When set, all native datatypes must be aligned
 * on a byte boundary equal to the data size.
 */
#if 0
#define TEST_ALIGNMENT

/* Alignment test stuff */
#ifdef TEST_ALIGNMENT
#define H5T_PACKAGE
#include "H5Tpkg.h"
#endif
#define SET_ALIGNMENT(TYPE, VAL) H5T_NATIVE_##TYPE##_ALIGN_g = MAX(H5T_NATIVE_##TYPE##_ALIGN_g, VAL)
#endif

const char *FILENAME[] = {"dtypes1.h5", "dtypes2.h5", "dtypes3.h5", NULL};

/*
 * Count up or down depending on whether the machine is big endian or little
 * endian.  If local variable `endian' is H5T_ORDER_BE then the result will
 * be I, otherwise the result will be Z-(I+1).
 */
#define ENDIAN(Z, I) (H5T_ORDER_BE == endian ? (I) : (Z) - ((I) + 1))

typedef enum flt_t { FLT_FLOAT, FLT_DOUBLE, FLT_LDOUBLE, FLT_OTHER } flt_t;

typedef enum int_t {
    INT_CHAR,
    INT_UCHAR,
    INT_SHORT,
    INT_USHORT,
    INT_INT,
    INT_UINT,
    INT_LONG,
    INT_ULONG,
    INT_LLONG,
    INT_ULLONG,
    INT_OTHER
} int_t;

typedef struct {
    int    a;
    float  b;
    long   c;
    double d;
} src_typ_t;

/*-------------------------------------------------------------------------
 * Function:    test_classes
 *
 * Purpose:     Test type classes
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *              January, 2007
 *
 *-------------------------------------------------------------------------
 */
static void
test_classes()
{
    SUBTEST("PredType::getClass()");
    try {
        // maybe later, int curr_nerrors = GetTestNumErrs();

        // PredType::NATIVE_INT should be in H5T_INTEGER class
        H5T_class_t tcls = PredType::NATIVE_INT.getClass();
        if (H5T_INTEGER != tcls) {
            puts("    Invalid type class for H5T_NATIVE_INT");
        }

        // PredType::NATIVE_DOUBLE should be in H5T_FLOAT class
        tcls = PredType::NATIVE_DOUBLE.getClass();
        if (H5T_FLOAT != tcls) {
            verify_val(tcls, H5T_FLOAT, "test_class: invalid type class for NATIVE_DOUBLE -", __LINE__,
                       __FILE__);
        }
        PASSED();
    } // end of try block
    catch (Exception &E) {
        issue_fail_msg("test_classes", __LINE__, __FILE__, E.getCDetailMsg());
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Test datatype copy functionality
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *              January, 2007
 *
 *-------------------------------------------------------------------------
 */
static void
test_copy()
{
    SUBTEST("DataType::copy() and DataType::operator=");
    try {
        // Test copying from a predefined datatype using DataType::operator=
        DataType assigned_type;
        assigned_type = PredType::NATIVE_SHORT;

        // Test copying a predefined type using DataType::copy
        DataType copied_type;
        copied_type.copy(PredType::STD_B8LE);

        // Test copying a user-defined type using DataType::operator=
        DataType assigned_usertype;
        assigned_usertype = copied_type;

        // Test copying from a user-defined datatype using DataType::copy
        DataType copied_usertype;
        copied_usertype.copy(copied_type);

        // Test copying a user-defined int type using DataType::operator=
        IntType  orig_int(PredType::STD_B8LE);
        DataType generic_type;
        generic_type = orig_int;

        // Test copying an integer predefined type
        IntType new_int_type(PredType::STD_B8LE);

        // Test copying an int predefined type using DataType::operator=
        IntType another_int_type;
        another_int_type = new_int_type;

        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_copy", __LINE__, __FILE__, E.getCDetailMsg());
    }
}

/*-------------------------------------------------------------------------
 * Function:    test_query
 *
 * Purpose:     Tests query functions of compound and enumeration types.
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2007
 *
 *-------------------------------------------------------------------------
 */

const H5std_string CompT_NAME("Compound_type");
const H5std_string EnumT_NAME("Enum_type");

static void
test_query()
{
    short enum_val;

    // Output message about test being performed
    SUBTEST("Query functions of compound and enumeration types");
    try {
        // Create File
        H5File file(FILENAME[2], H5F_ACC_TRUNC);

        // Create a compound datatype
        CompType tid1(sizeof(src_typ_t));

        tid1.insertMember("a", HOFFSET(src_typ_t, a), PredType::NATIVE_INT);
        tid1.insertMember("b", HOFFSET(src_typ_t, b), PredType::NATIVE_FLOAT);
        tid1.insertMember("c", HOFFSET(src_typ_t, c), PredType::NATIVE_LONG);
        tid1.insertMember("d", HOFFSET(src_typ_t, d), PredType::NATIVE_DOUBLE);

        // Create a enumerate datatype
        EnumType tid2(sizeof(short));

        tid2.insert("RED", (enum_val = 0, &enum_val));
        tid2.insert("GREEN", (enum_val = 1, &enum_val));
        tid2.insert("BLUE", (enum_val = 2, &enum_val));
        tid2.insert("ORANGE", (enum_val = 3, &enum_val));
        tid2.insert("YELLOW", (enum_val = 4, &enum_val));

        // Query member number and member index by name, for compound type
        int nmembs = tid1.getNmembers();
        verify_val(nmembs, 4, "CompType::getNmembers()", __LINE__, __FILE__);

        int index = tid1.getMemberIndex("c");
        verify_val(index, 2, "CompType::getMemberIndex()", __LINE__, __FILE__);

        // Query member number and member index by name, for enumeration type.
        nmembs = tid2.getNmembers();
        verify_val(nmembs, 5, "EnumType::getNmembers()", __LINE__, __FILE__);

        index = tid2.getMemberIndex("ORANGE");
        verify_val(index, 3, "EnumType::getMemberIndex()", __LINE__, __FILE__);

        // Commit compound datatype, and test getting the datatype creation
        // prop list, then close it
        tid1.commit(file, CompT_NAME);
        PropList tcpl = tid1.getCreatePlist();
        if (!IdComponent::isValid(tcpl.getId())) {
            // Throw an invalid action exception
            throw InvalidActionException("IdComponent::isValid",
                                         "Datatype creation property list is not valid");
        }
        tcpl.close();
        tid1.close();

        // Commit enumeration datatype, and test getting the datatype creation
        // prop list, then close it
        tid2.commit(file, EnumT_NAME);
        tcpl = tid2.getCreatePlist();
        if (!IdComponent::isValid(tcpl.getId())) {
            // Throw an invalid action exception
            throw InvalidActionException("IdComponent::isValid",
                                         "Datatype creation property list is not valid");
        }
        tcpl.close();
        tid2.close();

        // Open the datatypes for query
        tid1 = file.openCompType(CompT_NAME);
        tid2 = file.openEnumType(EnumT_NAME);

        // Query member number and member index by name, for compound type
        nmembs = tid1.getNmembers();
        verify_val(nmembs, 4, "CompType::getNmembers()", __LINE__, __FILE__);
        index = tid1.getMemberIndex("c");
        verify_val(index, 2, "CompType::getMemberIndex()", __LINE__, __FILE__);

        // Query member number and member index by name, for enumeration type
        nmembs = tid2.getNmembers();
        verify_val(nmembs, 5, "EnumType::getNmembers()", __LINE__, __FILE__);
        index = tid2.getMemberIndex("ORANGE");
        verify_val(index, 3, "EnumType::getMemberIndex()", __LINE__, __FILE__);

        // Close datatypes and file
        tid1.close();
        tid2.close();
        file.close();

        // Try truncating the file to make sure reference counting is good.
        // If any references to ids of tid1 and tid2 are left unterminated,
        // the truncating will fail, because the file will not be closed in
        // the file.close() above.
        H5File file1(FILENAME[2], H5F_ACC_TRUNC);

        PASSED();
    } // end of try block
    catch (Exception &E) {
        issue_fail_msg("test_query", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_query

/*-------------------------------------------------------------------------
 * Function:    test_transient
 *
 * Purpose:     Tests transient datatypes.
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2007
 *
 *-------------------------------------------------------------------------
 */
const char *filename1 = "dtypes1.h5";
static void
test_transient()
{
    static hsize_t ds_size[2] = {10, 20};

    SUBTEST("Transient datatypes");
    try {

        // Create the file and the dataspace.
        H5File    file(filename1, H5F_ACC_TRUNC);
        DataSpace space(2, ds_size, ds_size);

        // Copying a predefined type results in a modifiable copy
        IntType type(PredType::NATIVE_INT);
        type.setPrecision(256);

        // It should not be possible to create an attribute for a transient type
        try {
            Attribute attr(type.createAttribute("attr1", PredType::NATIVE_INT, space));
            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("H5Object::createAttribute",
                                         "Attempted to commit a predefined datatype.");
        }
        catch (AttributeIException &err) {
        } // do nothing, failure expected

        // Create a dataset from a transient datatype
        // type.close(); - put trace in H5Tclose to make sure it's closed
        type.copy(PredType::NATIVE_INT);
        DataSet dset(file.createDataSet("dset1", type, space));

        // The type returned from a dataset should not be modifiable
        IntType itype(dset);
        try {
            itype.setPrecision(256);

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("PredType::setPrecision",
                                         "Dataset datatypes should not be modifiable!");
        }
        catch (DataTypeIException &err) {
        }
        itype.close();

        // Get a copy of the dataset's datatype by applying DataType::copy()
        // to the dataset. The resulted datatype should be modifiable.
        itype.copy(dset);
        itype.setPrecision(256);
        itype.close();

        // Close the dataset and reopen it, testing that its type is still
        // read-only.  (Note that a copy of it is modifiable.)
        dset.close();
        dset = file.openDataSet("dset1");

        // Close objects and file.
        dset.close();
        file.close();
        type.close();
        space.close();
        PASSED();
    } // end of try block
    catch (Exception &E) {
        issue_fail_msg("test_transient", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_transient

/*-------------------------------------------------------------------------
 * Function:    test_named
 *
 * Purpose:     Tests named datatypes.
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2007
 *
 *-------------------------------------------------------------------------
 */
const H5std_string filename2("dtypes2.h5");
static void
test_named()
{
    static hsize_t ds_size[2] = {10, 20};
    hsize_t        i;
    unsigned       attr_data[10][20];
    DataType *     ds_type = NULL;

    SUBTEST("Named datatypes");
    try {
        // Create the file.
        H5File file(filename2, H5F_ACC_TRUNC);

        // Create a simple dataspace.
        DataSpace space(2, ds_size, ds_size);

        // Predefined types cannot be committed.
        try {
            PredType nativeint(PredType::NATIVE_INT);
            nativeint.commit(file, "test_named_1 (should not exist)");

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("PredType::commit", "Attempted to commit a predefined datatype.");
        }
        catch (DataTypeIException &err) {
        }

        // Copy a predefined datatype and commit the copy.
        IntType itype(PredType::NATIVE_INT);
        itype.commit(file, "native-int");

        // Test commit passing in const H5File& for prototype with const
        try {
            // Create random char type
            IntType atype(PredType::NATIVE_UCHAR);

            // Creating group, declared as const
            const Group const_grp = file.createGroup("GR as loc");

            // Commit type passing in const group; compilation would fail if
            // no matching prototype
            atype.commit(const_grp, "random uchar");
        } // end of try block
        catch (Exception &E) {
            issue_fail_msg("test_named", __LINE__, __FILE__, "Commit at const group");
        }

        // Check that it is committed.
        if (itype.committed() == false)
            cerr << "IntType::committed() returned false" << endl;

        // We should not be able to modify a type after it has been committed.
        try {
            itype.setPrecision(256); // attempt an invalid action...

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("IntType::setPrecision",
                                         "Attempted to modify a committed datatype.");
        }
        catch (DataTypeIException &err) {
        }

        // We should not be able to re-commit a committed type
        try {
            itype.commit(file, "test_named_2 (should not exist)");

            // Should FAIL but didn't, so throw an invalid action exception
            throw InvalidActionException("IntType::commit", "Attempted to re-commit a committed datatype.");
        }
        catch (DataTypeIException &err) {
        } // do nothing, failure expected

        // It should be possible to define an attribute for the named type
        Attribute attr1 = itype.createAttribute("attr1", PredType::NATIVE_UCHAR, space);
        for (i = 0; i < ds_size[0] * ds_size[1]; i++)
            attr_data[0][i] = (int)i; /*tricky*/
        attr1.write(PredType::NATIVE_UINT, attr_data);
        attr1.close();

        // Copying a committed type should result in a transient type which is
        // not locked.
        IntType trans_type;
        trans_type.copy(itype);
        bool iscommitted = trans_type.committed();
        verify_val(iscommitted, 0,
                   "DataType::committed() - Copying a named type should result in a transient type!",
                   __LINE__, __FILE__);
        trans_type.setPrecision(256);
        trans_type.close();

        // Close the committed type and reopen it.  It should be a named type.
        itype.close();
        itype       = file.openIntType("native-int");
        iscommitted = itype.committed();
        if (!iscommitted)
            throw InvalidActionException("IntType::committed()", "Opened named types should be named types!");

        // Create a dataset that uses the named type, then get the dataset's
        // datatype and make sure it's a named type.
        DataSet dset = file.createDataSet("dset1", itype, space);
        ds_type      = new DataType(dset.getDataType());
        iscommitted  = ds_type->committed();
        if (!iscommitted)
            throw InvalidActionException("IntType::committed()", "Dataset type should be named type!");
        dset.close();
        ds_type->close();
        delete ds_type;

        // Reopen the dataset and its type, then make sure the type is
        // a named type.
        dset        = file.openDataSet("dset1");
        ds_type     = new DataType(dset.getDataType());
        iscommitted = ds_type->committed();
        if (!iscommitted)
            throw InvalidActionException("IntType::committed()", "Dataset type should be named type!");

        // Close the dataset and create another with the type returned from
        // the first dataset.
        dset.close();
        dset = file.createDataSet("dset2", *ds_type, space);
        ds_type->close();
        dset.close();
        delete ds_type;

        // Reopen the second dataset and make sure the type is shared
        dset        = file.openDataSet("dset2");
        ds_type     = new DataType(dset.getDataType());
        iscommitted = ds_type->committed();
        if (!iscommitted)
            throw InvalidActionException("DataType::iscommitted()", "Dataset type should be named type!");
        ds_type->close();

        // Get the dataset datatype by applying DataType::copy() to the
        // dataset. The resulted datatype should be modifiable.
        IntType copied_type;
        copied_type.copy(dset);
        copied_type.setPrecision(256);
        copied_type.close();

        // Clean up
        dset.close();
        itype.close();
        space.close();
        file.close();
        PASSED();
    } // end of try block
    catch (Exception &E) {
        issue_fail_msg("test_named", __LINE__, __FILE__, E.getCDetailMsg());
    }

    if (ds_type)
        delete ds_type;
} // test_named

/*-------------------------------------------------------------------------
 * Function:    test_encode_decode
 *
 * Purpose:     Test datatype encode/decode functionality.
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (using C version)
 *              October, 2017
 *
 *-------------------------------------------------------------------------
 */
const H5std_string filename3("encode_decode.h5");
const int          ARRAY1_RANK = 1;
const int          ARRAY1_DIM  = 10;
static void
test_encode_decode()
{
    short enum_val;

    SUBTEST("DataType::encode() and DataType::decode()");
    try {
        // Create the file.
        H5File file(filename3, H5F_ACC_TRUNC);

        //
        // Test with CompType
        //

        // Create a compound datatype
        CompType cmptyp(sizeof(src_typ_t));

        cmptyp.insertMember("a", HOFFSET(src_typ_t, a), PredType::NATIVE_INT);
        cmptyp.insertMember("b", HOFFSET(src_typ_t, b), PredType::NATIVE_FLOAT);
        cmptyp.insertMember("c", HOFFSET(src_typ_t, c), PredType::NATIVE_LONG);
        cmptyp.insertMember("d", HOFFSET(src_typ_t, d), PredType::NATIVE_DOUBLE);

        // Encode compound type in its buffer
        cmptyp.encode();

        // Verify that encoding had been done
        verify_val(cmptyp.hasBinaryDesc(), true, "DataType::encode", __LINE__, __FILE__);

        // Decode compound type's buffer to a new CompType
        CompType *decoded_cmp_ptr(static_cast<CompType *>(cmptyp.decode()));

        // Verify that the datatype was copied exactly via encoding/decoding
        verify_val(cmptyp == *decoded_cmp_ptr, true, "DataType::decode", __LINE__, __FILE__);

        // Verify again via querying member number and member index by name.
        verify_val(decoded_cmp_ptr->getNmembers(), 4, "DataType::decode", __LINE__, __FILE__);
        verify_val(decoded_cmp_ptr->getMemberIndex("c"), 2, "DataType::decode", __LINE__, __FILE__);

        // Create a CompType instance from the pointer and verify it
        CompType cmptyp_clone(*decoded_cmp_ptr);
        verify_val(cmptyp == cmptyp_clone, true, "DataType::decode", __LINE__, __FILE__);
        verify_val(cmptyp_clone.getNmembers(), 4, "DataType::decode", __LINE__, __FILE__);
        verify_val(cmptyp_clone.getMemberIndex("c"), 2, "DataType::decode", __LINE__, __FILE__);

        delete decoded_cmp_ptr;

        //
        // Test with EnumType
        //

        // Create a enumerate datatype
        EnumType enumtyp(sizeof(short));

        enumtyp.insert("RED", (enum_val = 0, &enum_val));
        enumtyp.insert("GREEN", (enum_val = 1, &enum_val));
        enumtyp.insert("BLUE", (enum_val = 2, &enum_val));
        enumtyp.insert("ORANGE", (enum_val = 3, &enum_val));
        enumtyp.insert("YELLOW", (enum_val = 4, &enum_val));

        // Encode compound type in a buffer
        enumtyp.encode();

        // Verify that encoding had been done
        verify_val(enumtyp.hasBinaryDesc(), true, "DataType::encode", __LINE__, __FILE__);

        // Decode enumeration type's buffer to a new EnumType
        EnumType *decoded_enum_ptr(static_cast<EnumType *>(enumtyp.decode()));

        // Verify that the datatype was copied exactly via encoding/decoding
        verify_val(enumtyp == *decoded_enum_ptr, true, "DataType::decode", __LINE__, __FILE__);

        // Verify again via querying member number and member index by name.
        verify_val(decoded_enum_ptr->getNmembers(), 5, "DataType::decode", __LINE__, __FILE__);
        verify_val(decoded_enum_ptr->getMemberIndex("GREEN"), 1, "DataType::decode", __LINE__, __FILE__);

        // Create a EnumType instance from the pointer and verify it
        EnumType enumtyp_clone(*decoded_enum_ptr);
        verify_val(enumtyp == enumtyp_clone, true, "DataType::decode", __LINE__, __FILE__);
        verify_val(enumtyp_clone.getNmembers(), 5, "DataType::decode", __LINE__, __FILE__);
        verify_val(enumtyp_clone.getMemberIndex("GREEN"), 1, "DataType::decode", __LINE__, __FILE__);

        delete decoded_enum_ptr;

        //
        // Test with variable-length string
        //

        // Create a variable-length string type
        StrType vlsttyp(PredType::C_S1);
        vlsttyp.setSize(H5T_VARIABLE);

        // Encode the variable-length type in its buffer
        vlsttyp.encode();

        // Verify that encoding had been done
        verify_val(vlsttyp.hasBinaryDesc(), true, "DataType::encode", __LINE__, __FILE__);

        // Decode the variable-length type's buffer to a new StrType
        StrType *decoded_str_ptr(static_cast<StrType *>(vlsttyp.decode()));

        verify_val(vlsttyp == *decoded_str_ptr, true, "DataType::decode", __LINE__, __FILE__);
        verify_val(decoded_str_ptr->isVariableStr(), true, "DataType::decode", __LINE__, __FILE__);

        delete decoded_str_ptr;

        // Test decoding the type by way of DataType*

        // Decode variable-length string type to a new DataType
        DataType *decoded_vlstr_ptr(vlsttyp.decode());

        // Create a StrType instance from the DataType object and verify it
        StrType decoded_vlsttyp(decoded_vlstr_ptr->getId());
        verify_val(vlsttyp == decoded_vlsttyp, true, "DataType::decode", __LINE__, __FILE__);
        verify_val(decoded_vlsttyp.isVariableStr(), true, "DataType::decode", __LINE__, __FILE__);

        delete decoded_vlstr_ptr;

        //
        // Test with ArrayType
        //

        hsize_t tdims1[] = {ARRAY1_DIM};

        // Create an array datatype of the compound datatype
        ArrayType arrtyp(cmptyp, ARRAY1_RANK, tdims1);

        // Encode the array type in its buffer
        arrtyp.encode();

        // Verify that encoding had been done
        verify_val(arrtyp.hasBinaryDesc(), true, "DataType::encode", __LINE__, __FILE__);

        // Create an ArrayType instance from the decoded pointer and verify it
        ArrayType *decoded_arr_ptr(static_cast<ArrayType *>(arrtyp.decode()));

        verify_val(arrtyp == *decoded_arr_ptr, true, "DataType::decode", __LINE__, __FILE__);

        delete decoded_arr_ptr;

        // Test decoding the type by way of DataType*

        // Decode the array type's buffer
        DataType *decoded_dt_ptr = arrtyp.decode();

        // Create a ArrayType instance from the decoded pointer and verify it
        ArrayType decoded_arrtyp(decoded_dt_ptr->getId());
        verify_val(arrtyp == decoded_arrtyp, true, "DataType::decode", __LINE__, __FILE__);
        verify_val(decoded_arrtyp.getArrayNDims(), ARRAY1_RANK, "DataType::decode", __LINE__, __FILE__);

        delete decoded_dt_ptr;

        //
        // Test with IntType
        //

        // Create an int datatype
        IntType inttyp(PredType::NATIVE_UINT);

        // Encode the array type in its buffer
        inttyp.encode();

        // Verify that encoding had been done
        verify_val(inttyp.hasBinaryDesc(), true, "DataType::encode", __LINE__, __FILE__);

        // Create an IntType instance from the decoded pointer and verify it
        IntType *  decoded_int_ptr(static_cast<IntType *>(inttyp.decode()));
        H5T_sign_t int_sign = decoded_int_ptr->getSign();
        verify_val(int_sign, H5T_SGN_NONE, "DataType::decode", __LINE__, __FILE__);
        verify_val(inttyp == *decoded_int_ptr, true, "DataType::decode", __LINE__, __FILE__);

        delete decoded_int_ptr;

        //
        // Test decoding FloatType by way of DataType*
        //

        // Create a float datatype
        FloatType flttyp(PredType::NATIVE_FLOAT);

        // Encode the float type in its buffer
        flttyp.encode();

        // Verify that encoding had been done
        verify_val(flttyp.hasBinaryDesc(), true, "DataType::encode", __LINE__, __FILE__);

        // Decode the array type's buffer
        DataType *decoded_flt_ptr(flttyp.decode());

        // Create a IntType instance from the decoded pointer and verify it
        FloatType decoded_flttyp(decoded_flt_ptr->getId());
        verify_val(flttyp == decoded_flttyp, true, "DataType::decode", __LINE__, __FILE__);

        // H5std_string norm_string;
        // H5T_norm_t mant_norm = decoded_flttyp.getNorm(norm_string);
        // verify_val(decoded_flttyp.isVariableStr(), true, "DataType::decode", __LINE__, __FILE__);

        delete decoded_flt_ptr;

        PASSED();
    }
    catch (Exception &E) {
        issue_fail_msg("test_encode_decode", __LINE__, __FILE__, E.getCDetailMsg());
    }
}

/****************************************************************
**
**  test_types(): Main datatypes testing routine.
**
****************************************************************/
extern "C" void
test_types()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing Generic Data Types\n"));

    // Test basic datatypes
    test_classes();
    test_copy();
    test_query();
    test_transient();
    test_named();
    test_encode_decode();

} // test_types()

/*-------------------------------------------------------------------------
 * Function:    cleanup_types
 *
 * Purpose:     Cleanup temporary test files
 *
 * Return:      none
 *
 * Programmer:  Quincey Koziol
 *              September 10, 1999
 *
 *-------------------------------------------------------------------------
 */
extern "C" void
cleanup_types()
{
    for (int i = 0; i < 3; i++)
        HDremove(FILENAME[i]);
} // cleanup_types
