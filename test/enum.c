/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *      Test enum datatypes
 */

#include "h5test.h"

/* Convenience macro for inserting enum values */
#define CPTR(VAR, CONST) ((VAR) = (CONST), &(VAR))

static const char *FILENAME[] = {"enum1", NULL};

typedef enum { E1_RED, E1_GREEN, E1_BLUE, E1_WHITE, E1_BLACK } c_e1;

/*-------------------------------------------------------------------------
 * Function:    test_named
 *
 * Purpose:     Create an enumeration data type and store it in the file
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_named(hid_t file)
{
    hid_t       tid = H5I_INVALID_HID;
    hid_t       gid = H5I_INVALID_HID;
    c_e1        val;
    signed char val8;

    TESTING("named enumeration types");
    if ((gid = H5Gcreate2(file, "test_named", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* A native integer */
    if ((tid = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "RED", CPTR(val, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "GREEN", CPTR(val, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLUE", CPTR(val, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "WHITE", CPTR(val, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLACK", CPTR(val, E1_BLACK)) < 0)
        TEST_ERROR;
    if (H5Tcommit2(gid, "e1_a", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* A smaller type */
    if ((tid = H5Tcreate(H5T_ENUM, (size_t)1)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "RED", CPTR(val8, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "GREEN", CPTR(val8, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLUE", CPTR(val8, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "WHITE", CPTR(val8, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLACK", CPTR(val8, E1_BLACK)) < 0)
        TEST_ERROR;
    if (H5Tcommit2(gid, "e1_b", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    /* A non-native type */
    if (H5T_ORDER_BE == H5Tget_order(H5T_NATIVE_INT)) {
        if ((tid = H5Tenum_create(H5T_STD_U8LE)) < 0)
            TEST_ERROR;
    }
    else {
        if ((tid = H5Tenum_create(H5T_STD_U8BE)) < 0)
            TEST_ERROR;
    }
    if (H5Tenum_insert(tid, "RED", CPTR(val8, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "GREEN", CPTR(val8, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLUE", CPTR(val8, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "WHITE", CPTR(val8, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLACK", CPTR(val8, E1_BLACK)) < 0)
        TEST_ERROR;
    if (H5Tcommit2(gid, "e1_c", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    if (H5Gclose(gid) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid);
        H5Gclose(gid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv
 *
 * Purpose:     Tests writing and read data
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_conv(hid_t file)
{
    hid_t gid = H5I_INVALID_HID;
    hid_t tid = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t did = H5I_INVALID_HID;
    c_e1  val;
    /* Some values are out of range for testing. The library should accept them */
    c_e1    data1[] = {E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE, E1_WHITE, E1_BLACK,
                    E1_GREEN, E1_BLUE,  E1_RED,   E1_RED,   E1_BLUE,  E1_GREEN, E1_BLACK,
                    E1_WHITE, E1_RED,   E1_WHITE, (c_e1)0,  (c_e1)-1, (c_e1)-2};
    c_e1    data2[NELMTS(data1)];
    short   data_short[NELMTS(data1)];
    int     data_int[NELMTS(data1)];
    double  data_double[NELMTS(data1)];
    hsize_t ds_size = NELMTS(data1);
    size_t  i;

    TESTING("enumeration conversions");

    if ((gid = H5Gcreate2(file, "test_conv", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((tid = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "RED", CPTR(val, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "GREEN", CPTR(val, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLUE", CPTR(val, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "WHITE", CPTR(val, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLACK", CPTR(val, E1_BLACK)) < 0)
        TEST_ERROR;

    if ((sid = H5Screate_simple(1, &ds_size, NULL)) < 0)
        TEST_ERROR;

    /***************************************
     *    Dataset of enumeration type
     ***************************************/
    /* Create a dataset of enum type and write enum data to it */
    if ((did = H5Dcreate2(gid, "color_table1", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(did, tid, sid, sid, H5P_DEFAULT, data1) < 0)
        TEST_ERROR;

    /* Test reading back the data with no conversion */
    if (H5Dread(did, tid, sid, sid, H5P_DEFAULT, data2) < 0)
        TEST_ERROR;

    for (i = 0; i < ds_size; i++)
        if (data1[i] != data2[i]) {
            H5_FAILED();
            printf("    1. data1[%zu]=%d, data2[%zu]=%d (should be same)\n", i, (int)data1[i], i,
                   (int)data2[i]);
            goto error;
        }

    /* Test converting the data to integer. Read enum data back as integer */
    if (H5Dread(did, H5T_NATIVE_SHORT, sid, sid, H5P_DEFAULT, data_short) < 0)
        TEST_ERROR;

    for (i = 0; i < ds_size; i++)
        if ((int)data1[i] != (int)data_short[i]) {
            H5_FAILED();
            printf("    2. data1[%zu]=%d, data_short[%zu]=%d (should be same)\n", i, (int)data1[i], i,
                   (int)data_short[i]);
            goto error;
        }

    /* Test converting the data to floating number. Read enum data back as floating number */
    if (H5Dread(did, H5T_NATIVE_DOUBLE, sid, sid, H5P_DEFAULT, data_double) < 0)
        TEST_ERROR;

    for (i = 0; i < ds_size; i++)
        if ((int)data1[i] != (int)data_double[i]) {
            H5_FAILED();
            printf("    3. data1[%zu]=%d, data_double[%zu]=%d (should be same)\n", i, (int)data1[i], i,
                   (int)data_double[i]);
            goto error;
        }

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /***************************************
     *    Dataset of integer type
     ***************************************/
    /* Create a dataset of native integer and write enum data to it */
    if ((did = H5Dcreate2(gid, "color_table2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;

    if (H5Dwrite(did, tid, sid, sid, H5P_DEFAULT, data1) < 0)
        TEST_ERROR;

    /* Test reading back the data with no conversion */
    if (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data_int) < 0)
        TEST_ERROR;

    for (i = 0; i < ds_size; i++)
        if ((int)data1[i] != data_int[i]) {
            H5_FAILED();
            printf("    4. data1[%zu]=%d, data_int[%zu]=%d (should be same)\n", i, (int)data1[i], i,
                   data_int[i]);
            goto error;
        }

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /***************************************
     *    Dataset of double type
     ***************************************/
    /* Create a dataset of native double and write enum data to it */
    if ((did = H5Dcreate2(gid, "color_table3", H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, H5P_DEFAULT,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dwrite(did, tid, sid, sid, H5P_DEFAULT, data1) < 0)
        TEST_ERROR;

    /* Test reading back the data with no conversion */
    if (H5Dread(did, H5T_NATIVE_DOUBLE, sid, sid, H5P_DEFAULT, data_double) < 0)
        TEST_ERROR;

    for (i = 0; i < ds_size; i++)
        if ((int)data1[i] != (int)data_double[i]) {
            H5_FAILED();
            printf("    5. data1[%zu]=%d, data_double[%zu]=%d (should be same)\n", i, (int)data1[i], i,
                   (int)data_double[i]);
            goto error;
        }

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Tclose(tid) < 0)
        TEST_ERROR;
    if (H5Gclose(gid) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Tclose(tid);
        H5Gclose(gid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_tr1
 *
 * Purpose:     Writes enumerated data to a dataset which requires
 *              translation. Both memory and file data types use native
 *              integers but the file type has a different mapping between
 *              the integers and symbols.
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_tr1(hid_t file)
{
    hid_t   gid     = H5I_INVALID_HID;
    hid_t   m_tid   = H5I_INVALID_HID;
    hid_t   f_tid   = H5I_INVALID_HID;
    hid_t   sid     = H5I_INVALID_HID;
    hid_t   did     = H5I_INVALID_HID;
    hsize_t ds_size = 10;
    c_e1    eval;
    int     ival;
    c_e1    data1[10] = {E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
                      E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED};
    c_e1    data2[10];

    TESTING("O(1) conversions");

    if ((gid = H5Gcreate2(file, "test_tr1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((m_tid = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "RED", CPTR(eval, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "GREEN", CPTR(eval, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "BLUE", CPTR(eval, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "WHITE", CPTR(eval, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "BLACK", CPTR(eval, E1_BLACK)) < 0)
        TEST_ERROR;

    if ((f_tid = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "RED", CPTR(ival, 105)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "GREEN", CPTR(ival, 104)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "BLUE", CPTR(ival, 103)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "WHITE", CPTR(ival, 102)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "BLACK", CPTR(ival, 101)) < 0)
        TEST_ERROR;

    if ((sid = H5Screate_simple(1, &ds_size, NULL)) < 0)
        TEST_ERROR;
    if ((did = H5Dcreate2(gid, "color_table", f_tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(did, m_tid, sid, sid, H5P_DEFAULT, data1) < 0)
        TEST_ERROR;
    if (H5Dread(did, m_tid, sid, sid, H5P_DEFAULT, data2) < 0)
        TEST_ERROR;

    for (size_t i = 0; i < ds_size; i++)
        if (data1[i] != data2[i]) {
            H5_FAILED();
            printf("    data1[%zu]=%d, data2[%zu]=%d (should be same)\n", i, (int)data1[i], i, (int)data2[i]);
            goto error;
        }

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Tclose(m_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(f_tid) < 0)
        TEST_ERROR;
    if (H5Gclose(gid) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Tclose(m_tid);
        H5Tclose(f_tid);
        H5Gclose(gid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_tr2
 *
 * Purpose:     Tests conversions that use the O(log N) lookup function
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_tr2(hid_t file)
{
    hid_t   gid     = H5I_INVALID_HID;
    hid_t   m_tid   = H5I_INVALID_HID;
    hid_t   f_tid   = H5I_INVALID_HID;
    hid_t   sid     = H5I_INVALID_HID;
    hid_t   did     = H5I_INVALID_HID;
    hsize_t ds_size = 10;
    size_t  i;
    c_e1    val1;
    int     val2;
    c_e1    data1[10] = {E1_RED,   E1_GREEN, E1_BLUE,  E1_GREEN, E1_WHITE,
                      E1_WHITE, E1_BLACK, E1_GREEN, E1_BLUE,  E1_RED};
    c_e1    data2[10];

    TESTING("O(log N) conversions");

    if ((gid = H5Gcreate2(file, "test_tr2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((m_tid = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "RED", CPTR(val1, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "GREEN", CPTR(val1, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "BLUE", CPTR(val1, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "WHITE", CPTR(val1, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(m_tid, "BLACK", CPTR(val1, E1_BLACK)) < 0)
        TEST_ERROR;

    if ((f_tid = H5Tcreate(H5T_ENUM, sizeof(int))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "RED", CPTR(val2, 1050)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "GREEN", CPTR(val2, 1040)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "BLUE", CPTR(val2, 1030)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "WHITE", CPTR(val2, 1020)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(f_tid, "BLACK", CPTR(val2, 1010)) < 0)
        TEST_ERROR;

    if ((sid = H5Screate_simple(1, &ds_size, NULL)) < 0)
        TEST_ERROR;
    if ((did = H5Dcreate2(gid, "color_table", f_tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(did, m_tid, sid, sid, H5P_DEFAULT, data1) < 0)
        TEST_ERROR;
    if (H5Dread(did, m_tid, sid, sid, H5P_DEFAULT, data2) < 0)
        TEST_ERROR;

    for (i = 0; i < ds_size; i++)
        if (data1[i] != data2[i]) {
            H5_FAILED();
            printf("    data1[%zu]=%d, data2[%zu]=%d (should be same)\n", i, (int)data1[i], i, (int)data2[i]);
            goto error;
        }

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Tclose(m_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(f_tid) < 0)
        TEST_ERROR;
    if (H5Gclose(gid) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Tclose(m_tid);
        H5Tclose(f_tid);
        H5Gclose(gid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_value_dsnt_exist
 *
 * Purpose:     Create an enumeration datatype with "gaps in values"
 *              and then request a name of non-existing value within
 *              an existing range by calling H5Tenum_nameof function.
 *              Function should fail instead of succeeding and returning
 *              a name of one of the existing values.
 *              Request a value by supplying non-existing name by calling
 *              H5Tenum_nameof function. Function should fail.
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_value_dsnt_exist(void)
{

    hid_t       tid = H5I_INVALID_HID;
    int         val;
    char        name[32];
    size_t      size         = 32;
    const int   BAD_VALUES[] = {0, 3, 11};
    const int   N_BAD_VALUES = 3;
    const char *BAD_NAMES[]  = {"SAX", "TEEN", "A"};
    const int   N_BAD_NAMES  = 3;
    herr_t      ret;

    TESTING("for non-existing name and value");

    /* Create an empty enum datatype */
    if ((tid = H5Tenum_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    /* These calls should fail, since no members exist yet */
    H5E_BEGIN_TRY
    {
        ret = H5Tenum_valueof(tid, "SAX", &val);
    }
    H5E_END_TRY
    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Tenum_valueof should not pass with a non-existing name");

    val = 3;
    H5E_BEGIN_TRY
    {
        ret = H5Tenum_nameof(tid, &val, name, size);
    }
    H5E_END_TRY
    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Tenum_nameof should not pass with a non-existing value");

    /* Insert some enum values */
    val = 2;
    if (H5Tenum_insert(tid, "TWO", (int *)&val) < 0)
        TEST_ERROR;
    val = 6;
    if (H5Tenum_insert(tid, "SIX", (int *)&val) < 0)
        TEST_ERROR;
    val = 10;
    if (H5Tenum_insert(tid, "TEN", (int *)&val) < 0)
        TEST_ERROR;

    /* Check that H5Tenum_nameof() fails with non-existing values */
    for (int i = 0; i < N_BAD_VALUES; i++) {
        H5E_BEGIN_TRY
        {
            ret = H5Tenum_nameof(tid, &BAD_VALUES[i], name, size);
        }
        H5E_END_TRY
        if (ret >= 0) {
            H5_FAILED();
            printf("Bad value: %d -- ", BAD_VALUES[i]);
            PUTS_ERROR("H5Tenum_nameof should not pass with a non-existing value");
        }
    }

    /* Check that H5Tenum_valueof() fails with non-existing names */
    for (int i = 0; i < N_BAD_NAMES; i++) {
        H5E_BEGIN_TRY
        {
            ret = H5Tenum_valueof(tid, BAD_NAMES[i], &val);
        }
        H5E_END_TRY
        if (ret >= 0) {
            H5_FAILED();
            printf("Bad name: %s -- ", BAD_NAMES[i]);
            PUTS_ERROR("H5Tenum_valueof should not pass with a non-existing name");
        }
    }

    if (H5Tclose(tid) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_funcs
 *
 * Purpose:     Create an enumeration data type and test whether setters
 *              and getters work appropriately
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_funcs(void)
{
    hid_t      tid = H5I_INVALID_HID;
    c_e1       val;
    size_t     size;
    H5T_pad_t  inpad;
    H5T_cset_t cset;
    herr_t     ret;

    TESTING("setters and getters with enumeration types");

    /* Create an enum type for testing */
    if ((tid = H5Tcreate(H5T_ENUM, sizeof(c_e1))) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "RED", CPTR(val, E1_RED)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "GREEN", CPTR(val, E1_GREEN)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLUE", CPTR(val, E1_BLUE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "WHITE", CPTR(val, E1_WHITE)) < 0)
        TEST_ERROR;
    if (H5Tenum_insert(tid, "BLACK", CPTR(val, E1_BLACK)) < 0)
        TEST_ERROR;

    /* These functions should work with enum datatypes */
    if (H5Tget_precision(tid) == 0)
        TEST_ERROR;
    if (H5Tget_size(tid) == 0)
        TEST_ERROR;
    if (H5Tget_offset(tid) < 0)
        TEST_ERROR;
    if (H5Tget_sign(tid) < 0)
        TEST_ERROR;
    if (H5Tget_super(tid) < 0)
        TEST_ERROR;

    /* These functions should FAIL with enum datatypes */
    H5E_BEGIN_TRY
    {
        ret = H5Tset_pad(tid, H5T_PAD_ZERO, H5T_PAD_ONE);
    }
    H5E_END_TRY
    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Tset_pad should not work with enum types");

    H5E_BEGIN_TRY
    {
        size = H5Tget_ebias(tid);
    }
    H5E_END_TRY
    if (size > 0)
        FAIL_PUTS_ERROR("H5Tget_ebias should not work with enum types");

    H5E_BEGIN_TRY
    {
        inpad = H5Tget_inpad(tid);
    }
    H5E_END_TRY
    if (inpad > -1)
        FAIL_PUTS_ERROR("H5Tget_inpad should not work with enum types");

    H5E_BEGIN_TRY
    {
        cset = H5Tget_cset(tid);
    }
    H5E_END_TRY
    if (cset > -1)
        FAIL_PUTS_ERROR("H5Tget_cset should not work with enum types");

    size = 16;
    H5E_BEGIN_TRY
    {
        ret = H5Tset_offset(tid, size);
    }
    H5E_END_TRY
    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Tset_offset should not work with enum types");

    H5E_BEGIN_TRY
    {
        ret = H5Tset_order(tid, H5T_ORDER_BE);
    }
    H5E_END_TRY
    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Tset_order should not work with enum types");

    if (H5Tclose(tid) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_copying_empty_enum
 *
 * Purpose:     Test that copying an empty enum works, including implicitly
 *              when copying compound datatypes containing empty enums
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_compound_insert_empty_enum(void)
{
    hid_t  enum_id = H5I_INVALID_HID;
    hid_t  cmpd_id = H5I_INVALID_HID;
    hid_t  copy_id = H5I_INVALID_HID;
    size_t size;

    TESTING("copying empty enums works");

    /* Create an empty enum */
    if ((enum_id = H5Tenum_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    /* Copy the empty enum */
    if ((copy_id = H5Tcopy(enum_id)) < 0)
        TEST_ERROR;
    if (H5Tclose(copy_id) < 0)
        TEST_ERROR;

    /* Create a compound datatype containing the empty enum */
    size = H5Tget_size(H5T_NATIVE_LONG);
    if ((cmpd_id = H5Tcreate(H5T_COMPOUND, size)) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_id, "empty_enum", 0, enum_id))
        TEST_ERROR;

    /* Create a copy of the compound datatype */
    if ((copy_id = H5Tcopy(cmpd_id)) < 0)
        TEST_ERROR;
    if (H5Tclose(copy_id) < 0)
        TEST_ERROR;

    if (H5Tclose(enum_id) < 0)
        TEST_ERROR;
    if (H5Tclose(cmpd_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(enum_id);
        H5Tclose(cmpd_id);
        H5Tclose(copy_id);
    }
    H5E_END_TRY
    return 1;
}

int
main(void)
{
    hid_t fapl_id = H5I_INVALID_HID;
    hid_t fid     = H5I_INVALID_HID;
    char  name[1024];
    int   nerrors = 0;

    h5_reset();
    fapl_id = h5_fileaccess();

    /* Create the file */
    h5_fixname(FILENAME[0], fapl_id, name, sizeof name);
    if ((fid = H5Fcreate(name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        goto error;

    /* Tests */
    nerrors += test_named(fid);
    nerrors += test_conv(fid);
    nerrors += test_tr1(fid);
    nerrors += test_tr2(fid);
    nerrors += test_value_dsnt_exist();
    nerrors += test_funcs();
    nerrors += test_compound_insert_empty_enum();

    if (H5Fclose(fid) < 0)
        goto error;

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl_id) < 0 ? 1 : 0);

    if (nerrors)
        goto error;

    puts("All enum tests passed.");
    h5_cleanup(FILENAME, fapl_id);

    return EXIT_SUCCESS;

error:
    puts("*** ENUM TESTS FAILED ***");
    return EXIT_FAILURE;
}
