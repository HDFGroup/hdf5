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

#include "H5_api_datatype_test.h"

/*
 * Disable tests that currently compromise internal HDF5 integrity.
 */
#define PROBLEMATIC_TESTS

static int test_create_committed_datatype(void);
static int test_create_committed_datatype_invalid_params(void);
static int test_create_anonymous_committed_datatype(void);
static int test_create_anonymous_committed_datatype_invalid_params(void);
#ifndef PROBLEMATIC_TESTS
static int test_create_committed_datatype_empty_types(void);
#endif
static int test_recommit_committed_type(void);
static int test_open_committed_datatype(void);
static int test_open_committed_datatype_invalid_params(void);
static int test_reopen_committed_datatype_indirect(void);
static int test_close_committed_datatype_invalid_id(void);
static int test_datatype_property_lists(void);
static int test_create_dataset_with_committed_type(void);
static int test_create_attribute_with_committed_type(void);
static int test_delete_committed_type(void);
static int test_resurrect_datatype(void);
static int test_flush_committed_datatype(void);
static int test_flush_committed_datatype_invalid_params(void);
static int test_refresh_committed_datatype(void);
static int test_refresh_committed_datatype_invalid_params(void);
#ifndef PROBLEMATIC_TESTS
static int test_cant_commit_predefined(void);
#endif
static int test_cant_modify_committed_type(void);

/*
 * The array of datatype tests to be performed.
 */
static int (*datatype_tests[])(void) = {
    test_create_committed_datatype,
    test_create_committed_datatype_invalid_params,
    test_create_anonymous_committed_datatype,
    test_create_anonymous_committed_datatype_invalid_params,
#ifndef PROBLEMATIC_TESTS
    test_create_committed_datatype_empty_types,
#endif
    test_recommit_committed_type,
    test_open_committed_datatype,
    test_open_committed_datatype_invalid_params,
    test_reopen_committed_datatype_indirect,
    test_close_committed_datatype_invalid_id,
    test_datatype_property_lists,
    test_create_dataset_with_committed_type,
    test_create_attribute_with_committed_type,
    test_delete_committed_type,
    test_resurrect_datatype,
    test_flush_committed_datatype,
    test_flush_committed_datatype_invalid_params,
    test_refresh_committed_datatype,
    test_refresh_committed_datatype_invalid_params,
#ifndef PROBLEMATIC_TESTS
    test_cant_commit_predefined,
#endif
    test_cant_modify_committed_type,
};

/*
 * A test to check that a committed datatype can be created.
 */
static int
test_create_committed_datatype(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t type_id = H5I_INVALID_HID;

    TESTING("creation of a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_CREATE_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_CREATE_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype to commit\n");
        goto error;
    }

    if (H5Tcommit2(group_id, DATATYPE_CREATE_TEST_TYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATATYPE_CREATE_TEST_TYPE_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a committed datatype can't be
 * created when H5Tcommit2 is passed invalid parameters.
 */
static int
test_create_committed_datatype_invalid_params(void)
{
    herr_t err_ret         = -1;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  type_id = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Tcommit2 with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_CREATE_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATATYPE_CREATE_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype to commit\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Tcommit2_invalid_loc_id)
        {
            TESTING_2("H5Tcommit2 with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(H5I_INVALID_HID, DATATYPE_CREATE_INVALID_PARAMS_TEST_TYPE_NAME, type_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid loc_id!\n");
                PART_ERROR(H5Tcommit2_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Tcommit2_invalid_loc_id);

        PART_BEGIN(H5Tcommit2_invalid_type_name)
        {
            TESTING_2("H5Tcommit2 with an invalid datatype name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, NULL, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid datatype name!\n");
                PART_ERROR(H5Tcommit2_invalid_type_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, "", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid datatype name!\n");
                PART_ERROR(H5Tcommit2_invalid_type_name);
            }

            PASSED();
        }
        PART_END(H5Tcommit2_invalid_type_name);

        PART_BEGIN(H5Tcommit2_invalid_type_id)
        {
            TESTING_2("H5Tcommit2 with an invalid datatype ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, DATATYPE_CREATE_INVALID_PARAMS_TEST_TYPE_NAME, H5I_INVALID_HID,
                                     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid datatype ID!\n");
                PART_ERROR(H5Tcommit2_invalid_type_id);
            }

            PASSED();
        }
        PART_END(H5Tcommit2_invalid_type_id);

        PART_BEGIN(H5Tcommit2_invalid_lcpl)
        {
            TESTING_2("H5Tcommit2 with an invalid LCPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, DATATYPE_CREATE_INVALID_PARAMS_TEST_TYPE_NAME, type_id,
                                     H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid LCPL!\n");
                PART_ERROR(H5Tcommit2_invalid_lcpl);
            }

            PASSED();
        }
        PART_END(H5Tcommit2_invalid_lcpl);

        PART_BEGIN(H5Tcommit2_invalid_tcpl)
        {
            TESTING_2("H5Tcommit2 with an invalid TCPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, DATATYPE_CREATE_INVALID_PARAMS_TEST_TYPE_NAME, type_id,
                                     H5P_DEFAULT, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid TCPL!\n");
                PART_ERROR(H5Tcommit2_invalid_tcpl);
            }

            PASSED();
        }
        PART_END(H5Tcommit2_invalid_tcpl);

        PART_BEGIN(H5Tcommit2_invalid_tapl)
        {
            TESTING_2("H5Tcommit2 with an invalid TAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, DATATYPE_CREATE_INVALID_PARAMS_TEST_TYPE_NAME, type_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit2 succeeded with an invalid TAPL!\n");
                PART_ERROR(H5Tcommit2_invalid_tapl);
            }

            PASSED();
        }
        PART_END(H5Tcommit2_invalid_tapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an anonymous committed datatype
 * can be created with H5Tcommit_anon.
 */
static int
test_create_anonymous_committed_datatype(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t type_id = H5I_INVALID_HID;

    TESTING("creation of anonymous committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_CREATE_ANONYMOUS_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_CREATE_ANONYMOUS_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit_anon(group_id, type_id, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit anonymous datatype\n");
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a committed datatype can't be
 * created when H5Tcommit_anon is passed invalid parameters.
 */
static int
test_create_anonymous_committed_datatype_invalid_params(void)
{
    herr_t err_ret         = -1;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  type_id = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Tcommit_anon with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_CREATE_ANONYMOUS_INVALID_PARAMS_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATATYPE_CREATE_ANONYMOUS_INVALID_PARAMS_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Tcommit_anon_invalid_loc_id)
        {
            TESTING_2("H5Tcommit_anon with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit_anon(H5I_INVALID_HID, type_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit_anon succeeded with an invalid loc_id!\n");
                PART_ERROR(H5Tcommit_anon_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Tcommit_anon_invalid_loc_id);

        PART_BEGIN(H5Tcommit_anon_invalid_type_id)
        {
            TESTING_2("H5Tcommit_anon with an invalid datatype ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit_anon(group_id, H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit_anon succeeded with an invalid datatype ID!\n");
                PART_ERROR(H5Tcommit_anon_invalid_type_id);
            }

            PASSED();
        }
        PART_END(H5Tcommit_anon_invalid_type_id);

        PART_BEGIN(H5Tcommit_anon_invalid_tcpl)
        {
            TESTING_2("H5Tcommit_anon with an invalid TCPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit_anon(group_id, type_id, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit_anon succeeded with an invalid TCPL!\n");
                PART_ERROR(H5Tcommit_anon_invalid_tcpl);
            }

            PASSED();
        }
        PART_END(H5Tcommit_anon_invalid_tcpl);

        PART_BEGIN(H5Tcommit_anon_invalid_tapl)
        {
            TESTING_2("H5Tcommit_anon with an invalid TAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit_anon(group_id, type_id, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Tcommit_anon succeeded with an invalid TAPL!\n");
                PART_ERROR(H5Tcommit_anon_invalid_tapl);
            }

            PASSED();
        }
        PART_END(H5Tcommit_anon_invalid_tapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that committing a datatype fails with empty
 * compound and enum datatypes.
 */
#ifndef PROBLEMATIC_TESTS
static int
test_create_committed_datatype_empty_types(void)
{
    herr_t err_ret         = FAIL;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  type_id = H5I_INVALID_HID;

    TESTING_MULTIPART("creation of committed datatype with empty types");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_CREATE_EMPTY_TYPES_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_CREATE_EMPTY_TYPES_TEST_GROUP_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Tcommit_empty_compound_type)
        {
            TESTING_2("creation of committed datatype with empty compound type");

            if ((type_id = H5Tcreate(H5T_COMPOUND, (size_t)32)) < 0) {
                H5_FAILED();
                printf("    failed to create compound type\n");
                PART_ERROR(H5Tcommit_empty_compound_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, DATATYPE_CREATE_EMPTY_TYPES_TEST_CMPD_TYPE_NAME, type_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    committed empty compound datatype!\n");
                PART_ERROR(H5Tcommit_empty_compound_type);
            }

            /* Add a field to the compound datatype */
            if (H5Tinsert(type_id, "a", (size_t)0, H5T_NATIVE_INT) < 0) {
                H5_FAILED();
                printf("    failed to insert field into compound datatype\n");
                PART_ERROR(H5Tcommit_empty_compound_type);
            }

            /* Attempt to commit the now non-empty compound datatype */
            if (H5Tcommit2(group_id, DATATYPE_CREATE_EMPTY_TYPES_TEST_CMPD_TYPE_NAME, type_id, H5P_DEFAULT,
                           H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit non-empty compound datatype\n");
                PART_ERROR(H5Tcommit_empty_compound_type);
            }

            PASSED();
        }
        PART_END(H5Tcommit_empty_compound_type);

        if (type_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(type_id);
            }
            H5E_END_TRY
            type_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Tcommit_empty_enum_type)
        {
            int enum_val = 1;

            TESTING_2("creation of committed datatype with empty enum type");

            if ((type_id = H5Tenum_create(H5T_NATIVE_INT)) < 0) {
                H5_FAILED();
                printf("    failed to create enum type\n");
                PART_ERROR(H5Tcommit_empty_enum_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Tcommit2(group_id, DATATYPE_CREATE_EMPTY_TYPES_TEST_ENUM_TYPE_NAME, type_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    committed empty enum datatype!\n");
                PART_ERROR(H5Tcommit_empty_enum_type);
            }

            /* Add a field to the enum datatype */
            if (H5Tenum_insert(type_id, "a", &enum_val) < 0) {
                H5_FAILED();
                printf("    failed to insert field into enum datatype\n");
                PART_ERROR(H5Tcommit_empty_enum_type);
            }

            /* Attempt to commit the now non-empty enum datatype */
            if (H5Tcommit2(group_id, DATATYPE_CREATE_EMPTY_TYPES_TEST_ENUM_TYPE_NAME, type_id, H5P_DEFAULT,
                           H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit non-empty enum datatype\n");
                PART_ERROR(H5Tcommit_empty_enum_type);
            }

            PASSED();
        }
        PART_END(H5Tcommit_empty_enum_type);

        if (type_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(type_id);
            }
            H5E_END_TRY
            type_id = H5I_INVALID_HID;
        }
    }
    END_MULTIPART;

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}
#endif

/*
 * A test to check that a committed datatype can't be re-committed.
 */
static int
test_recommit_committed_type(void)
{
    htri_t is_committed = false;
    herr_t err_ret;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  type_id = H5I_INVALID_HID;

    TESTING("inability to re-commit a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, RECOMMIT_COMMITTED_TYPE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", RECOMMIT_COMMITTED_TYPE_TEST_GROUP_NAME);
        goto error;
    }

    /* Copy a predefined datatype and commit the copy */
    if ((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) {
        H5_FAILED();
        printf("    failed to copy predefined integer datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, "native_int", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    failed to commit datatype\n");
        goto error;
    }

    if ((is_committed = H5Tcommitted(type_id)) < 0) {
        H5_FAILED();
        printf("    failed to determine if datatype is committed\n");
        goto error;
    }

    if (!is_committed) {
        H5_FAILED();
        printf("    H5Tcommitted() returned false!\n");
        goto error;
    }

    /* We should not be able to re-commit a committed type */
    H5E_BEGIN_TRY
    {
        err_ret = H5Tcommit2(group_id, "native_int", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    re-committed an already committed datatype!\n");
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a committed datatype
 * can be opened using H5Topen2.
 */
static int
test_open_committed_datatype(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t type_id = H5I_INVALID_HID;

    TESTING("H5Topen2");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_OPEN_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_OPEN_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype to commit\n");
        goto error;
    }

    if (H5Tcommit2(group_id, DATATYPE_OPEN_TEST_TYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATATYPE_OPEN_TEST_TYPE_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;

    if ((type_id = H5Topen2(group_id, DATATYPE_OPEN_TEST_TYPE_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open committed datatype '%s'\n", DATATYPE_OPEN_TEST_TYPE_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a committed datatype can't
 * be opened when H5Topen2 is passed invalid parameters.
 */
static int
test_open_committed_datatype_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t type_id = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Topen2 with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATATYPE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype to commit\n");
        goto error;
    }

    if (H5Tcommit2(group_id, DATATYPE_OPEN_INVALID_PARAMS_TEST_TYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATATYPE_OPEN_INVALID_PARAMS_TEST_TYPE_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Topen2_invalid_loc_id)
        {
            TESTING_2("H5Topen2 with an invalid location ID");

            H5E_BEGIN_TRY
            {
                type_id = H5Topen2(H5I_INVALID_HID, DATATYPE_OPEN_INVALID_PARAMS_TEST_TYPE_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (type_id >= 0) {
                H5_FAILED();
                printf("    opened committed datatype with an invalid location ID!\n");
                H5Tclose(type_id);
                PART_ERROR(H5Topen2_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Topen2_invalid_loc_id);

        PART_BEGIN(H5Topen2_invalid_type_name)
        {
            TESTING_2("H5Topen2 with an invalid datatype name");

            H5E_BEGIN_TRY
            {
                type_id = H5Topen2(group_id, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (type_id >= 0) {
                H5_FAILED();
                printf("    opened committed datatype with an invalid datatype name!\n");
                H5Tclose(type_id);
                PART_ERROR(H5Topen2_invalid_type_name);
            }

            H5E_BEGIN_TRY
            {
                type_id = H5Topen2(group_id, "", H5P_DEFAULT);
            }
            H5E_END_TRY

            if (type_id >= 0) {
                H5_FAILED();
                printf("    opened committed datatype with an invalid datatype name!\n");
                H5Tclose(type_id);
                PART_ERROR(H5Topen2_invalid_type_name);
            }

            PASSED();
        }
        PART_END(H5Topen2_invalid_type_name);

        PART_BEGIN(H5Topen2_invalid_tapl)
        {
            TESTING_2("H5Topen2 with an invalid TAPL");

            H5E_BEGIN_TRY
            {
                type_id = H5Topen2(group_id, DATATYPE_OPEN_INVALID_PARAMS_TEST_TYPE_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (type_id >= 0) {
                H5_FAILED();
                printf("    opened committed datatype with an invalid TAPL!\n");
                H5Tclose(type_id);
                PART_ERROR(H5Topen2_invalid_tapl);
            }

            PASSED();
        }
        PART_END(H5Topen2_invalid_tapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that open named datatypes can be reopened indirectly
 * through H5Dget_type without causing problems.
 */
static int
test_reopen_committed_datatype_indirect(void)
{
    size_t dt_size         = 0;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  dset_id         = H5I_INVALID_HID;
    hid_t  type_id = H5I_INVALID_HID, reopened_type_id = H5I_INVALID_HID;
    hid_t  strtype  = H5I_INVALID_HID;
    hid_t  space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("reopening open committed datatypes using H5Dget_type");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or stored datatype aren't supported with "
               "this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_REOPEN_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_REOPEN_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(DATATYPE_REOPEN_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(reopen_compound_type)
        {
            TESTING_2("re-open of compound datatype");

            if ((strtype = H5Tcopy(H5T_C_S1)) < 0) {
                H5_FAILED();
                printf("    failed to copy C-string datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            if (H5Tset_size(strtype, H5T_VARIABLE) < 0) {
                H5_FAILED();
                printf("    failed to set string datatype's size to variable\n");
                PART_ERROR(reopen_compound_type);
            }

            if ((type_id = H5Tcreate(H5T_COMPOUND, sizeof(char *))) < 0) {
                H5_FAILED();
                printf("    failed to create compound datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            if (H5Tinsert(type_id, "vlstr", (size_t)0, strtype) < 0) {
                H5_FAILED();
                printf("    failed to insert field into compound datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            if (H5Tclose(strtype) < 0) {
                H5_FAILED();
                printf("    failed to close string datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            /* Get size of compound type */
            if ((dt_size = H5Tget_size(type_id)) == 0) {
                H5_FAILED();
                printf("    failed to retrieve size of compound datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            /* Commit compound type and verify the size doesn't change */
            if (H5Tcommit2(group_id, "cmpd_type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit compound datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            if (dt_size != H5Tget_size(type_id)) {
                H5_FAILED();
                printf("    committing datatype caused its size to change!\n");
                PART_ERROR(reopen_compound_type);
            }

            /* Create dataset with compound type */
            if ((dset_id = H5Dcreate2(group_id, "cmpd_dset", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to create dataset using committed datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            /* Indirectly reopen type and verify that the size doesn't change */
            if ((reopened_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to re-open committed datatype using H5Dget_type\n");
                PART_ERROR(reopen_compound_type);
            }

            if (dt_size != H5Tget_size(reopened_type_id)) {
                H5_FAILED();
                printf("    size of re-opened datatype didn't match size of original datatype\n");
                PART_ERROR(reopen_compound_type);
            }

            PASSED();
        }
        PART_END(reopen_compound_type);

        H5E_BEGIN_TRY
        {
            H5Tclose(strtype);
            strtype = H5I_INVALID_HID;
            H5Tclose(type_id);
            type_id = H5I_INVALID_HID;
            H5Tclose(reopened_type_id);
            reopened_type_id = H5I_INVALID_HID;
            H5Dclose(dset_id);
            dset_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(reopen_enum_type)
        {
            int enum_value;

            TESTING_2("re-open of enum datatype");

            if ((type_id = H5Tenum_create(H5T_NATIVE_INT)) < 0) {
                H5_FAILED();
                printf("    failed to create enum datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            enum_value = 0;
            if (H5Tenum_insert(type_id, "val1", &enum_value) < 0) {
                H5_FAILED();
                printf("    failed to insert value into enum datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            enum_value = 1;
            if (H5Tenum_insert(type_id, "val2", &enum_value) < 0) {
                H5_FAILED();
                printf("    failed to insert value into enum datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            /* Get size of enum type */
            if ((dt_size = H5Tget_size(type_id)) == 0) {
                H5_FAILED();
                printf("    failed to retrieve size of enum datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            /* Commit enum type and verify the size doesn't change */
            if (H5Tcommit2(group_id, "enum_type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit enum datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            if (dt_size != H5Tget_size(type_id)) {
                H5_FAILED();
                printf("    committing datatype caused its size to change!\n");
                PART_ERROR(reopen_enum_type);
            }

            /* Create dataset with enum type */
            if ((dset_id = H5Dcreate2(group_id, "enum_dset", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to create dataset using committed datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            /* Indirectly reopen type and verify that the size doesn't change */
            if ((reopened_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to re-open committed datatype using H5Dget_type\n");
                PART_ERROR(reopen_enum_type);
            }

            if (dt_size != H5Tget_size(reopened_type_id)) {
                H5_FAILED();
                printf("    size of re-opened datatype didn't match size of original datatype\n");
                PART_ERROR(reopen_enum_type);
            }

            PASSED();
        }
        PART_END(reopen_enum_type);

        H5E_BEGIN_TRY
        {
            H5Tclose(type_id);
            type_id = H5I_INVALID_HID;
            H5Tclose(reopened_type_id);
            reopened_type_id = H5I_INVALID_HID;
            H5Dclose(dset_id);
            dset_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(reopen_vlen_type)
        {
            TESTING_2("reopen of a variable-length datatype");

            if ((type_id = H5Tvlen_create(H5T_NATIVE_INT)) < 0) {
                H5_FAILED();
                printf("    failed to create variable-length datatype\n");
                PART_ERROR(reopen_vlen_type);
            }

            /* Get size of variable-length type */
            if ((dt_size = H5Tget_size(type_id)) == 0) {
                H5_FAILED();
                printf("    failed to retrieve size of variable-length datatype\n");
                PART_ERROR(reopen_vlen_type);
            }

            /* Commit variable-length type and verify the size doesn't change */
            if (H5Tcommit2(group_id, "vlen_type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit variable-length datatype\n");
                PART_ERROR(reopen_vlen_type);
            }

            if (dt_size != H5Tget_size(type_id)) {
                H5_FAILED();
                printf("    committing datatype caused its size to change!\n");
                PART_ERROR(reopen_vlen_type);
            }

            /* Create dataset with variable-length type */
            if ((dset_id = H5Dcreate2(group_id, "vlen_dset", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to create dataset using committed datatype\n");
                PART_ERROR(reopen_vlen_type);
            }

            /* Indirectly reopen type and verify that the size doesn't change */
            if ((reopened_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to re-open committed datatype using H5Dget_type\n");
                PART_ERROR(reopen_vlen_type);
            }

            if (dt_size != H5Tget_size(reopened_type_id)) {
                H5_FAILED();
                printf("    size of re-opened datatype didn't match size of original datatype\n");
                PART_ERROR(reopen_vlen_type);
            }

            PASSED();
        }
        PART_END(reopen_vlen_type);

        H5E_BEGIN_TRY
        {
            H5Tclose(type_id);
            type_id = H5I_INVALID_HID;
            H5Tclose(reopened_type_id);
            reopened_type_id = H5I_INVALID_HID;
            H5Dclose(dset_id);
            dset_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(reopen_opaque_type)
        {
            const char *tag = "opaque_tag";

            TESTING_2("reopen of an opaque datatype");

            if ((type_id = H5Tcreate(H5T_OPAQUE, (size_t)13)) < 0) {
                H5_FAILED();
                printf("    failed to create opaque datatype\n");
                PART_ERROR(reopen_opaque_type);
            }

            if (H5Tset_tag(type_id, tag) < 0) {
                H5_FAILED();
                printf("    failed to set tag on opaque datatype\n");
                PART_ERROR(reopen_opaque_type);
            }

            /* Get size of opaque type */
            if ((dt_size = H5Tget_size(type_id)) == 0) {
                H5_FAILED();
                printf("    failed to retrieve size of opaque datatype\n");
                PART_ERROR(reopen_opaque_type);
            }

            /* Commit opaque type and verify the size doesn't change */
            if (H5Tcommit2(group_id, "opaque_type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit opaque datatype\n");
                PART_ERROR(reopen_opaque_type);
            }

            if (dt_size != H5Tget_size(type_id)) {
                H5_FAILED();
                printf("    committing datatype caused its size to change!\n");
                PART_ERROR(reopen_opaque_type);
            }

            /* Create dataset with opaque type */
            if ((dset_id = H5Dcreate2(group_id, "opaque_dset", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to create dataset using committed datatype\n");
                PART_ERROR(reopen_opaque_type);
            }

            /* Indirectly reopen type and verify that the size doesn't change */
            if ((reopened_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to re-open committed datatype using H5Dget_type\n");
                PART_ERROR(reopen_opaque_type);
            }

            if (dt_size != H5Tget_size(reopened_type_id)) {
                H5_FAILED();
                printf("    size of re-opened datatype didn't match size of original datatype\n");
                PART_ERROR(reopen_opaque_type);
            }

            PASSED();
        }
        PART_END(reopen_opaque_type);

        H5E_BEGIN_TRY
        {
            H5Tclose(type_id);
            type_id = H5I_INVALID_HID;
            H5Tclose(reopened_type_id);
            reopened_type_id = H5I_INVALID_HID;
            H5Dclose(dset_id);
            dset_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(reopen_array_type)
        {
            hsize_t array_dims[] = {2, 3};

            TESTING_2("reopen of an array datatype");

            if ((type_id = H5Tarray_create2(H5T_NATIVE_INT, 1, array_dims)) < 0) {
                H5_FAILED();
                printf("    failed to create array datatype\n");
                PART_ERROR(reopen_array_type);
            }

            /* Get size of array type */
            if ((dt_size = H5Tget_size(type_id)) == 0) {
                H5_FAILED();
                printf("    failed to retrieve size of array datatype\n");
                PART_ERROR(reopen_array_type);
            }

            /* Commit array type and verify the size doesn't change */
            if (H5Tcommit2(group_id, "array_type", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to commit array datatype\n");
                PART_ERROR(reopen_array_type);
            }

            if (dt_size != H5Tget_size(type_id)) {
                H5_FAILED();
                printf("    committing datatype caused its size to change!\n");
                PART_ERROR(reopen_array_type);
            }

            /* Create dataset with array type */
            if ((dset_id = H5Dcreate2(group_id, "array_dset", type_id, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to create dataset using committed datatype\n");
                PART_ERROR(reopen_array_type);
            }

            /* Indirectly reopen type and verify that the size doesn't change */
            if ((reopened_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to re-open committed datatype using H5Dget_type\n");
                PART_ERROR(reopen_array_type);
            }

            if (dt_size != H5Tget_size(reopened_type_id)) {
                H5_FAILED();
                printf("    size of re-opened datatype didn't match size of original datatype\n");
                PART_ERROR(reopen_array_type);
            }

            PASSED();
        }
        PART_END(reopen_array_type);

        H5E_BEGIN_TRY
        {
            H5Tclose(type_id);
            type_id = H5I_INVALID_HID;
            H5Tclose(reopened_type_id);
            reopened_type_id = H5I_INVALID_HID;
            H5Dclose(dset_id);
            dset_id = H5I_INVALID_HID;
        }
        H5E_END_TRY
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Tclose(strtype);
        H5Tclose(type_id);
        H5Tclose(reopened_type_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Tclose fails when
 * it is passed an invalid datatype ID.
 */
static int
test_close_committed_datatype_invalid_id(void)
{
    herr_t err_ret = -1;
    hid_t  file_id = H5I_INVALID_HID;

    TESTING("H5Tclose with an invalid committed datatype ID");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file or stored datatype aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    H5E_BEGIN_TRY
    {
        err_ret = H5Tclose(H5I_INVALID_HID);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    H5Tclose succeeded with an invalid committed datatype ID!\n");
        goto error;
    }

    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a TCPL used for datatype creation
 * can be persisted and that a valid copy of that TCPL can
 * be retrieved later with a call to H5Tget_create_plist.
 */
static int
test_datatype_property_lists(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t type_id1 = H5I_INVALID_HID, type_id2 = H5I_INVALID_HID;
    hid_t tcpl_id1 = H5I_INVALID_HID, tcpl_id2 = H5I_INVALID_HID;

    TESTING_MULTIPART("datatype property list operations");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, stored datatype, or getting property list aren't "
               "supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_PROPERTY_LIST_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_PROPERTY_LIST_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((type_id1 = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if ((type_id2 = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if ((tcpl_id1 = H5Pcreate(H5P_DATATYPE_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create TCPL\n");
        goto error;
    }

    /* Currently no TCPL routines are defined */

    if (H5Tcommit2(group_id, DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME1, type_id1, H5P_DEFAULT, tcpl_id1,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME1);
        goto error;
    }

    if (H5Tcommit2(group_id, DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME2, type_id2, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME2);
        goto error;
    }

    if (H5Pclose(tcpl_id1) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Tget_create_plist)
        {
            TESTING_2("H5Tget_create_plist");

            /* Try to receive copies for the two property lists */
            if ((tcpl_id1 = H5Tget_create_plist(type_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Tget_create_plist);
            }

            if ((tcpl_id2 = H5Tget_create_plist(type_id2)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Tget_create_plist);
            }

            PASSED();
        }
        PART_END(H5Tget_create_plist);

        /* Now close the property lists and datatypes and see if we can still retrieve copies of
         * the property lists upon opening (instead of creating) a datatype
         */
        if (tcpl_id1 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(tcpl_id1);
            }
            H5E_END_TRY
            tcpl_id1 = H5I_INVALID_HID;
        }
        if (tcpl_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(tcpl_id2);
            }
            H5E_END_TRY
            tcpl_id2 = H5I_INVALID_HID;
        }
        if (type_id1 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(type_id1);
            }
            H5E_END_TRY
            type_id1 = H5I_INVALID_HID;
        }
        if (type_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(type_id2);
            }
            H5E_END_TRY
            type_id2 = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Tget_create_plist_reopened)
        {
            TESTING_2("H5Tget_create_plist after re-opening committed datatype");

            if ((type_id1 = H5Topen2(group_id, DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME1, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open datatype '%s'\n", DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME1);
                PART_ERROR(H5Tget_create_plist_reopened);
            }

            if ((type_id2 = H5Topen2(group_id, DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME2, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open datatype '%s'\n", DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME2);
                PART_ERROR(H5Tget_create_plist_reopened);
            }

            if ((tcpl_id1 = H5Tget_create_plist(type_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Tget_create_plist_reopened);
            }

            if ((tcpl_id2 = H5Tget_create_plist(type_id2)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Tget_create_plist_reopened);
            }

            PASSED();
        }
        PART_END(H5Tget_create_plist_reopened);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(tcpl_id1) < 0)
        TEST_ERROR;
    if (H5Pclose(tcpl_id2) < 0)
        TEST_ERROR;
    if (H5Tclose(type_id1) < 0)
        TEST_ERROR;
    if (H5Tclose(type_id2) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(tcpl_id1);
        H5Pclose(tcpl_id2);
        H5Tclose(type_id1);
        H5Tclose(type_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset can be created using
 * a committed datatype.
 */
static int
test_create_dataset_with_committed_type(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id   = H5I_INVALID_HID;
    hid_t type_id   = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;

    TESTING("dataset creation with a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or stored datatype aren't supported with "
               "this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_WITH_DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_CREATE_WITH_DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, DATASET_CREATE_WITH_DATATYPE_TEST_TYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATASET_CREATE_WITH_DATATYPE_TEST_TYPE_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gopen2(container_group, DATASET_CREATE_WITH_DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_CREATE_WITH_DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = H5Topen2(group_id, DATASET_CREATE_WITH_DATATYPE_TEST_TYPE_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open committed datatype '%s'\n", DATASET_CREATE_WITH_DATATYPE_TEST_TYPE_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATATYPE_CREATE_TEST_DATASET_DIMS, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_CREATE_WITH_DATATYPE_TEST_DSET_NAME, type_id, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s' using committed datatype\n",
               DATASET_CREATE_WITH_DATATYPE_TEST_DSET_NAME);
        goto error;
    }

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dopen2(group_id, DATASET_CREATE_WITH_DATATYPE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open dataset '%s'\n", DATASET_CREATE_WITH_DATATYPE_TEST_DSET_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can be created
 * using a committed datatype.
 */
static int
test_create_attribute_with_committed_type(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  attr_id  = H5I_INVALID_HID;
    hid_t  type_id  = H5I_INVALID_HID;
    hid_t  space_id = H5I_INVALID_HID;

    TESTING("attribute creation with a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or stored datatype aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, true)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_DTYPE_NAME, type_id, H5P_DEFAULT,
                   H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_DTYPE_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;

    if ((type_id = H5Topen2(group_id, ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_DTYPE_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open committed datatype '%s'\n", ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_DTYPE_NAME);
        goto error;
    }

    if ((space_id =
             generate_random_dataspace(ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME, type_id, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME);
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n",
               ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Aopen(group_id, ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open attribute '%s'\n", ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME);
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a committed datatype can
 * be deleted.
 */
static int
test_delete_committed_type(void)
{
    htri_t type_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  type_id = H5I_INVALID_HID;

    TESTING("committed datatype deletion");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or stored datatype aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_DELETE_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATATYPE_DELETE_TEST_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, DATATYPE_DELETE_TEST_DTYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", DATATYPE_DELETE_TEST_DTYPE_NAME);
        goto error;
    }

    if ((type_exists = H5Lexists(group_id, DATATYPE_DELETE_TEST_DTYPE_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if datatype '%s' exists\n", DATATYPE_DELETE_TEST_DTYPE_NAME);
        goto error;
    }

    if (!type_exists) {
        H5_FAILED();
        printf("    datatype didn't exist\n");
        goto error;
    }

    if (H5Ldelete(group_id, DATATYPE_DELETE_TEST_DTYPE_NAME, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't delete datatype '%s'\n", DATATYPE_DELETE_TEST_DTYPE_NAME);
        goto error;
    }

    if ((type_exists = H5Lexists(group_id, DATATYPE_DELETE_TEST_DTYPE_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if datatype '%s' exists\n", DATATYPE_DELETE_TEST_DTYPE_NAME);
        goto error;
    }

    if (type_exists) {
        H5_FAILED();
        printf("    datatype exists\n");
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a committed datatype can still be opened when
 * the link to the datatype is deleted and then a new one is created.
 */
static int
test_resurrect_datatype(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID;
    hid_t group_id        = H5I_INVALID_HID;
    hid_t type_id         = H5I_INVALID_HID;

    TESTING("resurrecting datatype after deletion");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_HARD_LINKS)) {
        SKIPPED();
        printf("    API functions for basic file, group, link, hard link, or stored datatype aren't "
               "supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATATYPE_RESURRECT_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATATYPE_RESURRECT_TEST_GROUP_NAME);
        goto error;
    }

    /* Create a named datatype in the file */
    if ((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) {
        H5_FAILED();
        printf("    failed to copy predefined integer type\n");
        goto error;
    }

    if (H5Tcommit2(group_id, DATATYPE_RESURRECT_TEST_DTYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    failed to commit datatype\n");
        goto error;
    }

    /* Unlink the datatype while it's open (will mark it for deletion when closed) */
    if (H5Ldelete(group_id, DATATYPE_RESURRECT_TEST_DTYPE_NAME, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    failed to delete datatype\n");
        goto error;
    }

    /* Check that datatype name is NULL */
    if (H5Iget_name(type_id, NULL, (size_t)0) != 0) {
        H5_FAILED();
        printf("    deleted datatype name was not NULL!\n");
        goto error;
    }

    /* Re-link the datatype to the group hierarchy (shouldn't get deleted now) */
    if (H5Lcreate_hard(type_id, ".", group_id, DATATYPE_RESURRECT_TEST_DTYPE_NAME2, H5P_DEFAULT,
                       H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    failed to create new link for deleted datatype\n");
        goto error;
    }

    /* Close things */
    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    /* Re-open the file */
    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gopen2(container_group, DATATYPE_RESURRECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n", DATATYPE_RESURRECT_TEST_GROUP_NAME);
        goto error;
    }

    /* Attempt to open the datatype under the new name */
    if ((type_id = H5Topen2(group_id, DATATYPE_RESURRECT_TEST_DTYPE_NAME2, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open resurrected datatype\n");
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

static int
test_flush_committed_datatype(void)
{
    TESTING("H5Tflush");

    SKIPPED();

    return 0;
}

static int
test_flush_committed_datatype_invalid_params(void)
{
    TESTING("H5Tflush with invalid parameters");

    SKIPPED();

    return 0;
}

static int
test_refresh_committed_datatype(void)
{
    TESTING("H5Trefresh");

    SKIPPED();

    return 0;
}

static int
test_refresh_committed_datatype_invalid_params(void)
{
    TESTING("H5Trefresh with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test to check that predefined HDF5 datatypes can't be directly committed.
 * An application should first copy the type with H5Tcopy and then commit the
 * copied datatype.
 */
#ifndef PROBLEMATIC_TESTS
static int
test_cant_commit_predefined(void)
{
    herr_t err_ret;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;

    TESTING("inability to commit predefined types directly");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, PREDEFINED_TYPE_COMMIT_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", PREDEFINED_TYPE_COMMIT_TEST_GROUP_NAME);
        goto error;
    }

    H5E_BEGIN_TRY
    {
        err_ret = H5Tcommit2(group_id, "committed_predefined_type", H5T_NATIVE_INT, H5P_DEFAULT, H5P_DEFAULT,
                             H5P_DEFAULT);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    committed a predefined datatype directly (without copying it)!\n");
        goto error;
    }

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}
#endif

/*
 * A test to check that a datatype cannot be modified once it has been committed.
 */
static int
test_cant_modify_committed_type(void)
{
    htri_t is_committed = false;
    herr_t err_ret;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  type_id         = H5I_INVALID_HID;

    TESTING("inability to modify a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, or stored datatype aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATATYPE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATATYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, MODIFY_COMMITTED_TYPE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", MODIFY_COMMITTED_TYPE_TEST_GROUP_NAME);
        goto error;
    }

    /* Copy a predefined datatype and commit the copy */
    if ((type_id = H5Tcopy(H5T_NATIVE_INT)) < 0) {
        H5_FAILED();
        printf("    failed to copy predefined integer datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, "native_int", type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    failed to commit datatype\n");
        goto error;
    }

    if ((is_committed = H5Tcommitted(type_id)) < 0) {
        H5_FAILED();
        printf("    failed to determine if datatype is committed\n");
        goto error;
    }

    if (!is_committed) {
        H5_FAILED();
        printf("    H5Tcommitted() returned false!\n");
        goto error;
    }

    /* We should not be able to modify a type after it has been committed. */
    H5E_BEGIN_TRY
    {
        err_ret = H5Tset_precision(type_id, (size_t)256);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    modified committed datatype!\n");
        goto error;
    }

    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

int
H5_api_datatype_test(void)
{
    size_t i;
    int    nerrors;

    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*             API Datatype Tests             *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(datatype_tests); i++) {
        nerrors += (*datatype_tests[i])() ? 1 : 0;
    }

    printf("\n");

    return nerrors;
}
