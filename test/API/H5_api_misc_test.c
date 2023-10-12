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

#include "H5_api_misc_test.h"

static int test_open_link_without_leading_slash(void);
static int test_object_creation_by_absolute_path(void);
static int test_absolute_vs_relative_path(void);
static int test_dot_for_object_name(void);
static int test_symbols_in_compound_field_name(void);
static int test_double_init_term(void);

/*
 * The array of miscellaneous tests to be performed.
 */
static int (*misc_tests[])(void) = {
    test_open_link_without_leading_slash, test_object_creation_by_absolute_path,
    test_absolute_vs_relative_path,       test_dot_for_object_name,
    test_symbols_in_compound_field_name,  test_double_init_term,
};

static int
test_open_link_without_leading_slash(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID;
    hid_t group_id        = H5I_INVALID_HID;
    hid_t dset_id         = H5I_INVALID_HID;
    hid_t dset_dtype      = H5I_INVALID_HID;
    hid_t space_id        = H5I_INVALID_HID;

    TESTING("opening a link without a leading slash");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, MISCELLANEOUS_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((space_id = generate_random_dataspace(OPEN_LINK_WITHOUT_SLASH_DSET_SPACE_RANK, NULL, NULL, false)) <
        0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(container_group, OPEN_LINK_WITHOUT_SLASH_DSET_NAME, dset_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset\n");
        goto error;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((group_id = H5Gopen2(file_id, "/", H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open root group\n");
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, MISCELLANEOUS_TEST_GROUP_NAME "/" OPEN_LINK_WITHOUT_SLASH_DSET_NAME,
                            H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset\n");
        goto error;
    }

    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

static int
test_object_creation_by_absolute_path(void)
{
    htri_t link_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID, sub_group_id = H5I_INVALID_HID;
    hid_t  dset_id    = H5I_INVALID_HID;
    hid_t  fspace_id  = H5I_INVALID_HID;
    hid_t  dtype_id   = H5I_INVALID_HID;
    hid_t  dset_dtype = H5I_INVALID_HID;

    TESTING_MULTIPART("object creation by absolute path");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, link, or stored datatype aren't "
               "supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, MISCELLANEOUS_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    /* Start by creating a group to hold all the objects for this test */
    if ((group_id = H5Gcreate2(container_group, OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group\n");
        goto error;
    }

    if ((link_exists = H5Lexists(file_id,
                                 "/" MISCELLANEOUS_TEST_GROUP_NAME
                                 "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME,
                                 H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if link exists\n");
        goto error;
    }

    if (!link_exists) {
        H5_FAILED();
        printf("    container group didn't exist at the correct location\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Gcreate_using_absolute_path)
        {
            TESTING_2("creation of group using absolute pathname");

            /* Try to create a group under the container group by using an absolute pathname */
            if ((sub_group_id = H5Gcreate2(file_id,
                                           "/" MISCELLANEOUS_TEST_GROUP_NAME
                                           "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME
                                           "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME,
                                           H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create subgroup by absolute pathname\n");
                PART_ERROR(H5Gcreate_using_absolute_path);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Gcreate_using_absolute_path);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    subgroup didn't exist at the correct location\n");
                PART_ERROR(H5Gcreate_using_absolute_path);
            }

            PASSED();
        }
        PART_END(H5Gcreate_using_absolute_path);

        PART_BEGIN(H5Dcreate_using_absolute_path)
        {
            TESTING_2("creation of dataset using absolute pathname");

            /* Try to create a dataset nested at the end of this group chain by using an absolute pathname */
            if ((fspace_id = generate_random_dataspace(OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DSET_SPACE_RANK,
                                                       NULL, NULL, false)) < 0) {
                H5_FAILED();
                printf("    failed to generate dataspace\n");
                PART_ERROR(H5Dcreate_using_absolute_path);
            }

            if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
                H5_FAILED();
                printf("    failed to generate datatype\n");
                PART_ERROR(H5Dcreate_using_absolute_path);
            }

            if ((dset_id = H5Dcreate2(file_id,
                                      "/" MISCELLANEOUS_TEST_GROUP_NAME
                                      "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME
                                      "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME
                                      "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DSET_NAME,
                                      dset_dtype, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset\n");
                PART_ERROR(H5Dcreate_using_absolute_path);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DSET_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_using_absolute_path);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    dataset didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_using_absolute_path);
            }

            PASSED();
        }
        PART_END(H5Dcreate_using_absolute_path);

        PART_BEGIN(H5Tcommit_using_absolute_path)
        {
            TESTING_2("creation of committed datatype using absolute pathname");

            /* Try to create a committed datatype in the same fashion as the preceding dataset */
            if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
                H5_FAILED();
                printf("    couldn't create datatype\n");
                PART_ERROR(H5Tcommit_using_absolute_path);
            }

            if (H5Tcommit2(file_id,
                           "/" MISCELLANEOUS_TEST_GROUP_NAME
                           "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME
                           "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME
                           "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DTYPE_NAME,
                           dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't commit datatype\n");
                PART_ERROR(H5Tcommit_using_absolute_path);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME
                                         "/" OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DTYPE_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Tcommit_using_absolute_path);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    datatype didn't exist at the correct location\n");
                PART_ERROR(H5Tcommit_using_absolute_path);
            }

            PASSED();
        }
        PART_END(H5Tcommit_using_absolute_path);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Gclose(sub_group_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Tclose(dtype_id);
        H5Gclose(sub_group_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/* XXX: Add testing for groups */
static int
test_absolute_vs_relative_path(void)
{
    htri_t link_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  dset_id1 = H5I_INVALID_HID, dset_id2 = H5I_INVALID_HID, dset_id3 = H5I_INVALID_HID,
          dset_id4 = H5I_INVALID_HID, dset_id5 = H5I_INVALID_HID, dset_id6 = H5I_INVALID_HID;
    hid_t dset_dtype1 = H5I_INVALID_HID, dset_dtype2 = H5I_INVALID_HID, dset_dtype3 = H5I_INVALID_HID,
          dset_dtype4 = H5I_INVALID_HID, dset_dtype5 = H5I_INVALID_HID, dset_dtype6 = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;

    TESTING_MULTIPART("absolute vs. relative pathnames");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or link aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, MISCELLANEOUS_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    /* Start by creating a group to be used during some of the dataset creation operations */
    if ((group_id = H5Gcreate2(container_group, ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group\n");
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET_SPACE_RANK, NULL, NULL,
                                               false)) < 0)
        TEST_ERROR;

    if ((dset_dtype1 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype2 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype3 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype4 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype5 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype6 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dcreate_absolute_from_root)
        {
            TESTING_2("dataset creation by absolute path from root group");

            /* Create a dataset by absolute path in the form "/group/dataset" starting from the root group */
            if ((dset_id1 = H5Dcreate2(file_id,
                                       "/" MISCELLANEOUS_TEST_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET1_NAME,
                                       dset_dtype1, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset by absolute path from root\n");
                PART_ERROR(H5Dcreate_absolute_from_root);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET1_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_absolute_from_root);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_absolute_from_root);
            }

            PASSED();
        }
        PART_END(H5Dcreate_absolute_from_root);

        PART_BEGIN(H5Dcreate_absolute_from_nonroot)
        {
            TESTING_2("dataset creation by absolute path from non-root group");

            /* Create a dataset by absolute path in the form "/group/dataset" starting from the container
             * group */
            if ((dset_id4 = H5Dcreate2(container_group,
                                       "/" MISCELLANEOUS_TEST_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET4_NAME,
                                       dset_dtype4, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset by absolute path from container group\n");
                PART_ERROR(H5Dcreate_absolute_from_nonroot);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET4_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_absolute_from_nonroot);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_absolute_from_nonroot);
            }

            PASSED();
        }
        PART_END(H5Dcreate_absolute_from_nonroot);

        PART_BEGIN(H5Dcreate_relative_from_root)
        {
            TESTING_2("dataset creation by relative path from root group");

            /* TODO: */

            SKIPPED();
            PART_EMPTY(H5Dcreate_relative_from_root);
        }
        PART_END(H5Dcreate_relative_from_root);

        PART_BEGIN(H5Dcreate_relative_from_nonroot)
        {
            TESTING_2("dataset creation by relative path from non-root group");

            /* Create a dataset by relative path in the form "dataset" starting from the test container group
             */
            if ((dset_id5 = H5Dcreate2(group_id, ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET5_NAME, dset_dtype5,
                                       fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset by relative path from container group\n");
                PART_ERROR(H5Dcreate_relative_from_nonroot);
            }

            /* Create a dataset by relative path in the form "group/dataset" starting from the top-level
             * container group */
            if ((dset_id2 = H5Dcreate2(container_group,
                                       ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET2_NAME,
                                       dset_dtype2, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset by relative path from container group\n");
                PART_ERROR(H5Dcreate_relative_from_nonroot);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET2_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_relative_from_nonroot);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_relative_from_nonroot);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET5_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_relative_from_nonroot);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_relative_from_nonroot);
            }

            PASSED();
        }
        PART_END(H5Dcreate_relative_from_nonroot);

        PART_BEGIN(H5Dcreate_relative_leading_dot_root)
        {
            TESTING_2("dataset creation by path with leading '.' from root group");

            /* Create a dataset by relative path in the form "./group/dataset" starting from the root group */
            if ((dset_id3 = H5Dcreate2(file_id,
                                       "./" MISCELLANEOUS_TEST_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                       "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET3_NAME,
                                       dset_dtype3, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset by relative path from root with leading '.'\n");
                PART_ERROR(H5Dcreate_relative_leading_dot_root);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET3_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_relative_leading_dot_root);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_relative_leading_dot_root);
            }

            PASSED();
        }
        PART_END(H5Dcreate_relative_leading_dot_root);

        PART_BEGIN(H5Dcreate_relative_leading_dot_nonroot)
        {
            TESTING_2("dataset creation by path with leading '.' from non-root group");

            /* Create a dataset by relative path in the form "./dataset" starting from the container group */
            if ((dset_id6 = H5Dcreate2(group_id, "./" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET6_NAME, dset_dtype6,
                                       fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf(
                    "    couldn't create dataset by relative path from container group with leading '.'\n");
                PART_ERROR(H5Dcreate_relative_leading_dot_nonroot);
            }

            if ((link_exists = H5Lexists(file_id,
                                         "/" MISCELLANEOUS_TEST_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME
                                         "/" ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET6_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link exists\n");
                PART_ERROR(H5Dcreate_relative_leading_dot_nonroot);
            }

            if (!link_exists) {
                H5_FAILED();
                printf("    didn't exist at the correct location\n");
                PART_ERROR(H5Dcreate_relative_leading_dot_nonroot);
            }

            PASSED();
        }
        PART_END(H5Dcreate_relative_leading_dot_nonroot);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype1) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype2) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype3) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype4) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype5) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype6) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id3) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id4) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id5) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id6) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype1);
        H5Tclose(dset_dtype2);
        H5Tclose(dset_dtype3);
        H5Tclose(dset_dtype4);
        H5Tclose(dset_dtype5);
        H5Tclose(dset_dtype6);
        H5Dclose(dset_id1);
        H5Dclose(dset_id2);
        H5Dclose(dset_id3);
        H5Dclose(dset_id4);
        H5Dclose(dset_id5);
        H5Dclose(dset_id6);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check creating/opening objects with the "." as the name
 */
static int
test_dot_for_object_name(void)
{
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, subgroup_id = H5I_INVALID_HID;
    hid_t  dset_id = H5I_INVALID_HID, dspace_id = H5I_INVALID_HID;
    hid_t  group_id = H5I_INVALID_HID;
    hid_t  dtype_id = H5I_INVALID_HID;
    hid_t  attr_id  = H5I_INVALID_HID;
    herr_t ret      = -1;

    TESTING_MULTIPART("creating objects with \".\" as the name");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
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

    if ((container_group = H5Gopen2(file_id, MISCELLANEOUS_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", MISCELLANEOUS_TEST_GROUP_NAME);
        goto error;
    }

    if ((subgroup_id = H5Gcreate2(container_group, DOT_AS_OBJECT_NAME_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", DOT_AS_OBJECT_NAME_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((dspace_id = H5Screate(H5S_SCALAR)) < 0) {
        H5_FAILED();
        printf("    couldn't create data space\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Gcreate_dot_as_name)
        {
            TESTING_2("invalid creation of group with '.' as name");

            /* Create a group with the "." as the name.  It should fail. */
            H5E_BEGIN_TRY
            {
                group_id = H5Gcreate2(subgroup_id, ".", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id >= 0) {
                H5_FAILED();
                printf("    a group was created with '.' as the name!\n");
                PART_ERROR(H5Gcreate_dot_as_name);
            }

            PASSED();
        }
        PART_END(H5Gcreate_dot_as_name);

        PART_BEGIN(H5Dcreate_dot_as_name)
        {
            TESTING_2("invalid creation of dataset with '.' as name");

            /* Create a dataset with the "." as the name.  It should fail. */
            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(subgroup_id, ".", H5T_NATIVE_INT, dspace_id, H5P_DEFAULT, H5P_DEFAULT,
                                     H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    a dataset was created with '.' as the name!\n");
                PART_ERROR(H5Dcreate_dot_as_name);
            }

            PASSED();
        }
        PART_END(H5Dcreate_dot_as_name);

        PART_BEGIN(H5Tcommit_dot_as_name)
        {
            TESTING_2("invalid creation of committed datatype with '.' as name");

            if ((dtype_id = H5Tcopy(H5T_NATIVE_INT)) < 0) {
                H5_FAILED();
                printf("    couldn't copy a native datatype\n");
                PART_ERROR(H5Tcommit_dot_as_name);
            }

            /* Commit a datatype with "." as the name. It should fail. */
            H5E_BEGIN_TRY
            {
                ret = H5Tcommit2(subgroup_id, ".", dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (ret >= 0) {
                H5_FAILED();
                printf("    a named datatype was committed with '.' as the name!\n");
                PART_ERROR(H5Tcommit_dot_as_name);
            }

            if (H5Tclose(dtype_id) < 0) {
                H5_FAILED();
                printf("    failed to close datatype\n");
                PART_ERROR(H5Tcommit_dot_as_name);
            }

            PASSED();
        }
        PART_END(H5Tcommit_dot_as_name);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(dspace_id) < 0)
        TEST_ERROR;
    if (H5Gclose(subgroup_id) < 0)
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
        H5Sclose(dspace_id);
        H5Aclose(attr_id);
        H5Dclose(dset_id);
        H5Tclose(dtype_id);
        H5Gclose(group_id);
        H5Gclose(subgroup_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that the initialization and termination
 * functions of a VOL connector can be called multiple times
 * in a row.
 *
 * TODO: Not sure if this test can be done from public APIs
 * at the moment.
 */
static int
test_double_init_term(void)
{
    TESTING("double init/term correctness");

    SKIPPED();

    return 0;

#if 0
error:
    return 1;
#endif
}

static int
test_symbols_in_compound_field_name(void)
{
    size_t i;
    size_t total_type_size;
    size_t next_offset;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  compound_type = H5I_INVALID_HID;
    hid_t  dset_id       = H5I_INVALID_HID;
    hid_t  fspace_id     = H5I_INVALID_HID;
    hid_t  type_pool[COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES];
    char   member_names[COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES][256];

    TESTING("usage of '{', '}' and '\\\"' symbols in compound field name");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    for (i = 0; i < COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES; i++)
        type_pool[i] = H5I_INVALID_HID;

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, MISCELLANEOUS_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group\n");
        goto error;
    }

    for (i = 0, total_type_size = 0; i < COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES; i++) {
        type_pool[i] = generate_random_datatype(H5T_NO_CLASS, false);
        total_type_size += H5Tget_size(type_pool[i]);
    }

    snprintf(member_names[0], 256, "{{{ member0");
    snprintf(member_names[1], 256, "member1 }}}");
    snprintf(member_names[2], 256, "{{{ member2 }}");
    snprintf(member_names[3], 256, "{{ member3 }}}");
    snprintf(member_names[4], 256, "\\\"member4");
    snprintf(member_names[5], 256, "member5\\\"");
    snprintf(member_names[6], 256, "mem\\\"ber6");
    snprintf(member_names[7], 256, "{{ member7\\\" }");
    snprintf(member_names[8], 256, "{{ member8\\\\");

    if ((compound_type = H5Tcreate(H5T_COMPOUND, total_type_size)) < 0) {
        H5_FAILED();
        printf("    couldn't create compound datatype\n");
        goto error;
    }

    for (i = 0, next_offset = 0; i < COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES; i++) {
        if (H5Tinsert(compound_type, member_names[i], next_offset, type_pool[i]) < 0) {
            H5_FAILED();
            printf("    couldn't insert compound member %zu\n", i);
            goto error;
        }

        next_offset += H5Tget_size(type_pool[i]);
    }

    if (H5Tpack(compound_type) < 0)
        TEST_ERROR;

    if ((fspace_id = generate_random_dataspace(COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_DSET_RANK, NULL,
                                               NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_DSET_NAME, compound_type,
                              fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset\n");
        goto error;
    }

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dopen2(group_id, COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_DSET_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    failed to open dataset\n");
        goto error;
    }

    for (i = 0; i < COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES; i++)
        if (type_pool[i] >= 0 && H5Tclose(type_pool[i]) < 0)
            TEST_ERROR;

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(compound_type) < 0)
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
        for (i = 0; i < COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES; i++)
            H5Tclose(type_pool[i]);
        H5Sclose(fspace_id);
        H5Tclose(compound_type);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

int
H5_api_misc_test(void)
{
    size_t i;
    int    nerrors;

    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*          API Miscellaneous Tests           *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(misc_tests); i++) {
        nerrors += (*misc_tests[i])() ? 1 : 0;
    }

    printf("\n");

    return nerrors;
}
