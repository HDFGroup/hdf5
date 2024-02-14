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

#include "H5_api_attribute_test.h"

/*
 * TODO: Additional tests to be written:
 *
 *       - Test for creating a large attribute.
 *       - Test for checking that object's max. attr. creation
 *         order value gets reset when all attributes are removed.
 */

static int test_create_attribute_on_root(void);
static int test_create_attribute_on_dataset(void);
static int test_create_attribute_on_datatype(void);
static int test_create_attribute_with_null_space(void);
static int test_create_attribute_with_scalar_space(void);
static int test_create_attribute_with_space_in_name(void);
static int test_create_attribute_invalid_params(void);
static int test_open_attribute(void);
static int test_open_attribute_invalid_params(void);
static int test_write_attribute(void);
static int test_write_attribute_invalid_params(void);
static int test_read_attribute(void);
static int test_read_attribute_invalid_params(void);
static int test_read_empty_attribute(void);
static int test_close_attribute_invalid_id(void);
static int test_get_attribute_space_and_type(void);
static int test_get_attribute_space_and_type_invalid_params(void);
static int test_attribute_property_lists(void);
static int test_get_attribute_name(void);
static int test_get_attribute_name_invalid_params(void);
static int test_get_attribute_storage_size(void);
static int test_get_attribute_info(void);
static int test_get_attribute_info_invalid_params(void);
static int test_rename_attribute(void);
static int test_rename_attribute_invalid_params(void);
static int test_attribute_iterate_group(void);
static int test_attribute_iterate_dataset(void);
static int test_attribute_iterate_datatype(void);
static int test_attribute_iterate_index_saving(void);
static int test_attribute_iterate_invalid_params(void);
static int test_attribute_iterate_0_attributes(void);
static int test_attribute_compound_subset(void);
static int test_attribute_string_encodings(void);
static int test_delete_attribute(void);
static int test_delete_attribute_invalid_params(void);
static int test_attribute_exists(void);
static int test_attribute_exists_invalid_params(void);
static int test_attribute_many(void);
static int test_attribute_duplicate_id(void);
static int test_get_number_attributes(void);
static int test_attr_shared_dtype(void);

static herr_t attr_iter_callback1(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo,
                                  void *op_data);
static herr_t attr_iter_callback2(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo,
                                  void *op_data);

/*
 * The array of attribute tests to be performed.
 */
static int (*attribute_tests[])(void) = {test_create_attribute_on_root,
                                         test_create_attribute_on_dataset,
                                         test_create_attribute_on_datatype,
                                         test_create_attribute_with_null_space,
                                         test_create_attribute_with_scalar_space,
                                         test_create_attribute_with_space_in_name,
                                         test_create_attribute_invalid_params,
                                         test_open_attribute,
                                         test_open_attribute_invalid_params,
                                         test_write_attribute,
                                         test_write_attribute_invalid_params,
                                         test_read_attribute,
                                         test_read_attribute_invalid_params,
                                         test_read_empty_attribute,
                                         test_close_attribute_invalid_id,
                                         test_get_attribute_space_and_type,
                                         test_get_attribute_space_and_type_invalid_params,
                                         test_attribute_property_lists,
                                         test_get_attribute_name,
                                         test_get_attribute_name_invalid_params,
                                         test_get_attribute_storage_size,
                                         test_get_attribute_info,
                                         test_get_attribute_info_invalid_params,
                                         test_rename_attribute,
                                         test_rename_attribute_invalid_params,
                                         test_attribute_iterate_group,
                                         test_attribute_iterate_dataset,
                                         test_attribute_iterate_datatype,
                                         test_attribute_iterate_index_saving,
                                         test_attribute_iterate_invalid_params,
                                         test_attribute_iterate_0_attributes,
                                         test_attribute_compound_subset,
                                         test_attribute_string_encodings,
                                         test_delete_attribute,
                                         test_delete_attribute_invalid_params,
                                         test_attribute_exists,
                                         test_attribute_exists_invalid_params,
                                         test_attribute_duplicate_id,
                                         test_attribute_many,
                                         test_get_number_attributes,
                                         test_attr_shared_dtype};

/*
 * A test to check that an attribute can be created on
 * the root group.
 */
static int
test_create_attribute_on_root(void)
{
    htri_t attr_exists;
    hid_t  file_id = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t  attr_dtype1 = H5I_INVALID_HID, attr_dtype2 = H5I_INVALID_HID;
    hid_t  space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute creation on the root group");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_CREATE_ON_ROOT_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype1 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;
    if ((attr_dtype2 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Acreate2)
        {
            TESTING_2("H5Acreate on the root group");

            if ((attr_id = H5Acreate2(file_id, ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME, attr_dtype1, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s' using H5Acreate\n",
                       ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME);
                PART_ERROR(H5Acreate2);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(file_id, ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME);
                PART_ERROR(H5Acreate2);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' did not exist\n", ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME);
                PART_ERROR(H5Acreate2);
            }

            PASSED();
        }
        PART_END(H5Acreate2);

        PART_BEGIN(H5Acreate_by_name)
        {
            TESTING_2("H5Acreate_by_name on the root group");

            if ((attr_id2 = H5Acreate_by_name(file_id, "/", ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME2, attr_dtype2,
                                              space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute on root group using H5Acreate_by_name\n");
                PART_ERROR(H5Acreate_by_name);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(file_id, ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME2);
                PART_ERROR(H5Acreate_by_name);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' did not exist\n", ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME2);
                PART_ERROR(H5Acreate_by_name);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype1) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype2) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Tclose(attr_dtype1);
        H5Tclose(attr_dtype2);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can be created on
 * a dataset.
 */
static int
test_create_attribute_on_dataset(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  dset_id         = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t  attr_dtype1 = H5I_INVALID_HID, attr_dtype2 = H5I_INVALID_HID;
    hid_t  dset_dtype    = H5I_INVALID_HID;
    hid_t  dset_space_id = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute creation on a dataset");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or attribute aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_ON_DATASET_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_CREATE_ON_DATASET_GROUP_NAME);
        goto error;
    }

    if ((dset_space_id =
             generate_random_dataspace(ATTRIBUTE_CREATE_ON_DATASET_DSET_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;
    if ((attr_space_id =
             generate_random_dataspace(ATTRIBUTE_CREATE_ON_DATASET_ATTR_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((attr_dtype1 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;
    if ((attr_dtype2 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, ATTRIBUTE_CREATE_ON_DATASET_DSET_NAME, dset_dtype, dset_space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Acreate_on_dataset)
        {
            TESTING_2("H5Acreate on a dataset");

            if ((attr_id = H5Acreate2(dset_id, ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME, attr_dtype1,
                                      attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute\n");
                PART_ERROR(H5Acreate_on_dataset);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(dset_id, ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME);
                PART_ERROR(H5Acreate_on_dataset);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' did not exist\n", ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME);
                PART_ERROR(H5Acreate_on_dataset);
            }

            PASSED();
        }
        PART_END(H5Acreate_on_dataset);

        PART_BEGIN(H5Acreate_by_name_on_dataset)
        {
            TESTING_2("H5Acreate_by_name on a dataset");

            if ((attr_id2 = H5Acreate_by_name(group_id, ATTRIBUTE_CREATE_ON_DATASET_DSET_NAME,
                                              ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME2, attr_dtype2,
                                              attr_space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute on dataset by name\n");
                PART_ERROR(H5Acreate_by_name_on_dataset);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(dset_id, ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME2);
                PART_ERROR(H5Acreate_by_name_on_dataset);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' did not exist\n", ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME2);
                PART_ERROR(H5Acreate_by_name_on_dataset);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_on_dataset);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(dset_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype1) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype2) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
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
        H5Sclose(dset_space_id);
        H5Sclose(attr_space_id);
        H5Tclose(dset_dtype);
        H5Tclose(attr_dtype1);
        H5Tclose(attr_dtype2);
        H5Dclose(dset_id);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can be created on
 * a committed datatype.
 */
static int
test_create_attribute_on_datatype(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  type_id         = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t  attr_dtype1 = H5I_INVALID_HID, attr_dtype2 = H5I_INVALID_HID;
    hid_t  space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute creation on a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, stored datatype, or attribute aren't supported "
               "with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_ON_DATATYPE_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_CREATE_ON_DATATYPE_GROUP_NAME);
        goto error;
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, ATTRIBUTE_CREATE_ON_DATATYPE_DTYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype\n");
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_CREATE_ON_DATATYPE_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype1 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;
    if ((attr_dtype2 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Acreate_on_datatype)
        {
            TESTING_2("H5Acreate on a committed datatype");

            if ((attr_id = H5Acreate2(type_id, ATTRIBUTE_CREATE_ON_DATATYPE_ATTR_NAME, attr_dtype1, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute on datatype using H5Acreate\n");
                PART_ERROR(H5Acreate_on_datatype);
            }

            if ((attr_exists = H5Aexists(type_id, ATTRIBUTE_CREATE_ON_DATATYPE_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Acreate_on_datatype);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute did not exist\n");
                PART_ERROR(H5Acreate_on_datatype);
            }

            PASSED();
        }
        PART_END(H5Acreate_on_datatype);

        PART_BEGIN(H5Acreate_by_name_on_datatype)
        {
            TESTING_2("H5Acreate_by_name on a committed datatype");

            if ((attr_id2 = H5Acreate_by_name(group_id, ATTRIBUTE_CREATE_ON_DATATYPE_DTYPE_NAME,
                                              ATTRIBUTE_CREATE_ON_DATATYPE_ATTR_NAME2, attr_dtype2, space_id,
                                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute on datatype using H5Acreate_by_name\n");
                PART_ERROR(H5Acreate_by_name_on_datatype);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(type_id, ATTRIBUTE_CREATE_ON_DATATYPE_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Acreate_by_name_on_datatype);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute did not exist\n");
                PART_ERROR(H5Acreate_by_name_on_datatype);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_on_datatype);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype1) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype2) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
        TEST_ERROR;
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype1);
        H5Tclose(attr_dtype2);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Tclose(type_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that creating an attribute with a
 * NULL dataspace is not problematic.
 */
static int
test_create_attribute_with_null_space(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  attr_id    = H5I_INVALID_HID;
    hid_t  attr_dtype = H5I_INVALID_HID;
    hid_t  space_id   = H5I_INVALID_HID;

    TESTING("attribute creation with a NULL dataspace");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_NULL_DATASPACE_TEST_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup\n");
        goto error;
    }

    if ((space_id = H5Screate(H5S_NULL)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_NULL_DATASPACE_TEST_ATTR_NAME, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_CREATE_NULL_DATASPACE_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Aopen(group_id, ATTRIBUTE_CREATE_NULL_DATASPACE_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open attribute\n");
        goto error;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that creating an attribute with a
 * scalar dataspace is not problematic.
 */
static int
test_create_attribute_with_scalar_space(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  attr_id    = H5I_INVALID_HID;
    hid_t  attr_dtype = H5I_INVALID_HID;
    hid_t  space_id   = H5I_INVALID_HID;

    TESTING("attribute creation with a SCALAR dataspace");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_SCALAR_DATASPACE_TEST_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup\n");
        goto error;
    }

    if ((space_id = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_SCALAR_DATASPACE_TEST_ATTR_NAME, attr_dtype,
                              space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_CREATE_SCALAR_DATASPACE_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Aopen(group_id, ATTRIBUTE_CREATE_SCALAR_DATASPACE_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open attribute\n");
        goto error;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a space in an attribute's name
 * is not problematic.
 */
static int
test_create_attribute_with_space_in_name(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id         = H5I_INVALID_HID;
    hid_t  attr_id2        = H5I_INVALID_HID;
    hid_t  attr_dtype      = H5I_INVALID_HID;
    hid_t  space_id        = H5I_INVALID_HID;

    TESTING("attribute creation with a space in attribute's name");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_GROUP_NAME);
        goto error;
    }

    if ((space_id =
             generate_random_dataspace(ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_ATTR_NAME, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can't be created when
 * H5Acreate is passed invalid parameters.
 */
static int
test_create_attribute_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t attr_id    = H5I_INVALID_HID;
    hid_t attr_dtype = H5I_INVALID_HID;
    hid_t space_id   = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute creation with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group\n");
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_CREATE_INVALID_PARAMS_SPACE_RANK, NULL, NULL, true)) <
        0)
        TEST_ERROR;
    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Acreate_invalid_loc_id)
        {
            TESTING_2("H5Acreate with invalid loc_id");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(H5I_INVALID_HID, ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype,
                                     space_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with an invalid loc_id!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Acreate_invalid_loc_id);

        PART_BEGIN(H5Acreate_invalid_attr_name)
        {
            TESTING_2("H5Acreate with invalid attribute name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(group_id, NULL, attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with a NULL name!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(group_id, "", attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with an invalid name of ''!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Acreate_invalid_attr_name);

        PART_BEGIN(H5Acreate_invalid_datatype)
        {
            TESTING_2("H5Acreate with an invalid datatype");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, H5I_INVALID_HID,
                                     space_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with an invalid datatype!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Acreate_invalid_datatype);

        PART_BEGIN(H5Acreate_invalid_dataspace)
        {
            TESTING_2("H5Acreate with an invalid dataspace");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype,
                                     H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with an invalid dataspace!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_dataspace);
            }

            PASSED();
        }
        PART_END(H5Acreate_invalid_dataspace);

        PART_BEGIN(H5Acreate_invalid_acpl)
        {
            TESTING_2("H5Acreate with an invalid ACPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype,
                                     space_id, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with an invalid ACPL!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_acpl);
            }

            PASSED();
        }
        PART_END(H5Acreate_invalid_acpl);

        PART_BEGIN(H5Acreate_invalid_aapl)
        {
            TESTING_2("H5Acreate with an invalid AAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate2(group_id, ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype,
                                     space_id, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate with an invalid AAPL!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_invalid_aapl);
            }

            PASSED();
        }
        PART_END(H5Acreate_invalid_aapl);

        PART_BEGIN(H5Acreate_by_name_invalid_loc_id)
        {
            TESTING_2("H5Acreate_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(H5I_INVALID_HID, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME,
                                            ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype, space_id,
                                            H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid loc_id!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_loc_id);

        PART_BEGIN(H5Acreate_by_name_invalid_obj_name)
        {
            TESTING_2("H5Acreate_by_name with invalid object name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(group_id, NULL, ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME,
                                            attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with a NULL object name!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(group_id, "", ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME,
                                            attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid object name of ''!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_obj_name);

        PART_BEGIN(H5Acreate_by_name_invalid_attr_name)
        {
            TESTING_2("H5Acreate_by_name with invalid attribute name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME, NULL,
                                            attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with a NULL attribute name!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME, "",
                                            attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf(
                    "    created attribute using H5Acreate_by_name with an invalid attribute name of ''!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_attr_name);

        PART_BEGIN(H5Acreate_by_name_invalid_datatype)
        {
            TESTING_2("H5Acreate_by_name with invalid datatype");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME,
                                            ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, H5I_INVALID_HID,
                                            space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid datatype!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_datatype);

        PART_BEGIN(H5Acreate_by_name_invalid_dataspace)
        {
            TESTING_2("H5Acreate_by_name with invalid dataspace");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME,
                                            ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype,
                                            H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid dataspace!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_dataspace);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_dataspace);

        PART_BEGIN(H5Acreate_by_name_invalid_acpl)
        {
            TESTING_2("H5Acreate_by_name with invalid ACPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME,
                                            ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype, space_id,
                                            H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid ACPL!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_acpl);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_acpl);

        PART_BEGIN(H5Acreate_by_name_invalid_aapl)
        {
            TESTING_2("H5Acreate_by_name with invalid AAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME,
                                            ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype, space_id,
                                            H5P_DEFAULT, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid AAPL!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_aapl);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_aapl);

        PART_BEGIN(H5Acreate_by_name_invalid_lapl)
        {
            TESTING_2("H5Acreate_by_name with invalid LAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Acreate_by_name(container_group, ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME,
                                            ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME, attr_dtype, space_id,
                                            H5P_DEFAULT, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    created attribute using H5Acreate_by_name with an invalid LAPL!\n");
                H5Aclose(attr_id);
                PART_ERROR(H5Acreate_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Acreate_by_name_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Aopen(_by_idx).
 */
static int
test_open_attribute(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t attr_id   = H5I_INVALID_HID;
    hid_t space_id  = H5I_INVALID_HID;
    hid_t attr_type = H5I_INVALID_HID;
    hid_t gcpl_id   = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute opening");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or attribute aren't supported "
               "with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create GCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5P_DEFAULT, gcpl_id,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_OPEN_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_OPEN_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_type = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    /* Create several attributes */
    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_OPEN_TEST_ATTR_NAME, attr_type, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_OPEN_TEST_ATTR_NAME2, attr_type, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME2);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_OPEN_TEST_ATTR_NAME3, attr_type, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME3);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aopen)
        {
            TESTING_2("H5Aopen");

            if ((attr_id = H5Aopen(group_id, ATTRIBUTE_OPEN_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' using H5Aopen\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen);
            }

            PASSED();
        }
        PART_END(H5Aopen);

        PART_BEGIN(H5Aopen_by_name)
        {
            TESTING_2("H5Aopen_by_name");

            if ((attr_id = H5Aopen_by_name(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME,
                                           ATTRIBUTE_OPEN_TEST_ATTR_NAME, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' using H5Aopen_by_name\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen_by_name);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen_by_name);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_name);

        PART_BEGIN(H5Aopen_by_idx_crt_order_increasing)
        {
            TESTING_2("H5Aopen_by_idx by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking is not supported with this VOL connector\n");
                PART_EMPTY(H5Aopen_by_idx_crt_order_increasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                          H5_ITER_INC, 0, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by creation "
                       "order in increasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME, 0);
                PART_ERROR(H5Aopen_by_idx_crt_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen_by_idx_crt_order_increasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                          H5_ITER_INC, 1, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by creation "
                       "order in increasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME2, 1);
                PART_ERROR(H5Aopen_by_idx_crt_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME2);
                PART_ERROR(H5Aopen_by_idx_crt_order_increasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                          H5_ITER_INC, 2, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by creation "
                       "order in increasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME3, 2);
                PART_ERROR(H5Aopen_by_idx_crt_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME3);
                PART_ERROR(H5Aopen_by_idx_crt_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_crt_order_increasing);

        PART_BEGIN(H5Aopen_by_idx_crt_order_decreasing)
        {
            TESTING_2("H5Aopen_by_idx by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking is not supported with this VOL connector\n");
                PART_EMPTY(H5Aopen_by_idx_crt_order_decreasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                          H5_ITER_DEC, 2, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by creation "
                       "order in decreasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME, 2);
                PART_ERROR(H5Aopen_by_idx_crt_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen_by_idx_crt_order_decreasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                          H5_ITER_DEC, 1, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by creation "
                       "order in decreasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME2, 1);
                PART_ERROR(H5Aopen_by_idx_crt_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME2);
                PART_ERROR(H5Aopen_by_idx_crt_order_decreasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                          H5_ITER_DEC, 0, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by creation "
                       "order in decreasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME3, 0);
                PART_ERROR(H5Aopen_by_idx_crt_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME3);
                PART_ERROR(H5Aopen_by_idx_crt_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_crt_order_decreasing);

        PART_BEGIN(H5Aopen_by_idx_name_order_increasing)
        {
            TESTING_2("H5Aopen_by_idx by alphabetical order in increasing order");

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_INC, 0, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by alphabetical "
                       "order in increasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME, 0);
                PART_ERROR(H5Aopen_by_idx_name_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen_by_idx_name_order_increasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_INC, 1, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by alphabetical "
                       "order in increasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME2, 1);
                PART_ERROR(H5Aopen_by_idx_name_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME2);
                PART_ERROR(H5Aopen_by_idx_name_order_increasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_INC, 2, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by alphabetical "
                       "order in increasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME3, 2);
                PART_ERROR(H5Aopen_by_idx_name_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME3);
                PART_ERROR(H5Aopen_by_idx_name_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_name_order_increasing);

        PART_BEGIN(H5Aopen_by_idx_name_order_decreasing)
        {
            TESTING_2("H5Aopen_by_idx by alphabetical order in decreasing order");

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_DEC, 2, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by "
                       "alphabetical order in decreasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME, 2);
                PART_ERROR(H5Aopen_by_idx_name_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME);
                PART_ERROR(H5Aopen_by_idx_name_order_decreasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_DEC, 1, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by "
                       "alphabetical order in decreasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME2, 1);
                PART_ERROR(H5Aopen_by_idx_name_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME2);
                PART_ERROR(H5Aopen_by_idx_name_order_decreasing);
            }

            if ((attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_DEC, 0, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s' at index %d using H5Aopen_by_idx by "
                       "alphabetical order in decreasing order\n",
                       ATTRIBUTE_OPEN_TEST_ATTR_NAME3, 0);
                PART_ERROR(H5Aopen_by_idx_name_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_OPEN_TEST_ATTR_NAME3);
                PART_ERROR(H5Aopen_by_idx_name_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_name_order_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_type) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
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
        H5Tclose(attr_type);
        H5Aclose(attr_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can't be opened when
 * H5Aopen(_by_name/_by_idx) is passed invalid parameters.
 */
static int
test_open_attribute_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t attr_id   = H5I_INVALID_HID;
    hid_t space_id  = H5I_INVALID_HID;
    hid_t attr_type = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute opening with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id =
             generate_random_dataspace(ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_type = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME, attr_type, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aopen_invalid_loc_id)
        {
            TESTING_2("H5Aopen with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen(H5I_INVALID_HID, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen with an invalid loc_id!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aopen_invalid_loc_id);

        PART_BEGIN(H5Aopen_invalid_attr_name)
        {
            TESTING_2("H5Aopen with an invalid attribute name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen(group_id, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen with a NULL attribute name!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen(group_id, "", H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen with an invalid attribute name of ''!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Aopen_invalid_attr_name);

        PART_BEGIN(H5Aopen_invalid_aapl)
        {
            TESTING_2("H5Aopen with an invalid AAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen(group_id, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen with an invalid AAPL!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_invalid_aapl);
            }

            PASSED();
        }
        PART_END(H5Aopen_invalid_aapl);

        PART_BEGIN(H5Aopen_by_name_invalid_loc_id)
        {
            TESTING_2("H5Aopen_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                attr_id =
                    H5Aopen_by_name(H5I_INVALID_HID, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                    ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_name with an invalid loc_id!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_name_invalid_loc_id);

        PART_BEGIN(H5Aopen_by_name_invalid_obj_name)
        {
            TESTING_2("H5Aopen_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_name(container_group, NULL, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME,
                                          H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_name with a NULL object name!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_name(container_group, "", ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME,
                                          H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_name with an invalid object name of ''!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_name_invalid_obj_name);

        PART_BEGIN(H5Aopen_by_name_invalid_attr_name)
        {
            TESTING_2("H5Aopen_by_name with an invalid attribute name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_name(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                          NULL, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_name with a NULL attribute name!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_name(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME, "",
                                          H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf(
                    "    opened attribute '%s' using H5Aopen_by_name with an invalid attribute name of ''!\n",
                    ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_name_invalid_attr_name);

        PART_BEGIN(H5Aopen_by_name_invalid_aapl)
        {
            TESTING_2("H5Aopen_by_name with an invalid AAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_name(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                          ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME, H5I_INVALID_HID,
                                          H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_name with an invalid AAPL!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_aapl);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_name_invalid_aapl);

        PART_BEGIN(H5Aopen_by_name_invalid_lapl)
        {
            TESTING_2("H5Aopen_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_name(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                          ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME, H5P_DEFAULT,
                                          H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_name with an invalid LAPL!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_name_invalid_lapl);

        PART_BEGIN(H5Aopen_by_idx_invalid_loc_id)
        {
            TESTING_2("H5Aopen_by_idx with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(H5I_INVALID_HID, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with an invalid loc_id!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_invalid_loc_id);

        PART_BEGIN(H5Aopen_by_idx_invalid_obj_name)
        {
            TESTING_2("H5Aopen_by_idx with an invalid object name");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, NULL, H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT,
                                         H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with a NULL object name!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, "", H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT,
                                         H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with an invalid object name of ''!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_invalid_obj_name);

        PART_BEGIN(H5Aopen_by_idx_invalid_index_type)
        {
            TESTING_2("H5Aopen_by_idx with an invalid index type");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_UNKNOWN, H5_ITER_INC, 0, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with invalid index type "
                       "H5_INDEX_UNKNOWN!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_N, H5_ITER_INC, 0, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with invalid index type H5_INDEX_N!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_invalid_index_type);

        PART_BEGIN(H5Aopen_by_idx_invalid_iter_order)
        {
            TESTING_2("H5Aopen_by_idx with an invalid iteration order");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_NAME, H5_ITER_UNKNOWN, 0, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with invalid iteration order "
                       "H5_ITER_UNKNOWN!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_iter_order);
            }

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_NAME, H5_ITER_N, 0, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with invalid iteration order "
                       "H5_ITER_N!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_iter_order);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_invalid_iter_order);

        PART_BEGIN(H5Aopen_by_idx_invalid_aapl)
        {
            TESTING_2("H5Aopen_by_idx with an invalid AAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_NAME, H5_ITER_INC, 0, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with an invalid AAPL!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_aapl);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_invalid_aapl);

        PART_BEGIN(H5Aopen_by_idx_invalid_lapl)
        {
            TESTING_2("H5Aopen_by_idx with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                attr_id = H5Aopen_by_idx(container_group, ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                         H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (attr_id >= 0) {
                H5_FAILED();
                printf("    opened attribute '%s' using H5Aopen_by_idx with an invalid LAPL!\n",
                       ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME);
                H5Aclose(attr_id);
                PART_ERROR(H5Aopen_by_idx_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aopen_by_idx_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_type) < 0)
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
        H5Tclose(attr_type);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a simple write to an attribute
 * can be made.
 */
static int
test_write_attribute(void)
{
    hsize_t dims[ATTRIBUTE_WRITE_TEST_SPACE_RANK];
    size_t  i, data_size;
    htri_t  attr_exists;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    void   *data            = NULL;

    TESTING("H5Awrite");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or file flush aren't supported with "
               "this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_WRITE_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_WRITE_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_WRITE_TEST_SPACE_RANK, NULL, dims, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_WRITE_TEST_ATTR_NAME, ATTRIBUTE_WRITE_TEST_ATTR_DTYPE,
                              space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_WRITE_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    for (i = 0, data_size = 1; i < ATTRIBUTE_WRITE_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= ATTRIBUTE_WRITE_TEST_ATTR_DTYPE_SIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / ATTRIBUTE_WRITE_TEST_ATTR_DTYPE_SIZE; i++)
        ((int *)data)[i] = (int)i;

    if (H5Awrite(attr_id, ATTRIBUTE_WRITE_TEST_ATTR_DTYPE, data) < 0) {
        H5_FAILED();
        printf("    couldn't write to attribute\n");
        goto error;
    }

    /* Make sure that the attribute can be flushed to the file */
    if (H5Fflush(file_id, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        printf("    couldn't flush the attribute\n");
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

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
        if (data)
            free(data);
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
 * A test to check that writing an attribute fails when
 * H5Awrite is passed invalid parameters.
 */
static int
test_write_attribute_invalid_params(void)
{
    hsize_t dims[ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_SPACE_RANK];
    size_t  i, data_size;
    htri_t  attr_exists;
    herr_t  err_ret         = -1;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    void   *data            = NULL;

    TESTING_MULTIPART("H5Awrite with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id =
             generate_random_dataspace(ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_SPACE_RANK, NULL, dims, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_NAME,
                              ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    for (i = 0, data_size = 1; i < ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE_SIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE_SIZE; i++)
        ((int *)data)[i] = (int)i;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Awrite_invalid_attr_id)
        {
            TESTING_2("H5Awrite with an invalid attr_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Awrite(H5I_INVALID_HID, ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to attribute using an invalid attr_id!\n");
                PART_ERROR(H5Awrite_invalid_attr_id);
            }

            PASSED();
        }
        PART_END(H5Awrite_invalid_attr_id);

        PART_BEGIN(H5Awrite_invalid_datatype)
        {
            TESTING_2("H5Awrite with an invalid datatype");

            H5E_BEGIN_TRY
            {
                err_ret = H5Awrite(attr_id, H5I_INVALID_HID, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to attribute using an invalid datatype!\n");
                PART_ERROR(H5Awrite_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Awrite_invalid_datatype);

        PART_BEGIN(H5Awrite_invalid_data_buf)
        {
            TESTING_2("H5Awrite with an invalid data buffer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Awrite(attr_id, ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to attribute using an invalid data buffer!\n");
                PART_ERROR(H5Awrite_invalid_data_buf);
            }

            PASSED();
        }
        PART_END(H5Awrite_invalid_data_buf);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (data) {
        free(data);
        data = NULL;
    }

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
        if (data)
            free(data);
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
 * A test to check that simple data can be read back
 * and verified after it has been written to an
 * attribute.
 */
static int
test_read_attribute(void)
{
    hsize_t dims[ATTRIBUTE_READ_TEST_SPACE_RANK];
    size_t  i, data_size;
    htri_t  attr_exists;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    void   *data            = NULL;
    void   *read_buf        = NULL;

    TESTING("H5Aread");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_READ_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_READ_TEST_SPACE_RANK, NULL, dims, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_READ_TEST_ATTR_NAME, ATTRIBUTE_READ_TEST_ATTR_DTYPE,
                              space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_READ_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    for (i = 0, data_size = 1; i < ATTRIBUTE_READ_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= ATTRIBUTE_READ_TEST_ATTR_DTYPE_SIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;
    if (NULL == (read_buf = calloc(1, data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / ATTRIBUTE_READ_TEST_ATTR_DTYPE_SIZE; i++)
        ((int *)data)[i] = (int)i;

    if (H5Awrite(attr_id, ATTRIBUTE_READ_TEST_ATTR_DTYPE, data) < 0) {
        H5_FAILED();
        printf("    couldn't write to attribute\n");
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Aopen(group_id, ATTRIBUTE_READ_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open attribute\n");
        goto error;
    }

    if (H5Aread(attr_id, ATTRIBUTE_READ_TEST_ATTR_DTYPE, read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from attribute\n");
        goto error;
    }

    for (i = 0; i < data_size / ATTRIBUTE_READ_TEST_ATTR_DTYPE_SIZE; i++) {
        if (((int *)read_buf)[i] != (int)i) {
            H5_FAILED();
            printf("    data verification failed\n");
            goto error;
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (data)
            free(data);
        if (read_buf)
            free(read_buf);
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
 * A test to check that reading an attribute fails when
 * H5Aread is passed invalid parameters.
 */
static int
test_read_attribute_invalid_params(void)
{
    hsize_t dims[ATTRIBUTE_READ_INVALID_PARAMS_TEST_SPACE_RANK];
    size_t  i, data_size;
    htri_t  attr_exists;
    herr_t  err_ret         = -1;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    void   *data            = NULL;
    void   *read_buf        = NULL;

    TESTING_MULTIPART("H5Aread with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_READ_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_READ_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id =
             generate_random_dataspace(ATTRIBUTE_READ_INVALID_PARAMS_TEST_SPACE_RANK, NULL, dims, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_NAME,
                              ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    for (i = 0, data_size = 1; i < ATTRIBUTE_READ_INVALID_PARAMS_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE_SIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;
    if (NULL == (read_buf = calloc(1, data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE_SIZE; i++)
        ((int *)data)[i] = (int)i;

    if (H5Awrite(attr_id, ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE, data) < 0) {
        H5_FAILED();
        printf("    couldn't write to attribute\n");
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Aopen(group_id, ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open attribute\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aread_invalid_attr_id)
        {
            TESTING_2("H5Aread with an invalid attr_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aread(H5I_INVALID_HID, ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read attribute with an invalid attr_id!\n");
                PART_ERROR(H5Aread_invalid_attr_id);
            }

            PASSED();
        }
        PART_END(H5Aread_invalid_attr_id);

        PART_BEGIN(H5Aread_invalid_datatype)
        {
            TESTING_2("H5Aread with an invalid datatype");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aread(attr_id, H5I_INVALID_HID, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read attribute with an invalid datatype!\n");
                PART_ERROR(H5Aread_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Aread_invalid_datatype);

        PART_BEGIN(H5Aread_invalid_read_buf)
        {
            TESTING_2("H5Aread with an invalid read buffer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aread(attr_id, ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read attribute with an invalid read buffer!\n");
                PART_ERROR(H5Aread_invalid_read_buf);
            }

            PASSED();
        }
        PART_END(H5Aread_invalid_read_buf);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (data)
            free(data);
        if (read_buf)
            free(read_buf);
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
 * Test reading an empty attribute is ok
 */
static int
test_read_empty_attribute(void)
{
    hsize_t dims[ATTRIBUTE_READ_EMPTY_SPACE_RANK];
    size_t  i, data_size;
    htri_t  attr_exists;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    void   *read_buf        = NULL;

    TESTING("reading an empty attribute");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_READ_EMPTY_ATTR_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_READ_EMPTY_ATTR_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_READ_EMPTY_SPACE_RANK, NULL, dims, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_READ_EMPTY_ATTR_NAME, ATTRIBUTE_READ_EMPTY_DTYPE, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_READ_EMPTY_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Aopen(group_id, ATTRIBUTE_READ_EMPTY_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open attribute\n");
        goto error;
    }

    for (i = 0, data_size = 1; i < ATTRIBUTE_READ_EMPTY_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= ATTRIBUTE_READ_EMPTY_DTYPE_SIZE;

    if (NULL == (read_buf = calloc(1, data_size)))
        TEST_ERROR;

    if (H5Aread(attr_id, ATTRIBUTE_READ_EMPTY_DTYPE, read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from attribute\n");
        goto error;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (read_buf)
            free(read_buf);
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
 * A test to check that H5Aclose fails when it is passed
 * an invalid attribute ID.
 */
static int
test_close_attribute_invalid_id(void)
{
    herr_t err_ret = -1;
    hid_t  file_id = H5I_INVALID_HID;

    TESTING("H5Aclose with an invalid attribute ID");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    H5E_BEGIN_TRY
    {
        err_ret = H5Aclose(H5I_INVALID_HID);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    H5Aclose succeeded with an invalid attribute ID!\n");
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
 * A test to check that valid copies of an attribute's
 * dataspace and datatype can be retrieved with
 * H5Aget_space and H5Aget_type, respectively.
 */
static int
test_get_attribute_space_and_type(void)
{
    hsize_t attr_dims[ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK];
    size_t  i;
    htri_t  attr_exists;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   attr_dtype      = H5I_INVALID_HID;
    hid_t   attr_space_id   = H5I_INVALID_HID;
    hid_t   tmp_type_id     = H5I_INVALID_HID;
    hid_t   tmp_space_id    = H5I_INVALID_HID;

    TESTING_MULTIPART("retrieval of an attribute's dataspace and datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or attribute aren't supported with "
               "this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_SPACE_TYPE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_GET_SPACE_TYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((attr_space_id =
             generate_random_dataspace(ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK, NULL, attr_dims, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME, attr_dtype, attr_space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /* Retrieve the attribute's datatype and dataspace and verify them */
        PART_BEGIN(H5Aget_type)
        {
            TESTING_2("H5Aget_type");

            if ((tmp_type_id = H5Aget_type(attr_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve attribute's datatype\n");
                PART_ERROR(H5Aget_type);
            }

            {
                htri_t types_equal = H5Tequal(tmp_type_id, attr_dtype);

                if (types_equal < 0) {
                    H5_FAILED();
                    printf("    datatype was invalid\n");
                    PART_ERROR(H5Aget_type);
                }

                if (!types_equal) {
                    H5_FAILED();
                    printf("    attribute's datatype did not match\n");
                    PART_ERROR(H5Aget_type);
                }
            }

            PASSED();
        }
        PART_END(H5Aget_type);

        PART_BEGIN(H5Aget_space)
        {
            TESTING_2("H5Aget_space");

            if ((tmp_space_id = H5Aget_space(attr_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve attribute's dataspace\n");
                PART_ERROR(H5Aget_space);
            }

            {
                hsize_t space_dims[ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK];

                if (H5Sget_simple_extent_dims(tmp_space_id, space_dims, NULL) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve dimensions of dataspace\n");
                    PART_ERROR(H5Aget_space);
                }

                for (i = 0; i < ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK; i++)
                    if (space_dims[i] != attr_dims[i]) {
                        H5_FAILED();
                        printf("    attribute's dataspace dims didn't match\n");
                        PART_ERROR(H5Aget_space);
                    }
            }

            PASSED();
        }
        PART_END(H5Aget_space);

        /* Now close the attribute and verify that this still works after opening an
         * attribute instead of creating it
         */
        if (attr_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Aclose(attr_id);
            }
            H5E_END_TRY
            attr_id = H5I_INVALID_HID;
        }
        if (tmp_type_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(tmp_type_id);
            }
            H5E_END_TRY
            tmp_type_id = H5I_INVALID_HID;
        }
        if (tmp_space_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(tmp_space_id);
            }
            H5E_END_TRY
            tmp_space_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Aget_type_reopened)
        {
            TESTING_2("H5Aget_type after re-opening an attribute");

            if ((attr_id = H5Aopen(group_id, ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s'\n", ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME);
                PART_ERROR(H5Aget_type_reopened);
            }

            if ((tmp_type_id = H5Aget_type(attr_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve attribute's datatype\n");
                PART_ERROR(H5Aget_type_reopened);
            }

            {
                htri_t types_equal = H5Tequal(tmp_type_id, attr_dtype);

                if (types_equal < 0) {
                    H5_FAILED();
                    printf("    datatype was invalid\n");
                    PART_ERROR(H5Aget_type_reopened);
                }

                if (!types_equal) {
                    H5_FAILED();
                    printf("    attribute's datatype did not match\n");
                    PART_ERROR(H5Aget_type_reopened);
                }
            }

            if (attr_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Aclose(attr_id);
                }
                H5E_END_TRY
                attr_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(H5Aget_type_reopened);

        PART_BEGIN(H5Aget_space_reopened)
        {
            TESTING_2("H5Aget_space after re-opening an attribute");

            if ((attr_id = H5Aopen(group_id, ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s'\n", ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME);
                PART_ERROR(H5Aget_space_reopened);
            }

            if ((tmp_space_id = H5Aget_space(attr_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve attribute's dataspace\n");
                PART_ERROR(H5Aget_space_reopened);
            }

            {
                hsize_t space_dims[ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK];

                if (H5Sget_simple_extent_dims(tmp_space_id, space_dims, NULL) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve dimensions of dataspace\n");
                    PART_ERROR(H5Aget_space_reopened);
                }

                for (i = 0; i < ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK; i++) {
                    if (space_dims[i] != attr_dims[i]) {
                        H5_FAILED();
                        printf("    dataspace dims didn't match!\n");
                        PART_ERROR(H5Aget_space_reopened);
                    }
                }
            }

            if (attr_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Aclose(attr_id);
                }
                H5E_END_TRY
                attr_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(H5Aget_space_reopened);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(tmp_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(tmp_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(tmp_space_id);
        H5Sclose(attr_space_id);
        H5Tclose(tmp_type_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute's dataspace and datatype
 * can't be retrieved when H5Aget_space and H5Aget_type are passed
 * invalid parameters, respectively.
 */
static int
test_get_attribute_space_and_type_invalid_params(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id         = H5I_INVALID_HID;
    hid_t  attr_dtype      = H5I_INVALID_HID;
    hid_t  attr_space_id   = H5I_INVALID_HID;
    hid_t  tmp_type_id     = H5I_INVALID_HID;
    hid_t  tmp_space_id    = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Aget_type/H5Aget_space with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n",
               ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((attr_space_id = generate_random_dataspace(ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_SPACE_RANK,
                                                   NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_ATTR_NAME, attr_dtype,
                              attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /* Retrieve the attribute's datatype and dataspace and verify them */
        PART_BEGIN(H5Aget_type_invalid_attr_id)
        {
            TESTING_2("H5Aget_type with an invalid attr_id");

            H5E_BEGIN_TRY
            {
                tmp_type_id = H5Aget_type(H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (tmp_type_id >= 0) {
                H5_FAILED();
                printf("    retrieved copy of attribute's datatype using an invalid attr_id!\n");
                PART_ERROR(H5Aget_type_invalid_attr_id);
            }

            PASSED();
        }
        PART_END(H5Aget_type_invalid_attr_id);

        PART_BEGIN(H5Aget_space_invalid_attr_id)
        {
            TESTING_2("H5Aget_space with an invalid attr_id");

            H5E_BEGIN_TRY
            {
                tmp_space_id = H5Aget_space(H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (tmp_space_id >= 0) {
                H5_FAILED();
                printf("    retrieved copy of attribute's dataspace using an invalid attr_id!\n");
                PART_ERROR(H5Aget_space_invalid_attr_id);
            }

            PASSED();
        }
        PART_END(H5Aget_space_invalid_attr_id);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(tmp_space_id);
        H5Sclose(attr_space_id);
        H5Tclose(tmp_type_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an ACPL used for attribute creation
 * can be persisted and that a valid copy of that ACPL can
 * be retrieved later with a call to H5Aget_create_plist.
 */
static int
test_attribute_property_lists(void)
{
    H5T_cset_t encoding = H5T_CSET_UTF8;
    htri_t     attr_exists;
    hid_t      file_id         = H5I_INVALID_HID;
    hid_t      container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t      attr_id1 = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t      attr_dtype1 = H5I_INVALID_HID, attr_dtype2 = H5I_INVALID_HID;
    hid_t      acpl_id1 = H5I_INVALID_HID, acpl_id2 = H5I_INVALID_HID;
    hid_t      space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute property list operations");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or getting property list aren't "
               "supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_PROPERTY_LIST_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group\n");
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_PROPERTY_LIST_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype1 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;
    if ((attr_dtype2 = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((acpl_id1 = H5Pcreate(H5P_ATTRIBUTE_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create ACPL\n");
        goto error;
    }

    if (H5Pset_char_encoding(acpl_id1, encoding) < 0) {
        H5_FAILED();
        printf("    couldn't set ACPL property value\n");
        goto error;
    }

    if ((attr_id1 = H5Acreate2(group_id, ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME1, attr_dtype1, space_id,
                               acpl_id1, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if ((attr_id2 = H5Acreate2(group_id, ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME2, attr_dtype2, space_id,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if (H5Pclose(acpl_id1) < 0)
        TEST_ERROR;

    /* Verify the attributes have been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME1)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME2)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aget_create_plist)
        {
            TESTING_2("H5Aget_create_plist");

            /* Try to retrieve copies of the two property lists, one which has the property set and one which
             * does not */
            if ((acpl_id1 = H5Aget_create_plist(attr_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Aget_create_plist);
            }

            if ((acpl_id2 = H5Aget_create_plist(attr_id2)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Aget_create_plist);
            }

            /* Ensure that property list 1 has the property list set and property list 2 does not */
            encoding = H5T_CSET_ERROR;

            if (H5Pget_char_encoding(acpl_id1, &encoding) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve ACPL property value\n");
                PART_ERROR(H5Aget_create_plist);
            }

            if (H5T_CSET_UTF8 != encoding) {
                H5_FAILED();
                printf("   ACPL property value was incorrect\n");
                PART_ERROR(H5Aget_create_plist);
            }

            encoding = H5T_CSET_ERROR;

            if (H5Pget_char_encoding(acpl_id2, &encoding) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve ACPL property value\n");
                PART_ERROR(H5Aget_create_plist);
            }

            if (H5T_CSET_UTF8 == encoding) {
                H5_FAILED();
                printf("    ACPL property value was set!\n");
                PART_ERROR(H5Aget_create_plist);
            }

            PASSED();
        }
        PART_END(H5Aget_create_plist);

        /* Now close the property lists and attribute and see if we can still retrieve copies of
         * the property lists upon opening (instead of creating) an attribute
         */
        if (acpl_id1 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(acpl_id1);
            }
            H5E_END_TRY
            acpl_id1 = H5I_INVALID_HID;
        }
        if (acpl_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(acpl_id2);
            }
            H5E_END_TRY
            acpl_id2 = H5I_INVALID_HID;
        }
        if (attr_id1 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Aclose(attr_id1);
            }
            H5E_END_TRY
            attr_id1 = H5I_INVALID_HID;
        }
        if (attr_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Aclose(attr_id2);
            }
            H5E_END_TRY
            attr_id2 = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Aget_create_plist_reopened)
        {
            TESTING_2("H5Aget_create_plist after re-opening an attribute");

            if ((attr_id1 = H5Aopen(group_id, ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME1, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s'\n", ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME1);
                PART_ERROR(H5Aget_create_plist_reopened);
            }

            if ((attr_id2 = H5Aopen(group_id, ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME2, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s'\n", ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME2);
                PART_ERROR(H5Aget_create_plist_reopened);
            }

            if ((acpl_id1 = H5Aget_create_plist(attr_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Aget_create_plist_reopened);
            }

            if ((acpl_id2 = H5Aget_create_plist(attr_id2)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Aget_create_plist_reopened);
            }

            /* XXX: Check the value to be tested as above */
            PASSED();
        }
        PART_END(H5Aget_create_plist_reopened);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(acpl_id1) < 0)
        TEST_ERROR;
    if (H5Pclose(acpl_id2) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype1) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype2) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id1) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
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
        H5Pclose(acpl_id1);
        H5Pclose(acpl_id2);
        H5Sclose(space_id);
        H5Tclose(attr_dtype1);
        H5Tclose(attr_dtype2);
        H5Aclose(attr_id1);
        H5Aclose(attr_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute's name can be
 * correctly retrieved with H5Aget_name and
 * H5Aget_name_by_idx.
 */
static int
test_get_attribute_name(void)
{
    ssize_t name_buf_size;
    htri_t  attr_exists;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   attr_dtype      = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    hid_t   gcpl_id         = H5I_INVALID_HID;
    char   *name_buf        = NULL;

    TESTING_MULTIPART("retrieval of an attribute's name");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or attribute aren't supported "
               "with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create GCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5P_DEFAULT, gcpl_id,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_GET_NAME_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_GET_NAME_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    /* Create several attributes */
    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    /* Verify the attributes have been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute '%s' did not exist\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute '%s' did not exist\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute '%s' did not exist\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
        goto error;
    }

    /* Allocate the name buffer */
    name_buf_size = strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME) + 2;
    if (NULL == (name_buf = (char *)malloc((size_t)name_buf_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for storing attribute's name\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aget_name)
        {
            TESTING_2("H5Aget_name");

            if ((attr_id = H5Aopen(group_id, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s'\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name);
            }

            *name_buf = '\0';
            if (H5Aget_name(attr_id, (size_t)name_buf_size, name_buf) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve attribute name\n");
                PART_ERROR(H5Aget_name);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name);
            }

            PASSED();
        }
        PART_END(H5Aget_name);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Aget_name_by_idx_crt_order_increasing)
        {
            TESTING_2("H5Aget_name_by_idx by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aget_name_by_idx_crt_order_increasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                   H5_ITER_INC, 0, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "creation order in increasing order\n",
                       0);
                PART_ERROR(H5Aget_name_by_idx_crt_order_increasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name_by_idx_crt_order_increasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                   H5_ITER_INC, 1, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "creation order in increasing order\n",
                       1);
                PART_ERROR(H5Aget_name_by_idx_crt_order_increasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
                PART_ERROR(H5Aget_name_by_idx_crt_order_increasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                   H5_ITER_INC, 2, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "creation order in increasing order\n",
                       2);
                PART_ERROR(H5Aget_name_by_idx_crt_order_increasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
                PART_ERROR(H5Aget_name_by_idx_crt_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_crt_order_increasing);

        PART_BEGIN(H5Aget_name_by_idx_crt_order_decreasing)
        {
            TESTING_2("H5Aget_name_by_idx by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aget_name_by_idx_crt_order_decreasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                   H5_ITER_DEC, 2, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "creation order in decreasing order\n",
                       2);
                PART_ERROR(H5Aget_name_by_idx_crt_order_decreasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name_by_idx_crt_order_decreasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                   H5_ITER_DEC, 1, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "creation order in decreasing order\n",
                       1);
                PART_ERROR(H5Aget_name_by_idx_crt_order_decreasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
                PART_ERROR(H5Aget_name_by_idx_crt_order_decreasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                   H5_ITER_DEC, 0, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "creation order in decreasing order\n",
                       0);
                PART_ERROR(H5Aget_name_by_idx_crt_order_decreasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
                PART_ERROR(H5Aget_name_by_idx_crt_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_crt_order_decreasing);

        PART_BEGIN(H5Aget_name_by_idx_name_order_increasing)
        {
            TESTING_2("H5Aget_name_by_idx by alphabetical order in increasing order");

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_NAME,
                                   H5_ITER_INC, 0, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "alphabetical order in increasing order\n",
                       0);
                PART_ERROR(H5Aget_name_by_idx_name_order_increasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name_by_idx_name_order_increasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_NAME,
                                   H5_ITER_INC, 1, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "alphabetical order in increasing order\n",
                       1);
                PART_ERROR(H5Aget_name_by_idx_name_order_increasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
                PART_ERROR(H5Aget_name_by_idx_name_order_increasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_NAME,
                                   H5_ITER_INC, 2, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index by "
                       "alphabetical order in increasing order\n",
                       2);
                PART_ERROR(H5Aget_name_by_idx_name_order_increasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
                PART_ERROR(H5Aget_name_by_idx_name_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_name_order_increasing);

        PART_BEGIN(H5Aget_name_by_idx_name_order_decreasing)
        {
            TESTING_2("H5Aget_name_by_idx by alphabetical order in decreasing order");

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_NAME,
                                   H5_ITER_DEC, 2, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index "
                       "by alphabetical order in decreasing order\n",
                       2);
                PART_ERROR(H5Aget_name_by_idx_name_order_decreasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME);
                PART_ERROR(H5Aget_name_by_idx_name_order_decreasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_NAME,
                                   H5_ITER_DEC, 1, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index "
                       "by alphabetical order in decreasing order\n",
                       1);
                PART_ERROR(H5Aget_name_by_idx_name_order_decreasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2);
                PART_ERROR(H5Aget_name_by_idx_name_order_decreasing);
            }

            *name_buf = '\0';
            if (H5Aget_name_by_idx(container_group, ATTRIBUTE_GET_NAME_TEST_GROUP_NAME, H5_INDEX_NAME,
                                   H5_ITER_DEC, 0, name_buf, (size_t)name_buf_size, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve name of attribute at index %d using H5Aget_name_by_index "
                       "by alphabetical order in decreasing order\n",
                       0);
                PART_ERROR(H5Aget_name_by_idx_name_order_decreasing);
            }

            if (strncmp(name_buf, ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3,
                        strlen(ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3) + 1)) {
                H5_FAILED();
                printf("    retrieved attribute name '%s' didn't match '%s'\n", name_buf,
                       ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3);
                PART_ERROR(H5Aget_name_by_idx_name_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_name_order_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (name_buf) {
        free(name_buf);
        name_buf = NULL;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
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
        if (name_buf)
            free(name_buf);
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute's name can't be
 * retrieved when H5Aget_name(_by_idx) is passed invalid
 * parameters.
 */
static int
test_get_attribute_name_invalid_params(void)
{
    ssize_t name_buf_size;
    htri_t  attr_exists;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   attr_id         = H5I_INVALID_HID;
    hid_t   attr_dtype      = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    char   *name_buf        = NULL;

    TESTING_MULTIPART("retrieval of an attribute's name with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n",
               ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_SPACE_RANK, NULL, NULL,
                                              true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_ATTRIBUTE_NAME, attr_dtype,
                              space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_ATTRIBUTE_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    /*
     * Allocate an actual buffer for the tests.
     */

    if ((name_buf_size = H5Aget_name(attr_id, 0, NULL)) < 0) {
        H5_FAILED();
        printf("    couldn't retrieve name buf size\n");
        goto error;
    }

    if (NULL == (name_buf = (char *)malloc((size_t)name_buf_size + 1)))
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aget_name_invalid_attr_id)
        {
            TESTING_2("H5Aget_name with an invalid attr_id");

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name(H5I_INVALID_HID, (size_t)name_buf_size + 1, name_buf);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name with an invalid attr_id!\n");
                PART_ERROR(H5Aget_name_invalid_attr_id);
            }

            PASSED();
        }
        PART_END(H5Aget_name_invalid_attr_id);

        PART_BEGIN(H5Aget_name_invalid_name_buf)
        {
            TESTING_2("H5Aget_name with an invalid name buffer");

            H5E_BEGIN_TRY
            {
                name_buf_size = 1;
                name_buf_size = H5Aget_name(attr_id, (size_t)name_buf_size, NULL);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name with an invalid name buffer!\n");
                PART_ERROR(H5Aget_name_invalid_name_buf);
            }

            PASSED();
        }
        PART_END(H5Aget_name_invalid_name_buf);

        PART_BEGIN(H5Aget_name_by_idx_invalid_loc_id)
        {
            TESTING_2("H5Aget_name_by_idx with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(
                    H5I_INVALID_HID, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_NAME,
                    H5_ITER_INC, 0, name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with an invalid loc_id!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_invalid_loc_id);

        PART_BEGIN(H5Aget_name_by_idx_invalid_obj_name)
        {
            TESTING_2("H5Aget_name_by_idx with an invalid object name");

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(container_group, NULL, H5_INDEX_NAME, H5_ITER_INC, 0,
                                                   name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with a NULL object name!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(container_group, "", H5_INDEX_NAME, H5_ITER_INC, 0,
                                                   name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with an invalid object name "
                       "of ''!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_invalid_obj_name);

        PART_BEGIN(H5Aget_name_by_idx_invalid_index_type)
        {
            TESTING_2("H5Aget_name_by_idx with an invalid index type");

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(
                    container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_UNKNOWN,
                    H5_ITER_INC, 0, name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with invalid index type "
                       "H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(
                    container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_N,
                    H5_ITER_INC, 0, name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with invalid index type "
                       "H5_INDEX_N!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_invalid_index_type);

        PART_BEGIN(H5Aget_name_by_idx_invalid_iter_order)
        {
            TESTING_2("H5Aget_name_by_idx with an invalid iteration order");

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(
                    container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_NAME,
                    H5_ITER_UNKNOWN, 0, name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with invalid iteration order "
                       "H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_iter_order);
            }

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(
                    container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_NAME,
                    H5_ITER_N, 0, name_buf, (size_t)name_buf_size + 1, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with invalid iteration order "
                       "H5_ITER_N!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_iter_order);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_invalid_iter_order);

        PART_BEGIN(H5Aget_name_by_idx_invalid_name_buf)
        {
            TESTING_2("H5Aget_name_by_idx with an invalid name buffer");

            H5E_BEGIN_TRY
            {
                name_buf_size = 1;
                name_buf_size = H5Aget_name_by_idx(
                    container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_NAME,
                    H5_ITER_INC, 0, NULL, (size_t)name_buf_size, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf(
                    "    retrieved attribute name using H5Aget_name_by_idx with an invalid name buffer!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_name_buf);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_invalid_name_buf);

        PART_BEGIN(H5Aget_name_by_idx_invalid_lapl)
        {
            TESTING_2("H5Aget_name_by_idx with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                name_buf_size = H5Aget_name_by_idx(
                    container_group, ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME, H5_INDEX_NAME,
                    H5_ITER_INC, 0, name_buf, (size_t)name_buf_size + 1, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (name_buf_size >= 0) {
                H5_FAILED();
                printf("    retrieved attribute name using H5Aget_name_by_idx with an invalid LAPL!\n");
                PART_ERROR(H5Aget_name_by_idx_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aget_name_by_idx_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (name_buf) {
        free(name_buf);
        name_buf = NULL;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        if (name_buf)
            free(name_buf);
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Aget_storage_size.
 */
static int
test_get_attribute_storage_size(void)
{
    TESTING("H5Aget_storage_size");

    SKIPPED();

    return 0;
}

/*
 * A test to check the functionality of H5Aget_info(_by_idx).
 */
static int
test_get_attribute_info(void)
{
    H5A_info_t attr_info;
    htri_t     attr_exists;
    hid_t      file_id         = H5I_INVALID_HID;
    hid_t      container_group = H5I_INVALID_HID;
    hid_t      group_id        = H5I_INVALID_HID;
    hid_t      attr_id         = H5I_INVALID_HID;
    hid_t      attr_dtype      = H5I_INVALID_HID;
    hid_t      space_id        = H5I_INVALID_HID;
    hid_t      gcpl_id         = H5I_INVALID_HID;

    TESTING_MULTIPART("retrieval of attribute info");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or attribute aren't supported "
               "with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create GCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_INFO_TEST_GROUP_NAME, H5P_DEFAULT, gcpl_id,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_GET_INFO_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_GET_INFO_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    /* Create several attributes */
    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3);
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;

    /* Verify the attributes have been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute '%s' did not exist\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME);
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute '%s' did not exist\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2);
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute '%s' exists\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3);
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute '%s' did not exist\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aget_info)
        {
            TESTING_2("H5Aget_info");

            if ((attr_id = H5Aopen(group_id, ATTRIBUTE_GET_INFO_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open attribute '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME);
                PART_ERROR(H5Aget_info);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info(attr_id, &attr_info) < 0) {
                H5_FAILED();
                printf("    couldn't get attribute info\n");
                PART_ERROR(H5Aget_info);
            }

            if (attr_info.corder_valid && (attr_info.corder != 0)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)0);
                PART_ERROR(H5Aget_info);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    couldn't close attribute '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME);
                PART_ERROR(H5Aget_info);
            }

            PASSED();
        }
        PART_END(H5Aget_info);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Aget_info_by_name)
        {
            TESTING_2("H5Aget_info_by_name");

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_name(group_id, ".", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME, &attr_info,
                                    H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get attribute info by name '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME);
                PART_ERROR(H5Aget_info_by_name);
            }

            if (attr_info.corder_valid && (attr_info.corder != 0)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)0);
                PART_ERROR(H5Aget_info_by_name);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_name);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_name(group_id, ".", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2, &attr_info,
                                    H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get attribute info by name '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2);
                PART_ERROR(H5Aget_info_by_name);
            }

            if (attr_info.corder_valid && (attr_info.corder != 1)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)1);
                PART_ERROR(H5Aget_info_by_name);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_name);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_name(group_id, ".", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3, &attr_info,
                                    H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get attribute info by name '%s'\n", ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3);
                PART_ERROR(H5Aget_info_by_name);
            }

            if (attr_info.corder_valid && (attr_info.corder != 2)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)2);
                PART_ERROR(H5Aget_info_by_name);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_name);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_name);

        PART_BEGIN(H5Aget_info_by_idx_crt_order_increasing)
        {
            TESTING_2("H5Aget_info_by_idx by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aget_info_by_idx_crt_order_increasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, 0, &attr_info,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "creation order in increasing order\n",
                       0);
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 0)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)0);
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, 1, &attr_info,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "creation order in increasing order\n",
                       1);
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 1)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)1);
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, 2, &attr_info,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "creation order in increasing order\n",
                       2);
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 2)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)2);
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_crt_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_crt_order_increasing);

        PART_BEGIN(H5Aget_info_by_idx_crt_order_decreasing)
        {
            TESTING_2("H5Aget_info_by_idx by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aget_info_by_idx_crt_order_decreasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, 2, &attr_info,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "creation order in decreasing order\n",
                       2);
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 0)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)0);
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, 1, &attr_info,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "creation order in decreasing order\n",
                       1);
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 1)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)1);
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, 0, &attr_info,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "creation order in decreasing order\n",
                       0);
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 2)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)2);
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_crt_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_crt_order_decreasing);

        PART_BEGIN(H5Aget_info_by_idx_name_order_increasing)
        {
            TESTING_2("H5Aget_info_by_idx by alphabetical order in increasing order");

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, 0, &attr_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "alphabetical order in increasing order\n",
                       0);
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 0)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)0);
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, 1, &attr_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "alphabetical order in increasing order\n",
                       1);
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 1)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)1);
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, 2, &attr_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "alphabetical order in increasing order\n",
                       2);
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 2)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)2);
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_name_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_name_order_increasing);

        PART_BEGIN(H5Aget_info_by_idx_name_order_decreasing)
        {
            TESTING_2("H5Aget_info_by_idx by alphabetical order in decreasing order");

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, 2, &attr_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "alphabetical order in decreasing order\n",
                       2);
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 0)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)0);
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, 1, &attr_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "alphabetical order in decreasing order\n",
                       1);
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 1)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)1);
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            memset(&attr_info, 0, sizeof(attr_info));
            if (H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, 0, &attr_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    couldn't get info for attribute at index %d using H5Aget_info_by_idx by "
                       "alphabetical order in decreasing order\n",
                       0);
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            if (attr_info.corder_valid && (attr_info.corder != 2)) {
                H5_FAILED();
                printf("    attribute's creation order value '%lld' did not match expected value '%lld'\n",
                       (long long)attr_info.corder, (long long)2);
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            /* Ensure that the cset field is at least set to a meaningful value */
            if (attr_info.cset != H5T_CSET_ASCII && attr_info.cset != H5T_CSET_UTF8 &&
                attr_info.cset != H5T_CSET_ERROR) {
                H5_FAILED();
                printf("    attribute info's 'cset' field wasn't set to a meaningful value\n");
                PART_ERROR(H5Aget_info_by_idx_name_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_name_order_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Aget_info(_by_name/_by_idx)
 * doesn't succeed when passed invalid parameters.
 */
static int
test_get_attribute_info_invalid_params(void)
{
    H5A_info_t attr_info;
    htri_t     attr_exists;
    herr_t     err_ret         = -1;
    hid_t      file_id         = H5I_INVALID_HID;
    hid_t      container_group = H5I_INVALID_HID;
    hid_t      group_id        = H5I_INVALID_HID;
    hid_t      attr_id         = H5I_INVALID_HID;
    hid_t      attr_dtype      = H5I_INVALID_HID;
    hid_t      space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("retrieval of attribute info with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n",
               ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_SPACE_RANK, NULL, NULL,
                                              true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME, attr_dtype,
                              space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aget_info_invalid_attr_id)
        {
            TESTING_2("H5Aget_info with an invalid attr_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info(H5I_INVALID_HID, &attr_info);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info with an invalid attr_id!\n");
                PART_ERROR(H5Aget_info_invalid_attr_id);
            }

            PASSED();
        }
        PART_END(H5Aget_info_invalid_attr_id);

        PART_BEGIN(H5Aget_info_invalid_attr_info_pointer)
        {
            TESTING_2("H5Aget_info with an invalid attribute info pointer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info(attr_id, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info with an invalid attr_id!\n");
                PART_ERROR(H5Aget_info_invalid_attr_info_pointer);
            }

            PASSED();
        }
        PART_END(H5Aget_info_invalid_attr_info_pointer);

        PART_BEGIN(H5Aget_info_by_name_invalid_loc_id)
        {
            TESTING_2("H5Aget_info_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_name(H5I_INVALID_HID, ".",
                                              ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME, &attr_info,
                                              H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_name with an invalid loc_id!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_name_invalid_loc_id);

        PART_BEGIN(H5Aget_info_by_name_invalid_obj_name)
        {
            TESTING_2("H5Aget_info_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aget_info_by_name(group_id, NULL, ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME,
                                        &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_name with a NULL object name!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_name(group_id, "", ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME,
                                              &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_name with an invalid object name "
                       "of ''!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_name_invalid_obj_name);

        PART_BEGIN(H5Aget_info_by_name_invalid_attr_name)
        {
            TESTING_2("H5Aget_info_by_name with an invalid attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_name(group_id, ".", NULL, &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf(
                    "    retrieved attribute info using H5Aget_info_by_name with a NULL attribute name!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_name(group_id, ".", "", &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_name with an invalid attribute "
                       "name of ''!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_name_invalid_attr_name);

        PART_BEGIN(H5Aget_info_by_name_invalid_attr_info_pointer)
        {
            TESTING_2("H5Aget_info_by_name with an invalid attribute info pointer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_name(group_id, ".", ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME,
                                              NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_name with an invalid attribute "
                       "info pointer!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_attr_info_pointer);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_name_invalid_attr_info_pointer);

        PART_BEGIN(H5Aget_info_by_name_invalid_lapl)
        {
            TESTING_2("H5Aget_info_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_name(group_id, ".", ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME,
                                              &attr_info, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_name with an invalid LAPL!\n");
                PART_ERROR(H5Aget_info_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_name_invalid_lapl);

        PART_BEGIN(H5Aget_info_by_idx_invalid_loc_id)
        {
            TESTING_2("H5Aget_info_by_idx with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_idx(H5I_INVALID_HID, ".", H5_INDEX_NAME, H5_ITER_INC, 0, &attr_info,
                                             H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with an invalid loc_id!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_invalid_loc_id);

        PART_BEGIN(H5Aget_info_by_idx_invalid_obj_name)
        {
            TESTING_2("H5Aget_info_by_idx with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_idx(group_id, NULL, H5_INDEX_NAME, H5_ITER_INC, 0, &attr_info,
                                             H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with a NULL object name!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aget_info_by_idx(group_id, "", H5_INDEX_NAME, H5_ITER_INC, 0, &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with an invalid object name "
                       "of ''!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_invalid_obj_name);

        PART_BEGIN(H5Aget_info_by_idx_invalid_index_type)
        {
            TESTING_2("H5Aget_info_by_idx with an invalid index type");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_idx(group_id, ".", H5_INDEX_UNKNOWN, H5_ITER_INC, 0, &attr_info,
                                             H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with invalid index type "
                       "H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aget_info_by_idx(group_id, ".", H5_INDEX_N, H5_ITER_INC, 0, &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with invalid index type "
                       "H5_INDEX_N!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_invalid_index_type);

        PART_BEGIN(H5Aget_info_by_idx_invalid_iter_order)
        {
            TESTING_2("H5Aget_info_by_idx with an invalid iteration order");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_UNKNOWN, 0, &attr_info,
                                             H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with invalid iteration order "
                       "H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_iter_order);
            }

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_N, 0, &attr_info, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with invalid iteration order "
                       "H5_ITER_N!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_iter_order);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_invalid_iter_order);

        PART_BEGIN(H5Aget_info_by_idx_invalid_attr_info_pointer)
        {
            TESTING_2("H5Aget_info_by_idx with an invalid attribute info pointer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, 0, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with an invalid attribute "
                       "info pointer!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_attr_info_pointer);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_invalid_attr_info_pointer);

        PART_BEGIN(H5Aget_info_by_idx_invalid_lapl)
        {
            TESTING_2("H5Aget_info_by_idx with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aget_info_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, 0, &attr_info,
                                             H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    retrieved attribute info using H5Aget_info_by_idx with an invalid LAPL!\n");
                PART_ERROR(H5Aget_info_by_idx_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aget_info_by_idx_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can be renamed
 * with H5Arename and H5Arename_by_name.
 */
static int
test_rename_attribute(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t  attr_dtype    = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute renaming");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_RENAME_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_RENAME_TEST_GROUP_NAME);
        goto error;
    }

    if ((attr_space_id = generate_random_dataspace(ATTRIBUTE_RENAME_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME, attr_dtype, attr_space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if ((attr_id2 = H5Acreate2(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME2, attr_dtype, attr_space_id,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attributes have been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME2)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Arename)
        {
            TESTING_2("H5Arename");

            if (H5Arename(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME, ATTRIBUTE_RENAME_TEST_NEW_NAME) < 0) {
                H5_FAILED();
                printf("    couldn't rename attribute '%s' to '%s' using H5Arename\n",
                       ATTRIBUTE_RENAME_TEST_ATTR_NAME, ATTRIBUTE_RENAME_TEST_NEW_NAME);
                PART_ERROR(H5Arename);
            }

            /* Verify the attribute has been renamed */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Arename);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute did not get renamed!\n");
                PART_ERROR(H5Arename);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_TEST_NEW_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Arename);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute did not get renamed!\n");
                PART_ERROR(H5Arename);
            }

            PASSED();
        }
        PART_END(H5Arename);

        PART_BEGIN(H5Arename_by_name)
        {
            TESTING_2("H5Arename_by_name");

            if (H5Arename_by_name(container_group, ATTRIBUTE_RENAME_TEST_GROUP_NAME,
                                  ATTRIBUTE_RENAME_TEST_ATTR_NAME2, ATTRIBUTE_RENAME_TEST_NEW_NAME2,
                                  H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't rename attribute '%s' to '%s' using H5Arename_by_name\n",
                       ATTRIBUTE_RENAME_TEST_ATTR_NAME2, ATTRIBUTE_RENAME_TEST_NEW_NAME2);
                PART_ERROR(H5Arename_by_name);
            }

            /* Verify the attribute has been renamed */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Arename_by_name);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute did not get renamed!\n");
                PART_ERROR(H5Arename_by_name);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_TEST_NEW_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Arename_by_name);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute did not get renamed!\n");
                PART_ERROR(H5Arename_by_name);
            }

            PASSED();
        }
        PART_END(H5Arename_by_name);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
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
        H5Sclose(attr_space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can't be renamed
 * when H5Arename(_by_name) is passed invalid parameters.
 */
static int
test_rename_attribute_invalid_params(void)
{
    htri_t attr_exists;
    herr_t err_ret         = -1;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t  attr_dtype    = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute renaming with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_MORE)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((attr_space_id = generate_random_dataspace(ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_SPACE_RANK, NULL,
                                                   NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME, attr_dtype,
                              attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if ((attr_id2 = H5Acreate2(group_id, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME2, attr_dtype,
                               attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attributes have been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME2)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Arename_invalid_loc_id)
        {
            TESTING_2("H5Arename with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename(H5I_INVALID_HID, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME,
                                    ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename with an invalid loc_id!\n");
                PART_ERROR(H5Arename_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Arename_invalid_loc_id);

        PART_BEGIN(H5Arename_invalid_old_attr_name)
        {
            TESTING_2("H5Arename with an invalid old attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename(group_id, NULL, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename with a NULL old attribute name!\n");
                PART_ERROR(H5Arename_invalid_old_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename(group_id, "", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename with an invalid old attribute name of ''!\n");
                PART_ERROR(H5Arename_invalid_old_attr_name);
            }

            PASSED();
        }
        PART_END(H5Arename_invalid_old_attr_name);

        PART_BEGIN(H5Arename_invalid_new_attr_name)
        {
            TESTING_2("H5Arename with an invalid new attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename(group_id, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename with a NULL new attribute name!\n");
                PART_ERROR(H5Arename_invalid_new_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename(group_id, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME, "");
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename with an invalid new attribute name of ''!\n");
                PART_ERROR(H5Arename_invalid_new_attr_name);
            }

            PASSED();
        }
        PART_END(H5Arename_invalid_new_attr_name);

        PART_BEGIN(H5Arename_by_name_invalid_loc_id)
        {
            TESTING_2("H5Arename_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Arename_by_name(H5I_INVALID_HID, ".", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME,
                                      ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with an invalid loc_id!\n");
                PART_ERROR(H5Arename_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Arename_by_name_invalid_loc_id);

        PART_BEGIN(H5Arename_by_name_invalid_obj_name)
        {
            TESTING_2("H5Arename_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, NULL, ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME,
                                            ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with a NULL object name!\n");
                PART_ERROR(H5Arename_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, "", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME,
                                            ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with an invalid object name of ''!\n");
                PART_ERROR(H5Arename_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Arename_by_name_invalid_obj_name);

        PART_BEGIN(H5Arename_by_name_invalid_old_attr_name)
        {
            TESTING_2("H5Arename_by_name with an invalid old attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, ".", NULL,
                                            ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with a NULL old attribute name!\n");
                PART_ERROR(H5Arename_by_name_invalid_old_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, ".", "", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME,
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with an invalid old attribute name "
                       "of ''!\n");
                PART_ERROR(H5Arename_by_name_invalid_old_attr_name);
            }

            PASSED();
        }
        PART_END(H5Arename_by_name_invalid_old_attr_name);

        PART_BEGIN(H5Arename_by_name_invalid_new_attr_name)
        {
            TESTING_2("H5Arename_by_name with an invalid new attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, ".", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME,
                                            NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with a NULL new attribute name!\n");
                PART_ERROR(H5Arename_by_name_invalid_new_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, ".", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME, "",
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with an invalid new attribute name "
                       "of ''!\n");
                PART_ERROR(H5Arename_by_name_invalid_new_attr_name);
            }

            PASSED();
        }
        PART_END(H5Arename_by_name_invalid_new_attr_name);

        PART_BEGIN(H5Arename_by_name_invalid_lapl)
        {
            TESTING_2("H5Arename_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Arename_by_name(group_id, ".", ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME,
                                            ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    renamed attribute using H5Arename_by_name with an invalid LAPL!\n");
                PART_ERROR(H5Arename_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Arename_by_name_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
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
        H5Sclose(attr_space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check the functionality of attribute
 * iteration using H5Aiterate(_by_name) on a group.
 * Iteration is done in increasing and decreasing
 * order of both attribute name and attribute
 * creation order.
 */
static int
test_attribute_iterate_group(void)
{
    size_t link_counter;
    size_t i;
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  attr_id       = H5I_INVALID_HID;
    hid_t  attr_dtype    = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;
    hid_t  gcpl_id       = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute iteration on a group");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or iterate aren't "
               "supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create GCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME, H5P_DEFAULT,
                               gcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME);
        goto error;
    }

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;
    if ((attr_space_id =
             generate_random_dataspace(ATTRIBUTE_ITERATE_TEST_ATTR_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    /* Create some attributes with a reverse-ordering naming scheme to test creation order */
    for (i = 0; i < ATTRIBUTE_ITERATE_TEST_NUM_ATTRS; i++) {
        char attr_name[ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE];

        snprintf(attr_name, ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE, ATTRIBUTE_ITERATE_TEST_ATTR_NAME "%d",
                 (int)(ATTRIBUTE_ITERATE_TEST_NUM_ATTRS - i - 1));

        if ((attr_id = H5Acreate2(group_id, attr_name, attr_dtype, attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) <
            0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s'\n", attr_name);
            goto error;
        }

        /* Verify the attribute has been created */
        if ((attr_exists = H5Aexists(group_id, attr_name)) < 0) {
            H5_FAILED();
            printf("    couldn't determine if attribute '%s' exists\n", attr_name);
            goto error;
        }

        if (!attr_exists) {
            H5_FAILED();
            printf("    attribute '%s' did not exist\n", attr_name);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    failed to close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /*
         * NOTE: A counter is passed to the iteration callback to try to match up the
         * expected attributes with a given step throughout all of the following
         * iterations. Since the only information we can count on in the attribute
         * iteration callback is the attribute's name, we need some other way of
         * ensuring that the attributes are coming back in the correct order.
         */

        PART_BEGIN(H5Aiterate2_name_increasing)
        {
            TESTING_2("H5Aiterate by attribute name in increasing order");

            link_counter = 0;

            /* Test basic attribute iteration capability using both index types and both index orders */
            if (H5Aiterate2(group_id, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter) <
                0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type name in increasing order failed\n");
                PART_ERROR(H5Aiterate2_name_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 0) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_name_increasing);

        PART_BEGIN(H5Aiterate2_name_decreasing)
        {
            TESTING_2("H5Aiterate by attribute name in decreasing order");
            /* Reset the counter to the appropriate value for the next test */
            link_counter = ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(group_id, H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter) <
                0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type name in decreasing order failed\n");
                PART_ERROR(H5Aiterate2_name_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_name_decreasing);

        PART_BEGIN(H5Aiterate2_creation_increasing)
        {
            TESTING_2("H5Aiterate by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate2_creation_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(group_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, attr_iter_callback1,
                            &link_counter) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type creation order in increasing order failed\n");
                PART_ERROR(H5Aiterate2_creation_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_creation_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_creation_increasing);

        PART_BEGIN(H5Aiterate2_creation_decreasing)
        {
            TESTING_2("H5Aiterate by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate2_creation_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(group_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, attr_iter_callback1,
                            &link_counter) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type creation order in decreasing order failed\n");
                PART_ERROR(H5Aiterate2_creation_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_creation_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_creation_decreasing);

        PART_BEGIN(H5Aiterate_by_name_name_increasing)
        {
            TESTING_2("H5Aiterate_by_name by attribute name in increasing order");

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 0;

            if (H5Aiterate_by_name(
                    file_id, "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME,
                    H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type name in increasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_name_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_name_increasing);

        PART_BEGIN(H5Aiterate_by_name_name_decreasing)
        {
            TESTING_2("H5Aiterate_by_name by attribute name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            link_counter = ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(
                    file_id, "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME,
                    H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type name in decreasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_name_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_name_decreasing);

        PART_BEGIN(H5Aiterate_by_name_creation_increasing)
        {
            TESTING_2("H5Aiterate_by_name by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate_by_name_creation_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME,
                                   H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type creation order in increasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_creation_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_creation_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_creation_increasing);

        PART_BEGIN(H5Aiterate_by_name_creation_decreasing)
        {
            TESTING_2("H5Aiterate_by_name by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate_by_name_creation_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME,
                                   H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type creation order in decreasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_creation_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_creation_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_creation_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
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
        H5Sclose(attr_space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check the functionality of attribute
 * iteration using H5Aiterate(_by_name) on a dataset.
 * Iteration is done in increasing and decreasing
 * order of both attribute name and attribute
 * creation order.
 */
static int
test_attribute_iterate_dataset(void)
{
    size_t link_counter;
    size_t i;
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  dset_id       = H5I_INVALID_HID;
    hid_t  attr_id       = H5I_INVALID_HID;
    hid_t  dset_dtype    = H5I_INVALID_HID;
    hid_t  attr_dtype    = H5I_INVALID_HID;
    hid_t  dset_space_id = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;
    hid_t  dcpl_id       = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute iteration on a dataset");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, attribute, or iterate "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME);
        goto error;
    }

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create DCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(dcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((dset_space_id =
             generate_random_dataspace(ATTRIBUTE_ITERATE_TEST_DSET_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;
    if ((attr_space_id =
             generate_random_dataspace(ATTRIBUTE_ITERATE_TEST_ATTR_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, ATTRIBUTE_ITERATE_TEST_DSET_NAME, dset_dtype, dset_space_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", ATTRIBUTE_ITERATE_TEST_DSET_NAME);
        goto error;
    }

    /* Create some attributes with a reverse-ordering naming scheme to test creation order */
    for (i = 0; i < ATTRIBUTE_ITERATE_TEST_NUM_ATTRS; i++) {
        char attr_name[ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE];

        snprintf(attr_name, ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE, ATTRIBUTE_ITERATE_TEST_ATTR_NAME "%d",
                 (int)(ATTRIBUTE_ITERATE_TEST_NUM_ATTRS - i - 1));

        if ((attr_id = H5Acreate2(dset_id, attr_name, attr_dtype, attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) <
            0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s'\n", attr_name);
            goto error;
        }

        /* Verify the attribute has been created */
        if ((attr_exists = H5Aexists(dset_id, attr_name)) < 0) {
            H5_FAILED();
            printf("    couldn't determine if attribute '%s' exists\n", attr_name);
            goto error;
        }

        if (!attr_exists) {
            H5_FAILED();
            printf("    attribute '%s' did not exist\n", attr_name);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    failed to close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /*
         * NOTE: A counter is passed to the iteration callback to try to match up the
         * expected attributes with a given step throughout all of the following
         * iterations. Since the only information we can count on in the attribute
         * iteration callback is the attribute's name, we need some other way of
         * ensuring that the attributes are coming back in the correct order.
         */

        PART_BEGIN(H5Aiterate2_name_increasing)
        {
            TESTING_2("H5Aiterate by attribute name in increasing order");

            link_counter = 0;

            /* Test basic attribute iteration capability using both index types and both index orders */
            if (H5Aiterate2(dset_id, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter) <
                0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type name in increasing order failed\n");
                PART_ERROR(H5Aiterate2_name_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 0) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_name_increasing);

        PART_BEGIN(H5Aiterate2_name_decreasing)
        {
            TESTING_2("H5Aiterate by attribute name in decreasing order");
            /* Reset the counter to the appropriate value for the next test */
            link_counter = ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(dset_id, H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter) <
                0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type name in decreasing order failed\n");
                PART_ERROR(H5Aiterate2_name_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_name_decreasing);

        PART_BEGIN(H5Aiterate2_creation_increasing)
        {
            TESTING_2("H5Aiterate by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate2_creation_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(dset_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, attr_iter_callback1,
                            &link_counter) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type creation order in increasing order failed\n");
                PART_ERROR(H5Aiterate2_creation_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_creation_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_creation_increasing);

        PART_BEGIN(H5Aiterate2_creation_decreasing)
        {
            TESTING_2("H5Aiterate by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate2_creation_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(dset_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, attr_iter_callback1,
                            &link_counter) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type creation order in decreasing order failed\n");
                PART_ERROR(H5Aiterate2_creation_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_creation_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_creation_decreasing);

        PART_BEGIN(H5Aiterate_by_name_name_increasing)
        {
            TESTING_2("H5Aiterate_by_name by attribute name in increasing order");

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 0;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DSET_NAME,
                                   H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type name in increasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_name_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_name_increasing);

        PART_BEGIN(H5Aiterate_by_name_name_decreasing)
        {
            TESTING_2("H5Aiterate_by_name by attribute name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            link_counter = ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DSET_NAME,
                                   H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type name in decreasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_name_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_name_decreasing);

        PART_BEGIN(H5Aiterate_by_name_creation_increasing)
        {
            TESTING_2("H5Aiterate_by_name by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate_by_name_creation_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DSET_NAME,
                                   H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type creation order in increasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_creation_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_creation_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_creation_increasing);

        PART_BEGIN(H5Aiterate_by_name_creation_decreasing)
        {
            TESTING_2("H5Aiterate_by_name by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate_by_name_creation_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DSET_NAME,
                                   H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type creation order in decreasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_creation_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_creation_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_creation_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(dset_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        H5Pclose(dcpl_id);
        H5Sclose(attr_space_id);
        H5Sclose(dset_space_id);
        H5Tclose(attr_dtype);
        H5Tclose(dset_dtype);
        H5Aclose(attr_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check the functionality of attribute
 * iteration using H5Aiterate(_by_name) on a committed
 * datatype. Iteration is done in increasing and
 * decreasing order of both attribute name and attribute
 * creation order.
 */
static int
test_attribute_iterate_datatype(void)
{
    size_t link_counter;
    size_t i;
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  type_id       = H5I_INVALID_HID;
    hid_t  attr_id       = H5I_INVALID_HID;
    hid_t  attr_dtype    = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;
    hid_t  tcpl_id       = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute iteration on a committed datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, stored datatype, attribute, or iterate "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME);
        goto error;
    }

    if ((tcpl_id = H5Pcreate(H5P_DATATYPE_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create TCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(tcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_space_id =
             generate_random_dataspace(ATTRIBUTE_ITERATE_TEST_ATTR_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if (H5Tcommit2(group_id, ATTRIBUTE_ITERATE_TEST_DTYPE_NAME, type_id, H5P_DEFAULT, tcpl_id, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", ATTRIBUTE_ITERATE_TEST_DTYPE_NAME);
        goto error;
    }

    /* Create some attributes with a reverse-ordering naming scheme to test creation order */
    for (i = 0; i < ATTRIBUTE_ITERATE_TEST_NUM_ATTRS; i++) {
        char attr_name[ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE];

        snprintf(attr_name, ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE, ATTRIBUTE_ITERATE_TEST_ATTR_NAME "%d",
                 (int)(ATTRIBUTE_ITERATE_TEST_NUM_ATTRS - i - 1));

        if ((attr_id = H5Acreate2(type_id, attr_name, attr_dtype, attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) <
            0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s'\n", attr_name);
            goto error;
        }

        /* Verify the attribute has been created */
        if ((attr_exists = H5Aexists(type_id, attr_name)) < 0) {
            H5_FAILED();
            printf("    couldn't determine if attribute '%s' exists\n", attr_name);
            goto error;
        }

        if (!attr_exists) {
            H5_FAILED();
            printf("    attribute '%s' did not exist\n", attr_name);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    failed to close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /*
         * NOTE: A counter is passed to the iteration callback to try to match up the
         * expected attributes with a given step throughout all of the following
         * iterations. Since the only information we can count on in the attribute
         * iteration callback is the attribute's name, we need some other way of
         * ensuring that the attributes are coming back in the correct order.
         */

        PART_BEGIN(H5Aiterate2_name_increasing)
        {
            TESTING_2("H5Aiterate by attribute name in increasing order");

            link_counter = 0;

            /* Test basic attribute iteration capability using both index types and both index orders */
            if (H5Aiterate2(type_id, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter) <
                0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type name in increasing order failed\n");
                PART_ERROR(H5Aiterate2_name_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 0) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_name_increasing);

        PART_BEGIN(H5Aiterate2_name_decreasing)
        {
            TESTING_2("H5Aiterate by attribute name in decreasing order");
            /* Reset the counter to the appropriate value for the next test */
            link_counter = ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(type_id, H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter) <
                0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type name in decreasing order failed\n");
                PART_ERROR(H5Aiterate2_name_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_name_decreasing);

        PART_BEGIN(H5Aiterate2_creation_increasing)
        {
            TESTING_2("H5Aiterate by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate2_creation_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(type_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, attr_iter_callback1,
                            &link_counter) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type creation order in increasing order failed\n");
                PART_ERROR(H5Aiterate2_creation_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_creation_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_creation_increasing);

        PART_BEGIN(H5Aiterate2_creation_decreasing)
        {
            TESTING_2("H5Aiterate by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate2_creation_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate2(type_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, attr_iter_callback1,
                            &link_counter) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 by index type creation order in decreasing order failed\n");
                PART_ERROR(H5Aiterate2_creation_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate sentinel value is unchanged; supplied callback function must not "
                       "have been called!\n");
                PART_ERROR(H5Aiterate2_creation_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate2_creation_decreasing);

        PART_BEGIN(H5Aiterate_by_name_name_increasing)
        {
            TESTING_2("H5Aiterate_by_name by attribute name in increasing order");

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 0;

            if (H5Aiterate_by_name(
                    file_id,
                    "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME
                    "/" ATTRIBUTE_ITERATE_TEST_DTYPE_NAME,
                    H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type name in increasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_name_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_name_increasing);

        PART_BEGIN(H5Aiterate_by_name_name_decreasing)
        {
            TESTING_2("H5Aiterate_by_name by attribute name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            link_counter = ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(
                    file_id,
                    "/" ATTRIBUTE_TEST_GROUP_NAME "/" ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME
                    "/" ATTRIBUTE_ITERATE_TEST_DTYPE_NAME,
                    H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type name in decreasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_name_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_name_decreasing);

        PART_BEGIN(H5Aiterate_by_name_creation_increasing)
        {
            TESTING_2("H5Aiterate_by_name by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate_by_name_creation_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DTYPE_NAME,
                                   H5_INDEX_CRT_ORDER, H5_ITER_INC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type creation order in increasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_creation_increasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 2 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_creation_increasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_creation_increasing);

        PART_BEGIN(H5Aiterate_by_name_creation_decreasing)
        {
            TESTING_2("H5Aiterate_by_name by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Aiterate_by_name_creation_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            link_counter = 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS;

            if (H5Aiterate_by_name(file_id,
                                   "/" ATTRIBUTE_TEST_GROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME
                                   "/" ATTRIBUTE_ITERATE_TEST_DTYPE_NAME,
                                   H5_INDEX_CRT_ORDER, H5_ITER_DEC, NULL, attr_iter_callback1, &link_counter,
                                   H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name by index type creation order in decreasing order failed\n");
                PART_ERROR(H5Aiterate_by_name_creation_decreasing);
            }

            /* Make sure that the attribute iteration callback was actually called */
            if (link_counter == 3 * ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    H5Aiterate_by_name sentinel value is unchanged; supplied callback function "
                       "must not have been called!\n");
                PART_ERROR(H5Aiterate_by_name_creation_decreasing);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_creation_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(tcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
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
        H5Pclose(tcpl_id);
        H5Sclose(attr_space_id);
        H5Tclose(attr_dtype);
        H5Tclose(type_id);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check the functionality of attribute
 * iteration index saving using H5Aiterate(_by_name).
 * Iteration is done in increasing and decreasing
 * order of both attribute name and attribute
 * creation order.
 */
static int
test_attribute_iterate_index_saving(void)
{
    TESTING("attribute iteration index saving capability");

    SKIPPED();

    return 1;
}

/*
 * A test to check that an object's attributes can't
 * be iterated over when H5Aiterate(_by_name) is
 * passed invalid parameters.
 */
static int
test_attribute_iterate_invalid_params(void)
{
    herr_t err_ret = -1;
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID, attr_id3 = H5I_INVALID_HID,
          attr_id4      = H5I_INVALID_HID;
    hid_t attr_dtype    = H5I_INVALID_HID;
    hid_t attr_space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute iteration with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or iterate aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup\n");
        goto error;
    }

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_space_id = generate_random_dataspace(ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_SPACE_RANK,
                                                   NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME, attr_dtype,
                              attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if ((attr_id2 = H5Acreate2(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME2, attr_dtype,
                               attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if ((attr_id3 = H5Acreate2(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME3, attr_dtype,
                               attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    if ((attr_id4 = H5Acreate2(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME4, attr_dtype,
                               attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attributes have been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME2)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME3)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME4)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aiterate_invalid_loc_id)
        {
            TESTING_2("H5Aiterate with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aiterate2(H5I_INVALID_HID, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback2, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate with an invalid loc_id!\n");
                PART_ERROR(H5Aiterate_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aiterate_invalid_loc_id);

        PART_BEGIN(H5Aiterate_invalid_index_type)
        {
            TESTING_2("H5Aiterate with an invalid index type");

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aiterate2(group_id, H5_INDEX_UNKNOWN, H5_ITER_INC, NULL, attr_iter_callback2, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate with invalid index type "
                       "H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Aiterate_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate2(group_id, H5_INDEX_N, H5_ITER_INC, NULL, attr_iter_callback2, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate with invalid index type H5_INDEX_N!\n");
                PART_ERROR(H5Aiterate_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Aiterate_invalid_index_type);

        PART_BEGIN(H5Aiterate_invalid_index_order)
        {
            TESTING_2("H5Aiterate with an invalid index ordering");

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Aiterate2(group_id, H5_INDEX_NAME, H5_ITER_UNKNOWN, NULL, attr_iter_callback2, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate with invalid index ordering "
                       "H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Aiterate_invalid_index_order);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate2(group_id, H5_INDEX_NAME, H5_ITER_N, NULL, attr_iter_callback2, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf(
                    "    iterated over attributes using H5Aiterate with invalid index ordering H5_ITER_N!\n");
                PART_ERROR(H5Aiterate_invalid_index_order);
            }

            PASSED();
        }
        PART_END(H5Aiterate_invalid_index_order);

        PART_BEGIN(H5Aiterate_by_name_invalid_loc_id)
        {
            TESTING_2("H5Aiterate_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(H5I_INVALID_HID, ".", H5_INDEX_NAME, H5_ITER_INC, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with an invalid loc_id!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_invalid_loc_id);

        PART_BEGIN(H5Aiterate_by_name_invalid_obj_name)
        {
            TESTING_2("H5Aiterate_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, NULL, H5_INDEX_NAME, H5_ITER_INC, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with a NULL object name!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, "", H5_INDEX_NAME, H5_ITER_INC, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with an invalid object name "
                       "of ''!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_invalid_obj_name);

        PART_BEGIN(H5Aiterate_by_name_invalid_index_type)
        {
            TESTING_2("H5Aiterate_by_name with an invalid index type");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, ".", H5_INDEX_UNKNOWN, H5_ITER_INC, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with invalid index type "
                       "H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, ".", H5_INDEX_N, H5_ITER_INC, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with invalid index type "
                       "H5_INDEX_N!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_invalid_index_type);

        PART_BEGIN(H5Aiterate_by_name_invalid_index_order)
        {
            TESTING_2("H5Aiterate_by_name with an invalid index ordering");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, ".", H5_INDEX_NAME, H5_ITER_UNKNOWN, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with invalid index ordering "
                       "H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_index_order);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, ".", H5_INDEX_NAME, H5_ITER_N, NULL,
                                             attr_iter_callback2, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with invalid index ordering "
                       "H5_ITER_N!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_index_order);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_invalid_index_order);

        PART_BEGIN(H5Aiterate_by_name_invalid_lapl)
        {
            TESTING_2("H5Aiterate_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aiterate_by_name(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, NULL,
                                             attr_iter_callback2, NULL, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    iterated over attributes using H5Aiterate_by_name with an invalid LAPL!\n");
                PART_ERROR(H5Aiterate_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id3) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id4) < 0)
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
        H5Sclose(attr_space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Aclose(attr_id3);
        H5Aclose(attr_id4);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that attribute iteration performed
 * on an object with no attributes attached to it is
 * not problematic.
 */
static int
test_attribute_iterate_0_attributes(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id       = H5I_INVALID_HID;
    hid_t dset_dtype    = H5I_INVALID_HID;
    hid_t dset_space_id = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute iteration on object with 0 attributes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, attribute, or iterate "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup\n");
        goto error;
    }

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_space_id = generate_random_dataspace(ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_SPACE_RANK, NULL,
                                                   NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_NAME, dset_dtype,
                              dset_space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aiterate_0_attributes_native)
        {
            TESTING_2("H5Aiterate (native order)");

            if (H5Aiterate2(dset_id, H5_INDEX_NAME, H5_ITER_NATIVE, NULL, attr_iter_callback2, NULL) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 on object with 0 attributes failed\n");
                PART_ERROR(H5Aiterate_0_attributes_native);
            }

            PASSED();
        }
        PART_END(H5Aiterate_0_attributes_native);

        PART_BEGIN(H5Aiterate_0_attributes_inc)
        {
            TESTING_2("H5Aiterate (increasing order)");

            if (H5Aiterate2(dset_id, H5_INDEX_NAME, H5_ITER_INC, NULL, attr_iter_callback2, NULL) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 on object with 0 attributes failed\n");
                PART_ERROR(H5Aiterate_0_attributes_inc);
            }

            PASSED();
        }
        PART_END(H5Aiterate_0_attributes_inc);

        PART_BEGIN(H5Aiterate_0_attributes_dec)
        {
            TESTING_2("H5Aiterate (decreasing order)");

            if (H5Aiterate2(dset_id, H5_INDEX_NAME, H5_ITER_DEC, NULL, attr_iter_callback2, NULL) < 0) {
                H5_FAILED();
                printf("    H5Aiterate2 on object with 0 attributes failed\n");
                PART_ERROR(H5Aiterate_0_attributes_dec);
            }

            PASSED();
        }
        PART_END(H5Aiterate_0_attributes_dec);

        PART_BEGIN(H5Aiterate_by_name_0_attributes_native)
        {
            TESTING_2("H5Aiterate_by_name (native order)");

            if (H5Aiterate_by_name(group_id, ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_NAME, H5_INDEX_NAME,
                                   H5_ITER_NATIVE, NULL, attr_iter_callback2, NULL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name on object with 0 attributes failed\n");
                PART_ERROR(H5Aiterate_by_name_0_attributes_native);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_0_attributes_native);

        PART_BEGIN(H5Aiterate_by_name_0_attributes_inc)
        {
            TESTING_2("H5Aiterate_by_name (increasing order)");

            if (H5Aiterate_by_name(group_id, ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_NAME, H5_INDEX_NAME,
                                   H5_ITER_INC, NULL, attr_iter_callback2, NULL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name on object with 0 attributes failed\n");
                PART_ERROR(H5Aiterate_by_name_0_attributes_inc);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_0_attributes_inc);

        PART_BEGIN(H5Aiterate_by_name_0_attributes_dec)
        {
            TESTING_2("H5Aiterate_by_name (decreasing order)");
            if (H5Aiterate_by_name(group_id, ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_NAME, H5_INDEX_NAME,
                                   H5_ITER_DEC, NULL, attr_iter_callback2, NULL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Aiterate_by_name on object with 0 attributes failed\n");
                PART_ERROR(H5Aiterate_by_name_0_attributes_dec);
            }

            PASSED();
        }
        PART_END(H5Aiterate_by_name_0_attributes_dec);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(dset_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        H5Sclose(dset_space_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/* A compound type for test_attribute_compound_subset */
typedef struct attribute_compound_io_t {
    int a;
    int b;
} attribute_compound_io_t;

/*
 * A test to ensure that data is read back correctly from a attribute after it has
 * been written, using subsets of compound datatypes
 */
static int
test_attribute_compound_subset(void)
{
    hsize_t                 dims[1] = {ATTRIBUTE_COMPOUND_IO_ATTR_DIMS};
    size_t                  i;
    hid_t                   file_id         = H5I_INVALID_HID;
    hid_t                   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t                   attr_id      = H5I_INVALID_HID;
    hid_t                   space_id     = H5I_INVALID_HID;
    hid_t                   full_type_id = H5I_INVALID_HID;
    hid_t                   a_type_id    = H5I_INVALID_HID;
    hid_t                   b_type_id    = H5I_INVALID_HID;
    attribute_compound_io_t wbuf[ATTRIBUTE_COMPOUND_IO_ATTR_DIMS];
    attribute_compound_io_t rbuf[ATTRIBUTE_COMPOUND_IO_ATTR_DIMS];
    attribute_compound_io_t fbuf[ATTRIBUTE_COMPOUND_IO_ATTR_DIMS];
    attribute_compound_io_t erbuf[ATTRIBUTE_COMPOUND_IO_ATTR_DIMS];

    TESTING_MULTIPART(
        "verification of attribute data using H5Awrite then H5Aread with compound type subsets");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_COMPOUND_IO_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", ATTRIBUTE_COMPOUND_IO_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((full_type_id = H5Tcreate(H5T_COMPOUND, sizeof(attribute_compound_io_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(full_type_id, "a", HOFFSET(attribute_compound_io_t, a), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(full_type_id, "b", HOFFSET(attribute_compound_io_t, b), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((a_type_id = H5Tcreate(H5T_COMPOUND, sizeof(attribute_compound_io_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(a_type_id, "a", HOFFSET(attribute_compound_io_t, a), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((b_type_id = H5Tcreate(H5T_COMPOUND, sizeof(attribute_compound_io_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(b_type_id, "b", HOFFSET(attribute_compound_io_t, b), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_COMPOUND_IO_TEST_ATTR_NAME, full_type_id, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute '%s'\n", ATTRIBUTE_COMPOUND_IO_TEST_ATTR_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(write_full_read_full)
        {
            TESTING_2("H5Awrite then H5Aread with all compound members");

            /* Initialize wbuf */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                wbuf[i].a = (int)(2 * i);
                wbuf[i].b = (int)(2 * i + 1);
            }

            /* Write data */
            if (H5Awrite(attr_id, full_type_id, wbuf) < 0)
                PART_TEST_ERROR(write_full_read_full);

            /* Update fbuf to match file state */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                fbuf[i].a = wbuf[i].a;
                fbuf[i].b = wbuf[i].b;
            }

            /* Initialize rbuf to -1 */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                rbuf[i].a = -1;
                rbuf[i].b = -1;
            }

            /* Set erbuf (simply match file state since we're reading the whole
             * thing) */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                erbuf[i].a = fbuf[i].a;
                erbuf[i].b = fbuf[i].b;
            }

            /* Read data */
            if (H5Aread(attr_id, full_type_id, rbuf) < 0)
                PART_TEST_ERROR(write_full_read_full);

            /* Verify data */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                if (rbuf[i].a != erbuf[i].a)
                    PART_TEST_ERROR(write_full_read_full);
                if (rbuf[i].b != erbuf[i].b)
                    PART_TEST_ERROR(write_full_read_full);
            }

            PASSED();
        }
        PART_END(write_full_read_full);

        PART_BEGIN(read_a)
        {
            TESTING_2("H5Aread with compound member a");

            /* Initialize rbuf to -1 */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                rbuf[i].a = -1;
                rbuf[i].b = -1;
            }

            /* Set erbuf (element a comes from the file, element b in untouched)
             */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                erbuf[i].a = fbuf[i].a;
                erbuf[i].b = rbuf[i].b;
            }

            /* Read data */
            if (H5Aread(attr_id, a_type_id, rbuf) < 0)
                PART_TEST_ERROR(read_a);

            /* Verify data */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                if (rbuf[i].a != erbuf[i].a)
                    PART_TEST_ERROR(read_a);
                if (rbuf[i].b != erbuf[i].b)
                    PART_TEST_ERROR(read_a);
            }

            PASSED();
        }
        PART_END(read_a);

        PART_BEGIN(write_b_read_full)
        {
            TESTING_2("H5Awrite with compound member b then H5Aread with all compound members");

            /* Initialize wbuf */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                wbuf[i].a = (int)(2 * ATTRIBUTE_COMPOUND_IO_ATTR_DIMS + 2 * i);
                wbuf[i].b = (int)(2 * ATTRIBUTE_COMPOUND_IO_ATTR_DIMS + 2 * i + 1);
            }

            /* Write data */
            if (H5Awrite(attr_id, b_type_id, wbuf) < 0)
                PART_TEST_ERROR(write_b_read_full);

            /* Update fbuf to match file state - only element b was updated */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                fbuf[i].b = wbuf[i].b;
            }

            /* Initialize rbuf to -1 */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                rbuf[i].a = -1;
                rbuf[i].b = -1;
            }

            /* Set erbuf (simply match file state since we're reading the whole
             * thing) */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                erbuf[i].a = fbuf[i].a;
                erbuf[i].b = fbuf[i].b;
            }

            /* Read data */
            if (H5Aread(attr_id, full_type_id, rbuf) < 0)
                PART_TEST_ERROR(write_b_read_full);

            /* Verify data */
            for (i = 0; i < ATTRIBUTE_COMPOUND_IO_ATTR_DIMS; i++) {
                if (rbuf[i].a != erbuf[i].a)
                    PART_TEST_ERROR(write_b_read_full);
                if (rbuf[i].b != erbuf[i].b)
                    PART_TEST_ERROR(write_b_read_full);
            }

            PASSED();
        }
        PART_END(write_b_read_full);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

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
    if (H5Tclose(full_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(a_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(b_type_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
        H5Tclose(full_type_id);
        H5Tclose(a_type_id);
        H5Tclose(b_type_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that attributes preserve data
 * correctness for strings with ASCII or UTF-8 char sets
 */
static int
test_attribute_string_encodings(void)
{
    hid_t   file_id                               = H5I_INVALID_HID;
    hid_t   container_group                       = H5I_INVALID_HID;
    hid_t   dset_id1                              = H5I_INVALID_HID;
    hid_t   dset_id2                              = H5I_INVALID_HID;
    hid_t   type_id1                              = H5I_INVALID_HID;
    hid_t   type_id2                              = H5I_INVALID_HID;
    hid_t   space_id                              = H5I_INVALID_HID;
    hid_t   attr_id1                              = H5I_INVALID_HID;
    hid_t   attr_id2                              = H5I_INVALID_HID;
    hsize_t dims[ATTRIBUTE_STRING_ENCODINGS_RANK] = {ATTRIBUTE_STRING_ENCODINGS_EXTENT};
    size_t  ascii_str_size                        = 0;
    size_t  utf8_str_size                         = 0;
    char   *write_buf                             = NULL;
    char   *read_buf                              = NULL;

    TESTING_MULTIPART("string encoding read/write correctness on attributes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    ascii_str_size = strlen(ATTRIBUTE_STRING_ENCODINGS_ASCII_STRING);
    utf8_str_size  = strlen(ATTRIBUTE_STRING_ENCODINGS_UTF8_STRING);

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = H5Screate_simple(ATTRIBUTE_STRING_ENCODINGS_RANK, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataspace\n");
        goto error;
    }

    if ((type_id1 = H5Tcopy(H5T_C_S1)) < 0) {
        H5_FAILED();
        printf("    couldn't copy builtin string datatype\n");
        goto error;
    }

    if ((H5Tset_size(type_id1, ascii_str_size)) < 0) {
        H5_FAILED();
        printf("    couldn't set size of string datatype\n");
        goto error;
    }

    if ((H5Tset_cset(type_id1, H5T_CSET_ASCII)) < 0) {
        H5_FAILED();
        printf("    couldn't set character set of string to ASCII\n");
        goto error;
    }

    if ((dset_id1 = H5Dcreate(container_group, ATTRIBUTE_STRING_ENCODINGS_DSET_NAME1, type_id1, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with ascii string\n");
        goto error;
    }

    if ((attr_id1 = H5Acreate(dset_id1, ATTRIBUTE_STRING_ENCODINGS_ATTR_NAME1, type_id1, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute with ascii string\n");
        goto error;
    }

    if ((type_id2 = H5Tcopy(H5T_C_S1)) < 0) {
        H5_FAILED();
        printf("    couldn't copy builtin string datatype\n");
        goto error;
    }

    if ((H5Tset_size(type_id2, utf8_str_size)) < 0) {
        H5_FAILED();
        printf("    couldn't set size of string datatype\n");
        goto error;
    }

    if ((H5Tset_cset(type_id2, H5T_CSET_UTF8)) < 0) {
        H5_FAILED();
        printf("    couldn't set character set of string to UTF-8\n");
        goto error;
    }

    if ((dset_id2 = H5Dcreate(container_group, ATTRIBUTE_STRING_ENCODINGS_DSET_NAME2, type_id2, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with UTF-8 string\n");
        goto error;
    }

    if ((attr_id2 = H5Acreate(dset_id2, ATTRIBUTE_STRING_ENCODINGS_ATTR_NAME2, type_id2, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute with ascii string\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(ASCII_cset)
        {
            TESTING_2("ASCII character set");
            if ((write_buf = calloc(1, ascii_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for write buffer\n");
                PART_ERROR(ASCII_cset);
            }

            memcpy(write_buf, ATTRIBUTE_STRING_ENCODINGS_ASCII_STRING, ascii_str_size);

            if ((read_buf = calloc(1, ascii_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer\n");
                PART_ERROR(ASCII_cset);
            }

            if (H5Awrite(attr_id1, type_id1, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to attribute with ASCII string\n");
                PART_ERROR(ASCII_cset);
            }

            if (H5Aread(attr_id1, type_id1, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from attribute with ASCII string\n");
                PART_ERROR(ASCII_cset);
            }

            if (strncmp(write_buf, read_buf, ascii_str_size)) {
                H5_FAILED();
                printf("    incorrect data read from attribute with ASCII string\n");
                PART_ERROR(ASCII_cset);
            }

            free(write_buf);
            write_buf = NULL;

            free(read_buf);
            read_buf = NULL;

            PASSED();
        }
        PART_END(ASCII_cset);

        PART_BEGIN(UTF8_cset)
        {
            TESTING_2("UTF-8 character set");

            if ((write_buf = calloc(1, utf8_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for write buffer\n");
                PART_ERROR(UTF8_cset);
            }

            memcpy(write_buf, ATTRIBUTE_STRING_ENCODINGS_UTF8_STRING, utf8_str_size);

            if ((read_buf = calloc(1, utf8_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer\n");
                PART_ERROR(UTF8_cset);
            }

            if (H5Awrite(attr_id2, type_id2, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to attribute with UTF-8 string\n");
                PART_ERROR(UTF8_cset);
            }

            if (H5Aread(attr_id2, type_id2, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from attribute with UTF-8 string\n");
                PART_ERROR(UTF8_cset);
            }

            if (strncmp(write_buf, read_buf, utf8_str_size)) {
                H5_FAILED();
                printf("    incorrect data read from attribute with UTF-8 string\n");
                PART_ERROR(UTF8_cset);
            }

            free(write_buf);
            write_buf = NULL;

            free(read_buf);
            read_buf = NULL;

            PASSED();
        }
        PART_END(UTF8_cset);

        PASSED();
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
        TEST_ERROR;
    if (H5Tclose(type_id1) < 0)
        TEST_ERROR;
    if (H5Tclose(type_id2) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id1) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
        TEST_ERROR;
    if (write_buf)
        free(write_buf);
    if (read_buf)
        free(read_buf);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file_id);
        H5Gclose(container_group);
        H5Dclose(dset_id1);
        H5Dclose(dset_id2);
        H5Tclose(type_id1);
        H5Tclose(type_id2);
        H5Aclose(attr_id1);
        H5Aclose(attr_id2);
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that an attribute can be deleted
 * using H5Adelete(_by_idx).
 */
static int
test_delete_attribute(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id         = H5I_INVALID_HID;
    hid_t  attr_dtype      = H5I_INVALID_HID;
    hid_t  space_id        = H5I_INVALID_HID;
    hid_t  gcpl_id         = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute deletion");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or attribute aren't supported "
               "with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create GCPL for attribute creation order tracking\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_attr_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED) < 0) {
            H5_FAILED();
            printf("    couldn't set attribute creation order tracking\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5P_DEFAULT, gcpl_id,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_DELETION_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_DELETION_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Adelete)
        {
            TESTING_2("H5Adelete");

            /* Test H5Adelete */
            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            /* Delete the attribute */
            if (H5Adelete(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            /* Verify the attribute has been deleted */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete);
            }

            PASSED();
        }
        PART_END(H5Adelete);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Adelete_by_name)
        {
            TESTING_2("H5Adelete_by_name");

            /* Test H5Adelete_by_name */
            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            /* Verify the attribute has been created */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            /* Delete the attribute */
            if (H5Adelete_by_name(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME,
                                  ATTRIBUTE_DELETION_TEST_ATTR_NAME, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            /* Verify the attribute has been deleted */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_name);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_name);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Adelete_by_idx_crt_order_increasing)
        {
            TESTING_2("H5Adelete_by_idx by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Adelete_by_idx_crt_order_increasing);
            }

            /* Create several attributes */
            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            /* Verify the attributes have been created */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            /* Delete an attribute */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                 H5_ITER_INC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by creation order in "
                       "increasing order\n");
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            /* Ensure that the attribute is gone and others remain */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            /* Repeat until all attributes have been deleted */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                 H5_ITER_INC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by creation order in "
                       "increasing order\n");
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                 H5_ITER_INC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by creation order in "
                       "increasing order\n");
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_crt_order_increasing);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Adelete_by_idx_crt_order_decreasing)
        {
            TESTING_2("H5Adelete_by_idx by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Adelete_by_idx_crt_order_decreasing);
            }

            /* Create several attributes */
            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            /* Verify the attributes have been created */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            /* Delete an attribute */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                 H5_ITER_DEC, 2, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by creation order in "
                       "decreasing order\n");
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            /* Ensure that the attribute is gone and others remain */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            /* Repeat until all attributes have been deleted */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                 H5_ITER_DEC, 1, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by creation order in "
                       "decreasing order\n");
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_CRT_ORDER,
                                 H5_ITER_DEC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by creation order in "
                       "decreasing order\n");
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_crt_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_crt_order_decreasing);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Adelete_by_idx_name_order_increasing)
        {
            TESTING_2("H5Adelete_by_idx by alphabetical order in increasing order");

            /* Create several attributes */
            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            /* Verify the attributes have been created */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            /* Delete an attribute */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_NAME,
                                 H5_ITER_INC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by alphabetical order in "
                       "increasing order\n");
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            /* Ensure that the attribute is gone and others remain */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            /* Repeat until all attributes have been deleted */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_NAME,
                                 H5_ITER_INC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by alphabetical order in "
                       "increasing order\n");
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_NAME,
                                 H5_ITER_INC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by alphabetical order in "
                       "increasing order\n");
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_name_order_increasing);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY

        PART_BEGIN(H5Adelete_by_idx_name_order_decreasing)
        {
            TESTING_2("H5Adelete_by_idx by alphabetical order in decreasing order");

            /* Create several attributes */
            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3, attr_dtype, space_id,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (H5Aclose(attr_id) < 0) {
                H5_FAILED();
                printf("    failed to close attribute '%s'\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            /* Verify the attributes have been created */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' didn't exist before deletion\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            /* Delete an attribute */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_NAME,
                                 H5_ITER_DEC, 2, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by alphabetical order in "
                       "decreasing order\n");
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            /* Ensure that the attribute is gone and others remain */
            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            /* Repeat until all attributes have been deleted */
            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_NAME,
                                 H5_ITER_DEC, 1, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by alphabetical order in "
                       "decreasing order\n");
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' doesn't exist after deletion of a different attribute!\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (H5Adelete_by_idx(container_group, ATTRIBUTE_DELETION_TEST_GROUP_NAME, H5_INDEX_NAME,
                                 H5_ITER_DEC, 0, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to delete attribute using H5Adelete_by_idx by alphabetical order in "
                       "decreasing order\n");
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME2)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME2);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_TEST_ATTR_NAME3)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute '%s' exists\n",
                       ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            if (attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' exists after deletion!\n", ATTRIBUTE_DELETION_TEST_ATTR_NAME3);
                PART_ERROR(H5Adelete_by_idx_name_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_name_order_decreasing);

        H5E_BEGIN_TRY
        {
            H5Aclose(attr_id);
            attr_id = H5I_INVALID_HID;
        }
        H5E_END_TRY
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an attribute can't be deleted
 * when H5Adelete(_by_name/_by_idx) is passed invalid
 * parameters.
 */
static int
test_delete_attribute_invalid_params(void)
{
    herr_t err_ret = -1;
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id         = H5I_INVALID_HID;
    hid_t  attr_dtype      = H5I_INVALID_HID;
    hid_t  space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute deletion with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n",
               ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_SPACE_RANK, NULL, NULL,
                                              true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME, attr_dtype,
                              space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute didn't exists\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Adelete_invalid_loc_id)
        {
            TESTING_2("H5Adelete with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete(H5I_INVALID_HID, ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete with an invalid loc_id!\n");
                PART_ERROR(H5Adelete_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Adelete_invalid_loc_id);

        PART_BEGIN(H5Adelete_invalid_attr_name)
        {
            TESTING_2("H5Adelete with an invalid attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete(group_id, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete with a NULL attribute name!\n");
                PART_ERROR(H5Adelete_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete(group_id, "");
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete with an invalid attribute name of ''!\n");
                PART_ERROR(H5Adelete_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Adelete_invalid_attr_name);

        PART_BEGIN(H5Adelete_by_name_invalid_loc_id)
        {
            TESTING_2("H5Adelete_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_name(H5I_INVALID_HID, ".",
                                            ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_name with an invalid loc_id!\n");
                PART_ERROR(H5Adelete_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_name_invalid_loc_id);

        PART_BEGIN(H5Adelete_by_name_invalid_obj_name)
        {
            TESTING_2("H5Adelete_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_name(group_id, NULL, ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME,
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_name with a NULL object name!\n");
                PART_ERROR(H5Adelete_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_name(group_id, "", ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME,
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf(
                    "    deleted an attribute using H5Adelete_by_name with an invalid object name of ''!\n");
                PART_ERROR(H5Adelete_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_name_invalid_obj_name);

        PART_BEGIN(H5Adelete_by_name_invalid_attr_name)
        {
            TESTING_2("H5Adelete_by_name with an invalid attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_name(group_id, ".", NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_name with a NULL attribute name!\n");
                PART_ERROR(H5Adelete_by_name_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_name(group_id, ".", "", H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_name with an invalid attribute name of "
                       "''!\n");
                PART_ERROR(H5Adelete_by_name_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_name_invalid_attr_name);

        PART_BEGIN(H5Adelete_by_name_invalid_lapl)
        {
            TESTING_2("H5Adelete_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_name(group_id, ".", ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME,
                                            H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_name with an invalid LAPL!\n");
                PART_ERROR(H5Adelete_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_name_invalid_lapl);

        PART_BEGIN(H5Adelete_by_idx_invalid_loc_id)
        {
            TESTING_2("H5Adelete_by_idx with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(H5I_INVALID_HID, ".", H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_idx with an invalid loc_id!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_invalid_loc_id);

        PART_BEGIN(H5Adelete_by_idx_invalid_obj_name)
        {
            TESTING_2("H5Adelete_by_idx with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, NULL, H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_idx with a NULL object name!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, "", H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf(
                    "    deleted an attribute using H5Adelete_by_idx with an invalid object name of ''!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_invalid_obj_name);

        PART_BEGIN(H5Adelete_by_idx_invalid_index_type)
        {
            TESTING_2("H5Adelete_by_idx with an invalid index type");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, ".", H5_INDEX_UNKNOWN, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_idx with invalid index type "
                       "H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, ".", H5_INDEX_N, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf(
                    "    deleted an attribute using H5Adelete_by_idx with invalid index type H5_INDEX_N!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_invalid_index_type);

        PART_BEGIN(H5Adelete_by_idx_invalid_index_order)
        {
            TESTING_2("H5Adelete_by_idx with an invalid index ordering");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_UNKNOWN, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_idx with invalid index ordering "
                       "H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_index_order);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_N, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_idx with invalid index ordering "
                       "H5_ITER_N!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_index_order);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_invalid_index_order);

        PART_BEGIN(H5Adelete_by_idx_invalid_lapl)
        {
            TESTING_2("H5Adelete_by_idx with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Adelete_by_idx(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, 0, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    deleted an attribute using H5Adelete_by_idx with an invalid LAPL!\n");
                PART_ERROR(H5Adelete_by_idx_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Adelete_by_idx_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Aexists and H5Aexists_by_name.
 */
static int
test_attribute_exists(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id         = H5I_INVALID_HID;
    hid_t  attr_dtype      = H5I_INVALID_HID;
    hid_t  space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute existence");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_EXISTS_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_EXISTS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_EXISTS_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_EXISTS_TEST_ATTR_NAME, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aexists)
        {
            TESTING_2("H5Aexists");

            if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_EXISTS_TEST_ATTR_NAME)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists\n");
                PART_ERROR(H5Aexists);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' did not exist\n", ATTRIBUTE_EXISTS_TEST_ATTR_NAME);
                PART_ERROR(H5Aexists);
            }

            PASSED();
        }
        PART_END(H5Aexists);

        PART_BEGIN(H5Aexists_by_name)
        {
            TESTING_2("H5Aexists_by_name");

            if ((attr_exists = H5Aexists_by_name(container_group, ATTRIBUTE_EXISTS_TEST_GROUP_NAME,
                                                 ATTRIBUTE_EXISTS_TEST_ATTR_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if attribute exists by name\n");
                PART_ERROR(H5Aexists_by_name);
            }

            if (!attr_exists) {
                H5_FAILED();
                printf("    attribute '%s' did not exist by name\n", ATTRIBUTE_EXISTS_TEST_ATTR_NAME);
                PART_ERROR(H5Aexists_by_name);
            }

            PASSED();
        }
        PART_END(H5Aexists_by_name);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that H5Aexists(_by_name) will fail when
 * given invalid parameters.
 */
static int
test_attribute_exists_invalid_params(void)
{
    herr_t err_ret = -1;
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id         = H5I_INVALID_HID;
    hid_t  attr_dtype      = H5I_INVALID_HID;
    hid_t  space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("attribute existence with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", ATTRIBUTE_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_SPACE_RANK, NULL, NULL,
                                              true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute didn't exists\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Aexists_invalid_loc_id)
        {
            TESTING_2("H5Aexists with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists(H5I_INVALID_HID, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists with an invalid loc_id succeeded!\n");
                PART_ERROR(H5Aexists_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aexists_invalid_loc_id);

        PART_BEGIN(H5Aexists_invalid_attr_name)
        {
            TESTING_2("H5Aexists with invalid attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists(group_id, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists with a NULL attribute name succeeded!\n");
                PART_ERROR(H5Aexists_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists(group_id, "");
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists with an invalid attribute name of '' succeeded!\n");
                PART_ERROR(H5Aexists_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Aexists_invalid_attr_name);

        PART_BEGIN(H5Aexists_by_name_invalid_loc_id)
        {
            TESTING_2("H5Aexists_by_name with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists_by_name(H5I_INVALID_HID, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME,
                                            ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists_by_name with an invalid loc_id succeeded!\n");
                PART_ERROR(H5Aexists_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Aexists_by_name_invalid_loc_id);

        PART_BEGIN(H5Aexists_by_name_invalid_obj_name)
        {
            TESTING_2("H5Aexists_by_name with invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists_by_name(file_id, NULL, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME,
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists_by_name with a NULL object name succeeded!\n");
                PART_ERROR(H5Aexists_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists_by_name(file_id, "", ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME,
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists_by_name with an invalid object name of '' succeeded!\n");
                PART_ERROR(H5Aexists_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Aexists_by_name_invalid_obj_name);

        PART_BEGIN(H5Aexists_by_name_invalid_attr_name)
        {
            TESTING_2("H5Aexists_by_name with invalid attribute name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists_by_name(file_id, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME, NULL,
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists_by_name with a NULL attribute name succeeded!\n");
                PART_ERROR(H5Aexists_by_name_invalid_attr_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists_by_name(file_id, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME, "",
                                            H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists_by_name with an invalid attribute name of '' succeeded!\n");
                PART_ERROR(H5Aexists_by_name_invalid_attr_name);
            }

            PASSED();
        }
        PART_END(H5Aexists_by_name_invalid_attr_name);

        PART_BEGIN(H5Aexists_by_name_invalid_lapl)
        {
            TESTING_2("H5Aexists_by_name with an invalid link access property list");

            H5E_BEGIN_TRY
            {
                err_ret = H5Aexists_by_name(file_id, ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME,
                                            ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Aexists_by_name with an invalid link access property list succeeded!\n");
                PART_ERROR(H5Aexists_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Aexists_by_name_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to make sure many attributes can be written
 * to the file
 */
static int
test_attribute_many(void)
{
    unsigned u;
    htri_t   attr_exists;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID;
    hid_t    group_id        = H5I_INVALID_HID;
    hid_t    attr_id         = H5I_INVALID_HID;
    hid_t    attr_dtype      = H5I_INVALID_HID;
    hid_t    space_id        = H5I_INVALID_HID;
    char     attrname[ATTRIBUTE_MANY_NAME_BUF_SIZE]; /* Name of attribute */

    TESTING("creating many attributes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_MANY_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create the group '%s'\n", ATTRIBUTE_MANY_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_MANY_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    /* Create many attributes */
    for (u = 0; u < ATTRIBUTE_MANY_NUMB; u++) {
        snprintf(attrname, sizeof(attrname), "many-%06u", u);

        if ((attr_id = H5Acreate2(group_id, attrname, attr_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute\n");
            goto error;
        }

        /* Verify the attribute has been created */
        if ((attr_exists = H5Aexists(group_id, attrname)) < 0) {
            H5_FAILED();
            printf("    couldn't determine if attribute exists\n");
            goto error;
        }

        if (!attr_exists) {
            H5_FAILED();
            printf("    attribute did not exist\n");
            goto error;
        }

        if (H5Aclose(attr_id) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to make sure an attribute can be opened for
 * a second time
 */
static int
test_attribute_duplicate_id(void)
{
    htri_t attr_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  attr_id = H5I_INVALID_HID, attr_id2 = H5I_INVALID_HID;
    hid_t  attr_dtype = H5I_INVALID_HID;
    hid_t  space_id   = H5I_INVALID_HID;

    TESTING("duplicated IDs for an attribute");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, group, or attribute aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_DUPLICATE_ID_GRP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create the group '%s'\n", ATTRIBUTE_DUPLICATE_ID_GRP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_DUPLICATE_ID_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_DUPLICATE_ID_ATTR_NAME, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_DUPLICATE_ID_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    /* Open the attribute just created and get a second ID */
    if ((attr_id2 = H5Aopen(group_id, ATTRIBUTE_DUPLICATE_ID_ATTR_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    attribute can't be opened for a second time\n");
        goto error;
    }

    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id2) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Aclose(attr_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that the number of attributes attached
 * to an object (group, dataset, datatype) can be retrieved.
 *
 * XXX: Cover all of the cases and move to H5O tests.
 */
static int
test_get_number_attributes(void)
{
    H5O_info2_t obj_info;
    htri_t      attr_exists;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID;
    hid_t       group_id        = H5I_INVALID_HID;
    hid_t       attr_id         = H5I_INVALID_HID;
    hid_t       attr_dtype      = H5I_INVALID_HID;
    hid_t       space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("retrieval of the number of attributes on an object");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, or object aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_GET_NUM_ATTRS_TEST_GRP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create the group '%s'\n", ATTRIBUTE_GET_NUM_ATTRS_TEST_GRP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_GET_NUM_ATTRS_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_GET_NUM_ATTRS_TEST_ATTR_NAME, attr_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_GET_NUM_ATTRS_TEST_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oget_info)
        {
            TESTING_2("H5Oget_info");

            /* Now get the number of attributes from the group */
            if (H5Oget_info3(group_id, &obj_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve group info using H5Oget_info3\n");
                PART_ERROR(H5Oget_info);
            }

            if (obj_info.num_attrs != 1) {
                H5_FAILED();
                printf("    invalid number of attributes received\n");
                PART_ERROR(H5Oget_info);
            }

            PASSED();
        }
        PART_END(H5Oget_info);

        PART_BEGIN(H5Oget_info_by_name)
        {
            TESTING_2("H5Oget_info_by_name");

            if (H5Oget_info_by_name3(container_group, ATTRIBUTE_GET_NUM_ATTRS_TEST_GRP_NAME, &obj_info,
                                     H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve group info using H5Oget_info_by_name3\n");
                PART_ERROR(H5Oget_info_by_name);
            }

            if (obj_info.num_attrs != 1) {
                H5_FAILED();
                printf("    invalid number of attributes received\n");
                PART_ERROR(H5Oget_info_by_name);
            }

            PASSED();
        }
        PART_END(H5Oget_info_by_name);

        PART_BEGIN(H5Oget_info_by_idx)
        {
            TESTING_2("H5Oget_info_by_idx");

            if (H5Oget_info_by_idx3(container_group, ".", H5_INDEX_NAME, H5_ITER_INC, 0, &obj_info,
                                    H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve group info using H5Oget_info_by_idx3\n");
                PART_ERROR(H5Oget_info_by_idx);
            }

            if (obj_info.num_attrs != 1) {
                H5_FAILED();
                printf("    invalid number of attributes received\n");
                PART_ERROR(H5Oget_info_by_idx);
            }

            PASSED();
        }
        PART_END(H5Oget_info_by_idx);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Sclose(space_id);
        H5Tclose(attr_dtype);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that the reference count of a named datatype used by
 * attribute and a dataset is correct.
 *
 * XXX: May move to H5O tests.
 */
static int
test_attr_shared_dtype(void)
{
    H5O_info2_t obj_info;
    htri_t      attr_exists;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID;
    hid_t       group_id        = H5I_INVALID_HID;
    hid_t       attr_id         = H5I_INVALID_HID;
    hid_t       attr_dtype      = H5I_INVALID_HID;
    hid_t       space_id        = H5I_INVALID_HID;
    hid_t       dset_id         = H5I_INVALID_HID;

    TESTING("shared datatype for attributes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, attribute, stored datatype, or object aren't "
               "supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, ATTRIBUTE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, ATTRIBUTE_SHARED_DTYPE_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create the group '%s'\n", ATTRIBUTE_SHARED_DTYPE_GROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(ATTRIBUTE_SHARED_DTYPE_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    /* Commit datatype to file */
    if (H5Tcommit2(group_id, ATTRIBUTE_SHARED_DTYPE_NAME, attr_dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't commit datatype\n");
        goto error;
    }

    if (H5Oget_info_by_name3(group_id, ATTRIBUTE_SHARED_DTYPE_NAME, &obj_info, H5O_INFO_ALL, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't retrieve root group info using H5Oget_info_by_name3\n");
        goto error;
    }

    if (obj_info.rc != 1) {
        H5_FAILED();
        printf("    reference count of the named datatype is wrong: %u\n", obj_info.rc);
        goto error;
    }

    if ((attr_id = H5Acreate2(group_id, ATTRIBUTE_SHARED_DTYPE_ATTR_NAME, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create attribute\n");
        goto error;
    }

    /* Verify the attribute has been created */
    if ((attr_exists = H5Aexists(group_id, ATTRIBUTE_SHARED_DTYPE_ATTR_NAME)) < 0) {
        H5_FAILED();
        printf("    couldn't determine if attribute exists\n");
        goto error;
    }

    if (!attr_exists) {
        H5_FAILED();
        printf("    attribute did not exist\n");
        goto error;
    }

    if (H5Oget_info_by_name3(group_id, ATTRIBUTE_SHARED_DTYPE_NAME, &obj_info, H5O_INFO_ALL, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't retrieve root group info using H5Oget_info_by_name3\n");
        goto error;
    }

    if (obj_info.rc != 2) {
        H5_FAILED();
        printf("    reference count of the named datatype is wrong: %u\n", obj_info.rc);
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, ATTRIBUTE_SHARED_DTYPE_DSET_NAME, attr_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset\n");
        goto error;
    }

    if (H5Oget_info_by_name3(group_id, ATTRIBUTE_SHARED_DTYPE_NAME, &obj_info, H5O_INFO_ALL, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't retrieve root group info using H5Oget_info_by_name3\n");
        goto error;
    }

    if (obj_info.rc != 3) {
        H5_FAILED();
        printf("    reference count of the named datatype is wrong: %u\n", obj_info.rc);
        goto error;
    }

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Tclose(attr_dtype) < 0)
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
        H5Tclose(attr_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

static herr_t
attr_iter_callback1(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo, void *op_data)
{
    size_t *i           = (size_t *)op_data;
    size_t  counter_val = *((size_t *)op_data);
    size_t  test_iteration;
    char    expected_attr_name[ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE];
    herr_t  ret_val = H5_ITER_CONT;

    UNUSED(location_id);
    UNUSED(ainfo);

    /*
     * Four tests are run in the following order per attribute iteration API call:
     *
     *  - iteration by attribute name in increasing order
     *  - iteration by attribute name in decreasing order
     *  - iteration by attribute creation order in increasing order
     *  - iteration by attribute creation order in decreasing order
     *
     * Based on how the test is written, this will mean that the attribute names
     * will run in increasing order on the first and fourth tests and decreasing
     * order on the second and third tests.
     */
    test_iteration = (counter_val / ATTRIBUTE_ITERATE_TEST_NUM_ATTRS);
    if (test_iteration == 0 || test_iteration == 3) {
        snprintf(expected_attr_name, ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE,
                 ATTRIBUTE_ITERATE_TEST_ATTR_NAME "%d",
                 (int)(counter_val % ATTRIBUTE_ITERATE_TEST_NUM_ATTRS));
    }
    else {
        snprintf(
            expected_attr_name, ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE,
            ATTRIBUTE_ITERATE_TEST_ATTR_NAME "%d",
            (int)(ATTRIBUTE_ITERATE_TEST_NUM_ATTRS - (counter_val % ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) - 1));
    }

    if (strncmp(attr_name, expected_attr_name, ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE)) {
        printf("    attribute name '%s' didn't match expected name '%s'\n", attr_name, expected_attr_name);
        ret_val = H5_ITER_ERROR;
        goto done;
    }

    /*
     * If the attribute's creation order is marked as valid, make sure
     * that it corresponds to what is expected based on the order that
     * the attributes were created in.
     */
    if (ainfo->corder_valid) {
        H5O_msg_crt_idx_t expected_crt_order;

        /*
         * As the attributes are created with a reverse-ordering naming
         * scheme to test creation order, their creation order values will
         * be listed in reverse ordering on the first and fourth tests and
         * in normal ordering on the second and third tests.
         */
        if (test_iteration == 0 || test_iteration == 3)
            expected_crt_order = (H5O_msg_crt_idx_t)(ATTRIBUTE_ITERATE_TEST_NUM_ATTRS -
                                                     (counter_val % ATTRIBUTE_ITERATE_TEST_NUM_ATTRS) - 1);
        else
            expected_crt_order = (H5O_msg_crt_idx_t)(counter_val % ATTRIBUTE_ITERATE_TEST_NUM_ATTRS);

        if (ainfo->corder != expected_crt_order) {
            H5_FAILED();
            printf("    attribute's creation order value of %lld didn't match expected value of %lld\n",
                   (long long)ainfo->corder, (long long)expected_crt_order);
            ret_val = H5_ITER_ERROR;
            goto done;
        }
    }

done:
    (*i)++;

    return ret_val;
}

static herr_t
attr_iter_callback2(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo, void *op_data)
{
    UNUSED(location_id);
    UNUSED(attr_name);
    UNUSED(ainfo);
    UNUSED(op_data);

    return 0;
}

int
H5_api_attribute_test(void)
{
    size_t i;
    int    nerrors;

    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*            API Attribute Tests             *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(attribute_tests); i++) {
        nerrors += (*attribute_tests[i])() ? 1 : 0;
    }

    printf("\n");

    return nerrors;
}
