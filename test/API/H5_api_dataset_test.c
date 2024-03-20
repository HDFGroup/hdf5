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

#include "H5_api_dataset_test.h"

/*
 * XXX: H5Dread_chunk/H5Dwrite_chunk, H5Dfill/scatter/gather
 */

static int test_create_dataset_under_root(void);
static int test_create_dataset_under_existing_group(void);
static int test_create_dataset_invalid_params(void);
static int test_create_anonymous_dataset(void);
static int test_create_anonymous_dataset_invalid_params(void);
static int test_create_dataset_null_space(void);
static int test_create_dataset_scalar_space(void);
static int test_create_zero_dim_dset(void);
static int test_create_dataset_random_shapes(void);
static int test_create_dataset_predefined_types(void);
static int test_create_dataset_string_types(void);
static int test_create_dataset_compound_types(void);
static int test_create_dataset_enum_types(void);
static int test_create_dataset_array_types(void);
static int test_create_dataset_creation_properties(void);
static int test_create_many_dataset(void);
static int test_open_dataset(void);
static int test_open_dataset_invalid_params(void);
static int test_close_dataset_invalid_params(void);
static int test_get_dataset_space_and_type(void);
static int test_get_dataset_space_and_type_invalid_params(void);
static int test_get_dataset_space_status(void);
static int test_get_dataset_space_status_invalid_params(void);
static int test_dataset_property_lists(void);
static int test_get_dataset_storage_size(void);
static int test_get_dataset_storage_size_invalid_params(void);
static int test_get_dataset_chunk_storage_size(void);
static int test_get_dataset_chunk_storage_size_invalid_params(void);
static int test_get_dataset_offset(void);
static int test_get_dataset_offset_invalid_params(void);
static int test_read_dataset_small_all(void);
static int test_read_dataset_small_hyperslab(void);
static int test_read_dataset_small_point_selection(void);
static int test_read_multi_dataset_small_all(void);
static int test_read_multi_dataset_small_hyperslab(void);
static int test_read_multi_dataset_small_point_selection(void);
static int test_dataset_io_point_selections(void);
static int test_read_dataset_invalid_params(void);
static int test_write_dataset_small_all(void);
static int test_write_dataset_small_hyperslab(void);
static int test_write_dataset_small_point_selection(void);
static int test_write_dataset_data_verification(void);
static int test_write_multi_dataset_small_all(void);
static int test_write_multi_dataset_small_hyperslab(void);
static int test_write_multi_dataset_small_point_selection(void);
static int test_write_multi_dataset_data_verification(void);
static int test_write_dataset_invalid_params(void);
static int test_dataset_string_encodings(void);
static int test_dataset_builtin_type_conversion(void);
static int test_dataset_real_to_int_conversion(void);
static int test_dataset_compound_partial_io(void);
static int test_dataset_vlen_io(void);
static int test_dataset_set_extent_chunked_unlimited(void);
static int test_dataset_set_extent_chunked_fixed(void);
static int test_dataset_set_extent_data(void);
static int test_dataset_set_extent_double_handles(void);
static int test_dataset_set_extent_invalid_params(void);
static int test_flush_dataset(void);
static int test_flush_dataset_invalid_params(void);
static int test_refresh_dataset(void);
static int test_refresh_dataset_invalid_params(void);

/*
 * Chunking tests
 */
static int test_create_single_chunk_dataset(void);
static int test_write_single_chunk_dataset(void);
static int test_create_multi_chunk_dataset(void);
static int test_write_multi_chunk_dataset_same_shape_read(void);
static int test_write_multi_chunk_dataset_diff_shape_read(void);
static int test_overwrite_multi_chunk_dataset_same_shape_read(void);
static int test_overwrite_multi_chunk_dataset_diff_shape_read(void);
static int test_read_partial_chunk_all_selection(void);
static int test_read_partial_chunk_hyperslab_selection(void);
static int test_read_partial_chunk_point_selection(void);

static int test_get_vlen_buf_size(void);

/*
 * The array of dataset tests to be performed.
 */
static int (*dataset_tests[])(void) = {
    test_create_dataset_under_root,
    test_create_dataset_under_existing_group,
    test_create_dataset_invalid_params,
    test_create_anonymous_dataset,
    test_create_anonymous_dataset_invalid_params,
    test_create_dataset_null_space,
    test_create_dataset_scalar_space,
    test_create_zero_dim_dset,
    test_create_dataset_random_shapes,
    test_create_dataset_predefined_types,
    test_create_dataset_string_types,
    test_create_dataset_compound_types,
    test_create_dataset_enum_types,
    test_create_dataset_array_types,
    test_create_dataset_creation_properties,
    test_create_many_dataset,
    test_open_dataset,
    test_open_dataset_invalid_params,
    test_close_dataset_invalid_params,
    test_get_dataset_space_and_type,
    test_get_dataset_space_and_type_invalid_params,
    test_get_dataset_space_status,
    test_get_dataset_space_status_invalid_params,
    test_dataset_property_lists,
    test_get_dataset_storage_size,
    test_get_dataset_storage_size_invalid_params,
    test_get_dataset_chunk_storage_size,
    test_get_dataset_chunk_storage_size_invalid_params,
    test_get_dataset_offset,
    test_get_dataset_offset_invalid_params,
    test_read_dataset_small_all,
    test_read_dataset_small_hyperslab,
    test_read_dataset_small_point_selection,
    test_read_multi_dataset_small_all,
    test_read_multi_dataset_small_hyperslab,
    test_read_multi_dataset_small_point_selection,
    test_dataset_io_point_selections,
    test_read_dataset_invalid_params,
    test_dataset_string_encodings,
    test_write_dataset_small_all,
    test_write_dataset_small_hyperslab,
    test_write_dataset_small_point_selection,
    test_write_dataset_data_verification,
    test_write_multi_dataset_small_all,
    test_write_multi_dataset_small_hyperslab,
    test_write_multi_dataset_small_point_selection,
    test_write_multi_dataset_data_verification,
    test_write_dataset_invalid_params,
    test_dataset_builtin_type_conversion,
    test_dataset_real_to_int_conversion,
    test_dataset_compound_partial_io,
    test_dataset_vlen_io,
    test_dataset_set_extent_chunked_unlimited,
    test_dataset_set_extent_chunked_fixed,
    test_dataset_set_extent_data,
    test_dataset_set_extent_double_handles,
    test_dataset_set_extent_invalid_params,
    test_flush_dataset,
    test_flush_dataset_invalid_params,
    test_refresh_dataset,
    test_refresh_dataset_invalid_params,
    test_create_single_chunk_dataset,
    test_write_single_chunk_dataset,
    test_create_multi_chunk_dataset,
    test_write_multi_chunk_dataset_same_shape_read,
    test_write_multi_chunk_dataset_diff_shape_read,
    test_overwrite_multi_chunk_dataset_same_shape_read,
    test_overwrite_multi_chunk_dataset_diff_shape_read,
    test_read_partial_chunk_all_selection,
    test_read_partial_chunk_hyperslab_selection,
    test_read_partial_chunk_point_selection,
    test_get_vlen_buf_size,
};

size_t filter(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
              const unsigned int H5_ATTR_UNUSED cd_values[], size_t nbytes, size_t H5_ATTR_UNUSED *buf_size,
              void H5_ATTR_UNUSED **buf);
/*
 * A test to check that a dataset can be
 * created under the root group.
 */
static int
test_create_dataset_under_root(void)
{
    hid_t file_id    = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING("dataset creation under root group");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_CREATE_UNDER_ROOT_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    /* Create the Dataset under the root group of the file */
    if ((dset_id = H5Dcreate2(file_id, DATASET_CREATE_UNDER_ROOT_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_CREATE_UNDER_ROOT_DSET_NAME);
        goto error;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
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
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset can be created
 * under a group that is not the root group.
 */
static int
test_create_dataset_under_existing_group(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING("dataset creation under an existing group");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_UNDER_EXISTING_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_CREATE_UNDER_EXISTING_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_CREATE_UNDER_EXISTING_SPACE_RANK, NULL, NULL, false)) <
        0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_CREATE_UNDER_EXISTING_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_CREATE_UNDER_EXISTING_DSET_NAME);
        goto error;
    }

    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset can't be created
 * when H5Dcreate is passed invalid parameters.
 */
static int
test_create_dataset_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Dcreate with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_INVALID_PARAMS_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_CREATE_INVALID_PARAMS_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_CREATE_INVALID_PARAMS_SPACE_RANK, NULL, NULL, false)) <
        0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dcreate_invalid_loc_id)
        {
            TESTING_2("H5Dcreate with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(H5I_INVALID_HID, DATASET_CREATE_INVALID_PARAMS_DSET_NAME, dset_dtype,
                                     fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid loc_id!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_loc_id);

        PART_BEGIN(H5Dcreate_invalid_dataset_name)
        {
            TESTING_2("H5Dcreate with an invalid dataset name");

            H5E_BEGIN_TRY
            {
                dset_id =
                    H5Dcreate2(group_id, NULL, dset_dtype, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with a NULL dataset name!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_dataset_name);
            }

            H5E_BEGIN_TRY
            {
                dset_id =
                    H5Dcreate2(group_id, "", dset_dtype, fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid dataset name of ''!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_dataset_name);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_dataset_name);

        PART_BEGIN(H5Dcreate_invalid_datatype)
        {
            TESTING_2("H5Dcreate with an invalid datatype");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(group_id, DATASET_CREATE_INVALID_PARAMS_DSET_NAME, H5I_INVALID_HID,
                                     fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid datatype!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_datatype);

        PART_BEGIN(H5Dcreate_invalid_dataspace)
        {
            TESTING_2("H5Dcreate with an invalid dataspace");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(group_id, DATASET_CREATE_INVALID_PARAMS_DSET_NAME, dset_dtype,
                                     H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid dataspace!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_dataspace);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_dataspace);

        PART_BEGIN(H5Dcreate_invalid_lcpl)
        {
            TESTING_2("H5Dcreate with an invalid LCPL");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(group_id, DATASET_CREATE_INVALID_PARAMS_DSET_NAME, dset_dtype, fspace_id,
                                     H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid LCPL!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_lcpl);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_lcpl);

        PART_BEGIN(H5Dcreate_invalid_dcpl)
        {
            TESTING_2("H5Dcreate with an invalid DCPL");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(group_id, DATASET_CREATE_INVALID_PARAMS_DSET_NAME, dset_dtype, fspace_id,
                                     H5P_DEFAULT, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid DCPL!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_dcpl);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_dcpl);

        PART_BEGIN(H5Dcreate_invalid_dapl)
        {
            TESTING_2("H5Dcreate with an invalid DAPL");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate2(group_id, DATASET_CREATE_INVALID_PARAMS_DSET_NAME, dset_dtype, fspace_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created dataset using H5Dcreate with an invalid DAPL!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_invalid_dapl);
            }

            PASSED();
        }
        PART_END(H5Dcreate_invalid_dapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an anonymous dataset can be created.
 */
static int
test_create_anonymous_dataset(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING("anonymous dataset creation");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_ANONYMOUS_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_CREATE_ANONYMOUS_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_CREATE_ANONYMOUS_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate_anon(group_id, dset_dtype, fspace_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create anonymous dataset\n");
        goto error;
    }

    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an anonymous dataset can't
 * be created when H5Dcreate_anon is passed invalid
 * parameters.
 */
static int
test_create_anonymous_dataset_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING_MULTIPART("anonymous dataset creation with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_ANONYMOUS_INVALID_PARAMS_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n",
               DATASET_CREATE_ANONYMOUS_INVALID_PARAMS_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_CREATE_ANONYMOUS_INVALID_PARAMS_SPACE_RANK, NULL, NULL,
                                               false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dcreate_anon_invalid_loc_id)
        {
            TESTING_2("H5Dcreate_anon with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate_anon(H5I_INVALID_HID, dset_dtype, fspace_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created anonymous dataset using an invalid loc_id!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_anon_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Dcreate_anon_invalid_loc_id);

        PART_BEGIN(H5Dcreate_anon_invalid_datatype)
        {
            TESTING_2("H5Dcreate_anon with an invalid dataset datatype");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate_anon(group_id, H5I_INVALID_HID, fspace_id, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created anonymous dataset using an invalid dataset datatype!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_anon_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Dcreate_anon_invalid_datatype);

        PART_BEGIN(H5Dcreate_anon_invalid_dataspace)
        {
            TESTING_2("H5Dcreate_anon with an invalid dataset dataspace");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate_anon(group_id, dset_dtype, H5I_INVALID_HID, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created anonymous dataset using an invalid dataset dataspace!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_anon_invalid_dataspace);
            }

            PASSED();
        }
        PART_END(H5Dcreate_anon_invalid_dataspace);

        PART_BEGIN(H5Dcreate_anon_invalid_dcpl)
        {
            TESTING_2("H5Dcreate_anon with an invalid DCPL");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate_anon(group_id, dset_dtype, fspace_id, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created anonymous dataset using an invalid DCPL!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_anon_invalid_dcpl);
            }

            PASSED();
        }
        PART_END(H5Dcreate_anon_invalid_dcpl);

        PART_BEGIN(H5Dcreate_anon_invalid_dapl)
        {
            TESTING_2("H5Dcreate_anon with an invalid DAPL");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dcreate_anon(group_id, dset_dtype, fspace_id, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    created anonymous dataset using an invalid DAPL!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dcreate_anon_invalid_dapl);
            }

            PASSED();
        }
        PART_END(H5Dcreate_anon_invalid_dapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that creating a dataset with a NULL
 * dataspace is not problematic.
 */
static int
test_create_dataset_null_space(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING("dataset creation with a NULL dataspace");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_NULL_DATASPACE_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n",
               DATASET_CREATE_NULL_DATASPACE_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate(H5S_NULL)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_CREATE_NULL_DATASPACE_TEST_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_CREATE_NULL_DATASPACE_TEST_DSET_NAME);
        goto error;
    }

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dopen2(group_id, DATASET_CREATE_NULL_DATASPACE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_CREATE_NULL_DATASPACE_TEST_DSET_NAME);
        goto error;
    }

    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that creating a dataset with a scalar
 * dataspace is not problematic.
 */
static int
test_create_dataset_scalar_space(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING("dataset creation with a SCALAR dataspace");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATE_SCALAR_DATASPACE_TEST_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n",
               DATASET_CREATE_SCALAR_DATASPACE_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_CREATE_SCALAR_DATASPACE_TEST_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_CREATE_SCALAR_DATASPACE_TEST_DSET_NAME);
        goto error;
    }

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dopen2(group_id, DATASET_CREATE_SCALAR_DATASPACE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_CREATE_SCALAR_DATASPACE_TEST_DSET_NAME);
        goto error;
    }

    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that creating a dataset with a dataspace
 * which contains a 0-sized dimension is not problematic.
 */
static int
test_create_zero_dim_dset(void)
{
    hsize_t dims[ZERO_DIM_DSET_TEST_SPACE_RANK] = {0};
    hid_t   file_id                             = H5I_INVALID_HID;
    hid_t   container_group                     = H5I_INVALID_HID;
    hid_t   group_id                            = H5I_INVALID_HID;
    hid_t   dset_id                             = H5I_INVALID_HID;
    hid_t   fspace_id                           = H5I_INVALID_HID;
    int     data[1];

    TESTING("creation of 0-sized dataset");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
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

    if ((group_id = H5Gcreate2(container_group, ZERO_DIM_DSET_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", ZERO_DIM_DSET_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, ZERO_DIM_DSET_TEST_DSET_NAME, H5T_NATIVE_INT, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to create 0-sized dataset\n");
        goto error;
    }

    if (H5Sselect_none(fspace_id) < 0) {
        H5_FAILED();
        printf("    failed to set none selection in dataset's file dataspace\n");
        goto error;
    }

    /* Attempt to write 0 elements to dataset */
    if (H5Dwrite(dset_id, H5T_NATIVE_INT, fspace_id, fspace_id, H5P_DEFAULT, data) < 0) {
        H5_FAILED();
        printf("    failed to write 0 elements to 0-sized dataset\n");
        goto error;
    }

    /* Attempt to read 0 elements from dataset */
    if (H5Dread(dset_id, H5T_NATIVE_INT, fspace_id, fspace_id, H5P_DEFAULT, data) < 0) {
        H5_FAILED();
        printf("    failed to read 0 elements from 0-sized dataset\n");
        goto error;
    }

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
 * A test to check that a dataset can be created with
 * a variety of different dataspace shapes.
 */
static int
test_create_dataset_random_shapes(void)
{
    size_t i;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  dset_id = H5I_INVALID_HID, space_id = H5I_INVALID_HID;
    hid_t  dset_dtype = H5I_INVALID_HID;

    TESTING("dataset creation with random dimension sizes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group\n");
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SHAPE_TEST_SUBGROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group\n");
        goto error;
    }

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_SHAPE_TEST_NUM_ITERATIONS; i++) {
        char name[100];
        int  ndims = rand() % DATASET_SHAPE_TEST_MAX_DIMS + 1;

        if ((space_id = generate_random_dataspace(ndims, NULL, NULL, false)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataspace\n");
            goto error;
        }

        snprintf(name, sizeof(name), "%s%zu", DATASET_SHAPE_TEST_DSET_BASE_NAME, i + 1);

        if ((dset_id = H5Dcreate2(group_id, name, dset_dtype, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset\n");
            goto error;
        }

        if (H5Sclose(space_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;
    }

    if (H5Tclose(dset_dtype) < 0)
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
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset can be created using
 * each of the predefined integer and floating-point
 * datatypes.
 */
static int
test_create_dataset_predefined_types(void)
{
    size_t i;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  fspace_id                    = H5I_INVALID_HID;
    hid_t  dset_id                      = H5I_INVALID_HID;
    hid_t  predefined_type_test_table[] = {
        H5T_STD_U8LE,   H5T_STD_U8BE,   H5T_STD_I8LE,   H5T_STD_I8BE,  H5T_STD_U16LE,  H5T_STD_U16BE,
        H5T_STD_I16LE,  H5T_STD_I16BE,  H5T_STD_U32LE,  H5T_STD_U32BE, H5T_STD_I32LE,  H5T_STD_I32BE,
        H5T_STD_U64LE,  H5T_STD_U64BE,  H5T_STD_I64LE,  H5T_STD_I64BE, H5T_IEEE_F16LE, H5T_IEEE_F16BE,
        H5T_IEEE_F32LE, H5T_IEEE_F32BE, H5T_IEEE_F64LE, H5T_IEEE_F64BE};

    TESTING("dataset creation with predefined datatypes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_PREDEFINED_TYPE_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create sub-container group '%s'\n", DATASET_PREDEFINED_TYPE_TEST_SUBGROUP_NAME);
        goto error;
    }

    for (i = 0; i < ARRAY_LENGTH(predefined_type_test_table); i++) {
        char name[100];

        if ((fspace_id =
                 generate_random_dataspace(DATASET_PREDEFINED_TYPE_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
            TEST_ERROR;

        snprintf(name, sizeof(name), "%s%zu", DATASET_PREDEFINED_TYPE_TEST_BASE_NAME, i);

        if ((dset_id = H5Dcreate2(group_id, name, predefined_type_test_table[i], fspace_id, H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", name);
            goto error;
        }

        if (H5Sclose(fspace_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;

        if ((dset_id = H5Dopen2(group_id, name, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    failed to open dataset '%s'\n", name);
            goto error;
        }

        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;
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
 * A test to check that a dataset can be created using
 * string datatypes.
 */
static int
test_create_dataset_string_types(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id_fixed = H5I_INVALID_HID, dset_id_variable = H5I_INVALID_HID;
    hid_t type_id_fixed = H5I_INVALID_HID, type_id_variable = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;

    TESTING_MULTIPART("dataset creation with string types");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_STRING_TYPE_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_STRING_TYPE_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((type_id_fixed = H5Tcreate(H5T_STRING, DATASET_STRING_TYPE_TEST_STRING_LENGTH)) < 0) {
        H5_FAILED();
        printf("    couldn't create fixed-length string type\n");
        goto error;
    }

    if ((type_id_variable = H5Tcreate(H5T_STRING, H5T_VARIABLE)) < 0) {
        H5_FAILED();
        printf("    couldn't create variable-length string type\n");
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_STRING_TYPE_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dcreate_fixed_string_type)
        {
            TESTING_2("creation of fixed-size string dataset");

            if ((dset_id_fixed = H5Dcreate2(group_id, DATASET_STRING_TYPE_TEST_DSET_NAME1, type_id_fixed,
                                            fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create fixed-length string dataset '%s'\n",
                       DATASET_STRING_TYPE_TEST_DSET_NAME1);
                PART_ERROR(H5Dcreate_fixed_string_type);
            }

            if (dset_id_fixed >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id_fixed);
                }
                H5E_END_TRY
                dset_id_fixed = H5I_INVALID_HID;
            }

            if ((dset_id_fixed = H5Dopen2(group_id, DATASET_STRING_TYPE_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open dataset '%s'\n", DATASET_STRING_TYPE_TEST_DSET_NAME1);
                PART_ERROR(H5Dcreate_fixed_string_type);
            }

            PASSED();
        }
        PART_END(H5Dcreate_fixed_string_type);

        PART_BEGIN(H5Dcreate_variable_string_type)
        {
            TESTING_2("creation of variable-length string dataset");

            if ((dset_id_variable =
                     H5Dcreate2(group_id, DATASET_STRING_TYPE_TEST_DSET_NAME2, type_id_variable, fspace_id,
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create variable-length string dataset '%s'\n",
                       DATASET_STRING_TYPE_TEST_DSET_NAME2);
                PART_ERROR(H5Dcreate_variable_string_type);
            }

            if (dset_id_variable >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id_variable);
                }
                H5E_END_TRY
                dset_id_variable = H5I_INVALID_HID;
            }

            if ((dset_id_variable = H5Dopen2(group_id, DATASET_STRING_TYPE_TEST_DSET_NAME2, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    failed to open dataset '%s'\n", DATASET_STRING_TYPE_TEST_DSET_NAME2);
                PART_ERROR(H5Dcreate_variable_string_type);
            }

            PASSED();
        }
        PART_END(H5Dcreate_variable_string_type);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Tclose(type_id_fixed) < 0)
        TEST_ERROR;
    if (H5Tclose(type_id_variable) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id_fixed) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id_variable) < 0)
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
        H5Tclose(type_id_fixed);
        H5Tclose(type_id_variable);
        H5Sclose(fspace_id);
        H5Dclose(dset_id_fixed);
        H5Dclose(dset_id_variable);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset can be created using
 * a variety of compound datatypes.
 */
static int
test_create_dataset_compound_types(void)
{
    size_t i, j;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  compound_type = H5I_INVALID_HID;
    hid_t  dset_id       = H5I_INVALID_HID;
    hid_t  fspace_id     = H5I_INVALID_HID;
    hid_t  type_pool[DATASET_COMPOUND_TYPE_TEST_MAX_SUBTYPES];
    int    num_passes;

    TESTING("dataset creation with compound datatypes");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    /*
     * Make sure to pre-initialize all the compound field IDs
     * so we don't try to close an uninitialized ID value;
     * memory checkers will likely complain.
     */
    for (j = 0; j < DATASET_COMPOUND_TYPE_TEST_MAX_SUBTYPES; j++)
        type_pool[j] = H5I_INVALID_HID;

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_COMPOUND_TYPE_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_COMPOUND_TYPE_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_COMPOUND_TYPE_TEST_DSET_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    num_passes = (rand() % DATASET_COMPOUND_TYPE_TEST_MAX_PASSES) + 1;

    for (i = 0; i < (size_t)num_passes; i++) {
        size_t num_subtypes;
        size_t compound_size = 0;
        size_t next_offset   = 0;
        char   dset_name[256];

        /*
         * Also pre-initialize all of the compound field IDs at the
         * beginning of each loop so that we don't try to close an
         * invalid ID.
         */
        for (j = 0; j < DATASET_COMPOUND_TYPE_TEST_MAX_SUBTYPES; j++)
            type_pool[j] = H5I_INVALID_HID;

        num_subtypes = (size_t)(rand() % DATASET_COMPOUND_TYPE_TEST_MAX_SUBTYPES) + 1;

        if ((compound_type = H5Tcreate(H5T_COMPOUND, 1)) < 0) {
            H5_FAILED();
            printf("    couldn't create compound datatype\n");
            goto error;
        }

        /* Start adding subtypes to the compound type */
        for (j = 0; j < num_subtypes; j++) {
            size_t member_size;
            char   member_name[256];

            snprintf(member_name, 256, "member%zu", j);

            if ((type_pool[j] = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
                H5_FAILED();
                printf("    couldn't create compound datatype member %zu\n", j);
                goto error;
            }

            if (!(member_size = H5Tget_size(type_pool[j]))) {
                H5_FAILED();
                printf("    couldn't get compound member %zu size\n", j);
                goto error;
            }

            compound_size += member_size;

            if (H5Tset_size(compound_type, compound_size) < 0)
                TEST_ERROR;

            if (H5Tinsert(compound_type, member_name, next_offset, type_pool[j]) < 0)
                TEST_ERROR;

            next_offset += member_size;
        }

        if (H5Tpack(compound_type) < 0)
            TEST_ERROR;

        snprintf(dset_name, sizeof(dset_name), "%s%zu", DATASET_COMPOUND_TYPE_TEST_DSET_NAME, i);

        if ((dset_id = H5Dcreate2(group_id, dset_name, compound_type, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", dset_name);
            goto error;
        }

        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;

        if ((dset_id = H5Dopen2(group_id, dset_name, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    failed to open dataset '%s'\n", dset_name);
            goto error;
        }

        for (j = 0; j < num_subtypes; j++)
            if (type_pool[j] >= 0 && H5Tclose(type_pool[j]) < 0)
                TEST_ERROR;
        if (H5Tclose(compound_type) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_COMPOUND_TYPE_TEST_MAX_SUBTYPES; i++)
            H5Tclose(type_pool[i]);
        H5Tclose(compound_type);
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
 * A test to check that a dataset can be created with
 * enum datatypes.
 */
static int
test_create_dataset_enum_types(void)
{
    size_t      i;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       dset_id_native = H5I_INVALID_HID, dset_id_non_native = H5I_INVALID_HID;
    hid_t       fspace_id   = H5I_INVALID_HID;
    hid_t       enum_native = H5I_INVALID_HID, enum_non_native = H5I_INVALID_HID;
    const char *enum_type_test_table[] = {"RED",    "GREEN",  "BLUE",   "BLACK", "WHITE",
                                          "PURPLE", "ORANGE", "YELLOW", "BROWN"};

    TESTING("dataset creation with enum types");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_ENUM_TYPE_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_ENUM_TYPE_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((enum_native = H5Tcreate(H5T_ENUM, sizeof(int))) < 0) {
        H5_FAILED();
        printf("    couldn't create native enum type\n");
        goto error;
    }

    for (i = 0; i < ARRAY_LENGTH(enum_type_test_table); i++)
        if (H5Tenum_insert(enum_native, enum_type_test_table[i], &i) < 0)
            TEST_ERROR;

    if ((enum_non_native = H5Tenum_create(H5T_STD_U32LE)) < 0) {
        H5_FAILED();
        printf("    couldn't create non-native enum type\n");
        goto error;
    }

    for (i = 0; i < DATASET_ENUM_TYPE_TEST_NUM_MEMBERS; i++) {
        char val_name[15];

        snprintf(val_name, 15, "%s%zu", DATASET_ENUM_TYPE_TEST_VAL_BASE_NAME, i);

        if (H5Tenum_insert(enum_non_native, val_name, &i) < 0)
            TEST_ERROR;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_ENUM_TYPE_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_id_native = H5Dcreate2(group_id, DATASET_ENUM_TYPE_TEST_DSET_NAME1, enum_native, fspace_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create native enum dataset '%s'\n", DATASET_ENUM_TYPE_TEST_DSET_NAME1);
        goto error;
    }

    if ((dset_id_non_native = H5Dcreate2(group_id, DATASET_ENUM_TYPE_TEST_DSET_NAME2, enum_non_native,
                                         fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create non-native enum dataset '%s'\n", DATASET_ENUM_TYPE_TEST_DSET_NAME2);
        goto error;
    }

    if (H5Dclose(dset_id_native) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id_non_native) < 0)
        TEST_ERROR;

    if ((dset_id_native = H5Dopen2(group_id, DATASET_ENUM_TYPE_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open dataset '%s'\n", DATASET_ENUM_TYPE_TEST_DSET_NAME1);
        goto error;
    }

    if ((dset_id_non_native = H5Dopen2(group_id, DATASET_ENUM_TYPE_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open dataset '%s'\n", DATASET_ENUM_TYPE_TEST_DSET_NAME2);
        goto error;
    }

    if (H5Tclose(enum_native) < 0)
        TEST_ERROR;
    if (H5Tclose(enum_non_native) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id_native) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id_non_native) < 0)
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
        H5Tclose(enum_native);
        H5Tclose(enum_non_native);
        H5Sclose(fspace_id);
        H5Dclose(dset_id_native);
        H5Dclose(dset_id_non_native);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset can be created using
 * array datatypes.
 */
static int
test_create_dataset_array_types(void)
{
    hsize_t array_dims1[DATASET_ARRAY_TYPE_TEST_RANK1];
    hsize_t array_dims2[DATASET_ARRAY_TYPE_TEST_RANK2];
    hsize_t array_dims3[DATASET_ARRAY_TYPE_TEST_RANK3];
    size_t  i;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id1 = H5I_INVALID_HID, dset_id2 = H5I_INVALID_HID, dset_id3 = H5I_INVALID_HID;
    hid_t   fspace_id      = H5I_INVALID_HID;
    hid_t   array_type_id1 = H5I_INVALID_HID, array_type_id2 = H5I_INVALID_HID,
          array_type_id3      = H5I_INVALID_HID;
    hid_t array_base_type_id1 = H5I_INVALID_HID, array_base_type_id2 = H5I_INVALID_HID,
          array_base_type_id3 = H5I_INVALID_HID;
    hid_t nested_type_id      = H5I_INVALID_HID;

    TESTING("dataset creation with array types");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_ARRAY_TYPE_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_ARRAY_TYPE_TEST_SUBGROUP_NAME);
        goto error;
    }

    /* Test creation of array with some different types */
    for (i = 0; i < DATASET_ARRAY_TYPE_TEST_RANK1; i++)
        array_dims1[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);

    if ((array_base_type_id1 = generate_random_datatype(H5T_ARRAY, false)) < 0)
        TEST_ERROR;

    if ((array_type_id1 = H5Tarray_create2(array_base_type_id1, DATASET_ARRAY_TYPE_TEST_RANK1, array_dims1)) <
        0) {
        H5_FAILED();
        printf("    couldn't create first array type\n");
        goto error;
    }

    for (i = 0; i < DATASET_ARRAY_TYPE_TEST_RANK2; i++)
        array_dims2[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);

    if ((array_base_type_id2 = generate_random_datatype(H5T_ARRAY, false)) < 0)
        TEST_ERROR;

    if ((array_type_id2 = H5Tarray_create2(array_base_type_id2, DATASET_ARRAY_TYPE_TEST_RANK2, array_dims2)) <
        0) {
        H5_FAILED();
        printf("    couldn't create second array type\n");
        goto error;
    }

    /* Test nested arrays */
    for (i = 0; i < DATASET_ARRAY_TYPE_TEST_RANK3; i++)
        array_dims3[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);

    if ((array_base_type_id3 = generate_random_datatype(H5T_ARRAY, false)) < 0)
        TEST_ERROR;

    if ((nested_type_id = H5Tarray_create2(array_base_type_id3, DATASET_ARRAY_TYPE_TEST_RANK3, array_dims3)) <
        0) {
        H5_FAILED();
        printf("    couldn't create nested array base type\n");
        goto error;
    }

    if ((array_type_id3 = H5Tarray_create2(nested_type_id, DATASET_ARRAY_TYPE_TEST_RANK3, array_dims3)) < 0) {
        H5_FAILED();
        printf("    couldn't create nested array type\n");
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_ARRAY_TYPE_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_id1 = H5Dcreate2(group_id, DATASET_ARRAY_TYPE_TEST_DSET_NAME1, array_type_id1, fspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create array type dataset '%s'\n", DATASET_ARRAY_TYPE_TEST_DSET_NAME1);
        goto error;
    }

    if ((dset_id2 = H5Dcreate2(group_id, DATASET_ARRAY_TYPE_TEST_DSET_NAME2, array_type_id2, fspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create array type dataset '%s'\n", DATASET_ARRAY_TYPE_TEST_DSET_NAME2);
        goto error;
    }

    if ((dset_id3 = H5Dcreate2(group_id, DATASET_ARRAY_TYPE_TEST_DSET_NAME3, array_type_id3, fspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create nested array type dataset '%s'\n", DATASET_ARRAY_TYPE_TEST_DSET_NAME3);
        goto error;
    }

    if (H5Dclose(dset_id1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id3) < 0)
        TEST_ERROR;

    if ((dset_id1 = H5Dopen2(group_id, DATASET_ARRAY_TYPE_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open dataset '%s'\n", DATASET_ARRAY_TYPE_TEST_DSET_NAME1);
        goto error;
    }

    if ((dset_id2 = H5Dopen2(group_id, DATASET_ARRAY_TYPE_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open dataset '%s'\n", DATASET_ARRAY_TYPE_TEST_DSET_NAME2);
        goto error;
    }

    if ((dset_id3 = H5Dopen2(group_id, DATASET_ARRAY_TYPE_TEST_DSET_NAME3, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open dataset '%s'\n", DATASET_ARRAY_TYPE_TEST_DSET_NAME3);
        goto error;
    }

    if (H5Tclose(array_base_type_id1) < 0)
        TEST_ERROR;
    if (H5Tclose(array_base_type_id2) < 0)
        TEST_ERROR;
    if (H5Tclose(array_base_type_id3) < 0)
        TEST_ERROR;
    if (H5Tclose(nested_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(array_type_id1) < 0)
        TEST_ERROR;
    if (H5Tclose(array_type_id2) < 0)
        TEST_ERROR;
    if (H5Tclose(array_type_id3) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id3) < 0)
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
        H5Tclose(array_base_type_id1);
        H5Tclose(array_base_type_id2);
        H5Tclose(array_base_type_id3);
        H5Tclose(nested_type_id);
        H5Tclose(array_type_id1);
        H5Tclose(array_type_id2);
        H5Tclose(array_type_id3);
        H5Sclose(fspace_id);
        H5Dclose(dset_id1);
        H5Dclose(dset_id2);
        H5Dclose(dset_id3);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

size_t
filter(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
       const unsigned int H5_ATTR_UNUSED cd_values[], size_t nbytes, size_t H5_ATTR_UNUSED *buf_size,
       void H5_ATTR_UNUSED **buf)
{
    *buf_size = 0;
    return nbytes;
}

/*
 * A test to check the functionality of the different
 * dataset creation properties.
 */
static int
test_create_dataset_creation_properties(void)
{
    hsize_t      dims[DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK];
    hsize_t      chunk_dims[DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK];
    size_t       i;
    hid_t        file_id         = H5I_INVALID_HID;
    hid_t        container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t        dset_id = H5I_INVALID_HID, dcpl_id = H5I_INVALID_HID;
    hid_t        dset_dtype = H5I_INVALID_HID, compact_dtype = H5I_INVALID_HID;
    hid_t        fspace_id = H5I_INVALID_HID, compact_fspace_id = H5I_INVALID_HID;
    void        *read_buf                                                             = NULL;
    unsigned int filter_params[DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NUM_PARAMS] = {1, 2, 3};
    unsigned int filter_params_out[DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NUM_PARAMS];
    char         ud_filter_name[sizeof(DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NAME)];
    int          nfilters            = 0;
    H5Z_filter_t retrieved_filter_id = H5I_INVALID_HID;
    size_t       num_filter_params   = DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NUM_PARAMS;
    TESTING_MULTIPART("dataset creation properties");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_CREATION_PROPERTIES_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", DATASET_CREATION_PROPERTIES_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id =
             generate_random_dataspace(DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK, NULL, dims, false)) < 0)
        TEST_ERROR;
    if ((compact_fspace_id =
             generate_random_dataspace(DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    /* Set chunk dims to be size of dataset - for filters test */
    for (i = 0; i < DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK; i++)
        chunk_dims[i] = dims[i];

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((compact_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        /* Test the alloc time property */
        PART_BEGIN(DCPL_alloc_time_test)
        {
            H5D_alloc_time_t alloc_times[] = {H5D_ALLOC_TIME_DEFAULT, H5D_ALLOC_TIME_EARLY,
                                              H5D_ALLOC_TIME_INCR, H5D_ALLOC_TIME_LATE};

            TESTING_2("dataset storage space allocation time property");

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_alloc_time_test);
            }

            for (i = 0; i < ARRAY_LENGTH(alloc_times); i++) {
                char name[100];

                if (H5Pset_alloc_time(dcpl_id, alloc_times[i]) < 0) {
                    H5_FAILED();
                    printf("    couldn't set alloc time property value\n");
                    PART_ERROR(DCPL_alloc_time_test);
                }

                snprintf(name, sizeof(name), "%s%zu", DATASET_CREATION_PROPERTIES_TEST_ALLOC_TIMES_BASE_NAME,
                         i);

                if ((dset_id = H5Dcreate2(group_id, name, dset_dtype, fspace_id, H5P_DEFAULT, dcpl_id,
                                          H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create dataset '%s'\n", name);
                    PART_ERROR(DCPL_alloc_time_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }

                if ((dset_id = H5Dopen2(group_id, name, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", name);
                    PART_ERROR(DCPL_alloc_time_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }
            }

            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_alloc_time_test);

        /* Test the attribute creation order property */
        PART_BEGIN(DCPL_attr_crt_order_test)
        {
            unsigned creation_orders[] = {H5P_CRT_ORDER_TRACKED,
                                          H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED};

            TESTING_2("attribute creation order property for DCPL");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking is not supported by this VOL connector\n");
                PART_EMPTY(DCPL_attr_crt_order_test);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_attr_crt_order_test);
            }

            for (i = 0; i < ARRAY_LENGTH(creation_orders); i++) {
                char name[100];

                if (H5Pset_attr_creation_order(dcpl_id, creation_orders[i]) < 0) {
                    H5_FAILED();
                    printf("    couldn't set attribute creation order property\n");
                    PART_ERROR(DCPL_attr_crt_order_test);
                }

                snprintf(name, sizeof(name), "%s%zu", DATASET_CREATION_PROPERTIES_TEST_CRT_ORDER_BASE_NAME,
                         i);

                if ((dset_id = H5Dcreate2(group_id, name, dset_dtype, fspace_id, H5P_DEFAULT, dcpl_id,
                                          H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create dataset '%s'\n", name);
                    PART_ERROR(DCPL_attr_crt_order_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }

                if ((dset_id = H5Dopen2(group_id, name, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", name);
                    PART_ERROR(DCPL_attr_crt_order_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }
            }

            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_attr_crt_order_test);

        /* Test the attribute phase change property */
        PART_BEGIN(DCPL_attr_phase_change_test)
        {
            TESTING_2("attribute phase change property for DCPL");

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_attr_phase_change_test);
            }

            if (H5Pset_attr_phase_change(dcpl_id, DATASET_CREATION_PROPERTIES_TEST_MAX_COMPACT,
                                         DATASET_CREATION_PROPERTIES_TEST_MIN_DENSE) < 0) {
                H5_FAILED();
                printf("    couldn't set attribute phase change property\n");
                PART_ERROR(DCPL_attr_phase_change_test);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_CREATION_PROPERTIES_TEST_PHASE_CHANGE_DSET_NAME,
                                      dset_dtype, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_PHASE_CHANGE_DSET_NAME);
                PART_ERROR(DCPL_attr_phase_change_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_CREATION_PROPERTIES_TEST_PHASE_CHANGE_DSET_NAME,
                                    H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_PHASE_CHANGE_DSET_NAME);
                PART_ERROR(DCPL_attr_phase_change_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }
            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_attr_phase_change_test);

        /* Test the fill time property */
        PART_BEGIN(DCPL_fill_time_property_test)
        {
            H5D_fill_time_t fill_times[] = {H5D_FILL_TIME_IFSET, H5D_FILL_TIME_ALLOC, H5D_FILL_TIME_NEVER};

            TESTING_2("dataset fill time property");

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_fill_time_property_test);
            }

            for (i = 0; i < ARRAY_LENGTH(fill_times); i++) {
                char name[100];

                if (H5Pset_fill_time(dcpl_id, fill_times[i]) < 0) {
                    H5_FAILED();
                    printf("    couldn't set dataset fill time property\n");
                    PART_ERROR(DCPL_fill_time_property_test);
                }

                snprintf(name, sizeof(name), "%s%zu", DATASET_CREATION_PROPERTIES_TEST_FILL_TIMES_BASE_NAME,
                         i);

                if ((dset_id = H5Dcreate2(group_id, name, dset_dtype, fspace_id, H5P_DEFAULT, dcpl_id,
                                          H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create dataset '%s'\n", name);
                    PART_ERROR(DCPL_fill_time_property_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }

                if ((dset_id = H5Dopen2(group_id, name, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", name);
                    PART_ERROR(DCPL_fill_time_property_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }
            }

            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_fill_time_property_test);

        PART_BEGIN(DCPL_fill_value_test)
        {
            TESTING_2("fill values");

            int    int_fill_value    = DATASET_FILL_VALUE_TEST_INT_FILL_VALUE;
            double double_fill_value = DATASET_FILL_VALUE_TEST_DOUBLE_FILL_VALUE;

            void  *val       = NULL;
            size_t num_elems = 1;
            hid_t  type_id   = H5I_INVALID_HID;

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILL_VALUES)) {
                SKIPPED();
                printf("    dataset fill values are not supported by this VOL connector\n");
                PART_EMPTY(DCPL_fill_value_test);
            }

            /* Integer Fill Value */
            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Pset_fill_value(dcpl_id, DATASET_FILL_VALUE_TEST_INT_TYPE, (const void *)&int_fill_value) <
                0) {
                H5_FAILED();
                printf("    couldn't set integer fill value in property list");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((dset_id =
                     H5Dcreate(group_id, DATASET_FILL_VALUE_TEST_DSET_NAME1, DATASET_FILL_VALUE_TEST_INT_TYPE,
                               fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset with integer fill value");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((H5Sget_simple_extent_dims(fspace_id, dims, NULL)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace dimensions");
                PART_ERROR(DCPL_fill_value_test);
            }

            for (i = 0; i < DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK; i++)
                num_elems *= (size_t)dims[i];

            if ((read_buf = calloc(num_elems, sizeof(DATASET_FILL_VALUE_TEST_INT_TYPE))) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Dread(dset_id, DATASET_FILL_VALUE_TEST_INT_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) <
                0) {
                H5_FAILED();
                printf("    couldn't read from dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            for (i = 0; i < num_elems; i++) {
                val = (int *)(read_buf) + i;

                if (*(int *)val != DATASET_FILL_VALUE_TEST_INT_FILL_VALUE) {
                    H5_FAILED();
                    printf("    incorrect value read from dataset");
                    PART_ERROR(DCPL_fill_value_test);
                }
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close integer fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dcpl");
                PART_ERROR(DCPL_fill_value_test);
            }

            /* Re-open integer dataset */
            if ((dset_id = H5Dopen2(group_id, DATASET_FILL_VALUE_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open integer fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close opened integer fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            free(read_buf);
            read_buf = NULL;

            /* Double fill value */
            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
                H5_FAILED();
                printf("    couldn't create dcpl");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((H5Pset_fill_value(dcpl_id, DATASET_FILL_VALUE_TEST_DOUBLE_TYPE,
                                   (const void *)&double_fill_value)) < 0) {
                H5_FAILED();
                printf("    couldn't set double fill value in property list");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_FILL_VALUE_TEST_DSET_NAME2,
                                      DATASET_FILL_VALUE_TEST_DOUBLE_TYPE, fspace_id, H5P_DEFAULT, dcpl_id,
                                      H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset with double fill value");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((read_buf = calloc(num_elems, sizeof(DATASET_FILL_VALUE_TEST_DOUBLE_TYPE))) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Dread(dset_id, DATASET_FILL_VALUE_TEST_DOUBLE_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            for (i = 0; i < num_elems; i++) {
                val = (double *)(read_buf) + i;

                if (!(H5_DBL_REL_EQUAL(*(double *)val, DATASET_FILL_VALUE_TEST_DOUBLE_FILL_VALUE,
                                       0.0000001))) {
                    H5_FAILED();
                    printf("    incorrect value read from dataset");
                    PART_ERROR(DCPL_fill_value_test);
                }
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close double fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dcpl");
                PART_ERROR(DCPL_fill_value_test);
            }

            /* Re-open double dataset */
            if ((dset_id = H5Dopen2(group_id, DATASET_FILL_VALUE_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open double fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close opened double fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            free(read_buf);
            read_buf = NULL;

            /* Fixed-length string fill value */
            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID) {
                H5_FAILED();
                printf("    couldn't create dcpl");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((type_id = H5Tcopy(H5T_C_S1)) < 0) {
                H5_FAILED();
                printf("    couldn't copy string datatype");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((H5Tset_size(type_id, DATASET_FILL_VALUE_TEST_STRING_SIZE)) < 0) {
                H5_FAILED();
                printf("    couldn't set size of string datatype");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((H5Pset_fill_value(dcpl_id, type_id,
                                   (const void *)DATASET_FILL_VALUE_TEST_STRING_FILL_VALUE)) < 0) {
                H5_FAILED();
                printf("    couldn't set string fill value in property list");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_FILL_VALUE_TEST_DSET_NAME3, type_id, fspace_id,
                                      H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset with string fill value");
                PART_ERROR(DCPL_fill_value_test);
            }

            if ((read_buf = calloc(num_elems, DATASET_FILL_VALUE_TEST_STRING_SIZE)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Dread(dset_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            for (i = 0; i < num_elems; i++) {
                char val_str[DATASET_FILL_VALUE_TEST_STRING_SIZE + 1];

                memcpy(val_str, ((char *)read_buf) + i * DATASET_FILL_VALUE_TEST_STRING_SIZE,
                       DATASET_FILL_VALUE_TEST_STRING_SIZE);
                val_str[DATASET_FILL_VALUE_TEST_STRING_SIZE] = '\0';

                if (strcmp(val_str, DATASET_FILL_VALUE_TEST_STRING_FILL_VALUE)) {
                    H5_FAILED();
                    printf("    incorrect value read from string  dataset");
                    PART_ERROR(DCPL_fill_value_test);
                }
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close string fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dcpl");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Tclose(type_id) < 0) {
                H5_FAILED();
                printf("    couldn't close string type");
                PART_ERROR(DCPL_fill_value_test);
            }

            free(read_buf);
            read_buf = NULL;

            /* Re-open string dataset */
            if ((dset_id = H5Dopen2(group_id, DATASET_FILL_VALUE_TEST_DSET_NAME3, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open string fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close opened string fill value dataset");
                PART_ERROR(DCPL_fill_value_test);
            }

            PASSED();
        }
        PART_END(DCPL_fill_value_test);

        /* Test filters */
        PART_BEGIN(DCPL_filters_test)
        {
            TESTING_2("dataset filters");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILTERS)) {
                SKIPPED();
                printf("    dataset filters are not supported by this VOL connector\n");
                PART_EMPTY(DCPL_filters_test);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }

            if (H5Pset_chunk(dcpl_id, DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK, chunk_dims) < 0) {
                H5_FAILED();
                printf("    couldn't set chunking on DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }

            /* Set all of the available filters on the DCPL */
            if (H5Pset_deflate(dcpl_id, 7) < 0) {
                H5_FAILED();
                printf("    couldn't set deflate filter on DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }
            if (H5Pset_shuffle(dcpl_id) < 0) {
                H5_FAILED();
                printf("    couldn't set shuffle filter on DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }
            if (H5Pset_fletcher32(dcpl_id) < 0) {
                H5_FAILED();
                printf("    couldn't set fletcher32 filter on DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }
            if (H5Pset_nbit(dcpl_id) < 0) {
                H5_FAILED();
                printf("    couldn't set nbit filter on DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }
            if (H5Pset_scaleoffset(dcpl_id, H5Z_SO_FLOAT_ESCALE, 2) < 0) {
                H5_FAILED();
                printf("    couldn't set scaleoffset filter on DCPL\n");
                PART_ERROR(DCPL_filters_test);
            }

            /*
             * Use a simple datatype, as not all filters support all datatypes.
             */
            if ((dset_id = H5Dcreate2(group_id, DATASET_CREATION_PROPERTIES_TEST_FILTERS_DSET_NAME,
                                      H5T_NATIVE_INT, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_FILTERS_DSET_NAME);
                PART_ERROR(DCPL_filters_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_CREATION_PROPERTIES_TEST_FILTERS_DSET_NAME,
                                    H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_FILTERS_DSET_NAME);
                PART_ERROR(DCPL_filters_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_filters_test);

        /* Test a user-defined filter */
        PART_BEGIN(DCPL_user_defined_filter_test)
        {
            TESTING_2("user-defined dataset filters");
            /* Create user-defined filter and register with library */
            const H5Z_class2_t filter_cls[1] = {
                {H5Z_CLASS_T_VERS, DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_ID, 1, 1,
                 DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NAME, NULL, NULL, &filter}};

            if (H5Zregister((const void *)&filter_cls) < 0) {
                H5_FAILED();
                printf("    couldn't register filter\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            if (H5Pset_chunk(dcpl_id, DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK, chunk_dims) < 0) {
                H5_FAILED();
                printf("    couldn't set chunking on DCPL\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            /* Set user-defined filter on the DCPL */
            if (H5Pset_filter(dcpl_id, (H5Z_filter_t)DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_ID,
                              H5Z_FLAG_MANDATORY, 3, filter_params) < 0) {
                H5_FAILED();
                printf("    couldn't set user-defined filter on DCPL\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            /* Use a simple datatype, as not all filters support all datatypes. */
            if ((dset_id = H5Dcreate2(group_id, DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_DSET_NAME,
                                      H5T_NATIVE_INT, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_DSET_NAME);
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY;
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_DSET_NAME,
                                    H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_DSET_NAME);
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY;
                dcpl_id = H5I_INVALID_HID;
            }

            /* Test that parameters are preserved in the DCPL */
            memset(filter_params_out, 0,
                   sizeof(unsigned int) * DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NUM_PARAMS);

            if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve DCPL\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            if ((nfilters = H5Pget_nfilters(dcpl_id)) != 1) {
                H5_FAILED();
                printf("    retrieved incorrect number of filters from DCPL\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            if ((retrieved_filter_id = H5Pget_filter2(
                     dcpl_id, 0, H5Z_FLAG_MANDATORY, &num_filter_params, filter_params_out,
                     strlen(DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NAME), ud_filter_name, NULL)) < 0) {
                H5_FAILED();
                printf("    retrieved incorrect user-defined filter ID\n");
                PART_ERROR(DCPL_user_defined_filter_test);
            }

            for (i = 0; i < DATASET_CREATION_PROPERTIES_TEST_UD_FILTER_NUM_PARAMS; i++)
                if (filter_params[i] != filter_params_out[i]) {
                    H5_FAILED();
                    printf("    retrieved incorrect parameter value from DCPL\n");
                    PART_ERROR(DCPL_user_defined_filter_test);
                }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY;
                dset_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_user_defined_filter_test)

        /* Test the dataset storage layout property */
        PART_BEGIN(DCPL_storage_layout_test)
        {
            H5D_layout_t layouts[] = {H5D_COMPACT, H5D_CONTIGUOUS, H5D_CHUNKED};

            TESTING_2("dataset storage layouts");

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_storage_layout_test);
            }

            for (i = 0; i < ARRAY_LENGTH(layouts); i++) {
                char name[100];

                if (H5Pset_layout(dcpl_id, layouts[i]) < 0) {
                    H5_FAILED();
                    printf("    couldn't set storage layout property\n");
                    PART_ERROR(DCPL_storage_layout_test);
                }

                if (H5D_CHUNKED == layouts[i]) {
                    hsize_t local_chunk_dims[DATASET_CREATION_PROPERTIES_TEST_CHUNK_DIM_RANK];
                    size_t  j;

                    for (j = 0; j < DATASET_CREATION_PROPERTIES_TEST_CHUNK_DIM_RANK; j++)
                        local_chunk_dims[j] = (hsize_t)(rand() % (int)dims[j] + 1);

                    if (H5Pset_chunk(dcpl_id, DATASET_CREATION_PROPERTIES_TEST_CHUNK_DIM_RANK,
                                     local_chunk_dims) < 0) {
                        H5_FAILED();
                        printf("    couldn't set chunk dimensionality\n");
                        PART_ERROR(DCPL_storage_layout_test);
                    }
                }

                snprintf(name, sizeof(name), "%s%zu", DATASET_CREATION_PROPERTIES_TEST_LAYOUTS_BASE_NAME, i);

                if ((dset_id =
                         H5Dcreate2(group_id, name, (H5D_COMPACT == layouts[i]) ? compact_dtype : dset_dtype,
                                    (H5D_COMPACT == layouts[i]) ? compact_fspace_id : fspace_id, H5P_DEFAULT,
                                    dcpl_id, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create dataset '%s'\n", name);
                    PART_ERROR(DCPL_storage_layout_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }

                if ((dset_id = H5Dopen2(group_id, name, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", name);
                    PART_ERROR(DCPL_storage_layout_test);
                }

                if (dset_id >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id);
                    }
                    H5E_END_TRY
                    dset_id = H5I_INVALID_HID;
                }
            }

            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_storage_layout_test);

        /* Test the "track object times" property */
        PART_BEGIN(DCPL_track_obj_times_test)
        {
            TESTING_2("object time tracking property for DCPL");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_TRACK_TIMES)) {
                SKIPPED();
                printf("    object time tracking is not supported by this VOL connector\n");
                PART_EMPTY(DCPL_track_obj_times_test);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    couldn't create DCPL\n");
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if (H5Pset_obj_track_times(dcpl_id, true) < 0) {
                H5_FAILED();
                printf("    couldn't set object time tracking property\n");
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_YES_DSET_NAME,
                                      dset_dtype, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_YES_DSET_NAME);
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_YES_DSET_NAME,
                                    H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_YES_DSET_NAME);
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if (H5Pset_obj_track_times(dcpl_id, false) < 0) {
                H5_FAILED();
                printf("    couldn't set object time tracking property\n");
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_NO_DSET_NAME,
                                      dset_dtype, fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_NO_DSET_NAME);
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_NO_DSET_NAME,
                                    H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n",
                       DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_NO_DSET_NAME);
                PART_ERROR(DCPL_track_obj_times_test);
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }
            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(DCPL_track_obj_times_test);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (H5Sclose(compact_fspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(compact_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        if (read_buf) {
            free(read_buf);
        }

        H5Sclose(compact_fspace_id);
        H5Sclose(fspace_id);
        H5Tclose(compact_dtype);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Pclose(dcpl_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);

        H5E_END_TRY

        return 1;
    }
}

/*
 * A test to create many small datasets (100,000)
 */
static int
test_create_many_dataset(void)
{
    hid_t         file_id         = H5I_INVALID_HID;
    hid_t         container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t         dset_id      = H5I_INVALID_HID;
    hid_t         dataspace_id = H5I_INVALID_HID;
    char          dset_name[DSET_NAME_BUF_SIZE];
    unsigned char data;
    unsigned int  i;

    TESTING("creating many datasets");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_MANY_CREATE_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_MANY_CREATE_GROUP_NAME);
        goto error;
    }

    if ((dataspace_id = H5Screate(H5S_SCALAR)) < 0) {
        H5_FAILED();
        printf("    couldn't create scalar data space\n");
        goto error;
    }

    printf("\n");
    for (i = 0; i < DATASET_NUMB; i++) {
        printf("\r %u/%u", i + 1, DATASET_NUMB);
        snprintf(dset_name, sizeof(dset_name), "dset_%02u", i);
        data = i % 256;

        if ((dset_id = H5Dcreate2(group_id, dset_name, H5T_NATIVE_UCHAR, dataspace_id, H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", dset_name);
            goto error;
        }

        if (H5Dwrite(dset_id, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT, &data) < 0) {
            H5_FAILED();
            printf("    couldn't write to dataset '%s'\n", dset_name);
            goto error;
        }

        if (H5Dclose(dset_id) < 0) {
            H5_FAILED();
            printf("    couldn't close dataset '%s'\n", dset_name);
            goto error;
        }
    }

    if (H5Sclose(dataspace_id) < 0)
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
        H5Dclose(dset_id);
        H5Sclose(dataspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that re-opening a dataset with
 * H5Dopen succeeds.
 */
static int
test_open_dataset(void)
{
    TESTING("H5Dopen");

    SKIPPED();

    return 0;
}

/*
 * A test to check that H5Dopen fails when it is
 * passed invalid parameters.
 */
static int
test_open_dataset_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Dopen with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_OPEN_INVALID_PARAMS_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_OPEN_INVALID_PARAMS_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_OPEN_INVALID_PARAMS_SPACE_RANK, NULL, NULL, false)) <
        0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_OPEN_INVALID_PARAMS_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_OPEN_INVALID_PARAMS_DSET_NAME);
        goto error;
    }

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dopen_invalid_loc_id)
        {
            TESTING_2("H5Dopen with an invalid loc_id");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dopen2(H5I_INVALID_HID, DATASET_OPEN_INVALID_PARAMS_DSET_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    opened dataset using H5Dopen2 with an invalid loc_id!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dopen_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Dopen_invalid_loc_id);

        PART_BEGIN(H5Dopen_invalid_dataset_name)
        {
            TESTING_2("H5Dopen with an invalid dataset name");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dopen2(group_id, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    opened dataset using H5Dopen2 with a NULL dataset name!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dopen_invalid_dataset_name);
            }

            H5E_BEGIN_TRY
            {
                dset_id = H5Dopen2(group_id, "", H5P_DEFAULT);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    opened dataset using H5Dopen2 with an invalid dataset name of ''!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dopen_invalid_dataset_name);
            }

            PASSED();
        }
        PART_END(H5Dopen_invalid_dataset_name);

        PART_BEGIN(H5Dopen_invalid_dapl)
        {
            TESTING_2("H5Dopen with an invalid DAPL");

            H5E_BEGIN_TRY
            {
                dset_id = H5Dopen2(group_id, DATASET_OPEN_INVALID_PARAMS_DSET_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (dset_id >= 0) {
                H5_FAILED();
                printf("    opened dataset using H5Dopen2 with an invalid DAPL!\n");
                H5Dclose(dset_id);
                PART_ERROR(H5Dopen_invalid_dapl);
            }

            PASSED();
        }
        PART_END(H5Dopen_invalid_dapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Dclose fails when it is
 * passed an invalid dataset ID.
 */
static int
test_close_dataset_invalid_params(void)
{
    herr_t err_ret = -1;
    hid_t  file_id = H5I_INVALID_HID;

    TESTING("H5Dclose with an invalid dataset ID");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    H5E_BEGIN_TRY
    {
        err_ret = H5Dclose(H5I_INVALID_HID);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    H5Dclose succeeded with an invalid dataset ID!\n");
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
 * A test to check that valid copies of a dataset's dataspace
 * and datatype can be retrieved with H5Dget_space and
 * H5Dget_type, respectively.
 */
static int
test_get_dataset_space_and_type(void)
{
    hsize_t dset_dims[DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK];
    size_t  i;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID;
    hid_t   group_id        = H5I_INVALID_HID;
    hid_t   dset_id         = H5I_INVALID_HID;
    hid_t   dset_dtype      = H5I_INVALID_HID;
    hid_t   dset_space_id   = H5I_INVALID_HID;
    hid_t   tmp_type_id     = H5I_INVALID_HID;
    hid_t   tmp_space_id    = H5I_INVALID_HID;

    TESTING_MULTIPART("retrieval of a dataset's dataspace and datatype");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_GET_SPACE_TYPE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n", DATASET_GET_SPACE_TYPE_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_space_id =
             generate_random_dataspace(DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK, NULL, dset_dims, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_GET_SPACE_TYPE_TEST_DSET_NAME, dset_dtype, dset_space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_GET_SPACE_TYPE_TEST_DSET_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /* Retrieve the dataset's datatype and dataspace and verify them */
        PART_BEGIN(H5Dget_type)
        {
            TESTING_2("H5Dget_type");

            if ((tmp_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve dataset's datatype\n");
                PART_ERROR(H5Dget_type);
            }

            {
                htri_t types_equal = H5Tequal(tmp_type_id, dset_dtype);

                if (types_equal < 0) {
                    H5_FAILED();
                    printf("    datatype was invalid\n");
                    PART_ERROR(H5Dget_type);
                }

                if (!types_equal) {
                    H5_FAILED();
                    printf("    dataset's datatype did not match\n");
                    PART_ERROR(H5Dget_type);
                }
            }

            PASSED();
        }
        PART_END(H5Dget_type);

        PART_BEGIN(H5Dget_space)
        {
            TESTING_2("H5Dget_space");

            if ((tmp_space_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve dataset's dataspace\n");
                PART_ERROR(H5Dget_space);
            }

            {
                hsize_t space_dims[DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK];

                if (H5Sget_simple_extent_dims(tmp_space_id, space_dims, NULL) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve dataspace dimensions\n");
                    PART_ERROR(H5Dget_space);
                }

                for (i = 0; i < DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK; i++)
                    if (space_dims[i] != dset_dims[i]) {
                        H5_FAILED();
                        printf("    dataset's dataspace dims didn't match\n");
                        PART_ERROR(H5Dget_space);
                    }
            }

            PASSED();
        }
        PART_END(H5Dget_space);

        /* Now close the dataset and verify that this still works after
         * opening an attribute instead of creating it.
         */
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
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

        PART_BEGIN(H5Dget_type_reopened)
        {
            TESTING_2("H5Dget_type after re-opening a dataset");

            if ((dset_id = H5Dopen2(group_id, DATASET_GET_SPACE_TYPE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_GET_SPACE_TYPE_TEST_DSET_NAME);
                PART_ERROR(H5Dget_type_reopened);
            }

            if ((tmp_type_id = H5Dget_type(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve dataset's datatype\n");
                PART_ERROR(H5Dget_type_reopened);
            }

            {
                htri_t types_equal = H5Tequal(tmp_type_id, dset_dtype);

                if (types_equal < 0) {
                    H5_FAILED();
                    printf("    datatype was invalid\n");
                    PART_ERROR(H5Dget_type_reopened);
                }

                if (!types_equal) {
                    H5_FAILED();
                    printf("    dataset's datatype did not match\n");
                    PART_ERROR(H5Dget_type_reopened);
                }
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(H5Dget_type_reopened);

        PART_BEGIN(H5Dget_space_reopened)
        {
            TESTING_2("H5Dget_space after re-opening a dataset");

            if ((dset_id = H5Dopen2(group_id, DATASET_GET_SPACE_TYPE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_GET_SPACE_TYPE_TEST_DSET_NAME);
                PART_ERROR(H5Dget_space_reopened);
            }

            if ((tmp_space_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't retrieve dataset's dataspace\n");
                PART_ERROR(H5Dget_space_reopened);
            }

            {
                hsize_t space_dims[DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK];

                if (H5Sget_simple_extent_dims(tmp_space_id, space_dims, NULL) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve dataspace dimensions\n");
                    PART_ERROR(H5Dget_space_reopened);
                }

                for (i = 0; i < DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK; i++) {
                    if (space_dims[i] != dset_dims[i]) {
                        H5_FAILED();
                        printf("    dataset's dataspace dims didn't match!\n");
                        PART_ERROR(H5Dget_space_reopened);
                    }
                }
            }

            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            PASSED();
        }
        PART_END(H5Dget_space_reopened);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(tmp_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(dset_space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(tmp_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
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
        H5Sclose(dset_space_id);
        H5Tclose(tmp_type_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset's dataspace and datatype
 * can't be retrieved when H5Dget_space and H5Dget_type are passed
 * invalid parameters, respectively.
 */
static int
test_get_dataset_space_and_type_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID;
    hid_t group_id        = H5I_INVALID_HID;
    hid_t dset_id         = H5I_INVALID_HID;
    hid_t dset_dtype      = H5I_INVALID_HID;
    hid_t dset_space_id   = H5I_INVALID_HID;
    hid_t tmp_type_id     = H5I_INVALID_HID;
    hid_t tmp_space_id    = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Dget_type/H5Dget_space with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
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

    if ((group_id = H5Gcreate2(container_group, DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container group '%s'\n",
               DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_space_id = generate_random_dataspace(DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_SPACE_RANK,
                                                   NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_DSET_NAME, dset_dtype,
                              dset_space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_DSET_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dget_type_invalid_dset_id)
        {
            TESTING_2("H5Dget_type with an invalid dset_id");

            H5E_BEGIN_TRY
            {
                tmp_type_id = H5Dget_type(H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (tmp_type_id >= 0) {
                H5_FAILED();
                printf("    retrieved copy of dataset's datatype using an invalid dataset ID!\n");
                PART_ERROR(H5Dget_type_invalid_dset_id);
            }

            PASSED();
        }
        PART_END(H5Dget_type_invalid_dset_id);

        PART_BEGIN(H5Dget_space_invalid_dset_id)
        {
            TESTING_2("H5Dget_space with an invalid dset_id");

            H5E_BEGIN_TRY
            {
                tmp_space_id = H5Dget_space(H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (tmp_space_id >= 0) {
                H5_FAILED();
                printf("    retrieved copy of dataset's dataspace using an invalid dataset ID!\n");
                PART_ERROR(H5Dget_space_invalid_dset_id);
            }

            PASSED();
        }
        PART_END(H5Dget_space_invalid_dset_id);
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
        H5Sclose(tmp_space_id);
        H5Sclose(dset_space_id);
        H5Tclose(tmp_type_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Dget_space_status.
 */
static int
test_get_dataset_space_status(void)
{
    TESTING("H5Dget_space_status");

    SKIPPED();

    return 0;
}

/*
 * A test to check that a dataset's dataspace allocation
 * status can't be retrieved with H5Dget_space_status when
 * it is passed invalid parameters.
 */
static int
test_get_dataset_space_status_invalid_params(void)
{
    TESTING("H5Dget_space_status with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test to check that a DCPL used for dataset creation
 * can be persisted and that a valid copy of that DCPL can
 * be retrieved later with a call to H5Dget_create_plist.
 * Also tests that a valid copy of a DAPL used for dataset
 * access can be retrieved with a call to H5Dget_access_plist.
 */
static int
test_dataset_property_lists(void)
{
    const char *path_prefix = "/test_prefix";
    hsize_t     dims[DATASET_PROPERTY_LIST_TEST_SPACE_RANK];
    hsize_t     chunk_dims[DATASET_PROPERTY_LIST_TEST_SPACE_RANK];
    size_t      i;
    herr_t      err_ret         = -1;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       dset_id1 = H5I_INVALID_HID, dset_id2 = H5I_INVALID_HID, dset_id3 = H5I_INVALID_HID,
          dset_id4 = H5I_INVALID_HID;
    hid_t dcpl_id1 = H5I_INVALID_HID, dcpl_id2 = H5I_INVALID_HID;
    hid_t dapl_id1 = H5I_INVALID_HID, dapl_id2 = H5I_INVALID_HID;
    hid_t dset_dtype1 = H5I_INVALID_HID, dset_dtype2 = H5I_INVALID_HID, dset_dtype3 = H5I_INVALID_HID,
          dset_dtype4 = H5I_INVALID_HID;
    hid_t space_id    = H5I_INVALID_HID;
    char *tmp_prefix  = NULL;
    char  vol_name[5];

    TESTING_MULTIPART("dataset property list operations");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    /** for DAOS VOL, this test is problematic since auto chunking can be selected, so skip for now */
    if (H5VLget_connector_name(file_id, vol_name, 5) < 0) {
        H5_FAILED();
        printf("    couldn't get VOL connector name\n");
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_PROPERTY_LIST_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_PROPERTY_LIST_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(DATASET_PROPERTY_LIST_TEST_SPACE_RANK, NULL, dims, false)) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_PROPERTY_LIST_TEST_SPACE_RANK; i++)
        chunk_dims[i] = (hsize_t)(rand() % (int)dims[i] + 1);

    if ((dset_dtype1 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype2 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype3 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype4 = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dcpl_id1 = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create DCPL\n");
        goto error;
    }

    if (H5Pset_chunk(dcpl_id1, DATASET_PROPERTY_LIST_TEST_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    couldn't set DCPL property\n");
        goto error;
    }

    if ((dset_id1 = H5Dcreate2(group_id, DATASET_PROPERTY_LIST_TEST_DSET_NAME1, dset_dtype1, space_id,
                               H5P_DEFAULT, dcpl_id1, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_PROPERTY_LIST_TEST_DSET_NAME1);
        goto error;
    }

    if ((dset_id2 = H5Dcreate2(group_id, DATASET_PROPERTY_LIST_TEST_DSET_NAME2, dset_dtype2, space_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_PROPERTY_LIST_TEST_DSET_NAME2);
        goto error;
    }

    if (H5Pclose(dcpl_id1) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dget_create_plist)
        {
            TESTING_2("H5Dget_create_plist");

            /* Try to receive copies of the two property lists, one which has the property set and one
             * which does not */
            if ((dcpl_id1 = H5Dget_create_plist(dset_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Dget_create_plist);
            }

            if ((dcpl_id2 = H5Dget_create_plist(dset_id2)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Dget_create_plist);
            }

            /* Ensure that property list 1 has the property set and property list 2 does not */
            {
                hsize_t tmp_chunk_dims[DATASET_PROPERTY_LIST_TEST_SPACE_RANK];

                memset(tmp_chunk_dims, 0, sizeof(tmp_chunk_dims));

                if (H5Pget_chunk(dcpl_id1, DATASET_PROPERTY_LIST_TEST_SPACE_RANK, tmp_chunk_dims) < 0) {
                    H5_FAILED();
                    printf("    couldn't get DCPL property value\n");
                    PART_ERROR(H5Dget_create_plist);
                }

                for (i = 0; i < DATASET_PROPERTY_LIST_TEST_SPACE_RANK; i++)
                    if (tmp_chunk_dims[i] != chunk_dims[i]) {
                        H5_FAILED();
                        printf("    DCPL property values were incorrect\n");
                        PART_ERROR(H5Dget_create_plist);
                    }

                H5E_BEGIN_TRY
                {
                    err_ret = H5Pget_chunk(dcpl_id2, DATASET_PROPERTY_LIST_TEST_SPACE_RANK, tmp_chunk_dims);
                }
                H5E_END_TRY

                /* DAOS VOL can auto chunk, so don't fail */
                if (err_ret >= 0 && strcmp(vol_name, "daos") != 0) {
                    H5_FAILED();
                    printf("    property list 2 shouldn't have had chunk dimensionality set (not a chunked "
                           "layout)\n");
                    PART_ERROR(H5Dget_create_plist);
                }
            }

            PASSED();
        }
        PART_END(H5Dget_create_plist);

        PART_BEGIN(H5Dget_access_plist)
        {
            TESTING_2("H5Dget_access_plist");

            if ((dapl_id1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0) {
                H5_FAILED();
                printf("    couldn't create DAPL\n");
                PART_ERROR(H5Dget_access_plist);
            }

            if (H5Pset_efile_prefix(dapl_id1, path_prefix) < 0) {
                H5_FAILED();
                printf("    couldn't set DAPL property\n");
                PART_ERROR(H5Dget_access_plist);
            }

            if ((dset_id3 = H5Dcreate2(group_id, DATASET_PROPERTY_LIST_TEST_DSET_NAME3, dset_dtype3, space_id,
                                       H5P_DEFAULT, H5P_DEFAULT, dapl_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset\n");
                PART_ERROR(H5Dget_access_plist);
            }

            if ((dset_id4 = H5Dcreate2(group_id, DATASET_PROPERTY_LIST_TEST_DSET_NAME4, dset_dtype4, space_id,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset\n");
                PART_ERROR(H5Dget_access_plist);
            }

            if (dapl_id1 >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dapl_id1);
                }
                H5E_END_TRY
                dapl_id1 = H5I_INVALID_HID;
            }

            /* Try to receive copies of the two property lists, one which has the property set and one
             * which does not */
            if ((dapl_id1 = H5Dget_access_plist(dset_id3)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Dget_access_plist);
            }

            if ((dapl_id2 = H5Dget_access_plist(dset_id4)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Dget_access_plist);
            }

            /* Ensure that property list 1 has the property set and property list 2 does not */
            {
                ssize_t buf_size = 0;

                if ((buf_size = H5Pget_efile_prefix(dapl_id1, NULL, 0)) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve size for property value buffer\n");
                    PART_ERROR(H5Dget_access_plist);
                }

                if (NULL == (tmp_prefix = (char *)calloc(1, (size_t)buf_size + 1))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for property value\n");
                    PART_ERROR(H5Dget_access_plist);
                }

                if (H5Pget_efile_prefix(dapl_id1, tmp_prefix, (size_t)buf_size + 1) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve property list value\n");
                    PART_ERROR(H5Dget_access_plist);
                }

                if (strncmp(tmp_prefix, path_prefix, (size_t)buf_size + 1)) {
                    H5_FAILED();
                    printf("    DAPL values were incorrect!\n");
                    PART_ERROR(H5Dget_access_plist);
                }

                memset(tmp_prefix, 0, (size_t)buf_size + 1);

                if (H5Pget_efile_prefix(dapl_id2, tmp_prefix, (size_t)buf_size) < 0) {
                    H5_FAILED();
                    printf("    couldn't retrieve property list value\n");
                    PART_ERROR(H5Dget_access_plist);
                }

                if (!strncmp(tmp_prefix, path_prefix, (size_t)buf_size + 1)) {
                    H5_FAILED();
                    printf("    DAPL property value was set!\n");
                    PART_ERROR(H5Dget_access_plist);
                }
            }

            PASSED();
        }
        PART_END(H5Dget_access_plist);

        /* Now close the property lists and datasets and see if we can still retrieve copies of
         * the property lists upon opening (instead of creating) a dataset
         */
        if (dcpl_id1 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(dcpl_id1);
            }
            H5E_END_TRY
            dcpl_id1 = H5I_INVALID_HID;
        }
        if (dcpl_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(dcpl_id2);
            }
            H5E_END_TRY
            dcpl_id2 = H5I_INVALID_HID;
        }
        if (dset_id1 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id1);
            }
            H5E_END_TRY
            dset_id1 = H5I_INVALID_HID;
        }
        if (dset_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id2);
            }
            H5E_END_TRY
            dset_id2 = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Dget_create_plist_reopened)
        {
            TESTING_2("H5Dget_create_plist after re-opening a dataset");

            if ((dset_id1 = H5Dopen2(group_id, DATASET_PROPERTY_LIST_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_PROPERTY_LIST_TEST_DSET_NAME1);
                PART_ERROR(H5Dget_create_plist_reopened);
            }

            if ((dset_id2 = H5Dopen2(group_id, DATASET_PROPERTY_LIST_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_PROPERTY_LIST_TEST_DSET_NAME2);
                PART_ERROR(H5Dget_create_plist_reopened);
            }

            if ((dcpl_id1 = H5Dget_create_plist(dset_id1)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Dget_create_plist_reopened);
            }

            if ((dcpl_id2 = H5Dget_create_plist(dset_id2)) < 0) {
                H5_FAILED();
                printf("    couldn't get property list\n");
                PART_ERROR(H5Dget_create_plist_reopened);
            }

            /* Ensure that property list 1 has the property set and property list 2 does not */
            {
                hsize_t tmp_chunk_dims[DATASET_PROPERTY_LIST_TEST_SPACE_RANK];

                memset(tmp_chunk_dims, 0, sizeof(tmp_chunk_dims));

                if (H5Pget_chunk(dcpl_id1, DATASET_PROPERTY_LIST_TEST_SPACE_RANK, tmp_chunk_dims) < 0) {
                    H5_FAILED();
                    printf("    couldn't get DCPL property value\n");
                    PART_ERROR(H5Dget_create_plist_reopened);
                }

                for (i = 0; i < DATASET_PROPERTY_LIST_TEST_SPACE_RANK; i++)
                    if (tmp_chunk_dims[i] != chunk_dims[i]) {
                        H5_FAILED();
                        printf("    DCPL property values were incorrect\n");
                        PART_ERROR(H5Dget_create_plist_reopened);
                    }

                H5E_BEGIN_TRY
                {
                    err_ret = H5Pget_chunk(dcpl_id2, DATASET_PROPERTY_LIST_TEST_SPACE_RANK, tmp_chunk_dims);
                }
                H5E_END_TRY

                /* DAOS VOL can auto chunk, so don't fail */
                if (err_ret >= 0 && strcmp(vol_name, "daos") != 0) {
                    H5_FAILED();
                    printf("    property list 2 shouldn't have had chunk dimensionality set (not a chunked "
                           "layout)\n");
                    PART_ERROR(H5Dget_create_plist_reopened);
                }
            }

            PASSED();
        }
        PART_END(H5Dget_create_plist_reopened);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (tmp_prefix) {
        free(tmp_prefix);
        tmp_prefix = NULL;
    }

    if (H5Pclose(dcpl_id1) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_id2) < 0)
        TEST_ERROR;
    if (H5Pclose(dapl_id1) < 0)
        TEST_ERROR;
    if (H5Pclose(dapl_id2) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype1) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype2) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype3) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype4) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id3) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id4) < 0)
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
        if (tmp_prefix)
            free(tmp_prefix);
        H5Pclose(dcpl_id1);
        H5Pclose(dcpl_id2);
        H5Pclose(dapl_id1);
        H5Pclose(dapl_id2);
        H5Sclose(space_id);
        H5Tclose(dset_dtype1);
        H5Tclose(dset_dtype2);
        H5Tclose(dset_dtype3);
        H5Tclose(dset_dtype4);
        H5Dclose(dset_id1);
        H5Dclose(dset_id2);
        H5Dclose(dset_id3);
        H5Dclose(dset_id4);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Dget_storage_size.
 */
static int
test_get_dataset_storage_size(void)
{
    TESTING("H5Dget_storage_size");

    SKIPPED();

    return 0;
}

/*
 * A test to check that a dataset's storage size can't
 * be retrieved when H5Dget_storage_size is passed
 * invalid parameters.
 */
static int
test_get_dataset_storage_size_invalid_params(void)
{
    TESTING("H5Dget_storage_size with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test for H5Dget_chunk_storage_size.
 */
static int
test_get_dataset_chunk_storage_size(void)
{
    TESTING("H5Dget_chunk_storage_size");

    SKIPPED();

    return 0;
}

/*
 * A test to check that the size of an allocated chunk in
 * a dataset can't be retrieved when H5Dget_chunk_storage_size
 * is passed invalid parameters.
 */
static int
test_get_dataset_chunk_storage_size_invalid_params(void)
{
    TESTING("H5Dget_chunk_storage_size with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test for H5Dget_offset.
 */
static int
test_get_dataset_offset(void)
{
    TESTING("H5Dget_offset");

    SKIPPED();

    return 0;
}

/*
 * A test to check that a dataset's offset can't be
 * retrieved when H5Dget_offset is passed invalid
 * parameters.
 */
static int
test_get_dataset_offset_invalid_params(void)
{
    TESTING("H5Dget_offset with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test to check that a small amount of data can be
 * read back from a dataset using an H5S_ALL selection.
 */
static int
test_read_dataset_small_all(void)
{
    hsize_t dims[DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK] = {10, 5, 3};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    void   *read_buf  = NULL;

    TESTING("small read from dataset with H5S_ALL");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_READ_TEST_ALL_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SMALL_READ_TEST_ALL_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_SMALL_READ_TEST_ALL_DSET_NAME,
                              DATASET_SMALL_READ_TEST_ALL_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SMALL_READ_TEST_ALL_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_SMALL_READ_TEST_ALL_DSET_DTYPESIZE;

    if (NULL == (read_buf = malloc(data_size)))
        TEST_ERROR;

    if (H5Dread(dset_id, DATASET_SMALL_READ_TEST_ALL_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) <
        0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SMALL_READ_TEST_ALL_DSET_NAME);
        goto error;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (read_buf)
            free(read_buf);
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
 * A test to check that a small amount of data can be
 * read back from a dataset using a hyperslab selection.
 */
static int
test_read_dataset_small_hyperslab(void)
{
    hsize_t start[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t stride[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t count[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t block[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t dims[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK] = {10, 5, 3};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID, fspace_id = H5I_INVALID_HID;
    void   *read_buf = NULL;

    TESTING("small read from dataset with a hyperslab selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_READ_TEST_HYPERSLAB_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_READ_TEST_HYPERSLAB_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK - 1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME,
                              DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK; i++) {
        start[i]  = 0;
        stride[i] = 1;
        count[i]  = dims[i];
        block[i]  = 1;
    }

    count[2] = 1;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    for (i = 0, data_size = 1; i < DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK - 1; i++)
        data_size *= dims[i];
    data_size *= DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPESIZE;

    if (NULL == (read_buf = malloc(data_size)))
        TEST_ERROR;

    if (H5Dread(dset_id, DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME);
        goto error;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
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
        if (read_buf)
            free(read_buf);
        H5Sclose(mspace_id);
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
 * A test to check that a small amount of data can be
 * read back from a dataset using a point selection.
 */
static int
test_read_dataset_small_point_selection(void)
{
    hsize_t points[DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS *
                   DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK];
    hsize_t dims[DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t mspace_dims[] = {DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    void   *data      = NULL;

    TESTING("small read from dataset with a point selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_READ_TEST_POINT_SELECTION_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_READ_TEST_POINT_SELECTION_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK, dims, NULL)) <
        0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(1, mspace_dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_NAME,
                              DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_NAME);
        goto error;
    }

    data_size = DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS *
                DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPESIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS; i++) {
        size_t j;

        for (j = 0; j < DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK; j++)
            points[(i * DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK) + j] = i;
    }

    if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS,
                           points) < 0) {
        H5_FAILED();
        printf("    couldn't select points\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPE, mspace_id, fspace_id,
                H5P_DEFAULT, data) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_NAME);
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
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
        if (data)
            free(data);
        H5Sclose(mspace_id);
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
 * A test to check that a small amount of data can be
 * read back from multiple datasets using H5S_ALL selections.
 */
static int
test_read_multi_dataset_small_all(void)
{

    hsize_t dims[DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK] = {10, 5, 3};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   dset_id_arr[DATASET_MULTI_COUNT];
    hid_t   fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t   dtype_id_arr[DATASET_MULTI_COUNT];
    void   *read_buf_arr[DATASET_MULTI_COUNT];

    TESTING("small multi read from datasets with H5S_ALL");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    /* Prevent uninitialized memory usage on test failure */
    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        read_buf_arr[i] = NULL;
        dset_id_arr[i]  = H5I_INVALID_HID;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_READ_MULTI_TEST_ALL_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SMALL_READ_TEST_ALL_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    for (i = 0, data_size = 1; i < DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_SMALL_READ_TEST_ALL_DSET_DTYPESIZE;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_READ_TEST_ALL_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] = H5Dcreate2(group_id, dset_name, DATASET_SMALL_READ_TEST_ALL_DSET_DTYPE,
                                         fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", DATASET_SMALL_READ_TEST_ALL_DSET_NAME);
            goto error;
        }

        fspace_id_arr[i] = fspace_id;
        dtype_id_arr[i]  = DATASET_SMALL_READ_TEST_ALL_DSET_DTYPE;

        if (NULL == (read_buf_arr[i] = malloc(data_size)))
            TEST_ERROR;
    }

    if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, fspace_id_arr, fspace_id_arr,
                      H5P_DEFAULT, read_buf_arr) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SMALL_READ_TEST_ALL_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        free(read_buf_arr[i]);
        read_buf_arr[i] = NULL;
    }
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    for (i = 0; i < DATASET_MULTI_COUNT; i++)
        if (H5Dclose(dset_id_arr[i]) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            free(read_buf_arr[i]);
            read_buf_arr[i] = NULL;
            H5Dclose(dset_id_arr[i]);
        }
        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that a small amount of data can be
 * read back from datasets using hyperslab selections.
 */
static int
test_read_multi_dataset_small_hyperslab(void)
{
    hsize_t start[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t stride[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t count[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t block[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t dims[DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK] = {10, 5, 3};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id_arr[DATASET_MULTI_COUNT];
    hid_t   mspace_id = H5I_INVALID_HID, fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id_arr[DATASET_MULTI_COUNT];
    hid_t   fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t   dtype_arr[DATASET_MULTI_COUNT];
    void   *read_buf_arr[DATASET_MULTI_COUNT];

    TESTING("small multi read from datasets with a hyperslab selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    /* Prevent uninitialized memory usage on test failure */
    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        read_buf_arr[i] = NULL;
        dset_id_arr[i]  = H5I_INVALID_HID;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_READ_MULTI_TEST_HYPERSLAB_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_READ_TEST_HYPERSLAB_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK - 1, dims, NULL)) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK; i++) {
        start[i]  = 0;
        stride[i] = 1;
        count[i]  = dims[i];
        block[i]  = 1;
    }

    count[2] = 1;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    for (i = 0, data_size = 1; i < DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK - 1; i++)
        data_size *= dims[i];
    data_size *= DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPESIZE;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] = H5Dcreate2(group_id, dset_name, DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPE,
                                         fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME);
            goto error;
        }

        if (NULL == (read_buf_arr[i] = malloc(data_size)))
            TEST_ERROR;

        mspace_id_arr[i] = mspace_id;
        fspace_id_arr[i] = fspace_id;
        dtype_arr[i]     = DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPE;
    }

    if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_arr, mspace_id_arr, fspace_id_arr, H5P_DEFAULT,
                      read_buf_arr) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        if (read_buf_arr[i]) {
            free(read_buf_arr[i]);
            read_buf_arr[i] = NULL;
        }
        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (read_buf_arr[i]) {
                free(read_buf_arr[i]);
                read_buf_arr[i] = NULL;
            }
            H5Dclose(dset_id_arr[i]);
        }
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that a small amount of data can be
 * read back from datasets using point selections.
 */
static int
test_read_multi_dataset_small_point_selection(void)
{
    hsize_t points[DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS *
                   DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK];
    hsize_t dims[DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t mspace_dims[] = {DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID, mspace_id = H5I_INVALID_HID;
    hid_t   dset_id_arr[DATASET_MULTI_COUNT];
    hid_t   mspace_id_arr[DATASET_MULTI_COUNT], fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t   dtype_arr[DATASET_MULTI_COUNT];
    void   *read_buf[DATASET_MULTI_COUNT];

    TESTING("small multi read from datasets with point selections");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    /* Prevent uninitialized memory usage on test failure */
    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        read_buf[i]    = NULL;
        dset_id_arr[i] = H5I_INVALID_HID;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_READ_MULTI_TEST_POINT_SELECTION_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_READ_TEST_POINT_SELECTION_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK, dims, NULL)) <
        0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(1, mspace_dims, NULL)) < 0)
        TEST_ERROR;

    data_size = DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS *
                DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPESIZE;

    for (i = 0; i < DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS; i++) {
        size_t j;

        for (j = 0; j < DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK; j++)
            points[(i * DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK) + j] = i;
    }

    if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS,
                           points) < 0) {
        H5_FAILED();
        printf("    couldn't select points\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] =
                 H5Dcreate2(group_id, dset_name, DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPE,
                            fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_NAME);
            goto error;
        }

        if (NULL == (read_buf[i] = malloc(data_size)))
            TEST_ERROR;

        dtype_arr[i]     = DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPE;
        mspace_id_arr[i] = mspace_id;
        fspace_id_arr[i] = fspace_id;
    }

    if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_arr, mspace_id_arr, fspace_id_arr, H5P_DEFAULT,
                      read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        if (read_buf[i]) {
            free(read_buf[i]);
            read_buf[i] = NULL;
        }
        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (read_buf[i]) {
                free(read_buf[i]);
                read_buf[i] = NULL;
            }
            H5Dclose(dset_id_arr[i]);
        }
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * Tests point selection I/O with different patterns
 */
#define DATASET_IO_POINT_DIM_0   6
#define DATASET_IO_POINT_DIM_1   9
#define DATASET_IO_POINT_CDIM_0  4
#define DATASET_IO_POINT_CDIM_1  3
#define DATASET_IO_POINT_NPOINTS 10
#define DATASET_IO_POINT_GEN_POINTS(POINTS, I, J)                                                            \
    {                                                                                                        \
        for ((I) = 0; (I) < DATASET_IO_POINT_NPOINTS; (I)++)                                                 \
            do {                                                                                             \
                (POINTS)[2 * (I)]     = (hsize_t)(rand() % DATASET_IO_POINT_DIM_0);                          \
                (POINTS)[2 * (I) + 1] = (hsize_t)(rand() % DATASET_IO_POINT_DIM_1);                          \
                for ((J) = 0; ((J) < (I)) && (((POINTS)[2 * (I)] != (POINTS)[2 * (J)]) ||                    \
                                              ((POINTS)[2 * (I) + 1] != (POINTS)[2 * (J) + 1]));             \
                     (J)++)                                                                                  \
                    ;                                                                                        \
            } while ((J) < (I));                                                                             \
    }
static int
test_dataset_io_point_selections(void)
{
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   dset_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   mspace_id_full = H5I_INVALID_HID, mspace_id_all = H5I_INVALID_HID, fspace_id = H5I_INVALID_HID;
    hid_t   dcpl_id_chunk = H5I_INVALID_HID;
    hsize_t dims[2]       = {DATASET_IO_POINT_DIM_0, DATASET_IO_POINT_DIM_1};
    hsize_t cdims[2]      = {DATASET_IO_POINT_CDIM_0, DATASET_IO_POINT_CDIM_1};
    hsize_t points[DATASET_IO_POINT_NPOINTS * 2];
    hsize_t points2[DATASET_IO_POINT_NPOINTS * 2];
    hsize_t npoints   = DATASET_IO_POINT_NPOINTS;
    hsize_t start[2]  = {1, 2};
    hsize_t stride[2] = {2, 5};
    hsize_t count[2]  = {2, 1};
    hsize_t block[2]  = {1, 5};
    int     buf_all[DATASET_IO_POINT_DIM_0][DATASET_IO_POINT_DIM_1];
    int     file_state[DATASET_IO_POINT_DIM_0][DATASET_IO_POINT_DIM_1];
    int     erbuf[DATASET_IO_POINT_DIM_0][DATASET_IO_POINT_DIM_1];
    int     buf_point[DATASET_IO_POINT_NPOINTS];
    bool    do_chunk;
    int     i, j;

    TESTING("point selection I/O with all selection in memory and points in file");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    /* Create dataspaces and DCPL */
    if ((mspace_id_full = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((mspace_id_all = H5Screate_simple(1, &npoints, NULL)) < 0)
        TEST_ERROR;
    if ((fspace_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl_id_chunk = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Enable chunking on chunk DCPL */
    if (H5Pset_chunk(dcpl_id_chunk, 2, cdims) < 0)
        TEST_ERROR;

    /* Open file */
    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Open container group */
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create group */
    if ((group_id = H5Gcreate2(container_group, DATASET_IO_POINT_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Perform with and without chunking */
    for (do_chunk = false;; do_chunk = true) {
        if (do_chunk) {
            TESTING("point selection I/O with all selection in memory and points in file with chunking");

            /* Create chunked dataset */
            if ((dset_id = H5Dcreate2(group_id, DATASET_IO_POINT_DSET_NAME_CHUNK, H5T_NATIVE_INT, fspace_id,
                                      H5P_DEFAULT, dcpl_id_chunk, H5P_DEFAULT)) < 0)
                TEST_ERROR;
        } /* end if */
        else
            /* Create non-chunked dataset */
            if ((dset_id = H5Dcreate2(group_id, DATASET_IO_POINT_DSET_NAME_NOCHUNK, H5T_NATIVE_INT, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                TEST_ERROR;

        /* Fill write buffer */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                buf_all[i][j] = rand();

        /* Write data */
        if (H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to write entire dataset");

        /* Update file_state */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                file_state[i][j] = buf_all[i][j];

        /* Generate points to read */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Wipe read buffer */
        memset(buf_point, 0, sizeof(buf_point));

        /* Read points to "all" memory buffer */
        if (H5Dread(dset_id, H5T_NATIVE_INT, mspace_id_all, fspace_id, H5P_DEFAULT, buf_point) < 0)
            FAIL_PUTS_ERROR("Failed to read points from dataset to all memory buffer");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            if (buf_point[i] != file_state[points[2 * i]][points[2 * i + 1]])
                FAIL_PUTS_ERROR("Incorrect data read from points to all memory buffer");

        /* Generate points to write */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Fill write buffer */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            buf_point[i] = rand();

        /* Write points from "all" memory buffer */
        if (H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id_all, fspace_id, H5P_DEFAULT, buf_point) < 0)
            FAIL_PUTS_ERROR("Failed to write points to dataset from all memory buffer");

        /* Update file state */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            file_state[points[2 * i]][points[2 * i + 1]] = buf_point[i];

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Read entire dataset */
        if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read entire dataset");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != file_state[i][j])
                    FAIL_PUTS_ERROR("Incorrect data found after writing from all memory buffer to points");

        PASSED();

        if (do_chunk)
            TESTING("point selection I/O with points in memory and file (same shape) with chunking");
        else
            TESTING("point selection I/O with points in memory and file (same shape)");

        /* Generate points to read */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Generate expected read buffer */
        memset(erbuf, 0, sizeof(erbuf));
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            erbuf[points[2 * i]][points[2 * i + 1]] = file_state[points[2 * i]][points[2 * i + 1]];

        /* Read data points->points */
        if (H5Dread(dset_id, H5T_NATIVE_INT, fspace_id, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read points from dataset to points in memory buffer");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != erbuf[i][j])
                    FAIL_PUTS_ERROR("Incorrect data found read from points in file to points in memory");

        /* Generate points to write */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Fill write buffer */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                buf_all[i][j] = rand();

        /* Write data points->points */
        if (H5Dwrite(dset_id, H5T_NATIVE_INT, fspace_id, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to write from in memory to points in dataset");

        /* Update file_state */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            file_state[points[2 * i]][points[2 * i + 1]] = buf_all[points[2 * i]][points[2 * i + 1]];

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Read entire dataset */
        if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read entire dataset");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != file_state[i][j])
                    FAIL_PUTS_ERROR(
                        "Incorrect data found after writing from points in memory to points in dataset");

        PASSED();

        if (do_chunk)
            TESTING("point selection I/O with points in memory and file (different shape) with chunking");
        else
            TESTING("point selection I/O with points in memory and file (different shape)");

        /* Generate points to read */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);
        DATASET_IO_POINT_GEN_POINTS(points2, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;
        if (H5Sselect_elements(mspace_id_full, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points2) < 0)
            TEST_ERROR;

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Generate expected read buffer */
        memset(erbuf, 0, sizeof(erbuf));
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            erbuf[points2[2 * i]][points2[2 * i + 1]] = file_state[points[2 * i]][points[2 * i + 1]];

        /* Read data points->points */
        if (H5Dread(dset_id, H5T_NATIVE_INT, mspace_id_full, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read points from dataset to points in memory buffer");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != erbuf[i][j])
                    FAIL_PUTS_ERROR(
                        "Incorrect data found after reading from points in file to points in memory");

        /* Generate points to write */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);
        DATASET_IO_POINT_GEN_POINTS(points2, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;
        if (H5Sselect_elements(mspace_id_full, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points2) < 0)
            TEST_ERROR;

        /* Fill write buffer */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                buf_all[i][j] = rand();

        /* Write data points->points */
        if (H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id_full, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to write from points in memory to points in dataset");

        /* Update file_state */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            file_state[points[2 * i]][points[2 * i + 1]] = buf_all[points2[2 * i]][points2[2 * i + 1]];

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Read entire dataset */
        if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read entire dataset");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != file_state[i][j])
                    FAIL_PUTS_ERROR(
                        "Incorrect data found after writing from points in memory to points in dataset");

        PASSED();

        if (do_chunk)
            TESTING("point selection I/O with hyperslab in memory and points in file with chunking");
        else
            TESTING("point selection I/O with hyperslab in memory and points in file");

        /* Generate points to read */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Select hyperslab */
        if (H5Sselect_hyperslab(mspace_id_full, H5S_SELECT_SET, start, stride, count, block) < 0)
            TEST_ERROR;

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Generate expected read buffer */
        memset(erbuf, 0, sizeof(erbuf));
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            erbuf[start[0] + (stride[0] * ((hsize_t)i / block[1]))][start[1] + ((hsize_t)i % block[1])] =
                file_state[points[2 * i]][points[2 * i + 1]];

        /* Read data points->hslab */
        if (H5Dread(dset_id, H5T_NATIVE_INT, mspace_id_full, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read points from dataset to hyperslab in memory buffer");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != erbuf[i][j])
                    FAIL_PUTS_ERROR(
                        "Incorrect data found after reading from points in file to hyperslab in memory");

        /* Generate points to write */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Fill write buffer */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                buf_all[i][j] = rand();

        /* Write data hlsab->points */
        if (H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id_full, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to write from hyperslab in memory to points in dataset");

        /* Update file_state */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            file_state[points[2 * i]][points[2 * i + 1]] =
                buf_all[start[0] + (stride[0] * ((hsize_t)i / block[1]))][start[1] + ((hsize_t)i % block[1])];

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Read entire dataset */
        if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read entire dataset");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != file_state[i][j])
                    FAIL_PUTS_ERROR("Incorrect data found after writing from hyperslab in memory to "
                                    "points in dataset");

        PASSED();

        if (do_chunk)
            TESTING("point selection I/O with points in memory and hyperslab in file with chunking");
        else
            TESTING("point selection I/O with points in memory and hyperslab in file");

        /* Generate points to read */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(mspace_id_full, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Select hyperslab */
        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
            TEST_ERROR;

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Generate expected read buffer */
        memset(erbuf, 0, sizeof(erbuf));
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            erbuf[points[2 * i]][points[2 * i + 1]] =
                file_state[start[0] + (stride[0] * ((hsize_t)i / block[1]))]
                          [start[1] + ((hsize_t)i % block[1])];

        /* Read data hslab->points */
        if (H5Dread(dset_id, H5T_NATIVE_INT, mspace_id_full, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read hyperslab from dataset to points in memory buffer");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != erbuf[i][j])
                    FAIL_PUTS_ERROR(
                        "Incorrect data found after reading from hyperslab in file to points in memory");

        /* Generate points to write */
        DATASET_IO_POINT_GEN_POINTS(points, i, j);

        /* Select points */
        if (H5Sselect_elements(mspace_id_full, H5S_SELECT_SET, DATASET_IO_POINT_NPOINTS, points) < 0)
            TEST_ERROR;

        /* Fill write buffer */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                buf_all[i][j] = rand();

        /* Write data points->hslab */
        if (H5Dwrite(dset_id, H5T_NATIVE_INT, mspace_id_full, fspace_id, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to write from points in memory to hyperslab in dataset");

        /* Update file_state */
        for (i = 0; i < DATASET_IO_POINT_NPOINTS; i++)
            file_state[start[0] + (stride[0] * ((hsize_t)i / block[1]))][start[1] + ((hsize_t)i % block[1])] =
                buf_all[points[2 * i]][points[2 * i + 1]];

        /* Wipe read buffer */
        memset(buf_all, 0, sizeof(buf_all));

        /* Read entire dataset */
        if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_all) < 0)
            FAIL_PUTS_ERROR("Failed to read entire dataset");

        /* Verify data */
        for (i = 0; i < DATASET_IO_POINT_DIM_0; i++)
            for (j = 0; j < DATASET_IO_POINT_DIM_1; j++)
                if (buf_all[i][j] != file_state[i][j])
                    FAIL_PUTS_ERROR("Incorrect data found after writing from points in memory to "
                                    "hyperslab in dataset");

        if (!do_chunk)
            PASSED();

        /* Close dataset */
        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;

        /* Exit after chunked run */
        if (do_chunk)
            break;
    } /* end for */

    /* Close */
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_id_chunk) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id_full) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id_all) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(fspace_id);
        H5Sclose(mspace_id_full);
        H5Sclose(mspace_id_all);
        H5Pclose(dcpl_id_chunk);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_dataset_io_point_selections() */

/*
 * A test to check that data can't be read from a
 * dataset when H5Dread is passed invalid parameters.
 */
static int
test_read_dataset_invalid_params(void)
{
    hsize_t dims[DATASET_READ_INVALID_PARAMS_TEST_DSET_SPACE_RANK] = {10, 5, 3};
    herr_t  err_ret                                                = -1;
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    void   *read_buf  = NULL;

    TESTING_MULTIPART("H5Dread with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_READ_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_READ_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_READ_INVALID_PARAMS_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_READ_INVALID_PARAMS_TEST_DSET_NAME,
                              DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_READ_INVALID_PARAMS_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_READ_INVALID_PARAMS_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPESIZE;

    if (NULL == (read_buf = malloc(data_size)))
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dread_invalid_dset_id)
        {
            TESTING_2("H5Dread with an invalid dataset ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dread(H5I_INVALID_HID, DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL,
                                  H5S_ALL, H5P_DEFAULT, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read from dataset using H5Dread with an invalid dataset ID!\n");
                PART_ERROR(H5Dread_invalid_dset_id);
            }

            PASSED();
        }
        PART_END(H5Dread_invalid_dset_id);

        PART_BEGIN(H5Dread_invalid_datatype)
        {
            TESTING_2("H5Dread with an invalid memory datatype");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dread(dset_id, H5I_INVALID_HID, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read from dataset using H5Dread with an invalid memory datatype!\n");
                PART_ERROR(H5Dread_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Dread_invalid_datatype);

        PART_BEGIN(H5Dread_invalid_mem_dataspace)
        {
            TESTING_2("H5Dread with an invalid memory dataspace");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dread(dset_id, DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE, H5I_INVALID_HID,
                                  H5S_ALL, H5P_DEFAULT, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read from dataset using H5Dread with an invalid memory dataspace!\n");
                PART_ERROR(H5Dread_invalid_mem_dataspace);
            }

            PASSED();
        }
        PART_END(H5Dread_invalid_mem_dataspace);

        PART_BEGIN(H5Dread_invalid_file_dataspace)
        {
            TESTING_2("H5Dread with an invalid file dataspace");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dread(dset_id, DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL,
                                  H5I_INVALID_HID, H5P_DEFAULT, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read from dataset using H5Dread with an invalid file dataspace!\n");
                PART_ERROR(H5Dread_invalid_file_dataspace);
            }

            PASSED();
        }
        PART_END(H5Dread_invalid_file_dataspace);

        PART_BEGIN(H5Dread_invalid_dxpl)
        {
            TESTING_2("H5Dread with an invalid DXPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dread(dset_id, DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                                  H5I_INVALID_HID, read_buf);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read from dataset using H5Dread with an invalid DXPL!\n");
                PART_ERROR(H5Dread_invalid_dxpl);
            }

            PASSED();
        }
        PART_END(H5Dread_invalid_dxpl);

        PART_BEGIN(H5Dread_invalid_data_buf)
        {
            TESTING_2("H5Dread with an invalid data buffer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dread(dset_id, DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                                  H5P_DEFAULT, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    read from dataset using H5Dread with an invalid data buffer!\n");
                PART_ERROR(H5Dread_invalid_data_buf);
            }

            PASSED();
        }
        PART_END(H5Dread_invalid_data_buf);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (read_buf)
            free(read_buf);
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
 * A test to check that a small write can be
 * made to a dataset using an H5S_ALL selection.
 */
static int
test_write_dataset_small_all(void)
{
    hssize_t space_npoints;
    hsize_t  dims[DATASET_SMALL_WRITE_TEST_ALL_DSET_SPACE_RANK] = {10, 5, 3};
    size_t   i;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    void    *data      = NULL;

    TESTING("small write to dataset with H5S_ALL");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_WRITE_TEST_ALL_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SMALL_WRITE_TEST_ALL_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_ALL_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_SMALL_WRITE_TEST_ALL_DSET_NAME,
                              DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SMALL_WRITE_TEST_ALL_DSET_NAME);
        goto error;
    }

    /* Close the dataset and dataspace to ensure that writing works correctly in this manner */
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dopen2(group_id, DATASET_SMALL_WRITE_TEST_ALL_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_SMALL_WRITE_TEST_ALL_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL == (data = malloc((hsize_t)space_npoints * DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPESIZE)))
        TEST_ERROR;

    for (i = 0; i < (hsize_t)space_npoints; i++)
        ((int *)data)[i] = (int)i;

    if (H5Dwrite(dset_id, DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_SMALL_WRITE_TEST_ALL_DSET_NAME);
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

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
        if (data)
            free(data);
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
 * A test to check that a small write can be made
 * to a dataset using a hyperslab selection.
 */
static int
test_write_dataset_small_hyperslab(void)
{
    hsize_t start[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t stride[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t count[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t block[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t dims[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK] = {10, 5, 3};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID, fspace_id = H5I_INVALID_HID;
    void   *data = NULL;

    TESTING("small write to dataset with a hyperslab selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_WRITE_TEST_HYPERSLAB_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_WRITE_TEST_HYPERSLAB_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK - 1, dims, NULL)) <
        0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_NAME,
                              DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK - 1; i++)
        data_size *= dims[i];
    data_size *= DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPESIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPESIZE; i++)
        ((int *)data)[i] = (int)i;

    for (i = 0; i < DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK; i++) {
        start[i]  = 0;
        stride[i] = 1;
        count[i]  = dims[i];
        block[i]  = 1;
    }

    count[2] = 1;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    if (H5Dwrite(dset_id, DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                 data) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_NAME);
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
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
        if (data)
            free(data);
        H5Sclose(mspace_id);
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
 * A test to check that a small write can be made
 * to a dataset using a point selection.
 */
static int
test_write_dataset_small_point_selection(void)
{
    hsize_t points[DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS *
                   DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK];
    hsize_t dims[DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t mdims[] = {DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS};
    size_t  i, data_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    void   *data      = NULL;

    TESTING("small write to dataset with a point selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_WRITE_TEST_POINT_SELECTION_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_WRITE_TEST_POINT_SELECTION_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK, dims, NULL)) <
        0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_NAME,
                              DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_NAME);
        goto error;
    }

    data_size = DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS *
                DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPESIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPESIZE; i++)
        ((int *)data)[i] = (int)i;

    for (i = 0; i < DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS; i++) {
        size_t j;

        for (j = 0; j < DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK; j++)
            points[(i * DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK) + j] = i;
    }

    if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS,
                           points) < 0) {
        H5_FAILED();
        printf("    couldn't select points\n");
        goto error;
    }

    if (H5Dwrite(dset_id, DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPE, mspace_id, fspace_id,
                 H5P_DEFAULT, data) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_NAME);
        goto error;
    }

    if (data) {
        free(data);
        data = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
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
        if (data)
            free(data);
        H5Sclose(mspace_id);
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
 * A test to ensure that data is read back correctly from
 * a dataset after it has been written.
 */
static int
test_write_dataset_data_verification(void)
{
    hssize_t space_npoints;
    hsize_t  dims[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t  start[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  stride[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  block[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t
           points[DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS * DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    size_t i, data_size;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  dset_id   = H5I_INVALID_HID;
    hid_t  fspace_id = H5I_INVALID_HID;
    hid_t  mspace_id = H5I_INVALID_HID;
    void  *data      = NULL;
    void  *write_buf = NULL;
    void  *read_buf  = NULL;

    TESTING_MULTIPART("verification of dataset data using H5Dwrite then H5Dread");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_DATA_VERIFY_WRITE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME,
                              DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE; i++)
        ((int *)data)[i] = (int)i;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dwrite_all_read)
        {
            TESTING_2("H5Dwrite using H5S_ALL then H5Dread");

            if (H5Dwrite(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                         data) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if (data) {
                free(data);
                data = NULL;
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (NULL ==
                (data = malloc((hsize_t)space_npoints * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            for (i = 0; i < (hsize_t)space_npoints; i++)
                if (((int *)data)[i] != (int)i) {
                    H5_FAILED();
                    printf("    H5S_ALL selection data verification failed\n");
                    PART_ERROR(H5Dwrite_all_read);
                }

            if (data) {
                free(data);
                data = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_all_read);

        PART_BEGIN(H5Dwrite_hyperslab_read)
        {
            TESTING_2("H5Dwrite using hyperslab selection then H5Dread");

            data_size = dims[1] * 2 * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < data_size / DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE; i++)
                ((int *)write_buf)[i] = 56;

            for (i = 0, data_size = 1; i < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

            if (NULL == (data = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset data verification\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < 2; i++) {
                size_t j;

                for (j = 0; j < dims[1]; j++)
                    ((int *)data)[(i * dims[1] * dims[2]) + (j * dims[2])] = 56;
            }

            /* Write to first two rows of dataset */
            start[0] = start[1] = start[2] = 0;
            stride[0] = stride[1] = stride[2] = 1;
            count[0]                          = 2;
            count[1]                          = dims[1];
            count[2]                          = 1;
            block[0] = block[1] = block[2] = 1;

            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            {
                hsize_t mdims[] = {(hsize_t)2 * dims[1]};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_hyperslab_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (NULL ==
                (read_buf = malloc((hsize_t)space_npoints * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (memcmp(data, read_buf, data_size)) {
                H5_FAILED();
                printf("    hyperslab selection data verification failed\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (data) {
                free(data);
                data = NULL;
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }

            if (read_buf) {
                free(read_buf);
                read_buf = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_hyperslab_read);

        PART_BEGIN(H5Dwrite_point_sel_read)
        {
            TESTING_2("H5Dwrite using point selection then H5Dread");

            data_size =
                DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < data_size / DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE; i++)
                ((int *)write_buf)[i] = 13;

            for (i = 0, data_size = 1; i < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

            if (NULL == (data = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset data verification\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < dims[0]; i++) {
                size_t j;

                for (j = 0; j < dims[1]; j++) {
                    size_t k;

                    for (k = 0; k < dims[2]; k++) {
                        if (i == j && j == k)
                            ((int *)data)[(i * dims[1] * dims[2]) + (j * dims[2]) + k] = 13;
                    }
                }
            }

            /* Select a series of 10 points in the dataset */
            for (i = 0; i < DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS; i++) {
                size_t j;

                for (j = 0; j < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; j++)
                    points[(i * DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK) + j] = i;
            }

            if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS,
                                   points) < 0) {
                H5_FAILED();
                printf("    couldn't select elements in dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            {
                hsize_t mdims[] = {(hsize_t)DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_point_sel_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (NULL ==
                (read_buf = malloc((hsize_t)space_npoints * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (memcmp(data, read_buf, data_size)) {
                H5_FAILED();
                printf("    point selection data verification failed\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            PASSED();
        }
        PART_END(H5Dwrite_point_sel_read);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (data) {
        free(data);
        data = NULL;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (data)
            free(data);
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
        H5Sclose(mspace_id);
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
 * A test to check that a small multi write can be
 * made to a dataset using an H5S_ALL selection.
 */
static int
test_write_multi_dataset_small_all(void)
{
    hssize_t    space_npoints;
    hsize_t     dims[DATASET_SMALL_WRITE_TEST_ALL_DSET_SPACE_RANK] = {10, 5, 3};
    size_t      i;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       dset_id_arr[DATASET_MULTI_COUNT];
    hid_t       fspace_id = H5I_INVALID_HID, fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t       dtype_id_arr[DATASET_MULTI_COUNT];
    const void *write_buf[DATASET_MULTI_COUNT];
    void       *wbuf_temp[DATASET_MULTI_COUNT];

    TESTING("small multi write to datasets with H5S_ALL");

    /* Prevent uninitialized memory usage on test failure */
    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        dset_id_arr[i] = H5I_INVALID_HID;
        write_buf[i]   = NULL;
        wbuf_temp[i]   = NULL;
    }

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_WRITE_MULTI_TEST_ALL_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_WRITE_MULTI_TEST_ALL_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_ALL_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_WRITE_MULTI_TEST_ALL_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] = H5Dcreate2(group_id, dset_name, DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPE,
                                         fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", DATASET_SMALL_WRITE_MULTI_TEST_ALL_DSET_NAME);
            goto error;
        }

        /* Close the dataset and dataspace to ensure that writing works correctly in this manner */
        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_WRITE_MULTI_TEST_ALL_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] = H5Dopen2(group_id, dset_name, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n", dset_name);
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id_arr[i])) < 0) {
            H5_FAILED();
            printf("    couldn't get dataset dataspace\n");
            goto error;
        }

        if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
            H5_FAILED();
            printf("    couldn't get dataspace num points\n");
            goto error;
        }

        dtype_id_arr[i]  = DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPE;
        fspace_id_arr[i] = H5S_ALL;

        if (NULL ==
            (wbuf_temp[i] = malloc((hsize_t)space_npoints * DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPESIZE)))
            TEST_ERROR;

        for (size_t j = 0; j < (size_t)space_npoints; j++)
            ((int **)wbuf_temp)[i][j] = (int)i;

        write_buf[i] = wbuf_temp[i];
    }

    if (H5Dwrite_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, fspace_id_arr, fspace_id_arr,
                       H5P_DEFAULT, write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_SMALL_WRITE_MULTI_TEST_ALL_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        free(wbuf_temp[i]);
        wbuf_temp[i] = NULL;
        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }
    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (wbuf_temp[i])
                free(wbuf_temp[i]);
            H5Dclose(dset_id_arr[i]);
        }

        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that a small multi write can be made
 * to a dataset using a hyperslab selection.
 */
static int
test_write_multi_dataset_small_hyperslab(void)
{
    hsize_t     start[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t     stride[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t     count[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t     block[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK];
    hsize_t     dims[DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK] = {10, 5, 3};
    size_t      i, data_size;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       dset_id_arr[DATASET_MULTI_COUNT];
    hid_t       mspace_id = H5I_INVALID_HID, fspace_id = H5I_INVALID_HID;
    hid_t       mspace_id_arr[DATASET_MULTI_COUNT], fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t       dtype_id_arr[DATASET_MULTI_COUNT];
    const void *write_buf[DATASET_MULTI_COUNT];
    void       *wbuf_temp[DATASET_MULTI_COUNT];

    TESTING("small multi write to datasets with hyperslab selections");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        dset_id_arr[i] = H5I_INVALID_HID;
        write_buf[i]   = NULL;
        wbuf_temp[i]   = NULL;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_WRITE_MULTI_TEST_HYPERSLAB_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_WRITE_MULTI_TEST_HYPERSLAB_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK - 1, dims, NULL)) <
        0)
        TEST_ERROR;

    for (i = 0, data_size = 1; i < DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK - 1; i++)
        data_size *= dims[i];
    data_size *= DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPESIZE;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_WRITE_MULTI_TEST_HYPERSLAB_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] = H5Dcreate2(group_id, dset_name, DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPE,
                                         fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", dset_name);
            goto error;
        }

        if (NULL == (wbuf_temp[i] = malloc(data_size)))
            TEST_ERROR;

        for (size_t j = 0; j < data_size / DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPESIZE; j++)
            ((int **)wbuf_temp)[i][j] = (int)i;

        write_buf[i] = (const void *)wbuf_temp[i];
    }

    for (i = 0; i < DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK; i++) {
        start[i]  = 0;
        stride[i] = 1;
        count[i]  = dims[i];
        block[i]  = 1;
    }

    count[2] = 1;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        dtype_id_arr[i]  = DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPE;
        mspace_id_arr[i] = mspace_id;
        fspace_id_arr[i] = fspace_id;
    }

    if (H5Dwrite_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, mspace_id_arr, fspace_id_arr,
                       H5P_DEFAULT, (const void **)write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_SMALL_WRITE_MULTI_TEST_HYPERSLAB_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        if (wbuf_temp[i]) {
            free(wbuf_temp[i]);
            wbuf_temp[i] = NULL;
        }
        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (wbuf_temp[i]) {
                free(wbuf_temp[i]);
                wbuf_temp[i] = NULL;
            }
            H5Dclose(dset_id_arr[i]);
        }
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that a small multi write can be made
 * to a dataset using a point selection.
 */
static int
test_write_multi_dataset_small_point_selection(void)
{
    hsize_t     points[DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS *
                   DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK];
    hsize_t     dims[DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t     mdims[] = {DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS};
    size_t      i, data_size;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       dset_id_arr[DATASET_MULTI_COUNT];
    hid_t       fspace_id = H5I_INVALID_HID, fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t       mspace_id = H5I_INVALID_HID, mspace_id_arr[DATASET_MULTI_COUNT];
    hid_t       dtype_id_arr[DATASET_MULTI_COUNT];
    const void *write_buf[DATASET_MULTI_COUNT];
    void       *wbuf_temp[DATASET_MULTI_COUNT];

    TESTING("small multi write to datasets with point selections");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        write_buf[i]   = NULL;
        wbuf_temp[i]   = NULL;
        dset_id_arr[i] = H5I_INVALID_HID;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SMALL_WRITE_MULTI_TEST_POINT_SELECTION_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SMALL_WRITE_MULTI_TEST_POINT_SELECTION_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK, dims, NULL)) <
        0)
        TEST_ERROR;
    if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0)
        TEST_ERROR;

    data_size = DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS *
                DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPESIZE;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        char dset_name[DSET_NAME_BUF_SIZE];

        if (DSET_NAME_BUF_SIZE <= snprintf(dset_name, DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_SMALL_WRITE_MULTI_TEST_POINT_SELECTION_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] =
                 H5Dcreate2(group_id, dset_name, DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPE,
                            fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", dset_name);
            goto error;
        }

        if (NULL == (wbuf_temp[i] = malloc(data_size)))
            TEST_ERROR;

        for (size_t j = 0; j < data_size / DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPESIZE; j++)
            ((int **)wbuf_temp)[i][j] = (int)i;

        write_buf[i] = (const void *)wbuf_temp[i];
    }

    for (i = 0; i < DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS; i++) {
        size_t j;

        for (j = 0; j < DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK; j++)
            points[(i * DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK) + j] = i;
    }

    if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS,
                           points) < 0) {
        H5_FAILED();
        printf("    couldn't select points\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        dtype_id_arr[i]  = DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPE;
        mspace_id_arr[i] = mspace_id;
        fspace_id_arr[i] = fspace_id;
    }

    if (H5Dwrite_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, mspace_id_arr, fspace_id_arr,
                       H5P_DEFAULT, (const void **)write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to multiple datasets\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        if (wbuf_temp[i]) {
            free(wbuf_temp[i]);
            wbuf_temp[i] = NULL;
        }

        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (wbuf_temp[i])
                free(wbuf_temp[i]);

            H5Dclose(dset_id_arr[i]);
        }

        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to ensure that data is read back correctly from
 * multiple datasets after it has been written.
 */
static int
test_write_multi_dataset_data_verification(void)
{
    hssize_t space_npoints[DATASET_MULTI_COUNT];
    hsize_t  dims[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t  start[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  stride[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  block[DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t
           points[DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS * DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK];
    size_t i, data_size;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  dset_id_arr[DATASET_MULTI_COUNT];
    hid_t  dtype_id_arr[DATASET_MULTI_COUNT];
    hid_t  fspace_id = H5I_INVALID_HID, fspace_id_arr[DATASET_MULTI_COUNT];
    hid_t  mspace_id = H5I_INVALID_HID, mspace_id_arr[DATASET_MULTI_COUNT];
    hid_t  select_all_arr[DATASET_MULTI_COUNT];
    void  *data[DATASET_MULTI_COUNT];
    const void *write_buf[DATASET_MULTI_COUNT];
    void       *wbuf_temp[DATASET_MULTI_COUNT];
    void       *read_buf[DATASET_MULTI_COUNT];
    char        dset_names[DATASET_MULTI_COUNT][DSET_NAME_BUF_SIZE];

    TESTING_MULTIPART("verification of datasets' data using H5Dwrite_multi then H5Dread_multi");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        select_all_arr[i] = H5S_ALL;
        read_buf[i]       = NULL;
        write_buf[i]      = NULL;
        wbuf_temp[i]      = NULL;
        data[i]           = NULL;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_DATA_VERIFY_WRITE_MULTI_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_DATA_VERIFY_WRITE_MULTI_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    for (i = 0, data_size = 1; i < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        if (DSET_NAME_BUF_SIZE <= snprintf(dset_names[i], DSET_NAME_BUF_SIZE, "%s%zu%c",
                                           DATASET_DATA_VERIFY_WRITE_MULTI_TEST_DSET_NAME, i, '\0'))
            TEST_ERROR;

        if ((dset_id_arr[i] = H5Dcreate2(group_id, dset_names[i], DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE,
                                         fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create dataset '%s'\n", dset_names[i]);
            goto error;
        }

        if (NULL == (data[i] = malloc(data_size)))
            TEST_ERROR;

        dtype_id_arr[i] = DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE;

        for (size_t j = 0; j < data_size / DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE; j++)
            ((int **)data)[i][j] = (int)j;

        write_buf[i] = (const void *)data[i];
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dwrite_multi_all_read)
        {
            TESTING_2("H5Dwrite_multi using H5S_ALL then H5Dread_multi");

            if (H5Dwrite_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, select_all_arr, select_all_arr,
                               H5P_DEFAULT, (const void **)write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to datasets");
                PART_ERROR(H5Dwrite_multi_all_read);
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (data[i]) {
                    free(data[i]);
                    data[i] = NULL;
                }

                if (dset_id_arr[i] >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id_arr[i]);
                    }
                    H5E_END_TRY;
                    dset_id_arr[i] = H5I_INVALID_HID;
                }

                if ((dset_id_arr[i] = H5Dopen2(group_id, dset_names[i], H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", dset_names[i]);
                    PART_ERROR(H5Dwrite_multi_all_read);
                }
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY;
                fspace_id = H5I_INVALID_HID;
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if ((fspace_id_arr[i] = H5Dget_space(dset_id_arr[i])) < 0) {
                    H5_FAILED();
                    printf("    couldn't get dataset dataspace\n");
                    PART_ERROR(H5Dwrite_multi_all_read);
                }

                if ((space_npoints[i] = H5Sget_simple_extent_npoints(fspace_id_arr[i])) < 0) {
                    H5_FAILED();
                    printf("    couldn't get dataspace num points\n");
                    PART_ERROR(H5Dwrite_multi_all_read);
                }

                if (NULL == (data[i] = malloc((hsize_t)space_npoints[i] *
                                              DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for dataset read\n");
                    PART_ERROR(H5Dwrite_multi_all_read);
                }
            }

            if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, select_all_arr, select_all_arr,
                              H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't read from datasets\n");
                PART_ERROR(H5Dwrite_multi_all_read);
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                for (size_t j = 0; j < (hsize_t)space_npoints[i]; j++)
                    if (((int **)data)[i][j] != (int)j) {
                        H5_FAILED();
                        printf("    H5S_ALL selection data verification failed\n");
                        PART_ERROR(H5Dwrite_multi_all_read);
                    }

                if (data[i]) {
                    free(data[i]);
                    data[i] = NULL;
                }
            }

            PASSED();
        }
        PART_END(H5Dwrite_multi_all_read);

        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (data[i]) {
                free(data[i]);
                data[i] = NULL;
            }
        }

        PART_BEGIN(H5Dwrite_multi_hyperslab_read)
        {
            TESTING_2("H5Dwrite_multi using hyperslab selection then H5Dread_multi");

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                data_size = dims[1] * 2 * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

                if (NULL == (wbuf_temp[i] = malloc(data_size))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for dataset write\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                for (size_t j = 0; j < data_size / DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE; j++) {
                    ((int *)wbuf_temp[i])[j] = 56;
                }

                data_size = 1;
                for (size_t j = 0; j < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; j++)
                    data_size *= dims[j];
                data_size *= DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

                if (NULL == (data[i] = malloc(data_size))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for datasets' data verification\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                write_buf[i] = (const void *)wbuf_temp[i];
            }

            if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, select_all_arr, select_all_arr,
                              H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't read from datasets\n");
                PART_ERROR(H5Dwrite_multi_hyperslab_read);
            }

            /* Reference data for verification */
            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                for (size_t j = 0; j < 2; j++) {
                    size_t k;

                    for (k = 0; k < dims[1]; k++) {
                        size_t index             = (j * dims[1] * dims[2]) + (k * dims[2]);
                        ((int **)data)[i][index] = (int)56;
                    }
                }
            }

            /* Write to first two rows of dataset */
            start[0] = start[1] = start[2] = 0;
            stride[0] = stride[1] = stride[2] = 1;
            count[0]                          = 2;
            count[1]                          = dims[1];
            count[2]                          = 1;
            block[0] = block[1] = block[2] = 1;

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (H5Sselect_hyperslab(fspace_id_arr[i], H5S_SELECT_SET, start, stride, count, block) < 0) {
                    H5_FAILED();
                    printf("    couldn't select hyperslab for dataset write\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                {
                    hsize_t mdims[] = {(hsize_t)2 * dims[1]};

                    if ((mspace_id_arr[i] = H5Screate_simple(1, mdims, NULL)) < 0) {
                        H5_FAILED();
                        printf("    couldn't create memory dataspace\n");
                        PART_ERROR(H5Dwrite_multi_hyperslab_read);
                    }
                }
            }

            if (H5Dwrite_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, mspace_id_arr, fspace_id_arr,
                               H5P_DEFAULT, (const void **)write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to datasets\n");
                PART_ERROR(H5Dwrite_multi_hyperslab_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY;
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY;
                fspace_id = H5I_INVALID_HID;
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (dset_id_arr[i] >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id_arr[i]);
                    }
                    H5E_END_TRY;
                    dset_id_arr[i] = H5I_INVALID_HID;
                }

                if ((dset_id_arr[i] = H5Dopen2(group_id, dset_names[i], H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", dset_names[i]);
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                if ((fspace_id_arr[i] = H5Dget_space(dset_id_arr[i])) < 0) {
                    H5_FAILED();
                    printf("    couldn't get dataset dataspace\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                if ((space_npoints[i] = H5Sget_simple_extent_npoints(fspace_id_arr[i])) < 0) {
                    H5_FAILED();
                    printf("    couldn't get dataspace num points\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                if (NULL == (read_buf[i] = malloc((hsize_t)space_npoints[i] *
                                                  DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for dataset read\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }
            }

            if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, select_all_arr, select_all_arr,
                              H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from datasets\n");
                PART_ERROR(H5Dwrite_multi_hyperslab_read);
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (memcmp(data[i], read_buf[i], data_size)) {
                    H5_FAILED();
                    printf("    hyperslab selection data verification failed\n");
                    PART_ERROR(H5Dwrite_multi_hyperslab_read);
                }

                if (data[i]) {
                    free(data[i]);
                    data[i] = NULL;
                }

                if (wbuf_temp[i]) {
                    free(wbuf_temp[i]);
                    wbuf_temp[i] = NULL;
                }

                if (read_buf[i]) {
                    free(read_buf[i]);
                    read_buf[i] = NULL;
                }
            }

            PASSED();
        }
        PART_END(H5Dwrite_multi_hyperslab_read);

        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (wbuf_temp[i]) {
                free(wbuf_temp[i]);
                wbuf_temp[i] = NULL;
            }

            if (read_buf[i]) {
                free(read_buf[i]);
                read_buf[i] = NULL;
            }
        }

        PART_BEGIN(H5Dwrite_multi_point_sel_read)
        {
            TESTING_2("H5Dwrite_multi using point selection then H5Dread_multi");

            data_size =
                DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS * DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (NULL == (wbuf_temp[i] = malloc(data_size))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for dataset write\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }

                for (size_t j = 0; j < data_size / DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE; j++)
                    ((int **)wbuf_temp)[i][j] = 13;

                write_buf[i] = (const void *)wbuf_temp[i];

                data_size = 1;

                for (size_t j = 0; j < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; j++)
                    data_size *= dims[j];
                data_size *= DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE;

                if (NULL == (data[i] = malloc(data_size))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for dataset data verification\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }
            }

            if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, select_all_arr, select_all_arr,
                              H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_VERIFY_WRITE_MULTI_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_multi_point_sel_read);
            }

            /* Select a series of 10 points in the dataset */
            for (i = 0; i < DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS; i++) {
                size_t j;

                for (j = 0; j < DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK; j++)
                    points[(i * DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK) + j] = i;
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (H5Sselect_elements(fspace_id_arr[i], H5S_SELECT_SET,
                                       DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS, points) < 0) {
                    H5_FAILED();
                    printf("    couldn't select elements in dataspace\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }

                {
                    hsize_t mdims[] = {(hsize_t)DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS};

                    if ((mspace_id_arr[i] = H5Screate_simple(1, mdims, NULL)) < 0) {
                        H5_FAILED();
                        printf("    couldn't create memory dataspace\n");
                        PART_ERROR(H5Dwrite_multi_point_sel_read);
                    }
                }
            }

            if (H5Dwrite_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, mspace_id_arr, fspace_id_arr,
                               H5P_DEFAULT, (const void **)write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to datasets\n");
                PART_ERROR(H5Dwrite_multi_point_sel_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY;
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY;
                fspace_id = H5I_INVALID_HID;
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                if (dset_id_arr[i] >= 0) {
                    H5E_BEGIN_TRY
                    {
                        H5Dclose(dset_id_arr[i]);
                    }
                    H5E_END_TRY;
                    dset_id_arr[i] = H5I_INVALID_HID;
                }

                if ((dset_id_arr[i] = H5Dopen2(group_id, dset_names[i], H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    couldn't open dataset '%s'\n", dset_names[i]);
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }

                if ((fspace_id = H5Dget_space(dset_id_arr[i])) < 0) {
                    H5_FAILED();
                    printf("    couldn't get dataset dataspace\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }

                if ((space_npoints[i] = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                    H5_FAILED();
                    printf("    couldn't get dataspace num points\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }

                if (NULL == (read_buf[i] = malloc((hsize_t)space_npoints[i] *
                                                  DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE))) {
                    H5_FAILED();
                    printf("    couldn't allocate buffer for dataset read\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }
            }

            if (H5Dread_multi(DATASET_MULTI_COUNT, dset_id_arr, dtype_id_arr, select_all_arr, select_all_arr,
                              H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_VERIFY_WRITE_MULTI_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_multi_point_sel_read);
            }

            for (i = 0; i < DATASET_MULTI_COUNT; i++) {
                for (size_t j = 0; j < dims[0]; j++) {
                    size_t k;

                    for (k = 0; k < dims[1]; k++) {
                        size_t l;

                        for (l = 0; l < dims[2]; l++) {
                            if (j == k && k == l)
                                ((int **)data)[i][(j * dims[1] * dims[2]) + (k * dims[2]) + l] = 13;
                        }
                    }
                }

                if (memcmp(data[i], read_buf[i], data_size)) {
                    H5_FAILED();
                    printf("    point selection data verification failed\n");
                    PART_ERROR(H5Dwrite_multi_point_sel_read);
                }
            }

            PASSED();
        }
        PART_END(H5Dwrite_multi_point_sel_read);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    for (i = 0; i < DATASET_MULTI_COUNT; i++) {
        if (data[i]) {
            free(data[i]);
            data[i] = NULL;
        }

        if (wbuf_temp[i]) {
            free(wbuf_temp[i]);
            wbuf_temp[i] = NULL;
        }

        if (read_buf[i]) {
            free(read_buf[i]);
            read_buf[i] = NULL;
        }

        if (H5Dclose(dset_id_arr[i]) < 0)
            TEST_ERROR;
    }

    if (H5Sclose(fspace_id) < 0)
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
        for (i = 0; i < DATASET_MULTI_COUNT; i++) {
            if (data[i])
                free(data[i]);
            if (wbuf_temp[i])
                free(wbuf_temp[i]);
            if (read_buf[i])
                free(read_buf[i]);

            H5Dclose(dset_id_arr[i]);
        }

        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to check that a dataset can't be written to
 * when H5Dwrite is passed invalid parameters.
 */
static int
test_write_dataset_invalid_params(void)
{
    hssize_t space_npoints;
    hsize_t  dims[DATASET_WRITE_INVALID_PARAMS_TEST_DSET_SPACE_RANK] = {10, 5, 3};
    herr_t   err_ret                                                 = -1;
    size_t   i;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    void    *data      = NULL;

    TESTING_MULTIPART("H5Dwrite with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_INVALID_PARAMS_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_INVALID_PARAMS_TEST_DSET_NAME,
                              DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_INVALID_PARAMS_TEST_DSET_NAME);
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL == (data = malloc((hsize_t)space_npoints * DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPESIZE)))
        TEST_ERROR;

    for (i = 0; i < (hsize_t)space_npoints; i++)
        ((int *)data)[i] = (int)i;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dwrite_invalid_dset_id)
        {
            TESTING_2("H5Dwrite with an invalid dataset ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dwrite(H5I_INVALID_HID, DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL,
                                   H5S_ALL, H5P_DEFAULT, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to dataset using H5Dwrite with an invalid dataset ID!\n");
                PART_ERROR(H5Dwrite_invalid_dset_id);
            }

            PASSED();
        }
        PART_END(H5Dwrite_invalid_dset_id);

        PART_BEGIN(H5Dwrite_invalid_datatype)
        {
            TESTING_2("H5Dwrite with an invalid memory datatype");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dwrite(dset_id, H5I_INVALID_HID, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to dataset using H5Dwrite with an invalid memory datatype!\n");
                PART_ERROR(H5Dwrite_invalid_datatype);
            }

            PASSED();
        }
        PART_END(H5Dwrite_invalid_datatype);

        PART_BEGIN(H5Dwrite_invalid_mem_dataspace)
        {
            TESTING_2("H5Dwrite with an invalid memory dataspace");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dwrite(dset_id, DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPE, H5I_INVALID_HID,
                                   H5S_ALL, H5P_DEFAULT, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to dataset using H5Dwrite with an invalid memory dataspace!\n");
                PART_ERROR(H5Dwrite_invalid_mem_dataspace);
            }

            PASSED();
        }
        PART_END(H5Dwrite_invalid_mem_dataspace);

        PART_BEGIN(H5Dwrite_invalid_file_dataspace)
        {
            TESTING_2("H5Dwrite with an invalid file dataspace");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dwrite(dset_id, DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL,
                                   H5I_INVALID_HID, H5P_DEFAULT, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to dataset using H5Dwrite with an invalid file dataspace!\n");
                PART_ERROR(H5Dwrite_invalid_file_dataspace);
            }

            PASSED();
        }
        PART_END(H5Dwrite_invalid_file_dataspace);

        PART_BEGIN(H5Dwrite_invalid_dxpl)
        {
            TESTING_2("H5Dwrite with an invalid DXPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dwrite(dset_id, DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                                   H5I_INVALID_HID, data);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to dataset using H5Dwrite with an invalid DXPL!\n");
                PART_ERROR(H5Dwrite_invalid_dxpl);
            }

            PASSED();
        }
        PART_END(H5Dwrite_invalid_dxpl);

        PART_BEGIN(H5Dwrite_invalid_data_buf)
        {
            TESTING_2("H5Dwrite with an invalid data buffer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dwrite(dset_id, DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                                   H5P_DEFAULT, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    wrote to dataset using H5Dwrite with an invalid data buffer!\n");
                PART_ERROR(H5Dwrite_invalid_data_buf);
            }

            PASSED();
        }
        PART_END(H5Dwrite_invalid_data_buf);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (data) {
        free(data);
        data = NULL;
    }

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
        if (data)
            free(data);
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
 * A test to ensure that strings of any encoding
 * can be written to and read from a dataset
 */
static int
test_dataset_string_encodings(void)
{
    hid_t   file_id                             = H5I_INVALID_HID;
    hid_t   container_group                     = H5I_INVALID_HID;
    hid_t   dset_id1                            = H5I_INVALID_HID;
    hid_t   dset_id2                            = H5I_INVALID_HID;
    hid_t   type_id1                            = H5I_INVALID_HID;
    hid_t   type_id2                            = H5I_INVALID_HID;
    hid_t   space_id                            = H5I_INVALID_HID;
    hsize_t dims[DATASET_STRING_ENCODINGS_RANK] = {DATASET_STRING_ENCODINGS_EXTENT};
    size_t  ascii_str_size                      = 0;
    size_t  utf8_str_size                       = 0;
    char   *write_buf                           = NULL;
    char   *read_buf                            = NULL;

    TESTING_MULTIPART("string encoding read/write correctness on datasets");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    ascii_str_size = strlen(DATASET_STRING_ENCODINGS_ASCII_STRING);
    utf8_str_size  = strlen(DATASET_STRING_ENCODINGS_UTF8_STRING);

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = H5Screate_simple(DATASET_STRING_ENCODINGS_RANK, dims, NULL)) < 0) {
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

    if ((dset_id1 = H5Dcreate(container_group, DATASET_STRING_ENCODINGS_DSET_NAME1, type_id1, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with ascii string\n");
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

    if ((dset_id2 = H5Dcreate(container_group, DATASET_STRING_ENCODINGS_DSET_NAME2, type_id2, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with UTF-8 string\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(ASCII_cset)
        {
            TESTING_2("ASCII character set");
            /* Dataset with ASCII string datatype */
            if ((write_buf = calloc(1, ascii_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for write buffer\n");
                PART_ERROR(ASCII_cset);
            }

            memcpy(write_buf, DATASET_STRING_ENCODINGS_ASCII_STRING, ascii_str_size);

            if ((H5Dwrite(dset_id1, type_id1, H5S_ALL, H5S_ALL, H5P_DEFAULT, write_buf)) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset with ASCII string\n");
                PART_ERROR(ASCII_cset);
            }

            if ((read_buf = calloc(1, ascii_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer\n");
                PART_ERROR(ASCII_cset);
            }

            if ((H5Dread(dset_id1, type_id1, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf)) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset with ASCII string\n");
                PART_ERROR(ASCII_cset);
            }

            if (strncmp(write_buf, read_buf, ascii_str_size)) {
                H5_FAILED();
                printf("    incorrect data read from dataset with ASCII string\n");
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
            /* Dataset with UTF-8 string datatype */
            if ((write_buf = calloc(1, utf8_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for write buffer\n");
                PART_ERROR(UTF8_cset);
            }

            memcpy(write_buf, DATASET_STRING_ENCODINGS_UTF8_STRING, utf8_str_size);

            if ((H5Dwrite(dset_id2, type_id2, H5S_ALL, H5S_ALL, H5P_DEFAULT, write_buf)) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset with ASCII string\n");
                PART_ERROR(UTF8_cset);
            }

            if ((read_buf = calloc(1, utf8_str_size + 1)) == NULL) {
                H5_FAILED();
                printf("    couldn't allocate memory for read buffer\n");
                PART_ERROR(UTF8_cset);
            }

            if ((H5Dread(dset_id2, type_id2, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf)) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset with ASCII string\n");
                PART_ERROR(UTF8_cset);
            }

            if (strncmp(write_buf, read_buf, utf8_str_size)) {
                H5_FAILED();
                printf("    incorrect data read from dataset with ASCII string\n");
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
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to ensure that data is read back correctly from a dataset after it has
 * been written, using type conversion with builtin types.
 */
static int
test_dataset_builtin_type_conversion(void)
{
    hssize_t    space_npoints;
    hsize_t     dims[DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t     start[DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t     stride[DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t     count[DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t     block[DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t     points[DATASET_DATA_BUILTIN_CONVERSION_TEST_NUM_POINTS *
                   DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK];
    size_t      i, data_size;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       dset_id      = H5I_INVALID_HID;
    hid_t       fspace_id    = H5I_INVALID_HID;
    hid_t       mspace_id    = H5I_INVALID_HID;
    hid_t       file_type_id = H5I_INVALID_HID;
    H5T_order_t native_order;
    void       *data      = NULL;
    void       *write_buf = NULL;
    void       *read_buf  = NULL;

    TESTING_MULTIPART(
        "verification of dataset data using H5Dwrite then H5Dread with type conversion of builtin types");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((native_order = H5Tget_order(DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE)) < 0) {
        H5_FAILED();
        printf("    couldn't get native byte order\n");
        goto error;
    }
    if (native_order == H5T_ORDER_LE)
        file_type_id = H5T_STD_I32BE;
    else
        file_type_id = H5T_STD_I32LE;

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_DATA_BUILTIN_CONVERSION_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_DATA_VERIFY_WRITE_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME, file_type_id,
                              fspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE; i++)
        ((int *)data)[i] = (int)i;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dwrite_all_read)
        {
            TESTING_2("H5Dwrite then H5Dread with H5S_ALL selection");

            if (H5Dwrite(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, H5S_ALL, H5S_ALL,
                         H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if (data) {
                free(data);
                data = NULL;
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (NULL == (data = malloc((hsize_t)space_npoints *
                                       DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, H5S_ALL, H5S_ALL,
                        H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            for (i = 0; i < (hsize_t)space_npoints; i++)
                if (((int *)data)[i] != (int)i) {
                    H5_FAILED();
                    printf("    H5S_ALL selection data verification failed\n");
                    PART_ERROR(H5Dwrite_all_read);
                }

            if (data) {
                free(data);
                data = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_all_read);

        PART_BEGIN(H5Dwrite_hyperslab_read)
        {
            TESTING_2("H5Dwrite using hyperslab selection then H5Dread");

            data_size = dims[1] * 2 * DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < data_size / DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE; i++)
                ((int *)write_buf)[i] = 56;

            for (i = 0, data_size = 1; i < DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE;

            if (NULL == (data = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset data verification\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, H5S_ALL, H5S_ALL,
                        H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < 2; i++) {
                size_t j;

                for (j = 0; j < dims[1]; j++)
                    ((int *)data)[(i * dims[1] * dims[2]) + (j * dims[2])] = 56;
            }

            /* Write to first two rows of dataset */
            start[0] = start[1] = start[2] = 0;
            stride[0] = stride[1] = stride[2] = 1;
            count[0]                          = 2;
            count[1]                          = dims[1];
            count[2]                          = 1;
            block[0] = block[1] = block[2] = 1;

            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            {
                hsize_t mdims[] = {(hsize_t)2 * dims[1]};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_hyperslab_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (NULL == (read_buf = malloc((hsize_t)space_npoints *
                                           DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, H5S_ALL, H5S_ALL,
                        H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (memcmp(data, read_buf, data_size)) {
                H5_FAILED();
                printf("    hyperslab selection data verification failed\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (data) {
                free(data);
                data = NULL;
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }

            if (read_buf) {
                free(read_buf);
                read_buf = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_hyperslab_read);

        PART_BEGIN(H5Dwrite_point_sel_read)
        {
            TESTING_2("H5Dwrite using point selection then H5Dread");

            data_size = DATASET_DATA_BUILTIN_CONVERSION_TEST_NUM_POINTS *
                        DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < data_size / DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE; i++)
                ((int *)write_buf)[i] = 13;

            for (i = 0, data_size = 1; i < DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE;

            if (NULL == (data = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset data verification\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, H5S_ALL, H5S_ALL,
                        H5P_DEFAULT, data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < dims[0]; i++) {
                size_t j;

                for (j = 0; j < dims[1]; j++) {
                    size_t k;

                    for (k = 0; k < dims[2]; k++) {
                        if (i == j && j == k)
                            ((int *)data)[(i * dims[1] * dims[2]) + (j * dims[2]) + k] = 13;
                    }
                }
            }

            /* Select a series of 10 points in the dataset */
            for (i = 0; i < DATASET_DATA_BUILTIN_CONVERSION_TEST_NUM_POINTS; i++) {
                size_t j;

                for (j = 0; j < DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK; j++)
                    points[(i * DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK) + j] = i;
            }

            if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_DATA_BUILTIN_CONVERSION_TEST_NUM_POINTS,
                                   points) < 0) {
                H5_FAILED();
                printf("    couldn't select elements in dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            {
                hsize_t mdims[] = {(hsize_t)DATASET_DATA_BUILTIN_CONVERSION_TEST_NUM_POINTS};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_point_sel_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (NULL == (read_buf = malloc((hsize_t)space_npoints *
                                           DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE, H5S_ALL, H5S_ALL,
                        H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (memcmp(data, read_buf, data_size)) {
                H5_FAILED();
                printf("    point selection data verification failed\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            PASSED();
        }
        PART_END(H5Dwrite_point_sel_read);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (data) {
        free(data);
        data = NULL;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (data)
            free(data);
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

static int
test_dataset_real_to_int_conversion(void)
{
    hssize_t space_npoints;
    hsize_t  dims[DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK] = {10, 10, 10};
    hsize_t  start[DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t  stride[DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t  block[DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK];
    hsize_t  points[DATASET_DATA_REAL_CONVERSION_TEST_NUM_POINTS *
                   DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id      = H5I_INVALID_HID;
    hid_t    fspace_id    = H5I_INVALID_HID;
    hid_t    mspace_id    = H5I_INVALID_HID;
    hid_t    real_type_id = DATASET_DATA_REAL_CONVERSION_TEST_REAL_TYPE;
    void    *data         = NULL;
    void    *write_buf    = NULL;
    void    *read_buf     = NULL;

    TESTING_MULTIPART(
        "verification of dataset data using H5Dwrite then H5Dread with real <-> integer type conversion");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_DATA_REAL_CONVERSION_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_DATA_REAL_CONVERSION_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME, real_type_id, fspace_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE;

    if (NULL == (data = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE; i++)
        ((int *)data)[i] = (int)i;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dwrite_all_read)
        {
            TESTING_2("write then read int from real dataset with H5S_ALL selection");

            if (H5Dwrite(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                         data) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if (data) {
                free(data);
                data = NULL;
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY;
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY;
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (NULL ==
                (data = malloc((hsize_t)space_npoints * DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            for (i = 0; i < (hsize_t)space_npoints; i++)
                if (((int *)data)[i] != (int)i) {
                    H5_FAILED();
                    printf("    H5S_ALL selection data verification failed\n");
                    PART_ERROR(H5Dwrite_all_read);
                }

            if (data) {
                free(data);
                data = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_all_read);

        if (data) {
            free(data);
            data = NULL;
        }

        if (write_buf) {
            free(write_buf);
            write_buf = NULL;
        }

        if (read_buf) {
            free(read_buf);
            read_buf = NULL;
        }

        PART_BEGIN(H5Dwrite_hyperslab_read)
        {
            TESTING_2("write then read int from real dataset with hyperslab selection");

            data_size = dims[1] * 2 * DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < data_size / DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE; i++)
                ((int *)write_buf)[i] = 56;

            for (i = 0, data_size = 1; i < DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE;

            if (NULL == (data = calloc(1, data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset data verification\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < dims[0] * dims[1] * dims[2]; i++)
                ((int *)data)[i] = (int)i;

            for (i = 0; i < 2; i++) {
                size_t j;

                for (j = 0; j < dims[1]; j++)
                    ((int *)data)[(i * dims[1] * dims[2]) + (j * dims[2])] = 56;
            }

            /* Write to first two rows of dataset */
            start[0] = start[1] = start[2] = 0;
            stride[0] = stride[1] = stride[2] = 1;
            count[0]                          = 2;
            count[1]                          = dims[1];
            count[2]                          = 1;
            block[0] = block[1] = block[2] = 1;

            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            {
                hsize_t mdims[] = {(hsize_t)2 * dims[1]};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_hyperslab_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY;
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY;
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY;
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (NULL == (read_buf = malloc((hsize_t)space_npoints *
                                           DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (memcmp(data, read_buf, data_size)) {
                H5_FAILED();
                printf("    hyperslab selection data verification failed\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (data) {
                free(data);
                data = NULL;
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }

            if (read_buf) {
                free(read_buf);
                read_buf = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_hyperslab_read);

        if (data) {
            free(data);
            data = NULL;
        }

        if (write_buf) {
            free(write_buf);
            write_buf = NULL;
        }

        if (read_buf) {
            free(read_buf);
            read_buf = NULL;
        }

        PART_BEGIN(H5Dwrite_point_sel_read)
        {
            TESTING_2("write then read int from real dataset with point selection");

            data_size = DATASET_DATA_REAL_CONVERSION_TEST_NUM_POINTS *
                        DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < data_size / DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE; i++)
                ((int *)write_buf)[i] = 13;

            for (i = 0, data_size = 1; i < DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE;

            if (NULL == (data = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset data verification\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        data) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < dims[0]; i++) {
                size_t j;

                for (j = 0; j < dims[1]; j++) {
                    size_t k;

                    for (k = 0; k < dims[2]; k++) {
                        if (i == j && j == k)
                            ((int *)data)[(i * dims[1] * dims[2]) + (j * dims[2]) + k] = 13;
                    }
                }
            }

            /* Select a series of 10 points in the dataset */
            for (i = 0; i < DATASET_DATA_REAL_CONVERSION_TEST_NUM_POINTS; i++) {
                size_t j;

                for (j = 0; j < DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK; j++)
                    points[(i * DATASET_DATA_REAL_CONVERSION_TEST_DSET_SPACE_RANK) + j] = i;
            }

            if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, DATASET_DATA_REAL_CONVERSION_TEST_NUM_POINTS,
                                   points) < 0) {
                H5_FAILED();
                printf("    couldn't select elements in dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            {
                hsize_t mdims[] = {(hsize_t)DATASET_DATA_REAL_CONVERSION_TEST_NUM_POINTS};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_point_sel_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY;
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY;
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY;
                dset_id = H5I_INVALID_HID;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (NULL == (read_buf = malloc((hsize_t)space_npoints *
                                           DATASET_DATA_REAL_CONVERSION_TEST_INT_DTYPESIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_DATA_REAL_CONVERSION_TEST_INT_TYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_DATA_REAL_CONVERSION_TEST_DSET_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (memcmp(data, read_buf, data_size)) {
                H5_FAILED();
                printf("    point selection data verification failed\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            PASSED();
        }
        PART_END(H5Dwrite_point_sel_read);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (data) {
        free(data);
        data = NULL;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

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
        if (data)
            free(data);
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test to ensure that data is read back correctly from a dataset after it has
 * been written, using partial element I/O with compound types
 */
typedef struct dataset_compount_partial_io_t {
    int a;
    int b;
} dataset_compount_partial_io_t;

static int
test_dataset_compound_partial_io(void)
{
    hsize_t                       dims[1] = {DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS};
    size_t                        i;
    hid_t                         file_id         = H5I_INVALID_HID;
    hid_t                         container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t                         dset_id      = H5I_INVALID_HID;
    hid_t                         space_id     = H5I_INVALID_HID;
    hid_t                         full_type_id = H5I_INVALID_HID;
    hid_t                         a_type_id    = H5I_INVALID_HID;
    hid_t                         b_type_id    = H5I_INVALID_HID;
    dataset_compount_partial_io_t wbuf[DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS];
    dataset_compount_partial_io_t rbuf[DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS];
    dataset_compount_partial_io_t fbuf[DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS];
    dataset_compount_partial_io_t erbuf[DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS];

    TESTING_MULTIPART("verification of dataset data using H5Dwrite then H5Dread with partial element "
                      "compound type I/O");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_DATA_COMPOUND_PARTIAL_IO_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_DATA_COMPOUND_PARTIAL_IO_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((full_type_id = H5Tcreate(H5T_COMPOUND, sizeof(dataset_compount_partial_io_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(full_type_id, "a", HOFFSET(dataset_compount_partial_io_t, a), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(full_type_id, "b", HOFFSET(dataset_compount_partial_io_t, b), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((a_type_id = H5Tcreate(H5T_COMPOUND, sizeof(dataset_compount_partial_io_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(a_type_id, "a", HOFFSET(dataset_compount_partial_io_t, a), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((b_type_id = H5Tcreate(H5T_COMPOUND, sizeof(dataset_compount_partial_io_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(b_type_id, "b", HOFFSET(dataset_compount_partial_io_t, b), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_DATA_COMPOUND_PARTIAL_IO_TEST_DSET_NAME, full_type_id,
                              space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_DATA_COMPOUND_PARTIAL_IO_TEST_DSET_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(write_full_read_full)
        {
            TESTING_2("H5Dwrite then H5Dread with all compound members");

            /* Initialize wbuf */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                wbuf[i].a = (int)(2 * i);
                wbuf[i].b = (int)(2 * i + 1);
            }

            /* Write data */
            if (H5Dwrite(dset_id, full_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(write_full_read_full);

            /* Update fbuf to match file state */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                fbuf[i].a = wbuf[i].a;
                fbuf[i].b = wbuf[i].b;
            }

            /* Initialize rbuf to -1 */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                rbuf[i].a = -1;
                rbuf[i].b = -1;
            }

            /* Set erbuf (simply match file state since we're reading the whole
             * thing) */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                erbuf[i].a = fbuf[i].a;
                erbuf[i].b = fbuf[i].b;
            }

            /* Read data */
            if (H5Dread(dset_id, full_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(write_full_read_full);

            /* Verify data */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
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
            TESTING_2("H5Dread with compound member a");

            /* Initialize rbuf to -1 */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                rbuf[i].a = -1;
                rbuf[i].b = -1;
            }

            /* Set erbuf (element a comes from the file, element b in untouched)
             */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                erbuf[i].a = fbuf[i].a;
                erbuf[i].b = rbuf[i].b;
            }

            /* Read data */
            if (H5Dread(dset_id, a_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(read_a);

            /* Verify data */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
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
            TESTING_2("H5Dwrite with compound member b then H5Dread with all compound members");

            /* Initialize wbuf */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                wbuf[i].a = (int)(2 * DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS + 2 * i);
                wbuf[i].b = (int)(2 * DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS + 2 * i + 1);
            }

            /* Write data */
            if (H5Dwrite(dset_id, b_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(write_b_read_full);

            /* Update fbuf to match file state - only element b was updated */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                fbuf[i].b = wbuf[i].b;
            }

            /* Initialize rbuf to -1 */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                rbuf[i].a = -1;
                rbuf[i].b = -1;
            }

            /* Set erbuf (simply match file state since we're reading the whole
             * thing) */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
                erbuf[i].a = fbuf[i].a;
                erbuf[i].b = fbuf[i].b;
            }

            /* Read data */
            if (H5Dread(dset_id, full_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(write_b_read_full);

            /* Verify data */
            for (i = 0; i < DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS; i++) {
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
    if (H5Dclose(dset_id) < 0)
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
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
        H5Tclose(full_type_id);
        H5Tclose(a_type_id);
        H5Tclose(b_type_id);
    }
    H5E_END_TRY

    return 1;
}

/* A test to check that vlen sequences can be written and read back
 * with basic parent types and selections */
static int
test_dataset_vlen_io(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID;
    hid_t space_id        = H5I_INVALID_HID;
    hid_t dset_int        = H5I_INVALID_HID;
    hid_t dset_float      = H5I_INVALID_HID;
    hid_t dset_string     = H5I_INVALID_HID;
    hid_t vlen_int        = H5I_INVALID_HID;
    hid_t vlen_float      = H5I_INVALID_HID;
    hid_t vlen_string     = H5I_INVALID_HID;
    hid_t str_base_type   = H5I_INVALID_HID;

    hsize_t dims[1] = {DATASET_VLEN_IO_DSET_DIMS};
    hsize_t point_coords[DATASET_VLEN_IO_DSET_DIMS / 2];

    hvl_t wbuf[DATASET_VLEN_IO_DSET_DIMS];
    hvl_t rbuf[DATASET_VLEN_IO_DSET_DIMS];

    memset(wbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);
    memset(rbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);

    TESTING_MULTIPART(
        "verification of dataset data with H5Dwrite and then H5D read with variable length sequence data");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    /* Skipped for now due to segfault with the Cache VOL */
    SKIPPED();
    return 0;

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((space_id = H5Screate_simple(1, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataspace");
        goto error;
    }

    if ((vlen_int = H5Tvlen_create(H5T_NATIVE_INT)) < 0) {
        H5_FAILED();
        printf("    couldn't create vlen integer sequence");
        goto error;
    }

    if ((vlen_float = H5Tvlen_create(H5T_NATIVE_FLOAT)) < 0) {
        H5_FAILED();
        printf("    couldn't create vlen float sequence");
        goto error;
    }

    if ((str_base_type = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;

    if ((H5Tset_size(str_base_type, DATASET_VLEN_IO_STR_LEN)) < 0)
        TEST_ERROR;

    if ((vlen_string = H5Tvlen_create(str_base_type)) < 0) {
        H5_FAILED();
        printf("    couldn't create vlen string sequence");
        goto error;
    }

    if ((dset_int = H5Dcreate2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", vlen_int, space_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with vlen integer sequence datatype");
        goto error;
    }

    if ((dset_float = H5Dcreate2(container_group, DATASET_VLEN_IO_DSET_NAME "_float", vlen_float, space_id,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with vlen float sequence datatype");
        goto error;
    }

    if ((dset_string = H5Dcreate2(container_group, DATASET_VLEN_IO_DSET_NAME "_string", vlen_string, space_id,
                                  H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset with vlen string sequence datatype");
        goto error;
    }

    /* Close datasets until individual tests */
    if (H5Dclose(dset_int) < 0) {
        H5_FAILED();
        printf("    couldn't close dataset with vlen integer sequence datatype");
    }

    dset_int = H5I_INVALID_HID;

    if (H5Dclose(dset_float) < 0) {
        H5_FAILED();
        printf("    couldn't close dataset with vlen float sequence datatype");
    }

    dset_float = H5I_INVALID_HID;

    if (H5Dclose(dset_string) < 0) {
        H5_FAILED();
        printf("    couldn't close dataset with vlen string sequence datatype");
    }

    dset_string = H5I_INVALID_HID;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(rw_all_int)
        {
            TESTING_2("write and read entire dataspace with integer sequence");
            /* Set up write buffer */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if ((wbuf[i].p = calloc(i + 1, sizeof(int) * (i + 1))) == NULL)
                    PART_TEST_ERROR(rw_all_int);

                for (size_t j = 0; j < i + 1; j++) {
                    ((int *)wbuf[i].p)[j] = (int)(i * j + 1);
                }

                wbuf[i].len = i + 1;
            }

            /* Open dataset */
            if ((dset_int = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_int);

            /* Perform write */
            if ((H5Dwrite(dset_int, vlen_int, space_id, H5S_ALL, H5P_DEFAULT, (const void *)wbuf)) < 0)
                PART_TEST_ERROR(rw_all_int);

            if (H5Dflush(dset_int) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Close and reopen file objects to flush cache */
            if (H5Dclose(dset_int) < 0)
                PART_TEST_ERROR(rw_all_int);

            dset_int = H5I_INVALID_HID;

            if (H5Gclose(container_group) < 0)
                PART_TEST_ERROR(rw_all_int);

            container_group = H5I_INVALID_HID;

            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(rw_all_int);

            file_id = H5I_INVALID_HID;

            if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_int);

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_int);

            if ((dset_int = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_int);

            /* Perform read */
            if ((H5Dread(dset_int, vlen_int, space_id, H5S_ALL, H5P_DEFAULT, (void *)rbuf)) < 0)
                PART_TEST_ERROR(rw_all_int);

            /* Close to finalize read */
            if (H5Dclose(dset_int) < 0)
                PART_TEST_ERROR(rw_all_int);
            dset_int = H5I_INVALID_HID;

            /* Verify data */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if (!rbuf[i].p)
                    PART_TEST_ERROR(rw_all_int);

                if (rbuf[i].len != wbuf[i].len)
                    PART_TEST_ERROR(rw_all_int);

                for (size_t j = 0; j < i + 1; j++)
                    if (((int *)rbuf[i].p)[j] != ((int *)wbuf[i].p)[j])
                        PART_TEST_ERROR(rw_all_int);
            }

            /* Reset buffers */
            if (H5Treclaim(vlen_int, space_id, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(rw_all_int);

            if (H5Treclaim(vlen_int, space_id, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(rw_all_int);

            memset(wbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);
            memset(rbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);

            PASSED();
        }
        PART_END(rw_all_int)
        PART_BEGIN(rw_all_float)
        {
            TESTING_2("write and read entire dataspace with float sequence");
            /* Set up write buffer */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if ((wbuf[i].p = calloc(i + 1, sizeof(float) * (i + 1))) == NULL)
                    PART_TEST_ERROR(rw_all_float);

                for (size_t j = 0; j < i + 1; j++) {
                    ((float *)wbuf[i].p)[j] = (float)(i * j + 1);
                }

                wbuf[i].len = i + 1;
            }

            /* Open dataset */
            if ((dset_float = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_float", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_float);

            /* Perform write */
            if ((H5Dwrite(dset_float, vlen_float, space_id, H5S_ALL, H5P_DEFAULT, (const void *)wbuf)) < 0)
                PART_TEST_ERROR(rw_all_float);

            if (H5Dflush(dset_float) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Close and reopen file objects to flush cache */
            if (H5Dclose(dset_float) < 0)
                PART_TEST_ERROR(rw_all_float);

            dset_float = H5I_INVALID_HID;

            if (H5Gclose(container_group) < 0)
                PART_TEST_ERROR(rw_all_float);

            container_group = H5I_INVALID_HID;

            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(rw_all_float);

            file_id = H5I_INVALID_HID;

            if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_float);

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_float);

            if ((dset_float = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_float", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_float);

            /* Perform read */
            if ((H5Dread(dset_float, vlen_float, space_id, H5S_ALL, H5P_DEFAULT, (void *)rbuf)) < 0)
                PART_TEST_ERROR(rw_all_float);

            /* Close to finalize read */
            if (H5Dclose(dset_float) < 0)
                PART_TEST_ERROR(rw_all_float);
            dset_float = H5I_INVALID_HID;

            /* Verify data */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if (!rbuf[i].p)
                    PART_TEST_ERROR(rw_all_float);

                if (rbuf[i].len != wbuf[i].len)
                    PART_TEST_ERROR(rw_all_float);

                for (size_t j = 0; j < i + 1; j++) {
                    float expected = ((float *)wbuf[i].p)[j];
                    float actual   = ((float *)rbuf[i].p)[j];

                    if (!(H5_DBL_REL_EQUAL(expected, actual, 0.001)))
                        PART_TEST_ERROR(rw_all_float);
                }
            }

            /* Reset buffers */
            if (H5Treclaim(vlen_float, space_id, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(rw_all_float);

            if (H5Treclaim(vlen_float, space_id, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(rw_all_float);

            memset(wbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);
            memset(rbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);

            PASSED();
        }
        PART_END(rw_all_float);

        PART_BEGIN(rw_all_string)
        {
            TESTING_2("write and read entire dataspace with string sequence");
            /* Set up write buffer */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if ((wbuf[i].p = calloc(i + 1, DATASET_VLEN_IO_STR_LEN)) == NULL)
                    PART_TEST_ERROR(rw_all_string);

                for (size_t j = 0; j < i + 1; j++) {
                    char *str_ptr = ((char *)wbuf[i].p) + DATASET_VLEN_IO_STR_LEN * j;
                    memcpy(str_ptr, DATASET_VLEN_IO_STR_VALUE, DATASET_VLEN_IO_STR_LEN);
                }

                wbuf[i].len = i + 1;
            }

            /* Open the dataset */
            if ((dset_string = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_string", H5P_DEFAULT)) <
                0)
                PART_TEST_ERROR(rw_all_string);

            /* Perform write */
            if ((H5Dwrite(dset_string, vlen_string, space_id, H5S_ALL, H5P_DEFAULT, (const void *)wbuf)) < 0)
                PART_TEST_ERROR(rw_all_string);

            if (H5Dflush(dset_string) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Close and reopen file objects to flush cache */
            if (H5Dclose(dset_string) < 0)
                PART_TEST_ERROR(rw_all_string);

            dset_string = H5I_INVALID_HID;

            if (H5Gclose(container_group) < 0)
                PART_TEST_ERROR(rw_all_string);

            container_group = H5I_INVALID_HID;

            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(rw_all_string);

            file_id = H5I_INVALID_HID;

            if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_string);

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_all_string);

            if ((dset_string = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_string", H5P_DEFAULT)) <
                0)
                PART_TEST_ERROR(rw_all_string);

            /* Perform read */
            if ((H5Dread(dset_string, vlen_string, space_id, H5S_ALL, H5P_DEFAULT, (void *)rbuf)) < 0)
                PART_TEST_ERROR(rw_all_string);

            /* Close to finalize read */
            if (H5Dclose(dset_string) < 0)
                PART_TEST_ERROR(rw_all_string);
            dset_string = H5I_INVALID_HID;

            /* Verify data */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if (!rbuf[i].p)
                    PART_TEST_ERROR(rw_all_string);

                if (rbuf[i].len != wbuf[i].len)
                    PART_TEST_ERROR(rw_all_string);

                for (size_t j = 0; j < i + 1; j++) {
                    char  str_buf[DATASET_VLEN_IO_STR_LEN + 1];
                    char *str_ptr = (char *)rbuf[i].p + DATASET_VLEN_IO_STR_LEN * j;
                    memcpy(str_buf, str_ptr, DATASET_VLEN_IO_STR_LEN);
                    str_buf[DATASET_VLEN_IO_STR_LEN] = '\0';

                    if (strcmp(str_buf, DATASET_VLEN_IO_STR_VALUE))
                        PART_TEST_ERROR(rw_all_string);
                }
            }

            /* Reset buffers */
            if (H5Treclaim(vlen_string, space_id, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(rw_all_string);

            if (H5Treclaim(vlen_string, space_id, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(rw_all_string);

            memset(wbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);
            memset(rbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);

            PASSED();
        }
        PART_END(rw_all_string);

        PART_BEGIN(rw_point_selection)
        {
            TESTING_2("write with point selection");
            /* Select even-indexed points */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS / 2; i++)
                point_coords[i] = i * 2;

            /* Select points on dataspace */
            if (H5Sselect_elements(space_id, H5S_SELECT_SET, DATASET_VLEN_IO_DSET_DIMS / 2,
                                   (const hsize_t *)point_coords) < 0)
                PART_TEST_ERROR(rw_point_selection);

            /* Set up write buffer */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if ((wbuf[i].p = calloc(i + 1, sizeof(int) * (i + 1))) == NULL)
                    PART_TEST_ERROR(rw_point_selection);

                for (size_t j = 0; j < i + 1; j++) {
                    ((int *)wbuf[i].p)[j] = (int)(i * j + 1);
                }

                wbuf[i].len = i + 1;
            }

            /* Open dataset */
            if ((dset_int = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_point_selection);

            /* Perform write */
            if ((H5Dwrite(dset_int, vlen_int, space_id, space_id, H5P_DEFAULT, (const void *)wbuf)) < 0)
                PART_TEST_ERROR(rw_point_selection);

            if (H5Dflush(dset_int) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Close and reopen file objects to flush cache */
            if (H5Dclose(dset_int) < 0)
                PART_TEST_ERROR(rw_point_selection);
            dset_int = H5I_INVALID_HID;

            if (H5Gclose(container_group) < 0)
                PART_TEST_ERROR(rw_point_selection);
            container_group = H5I_INVALID_HID;

            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(rw_point_selection);
            file_id = H5I_INVALID_HID;

            if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_point_selection);

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_point_selection);

            if ((dset_int = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_point_selection);

            /* Perform read */
            if ((H5Dread(dset_int, vlen_int, space_id, space_id, H5P_DEFAULT, (void *)rbuf)) < 0)
                PART_TEST_ERROR(rw_point_selection);

            /* Close to finalize read */
            if (H5Dclose(dset_int) < 0)
                PART_TEST_ERROR(rw_point_selection);
            dset_int = H5I_INVALID_HID;

            /* Verify data */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if (i % 2 == 0) {
                    if (!rbuf[i].p)
                        PART_TEST_ERROR(rw_point_selection);

                    if (rbuf[i].len != wbuf[i].len)
                        PART_TEST_ERROR(rw_point_selection);

                    for (size_t j = 0; j < i + 1; j++)
                        if (((int *)rbuf[i].p)[j] != ((int *)wbuf[i].p)[j])
                            PART_TEST_ERROR(rw_point_selection);
                }
                else {
                    /* Odd positions in buffer should still read 0 */
                    if (rbuf[i].p)
                        PART_TEST_ERROR(rw_point_selection);
                    if (rbuf[i].len)
                        PART_TEST_ERROR(rw_point_selection);
                }
            }

            /* Reset buffers */
            if (H5Treclaim(vlen_int, space_id, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(rw_point_selection);

            if (H5Treclaim(vlen_int, space_id, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(rw_point_selection);

            memset(wbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);
            memset(rbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);

            PASSED();
        }
        PART_END(rw_point_selection);

        PART_BEGIN(rw_hyperslab_selection)
        {
            TESTING_2("write with hyperslab selection");
            /* Select hyperslab of every 3rd element */
            const hsize_t start[1]  = {0};
            const hsize_t stride[1] = {3};
            const hsize_t count[1]  = {1 + (DATASET_VLEN_IO_DSET_DIMS / stride[0])};
            const hsize_t block[1]  = {1};

            if ((H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, stride, count, block)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Set up write buffer */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if ((wbuf[i].p = calloc(i + 1, sizeof(int) * (i + 1))) == NULL)
                    PART_TEST_ERROR(rw_hyperslab_selection);

                for (size_t j = 0; j < i + 1; j++) {
                    ((int *)wbuf[i].p)[j] = (int)(i * j + 1);
                }

                wbuf[i].len = i + 1;
            }

            /* Open dataset */
            if ((dset_int = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Perform write */
            if ((H5Dwrite(dset_int, vlen_int, space_id, space_id, H5P_DEFAULT, (const void *)wbuf)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            if (H5Dflush(dset_int) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Close and reopen file objects to flush cache */
            if (H5Dclose(dset_int) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);
            dset_int = H5I_INVALID_HID;

            if (H5Gclose(container_group) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);
            container_group = H5I_INVALID_HID;

            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);
            file_id = H5I_INVALID_HID;

            if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            if ((dset_int = H5Dopen2(container_group, DATASET_VLEN_IO_DSET_NAME "_int", H5P_DEFAULT)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Perform read */
            if ((H5Dread(dset_int, vlen_int, space_id, space_id, H5P_DEFAULT, (void *)rbuf)) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            /* Close to finalize read */
            if (H5Dclose(dset_int) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);
            dset_int = H5I_INVALID_HID;

            /* Verify data */
            for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
                if (i % stride[0] == 0) {
                    if (!rbuf[i].p)
                        PART_TEST_ERROR(rw_hyperslab_selection);

                    if (rbuf[i].len != wbuf[i].len)
                        PART_TEST_ERROR(rw_hyperslab_selection);

                    for (size_t j = 0; j < i + 1; j++)
                        if (((int *)rbuf[i].p)[j] != ((int *)wbuf[i].p)[j])
                            PART_TEST_ERROR(rw_hyperslab_selection);
                }
                else {
                    /* Unread positions should still be 0 */
                    if (rbuf[i].p)
                        PART_TEST_ERROR(rw_hyperslab_selection);
                    if (rbuf[i].len)
                        PART_TEST_ERROR(rw_hyperslab_selection);
                }
            }

            /* Reset buffers */
            if (H5Treclaim(vlen_int, space_id, H5P_DEFAULT, rbuf) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            if (H5Treclaim(vlen_int, space_id, H5P_DEFAULT, wbuf) < 0)
                PART_TEST_ERROR(rw_hyperslab_selection);

            memset(wbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);
            memset(rbuf, 0, sizeof(hvl_t) * DATASET_VLEN_IO_DSET_DIMS);

            PASSED();
        }
        PART_END(rw_hyperslab_selection);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(vlen_int) < 0)
        TEST_ERROR;
    if (H5Tclose(vlen_float) < 0)
        TEST_ERROR;
    if (H5Tclose(vlen_string) < 0)
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
        H5Dclose(dset_int);
        H5Dclose(dset_float);
        H5Dclose(dset_string);
        H5Sclose(space_id);
        /* In case of memory allocation error, not all hvl_t buffers in array may be allocated.
         * Free one-by-one */
        for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
            if (wbuf[i].p) {
                free(wbuf[i].p);
                wbuf[i].p = NULL;
            }
        }

        for (size_t i = 0; i < DATASET_VLEN_IO_DSET_DIMS; i++) {
            if (rbuf[i].p) {
                free(rbuf[i].p);
                rbuf[i].p = NULL;
            }
        }
        H5Tclose(vlen_int);
        H5Tclose(vlen_float);
        H5Tclose(vlen_string);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a chunked dataset's extent can be
 * changed by using H5Dset_extent. This test uses unlimited
 * dimensions for the dataset, so the dimensionality of the
 * dataset may both shrink and grow.
 */
static int
test_dataset_set_extent_chunked_unlimited(void)
{
    hsize_t dims[DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK];
    hsize_t max_dims[DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK];
    hsize_t chunk_dims[DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK];
    hsize_t new_dims[DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK];
    size_t  i;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id    = H5I_INVALID_HID;
    hid_t   dset_dtype = H5I_INVALID_HID;
    hid_t   dcpl_id    = H5I_INVALID_HID;
    hid_t   fspace_id  = H5I_INVALID_HID;

    TESTING("H5Dset_extent on chunked dataset with unlimited dimensions");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK; i++) {
        max_dims[i]   = H5S_UNLIMITED;
        chunk_dims[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
    }

    if ((fspace_id = generate_random_dataspace(DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK, max_dims,
                                               dims, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    unable to set dataset chunk dimensionality\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_DSET_NAME, dset_dtype,
                              fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_NUM_PASSES; i++) {
        size_t j;

        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK; j++) {
            /* Ensure that the new dimensionality doesn't match the old dimensionality. */
            do {
                new_dims[j] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
            } while (new_dims[j] == dims[j]);
        }

        if (H5Dset_extent(dset_id, new_dims) < 0) {
            H5_FAILED();
            printf("    failed to set dataset extent\n");
            goto error;
        }

        /* Retrieve the new dimensions of the dataset and ensure they
         * are different from the original.
         */
        if (H5Sclose(fspace_id) < 0)
            TEST_ERROR;

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset's dataspace\n");
            goto error;
        }

        if (H5Sget_simple_extent_dims(fspace_id, new_dims, NULL) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset dimensionality\n");
            goto error;
        }

        /*
         * Make sure the dimensions have been changed.
         */
        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK; j++) {
            if (dims[j] == new_dims[j]) {
                H5_FAILED();
                printf("    dataset dimension %llu wasn't changed!\n", (unsigned long long)j);
                goto error;
            }
        }

        /*
         * Remember the current dimensionality of the dataset before
         * changing them again.
         */
        memcpy(dims, new_dims, sizeof(new_dims));
    }

    /*
     * Now close and re-open the dataset each pass to check the persistence
     * of the changes to the dataset's dimensionality.
     */
    for (i = 0; i < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_NUM_PASSES; i++) {
        size_t j;

        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK; j++) {
            /* Ensure that the new dimensionality doesn't match the old dimensionality. */
            do {
                new_dims[j] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
            } while (new_dims[j] == dims[j]);
        }

        if (H5Dset_extent(dset_id, new_dims) < 0) {
            H5_FAILED();
            printf("    failed to set dataset extent\n");
            goto error;
        }

        /* Retrieve the new dimensions of the dataset and ensure they
         * are different from the original.
         */
        if (H5Sclose(fspace_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;

        if ((dset_id = H5Dopen2(group_id, DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_DSET_NAME, H5P_DEFAULT)) <
            0) {
            H5_FAILED();
            printf("    failed to open dataset '%s'\n", DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_DSET_NAME);
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset's dataspace\n");
            goto error;
        }

        if (H5Sget_simple_extent_dims(fspace_id, new_dims, NULL) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset dimensionality\n");
            goto error;
        }

        /*
         * Make sure the dimensions have been changed.
         */
        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK; j++) {
            if (dims[j] == new_dims[j]) {
                H5_FAILED();
                printf("    dataset dimension %llu wasn't changed!\n", (unsigned long long)j);
                goto error;
            }
        }

        /*
         * Remember the current dimensionality of the dataset before
         * changing them again.
         */
        memcpy(dims, new_dims, sizeof(new_dims));
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a chunked dataset's extent can be
 * changed by using H5Dset_extent. This test uses fixed-size
 * dimensions for the dataset, so the dimensionality of the
 * dataset may only shrink.
 */
static int
test_dataset_set_extent_chunked_fixed(void)
{
    hsize_t dims[DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK];
    hsize_t dims2[DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK];
    hsize_t chunk_dims[DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK];
    hsize_t new_dims[DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK];
    size_t  i;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id = H5I_INVALID_HID, dset_id2 = H5I_INVALID_HID;
    hid_t   dset_dtype = H5I_INVALID_HID;
    hid_t   dcpl_id    = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID, fspace_id2 = H5I_INVALID_HID;

    TESTING("H5Dset_extent on chunked dataset with fixed dimensions");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK; i++) {
        dims[i]  = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
        dims2[i] = dims[i];
        do {
            chunk_dims[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
        } while (chunk_dims[i] > dims[i]);
    }

    if ((fspace_id = H5Screate_simple(DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;
    if ((fspace_id2 = H5Screate_simple(DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK, dims2, NULL)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    unable to set dataset chunk dimensionality\n");
        goto error;
    }

    /*
     * NOTE: Since shrinking the dimension size can quickly end in a situation
     * where the dimensions are of size 1 and we can't shrink them further, we
     * use two datasets here to ensure the second test can run at least once.
     */
    if ((dset_id = H5Dcreate2(group_id, DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME, dset_dtype,
                              fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME);
        goto error;
    }

    if ((dset_id2 = H5Dcreate2(group_id, DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME2, dset_dtype,
                               fspace_id2, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME2);
        goto error;
    }

    for (i = 0; i < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_NUM_PASSES; i++) {
        bool   skip_iterations = false;
        size_t j;

        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK; j++) {
            /* Ensure that the new dimensionality is less than the old dimensionality. */
            do {
                if (dims[j] == 1) {
                    skip_iterations = true;
                    break;
                }
                else
                    new_dims[j] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
            } while (new_dims[j] >= dims[j]);
        }

        /*
         * If we've shrunk one of the dimensions to size 1, skip the rest of
         * the iterations.
         */
        if (skip_iterations)
            break;

        if (H5Dset_extent(dset_id, new_dims) < 0) {
            H5_FAILED();
            printf("    failed to set dataset extent\n");
            goto error;
        }

        /* Retrieve the new dimensions of the dataset and ensure they
         * are different from the original.
         */
        if (H5Sclose(fspace_id) < 0)
            TEST_ERROR;

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset's dataspace\n");
            goto error;
        }

        if (H5Sget_simple_extent_dims(fspace_id, new_dims, NULL) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset dimensionality\n");
            goto error;
        }

        /*
         * Make sure the dimensions have been changed.
         */
        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK; j++) {
            if (dims[j] == new_dims[j]) {
                H5_FAILED();
                printf("    dataset dimension %llu wasn't changed!\n", (unsigned long long)j);
                goto error;
            }
        }

        /*
         * Remember the current dimensionality of the dataset before
         * changing them again.
         */
        memcpy(dims, new_dims, sizeof(new_dims));
    }

    /*
     * Now close and re-open the dataset each pass to check the persistence
     * of the changes to the dataset's dimensionality.
     */
    for (i = 0; i < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_NUM_PASSES; i++) {
        bool   skip_iterations = false;
        size_t j;

        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK; j++) {
            /* Ensure that the new dimensionality is less than the old dimensionality. */
            do {
                if (dims2[j] == 1) {
                    skip_iterations = true;
                    break;
                }
                else
                    new_dims[j] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
            } while (new_dims[j] >= dims2[j]);
        }

        /*
         * If we've shrunk one of the dimensions to size 1, skip the rest of
         * the iterations.
         */
        if (skip_iterations)
            break;

        if (H5Dset_extent(dset_id2, new_dims) < 0) {
            H5_FAILED();
            printf("    failed to set dataset extent2\n");
            goto error;
        }

        /* Retrieve the new dimensions of the dataset and ensure they
         * are different from the original.
         */
        if (H5Sclose(fspace_id2) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_id2) < 0)
            TEST_ERROR;

        if ((dset_id2 = H5Dopen2(group_id, DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME2, H5P_DEFAULT)) <
            0) {
            H5_FAILED();
            printf("    failed to open dataset '%s'\n", DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME2);
            goto error;
        }

        if ((fspace_id2 = H5Dget_space(dset_id2)) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset's dataspace\n");
            goto error;
        }

        if (H5Sget_simple_extent_dims(fspace_id2, new_dims, NULL) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataset dimensionality\n");
            goto error;
        }

        /*
         * Make sure the dimensions have been changed.
         */
        for (j = 0; j < DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK; j++) {
            if (dims2[j] == new_dims[j]) {
                H5_FAILED();
                printf("    dataset dimension %llu wasn't changed!\n", (unsigned long long)j);
                goto error;
            }
        }

        /*
         * Remember the current dimensionality of the dataset before
         * changing them again.
         */
        memcpy(dims2, new_dims, sizeof(new_dims));
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id2) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
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
        H5Sclose(fspace_id);
        H5Sclose(fspace_id2);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Dclose(dset_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check the data is correct after expanding
 * and shrinking the dataset with H5Dset_extent
 */
static int
test_dataset_set_extent_data(void)
{
    hsize_t dims_origin[DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK] = {DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM,
                                                                    DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM};
    hsize_t dims_expand[DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK] = {
        DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM * 2 - 1, DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM * 2 - 1};
    hsize_t dims_shrink[DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK] = {
        DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM / 2 + 1, DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM / 2 + 1};
    hsize_t dims_chunk[DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK] = {DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM,
                                                                   DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM};
    hsize_t dims_max[DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK]   = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t dims_out[DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK];
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID, dset_space_id = H5I_INVALID_HID;
    int     buf_origin[DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM][DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM];
    int     buf_expand2[DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM][DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM];
    int     buf_expand[DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM * 2 - 1]
                  [DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM * 2 - 1];
    int buf_shrink[DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM / 2 + 1]
                  [DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM / 2 + 1];
    int i, j;

    TESTING_MULTIPART("H5Dset_extent on data correctness");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SET_EXTENT_DATA_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SET_EXTENT_DATA_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK, dims_origin, dims_max)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK, dims_chunk) < 0) {
        H5_FAILED();
        printf("    unable to set dataset chunk dimensionality\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_SET_EXTENT_DATA_TEST_DSET_NAME, H5T_NATIVE_INT, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SET_EXTENT_DATA_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM; i++)
        for (j = 0; j < DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM; j++)
            buf_origin[i][j] = i + j;

    /* Write the original data
     * X X X X X X X X
     * X X X X X X X X
     * X X X X X X X X
     * X X X X X X X X
     * X X X X X X X X
     * X X X X X X X X
     * X X X X X X X X
     * X X X X X X X X
     */
    if (H5Dwrite(dset_id, H5T_NATIVE_INT, fspace_id, H5S_ALL, H5P_DEFAULT, buf_origin) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dset_extent_data_expand)
        {
            TESTING_2("H5Dset_extent for data expansion");

            /* Expand the dataset.  The extended space should be initialized with the
             * the default value (0)
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * X X X X X X X X 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             */
            if (H5Dset_extent(dset_id, dims_expand) < 0)
                PART_ERROR(H5Dset_extent_data_expand);

            if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_expand) < 0)
                PART_ERROR(H5Dset_extent_data_expand);

            /* compare the expanded data */
            for (i = 0; i < (int)dims_expand[0]; i++) {
                for (j = 0; j < (int)dims_expand[1]; j++) {
                    if (i >= (int)dims_origin[0] || j >= (int)dims_origin[1]) {
                        if (buf_expand[i][j] != 0) {
                            H5_FAILED();
                            printf("    buf_expand[%d][%d] = %d. It should be 0\n", i, j, buf_expand[i][j]);
                            PART_ERROR(H5Dset_extent_data_expand);
                        }
                    }
                    else {
                        if (buf_expand[i][j] != buf_origin[i][j]) {
                            H5_FAILED();
                            printf("    buf_expand[%d][%d] = %d. It should be %d\n", i, j, buf_expand[i][j],
                                   buf_origin[i][j]);
                            PART_ERROR(H5Dset_extent_data_expand);
                        }
                    }
                }
            }

            PASSED();
        }
        PART_END(H5Dset_extent_data_expand);

        PART_BEGIN(H5Dset_extent_data_shrink)
        {
            TESTING_2("H5Dset_extent for data shrinking");

            /* Shrink the dataset.
             * X X X X X
             * X X X X X
             * X X X X X
             * X X X X X
             * X X X X X
             */
            if (H5Dset_extent(dset_id, dims_shrink) < 0)
                PART_ERROR(H5Dset_extent_data_shrink);

            if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_shrink) < 0)
                PART_ERROR(H5Dset_extent_data_shrink);

            /* compare the shrunk data */
            for (i = 0; i < (int)dims_shrink[0]; i++) {
                for (j = 0; j < (int)dims_shrink[1]; j++) {
                    if (buf_shrink[i][j] != buf_origin[i][j]) {
                        H5_FAILED();
                        printf("    buf_shrink[%d][%d] = %d. It should be %d\n", i, j, buf_shrink[i][j],
                               buf_origin[i][j]);
                        PART_ERROR(H5Dset_extent_data_shrink);
                    }
                }
            }

            PASSED();
        }
        PART_END(H5Dset_extent_data_shrink);

        PART_BEGIN(H5Dset_extent_data_expand_to_origin)
        {
            TESTING_2("H5Dset_extent for data back to the original size");

            /* Expand the dataset back to the original size. The data should look like this:
             * X X X X X 0 0 0
             * X X X X X 0 0 0
             * X X X X X 0 0 0
             * X X X X X 0 0 0
             * X X X X X 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             */
            if (H5Dset_extent(dset_id, dims_origin) < 0)
                PART_ERROR(H5Dset_extent_data_expand_to_origin);

            if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_expand2) < 0)
                PART_ERROR(H5Dset_extent_data_expand_to_origin);

            /* compare the expanded data */
            for (i = 0; i < (int)dims_origin[0]; i++) {
                for (j = 0; j < (int)dims_origin[1]; j++) {
                    if (i >= (int)dims_shrink[0] || j >= (int)dims_shrink[1]) {
                        if (buf_expand2[i][j] != 0) {
                            H5_FAILED();
                            printf("    buf_expand2[%d][%d] = %d. It should be 0\n", i, j, buf_expand2[i][j]);
                            PART_ERROR(H5Dset_extent_data_expand_to_origin);
                        }
                    }
                    else {
                        if (buf_expand2[i][j] != buf_origin[i][j]) {
                            H5_FAILED();
                            printf("    buf_expand2[%d][%d] = %d. It should be %d.\n", i, j,
                                   buf_expand2[i][j], buf_origin[i][j]);
                            PART_ERROR(H5Dset_extent_data_expand_to_origin);
                        }
                    }
                }
            }

            PASSED();
        }
        PART_END(H5Dset_extent_data_expand_to_origin);

        PART_BEGIN(H5Dset_extent_data_shrink_to_zero)
        {
            TESTING_2("H5Dset_extent for data shrink to zero size");

            /* Shrink the dimensions to 0 and verify it */
            dims_shrink[0] = dims_shrink[1] = 0;

            if (H5Dset_extent(dset_id, dims_shrink) < 0)
                PART_ERROR(H5Dset_extent_data_shrink_to_zero);

            /* get the space */
            if ((dset_space_id = H5Dget_space(dset_id)) < 0)
                PART_ERROR(H5Dset_extent_data_shrink_to_zero);

            /* get dimensions */
            if (H5Sget_simple_extent_dims(dset_space_id, dims_out, NULL) < 0)
                PART_ERROR(H5Dset_extent_data_shrink_to_zero);

            if (H5Sclose(dset_space_id) < 0)
                PART_ERROR(H5Dset_extent_data_shrink_to_zero);

            /* Verify the dimensions are 0 */
            for (i = 0; i < DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK; i++)
                if (dims_out[i] != 0) {
                    H5_FAILED();
                    printf("    dims_out[%d] = %llu.  It should be 0.\n", i,
                           (long long unsigned int)dims_out[i]);
                    PART_ERROR(H5Dset_extent_data_shrink_to_zero);
                }

            PASSED();
        }
        PART_END(H5Dset_extent_data_shrink_to_zero);

        PART_BEGIN(H5Dset_extent_data_expand_to_origin_again)
        {
            TESTING_2("H5Dset_extent for data expansion back to the original again");

            /* Expand the dataset back to the original size. The data should look like this:
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             * 0 0 0 0 0 0 0 0
             */
            if (H5Dset_extent(dset_id, dims_origin) < 0)
                PART_ERROR(H5Dset_extent_data_expand_to_origin_again);

            if (H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_expand2) < 0)
                PART_ERROR(H5Dset_extent_data_expand_to_origin_again);

            /* The data should be all zeros */
            for (i = 0; i < (int)dims_origin[0]; i++) {
                for (j = 0; j < (int)dims_origin[1]; j++) {
                    if (buf_expand2[i][j] != 0) {
                        H5_FAILED();
                        printf("    buf_expand2[%d][%d] = %d. It should be 0.\n", i, j, buf_expand2[i][j]);
                        PART_ERROR(H5Dset_extent_data_expand_to_origin_again);
                    }
                }
            }

            PASSED();
        }
        PART_END(H5Dset_extent_data_expand_to_origin_again);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(dcpl_id) < 0)
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
        H5Pclose(dcpl_id);
        H5Sclose(fspace_id);
        H5Sclose(dset_space_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
} /* test_dataset_set_extent_data */

/*
 * If a dataset is opened twice and one of the handles is
 * used to extend the dataset, then the other handle should
 * return the new size when queried.
 */
static int
test_dataset_set_extent_double_handles(void)
{
    hsize_t dims_origin[DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK] = {
        DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM, DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM};
    hsize_t dims_expand[DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK] = {
        DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM * 2,
        DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM * 2};
    hsize_t dims_chunk[DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK] = {
        DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM / 2,
        DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM / 2};
    hsize_t dims_max[DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t dims_out[DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK];
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id = H5I_INVALID_HID, dset_id2 = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID, dset_space_id = H5I_INVALID_HID;
    int     i;

    TESTING("H5Dset_extent on double dataset handles");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SET_EXTENT_DATA_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id =
             H5Screate_simple(DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK, dims_origin, dims_max)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK, dims_chunk) < 0) {
        H5_FAILED();
        printf("    unable to set dataset chunk dimensionality\n");
        goto error;
    }

    /* Create the dataset */
    if ((dset_id = H5Dcreate2(group_id, DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_DSET_NAME, H5T_NATIVE_INT,
                              fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_DSET_NAME);
        goto error;
    }

    /* Open the same dataset again */
    if ((dset_id2 = H5Dopen2(group_id, DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_DSET_NAME);
        goto error;
    }

    /* Expand the dataset's dimensions with the first dataset handle */
    if (H5Dset_extent(dset_id, dims_expand) < 0)
        TEST_ERROR;

    /* Get the data space with the second dataset handle */
    if ((dset_space_id = H5Dget_space(dset_id2)) < 0)
        TEST_ERROR;

    /* Get the dimensions with the second dataset handle */
    if (H5Sget_simple_extent_dims(dset_space_id, dims_out, NULL) < 0)
        TEST_ERROR;

    if (H5Sclose(dset_space_id) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK; i++)
        if (dims_out[i] != dims_expand[i]) {
            H5_FAILED();
            printf("    dims_out[%d] = %" PRIuHSIZE ".  It should be %" PRIuHSIZE ".\n", i, dims_out[i],
                   dims_expand[i]);
            goto error;
        }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
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
        H5Sclose(fspace_id);
        H5Sclose(dset_space_id);
        H5Dclose(dset_id);
        H5Dclose(dset_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
} /* test_dataset_set_extent_double_handles */

/*
 * A test to check that a dataset's extent can't be
 * changed when H5Dset_extent is passed invalid parameters.
 */
static int
test_dataset_set_extent_invalid_params(void)
{
    hsize_t dims[DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK];
    hsize_t chunk_dims[DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK];
    hsize_t new_dims[DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK];
    hsize_t compact_dims[DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK] = {3, 3};
    size_t  i;
    herr_t  err_ret         = -1;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   chunked_dset_id = H5I_INVALID_HID, compact_dset_id = H5I_INVALID_HID,
          contiguous_dset_id = H5I_INVALID_HID;
    hid_t dset_dtype         = H5I_INVALID_HID;
    hid_t chunked_dcpl_id = H5I_INVALID_HID, compact_dcpl_id = H5I_INVALID_HID,
          contiguous_dcpl_id = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID, compact_fspace_id = H5I_INVALID_HID;
    char  vol_name[5];

    TESTING_MULTIPART("H5Dset_extent with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, basic or more dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    /** for DAOS VOL, this test is problematic since auto chunking can be selected, so skip for now */
    if (H5VLget_connector_name(file_id, vol_name, 5) < 0) {
        H5_FAILED();
        printf("    couldn't get VOL connector name\n");
        goto error;
    }
    if (strcmp(vol_name, "daos") == 0) {
        if (H5Fclose(file_id) < 0)
            TEST_ERROR;
        SKIPPED();
        return 0;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SET_EXTENT_INVALID_PARAMS_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_SET_EXTENT_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK, NULL, dims,
                                               false)) < 0)
        TEST_ERROR;

    for (i = 0; i < DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK; i++) {
        do {
            new_dims[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
        } while (new_dims[i] > dims[i]);
        do {
            chunk_dims[i] = (hsize_t)(rand() % MAX_DIM_SIZE + 1);
        } while (chunk_dims[i] > dims[i]);
    }

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    /* Create a compact dataset */
    if ((compact_dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(compact_dcpl_id, H5D_COMPACT) < 0)
        TEST_ERROR;

    /* Keep the data space small because the storage size of compact dataset is limited to 64K */
    if ((compact_fspace_id =
             H5Screate_simple(DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK, compact_dims, NULL)) < 0)
        TEST_ERROR;

    if ((compact_dset_id =
             H5Dcreate2(group_id, DATASET_SET_EXTENT_INVALID_LAYOUT_TEST_COMPACT_DSET_NAME, H5T_NATIVE_INT,
                        compact_fspace_id, H5P_DEFAULT, compact_dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n",
               DATASET_SET_EXTENT_INVALID_LAYOUT_TEST_COMPACT_DSET_NAME);
        goto error;
    }

    /* Create a contiguous dataset */
    if ((contiguous_dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(contiguous_dcpl_id, H5D_CONTIGUOUS) < 0)
        TEST_ERROR;

    if ((contiguous_dset_id =
             H5Dcreate2(group_id, DATASET_SET_EXTENT_INVALID_LAYOUT_TEST_CONTIGUOUS_DSET_NAME, dset_dtype,
                        fspace_id, H5P_DEFAULT, contiguous_dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n",
               DATASET_SET_EXTENT_INVALID_LAYOUT_TEST_CONTIGUOUS_DSET_NAME);
        goto error;
    }

    /* Create a chunked dataset */
    if ((chunked_dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(chunked_dcpl_id, DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    unable to set dataset chunk dimensionality\n");
        goto error;
    }

    if ((chunked_dset_id = H5Dcreate2(group_id, DATASET_SET_EXTENT_INVALID_PARAMS_TEST_DSET_NAME, dset_dtype,
                                      fspace_id, H5P_DEFAULT, chunked_dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SET_EXTENT_INVALID_PARAMS_TEST_DSET_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dset_extent_invalid_layout_compact)
        {
            TESTING_2("H5Dset_extent with an invalid dataset layout (compact)");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dset_extent(compact_dset_id, new_dims);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    setting dataset extent succeeded with an invalid layout (compact)\n");
                PART_ERROR(H5Dset_extent_invalid_layout_compact);
            }

            PASSED();
        }
        PART_END(H5Dset_extent_invalid_layout_compact);

        PART_BEGIN(H5Dset_extent_invalid_layout_contiguous)
        {
            TESTING_2("H5Dset_extent with an invalid dataset layout (contiguous)");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dset_extent(contiguous_dset_id, new_dims);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    setting dataset extent succeeded with an invalid layout (contiguous)\n");
                PART_ERROR(H5Dset_extent_invalid_layout_contiguous);
            }

            PASSED();
        }
        PART_END(H5Dset_extent_invalid_layout_contiguous);

        PART_BEGIN(H5Dset_extent_invalid_dset_id)
        {
            TESTING_2("H5Dset_extent with an invalid dataset ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dset_extent(H5I_INVALID_HID, new_dims);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    setting dataset extent succeeded with an invalid dataset ID\n");
                PART_ERROR(H5Dset_extent_invalid_dset_id);
            }

            PASSED();
        }
        PART_END(H5Dset_extent_invalid_dset_id);

        PART_BEGIN(H5Dset_extent_null_dim_pointer)
        {
            TESTING_2("H5Dset_extent with NULL dimension pointer");

            H5E_BEGIN_TRY
            {
                err_ret = H5Dset_extent(chunked_dset_id, NULL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    setting dataset extent succeeded with a NULL dimension pointer\n");
                PART_ERROR(H5Dset_extent_null_dim_pointer);
            }

            PASSED();
        }
        PART_END(H5Dset_extent_null_dim_pointer);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(chunked_dcpl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(compact_dcpl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(contiguous_dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(compact_fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Dclose(chunked_dset_id) < 0)
        TEST_ERROR;
    if (H5Dclose(compact_dset_id) < 0)
        TEST_ERROR;
    if (H5Dclose(contiguous_dset_id) < 0)
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
        H5Pclose(chunked_dcpl_id);
        H5Pclose(compact_dcpl_id);
        H5Pclose(contiguous_dcpl_id);
        H5Sclose(fspace_id);
        H5Sclose(compact_fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(chunked_dset_id);
        H5Dclose(compact_dset_id);
        H5Dclose(contiguous_dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
} /* test_dataset_set_extent_invalid_params */

/*
 * A test for H5Dflush.
 */
static int
test_flush_dataset(void)
{
    TESTING("H5Dflush");

    SKIPPED();

    return 0;
}

/*
 * A test to check that H5Dflush fails when it is
 * passed invalid parameters.
 */
static int
test_flush_dataset_invalid_params(void)
{
    TESTING("H5Dflush with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test for H5Drefresh.
 */
static int
test_refresh_dataset(void)
{
    TESTING("H5Drefresh");

    SKIPPED();

    return 0;
}

/*
 * A test to check that H5Drefresh fails when it is
 * passed invalid parameters.
 */
static int
test_refresh_dataset_invalid_params(void)
{
    TESTING("H5Drefresh");

    SKIPPED();

    return 0;
}

/*
 * A test to create a dataset composed of a single chunk.
 */
static int
test_create_single_chunk_dataset(void)
{
    hsize_t dims[DATASET_SINGLE_CHUNK_TEST_SPACE_RANK];
    hsize_t retrieved_chunk_dims[DATASET_SINGLE_CHUNK_TEST_SPACE_RANK];
    size_t  i;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id    = H5I_INVALID_HID;
    hid_t   dset_dtype = H5I_INVALID_HID;
    hid_t   dcpl_id    = H5I_INVALID_HID;
    hid_t   fspace_id  = H5I_INVALID_HID;

    TESTING("creation of dataset with single chunk");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SINGLE_CHUNK_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SINGLE_CHUNK_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_SINGLE_CHUNK_TEST_SPACE_RANK, NULL, dims, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_SINGLE_CHUNK_TEST_SPACE_RANK, dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_SINGLE_CHUNK_TEST_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SINGLE_CHUNK_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_SINGLE_CHUNK_TEST_SPACE_RANK, retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_SINGLE_CHUNK_TEST_SPACE_RANK; i++) {
        if (dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    /*
     * Now close the dataset and retrieve a copy
     * of the DCPL after re-opening it.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if (H5Dclose(dset_id) < 0) {
        H5_FAILED();
        printf("    failed to close dataset\n");
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_SINGLE_CHUNK_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to re-open dataset\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_SINGLE_CHUNK_TEST_SPACE_RANK, retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_SINGLE_CHUNK_TEST_SPACE_RANK; i++) {
        if (dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a single-chunk dataset can be written
 * and read correctly.
 */
static int
test_write_single_chunk_dataset(void)
{
    hssize_t space_npoints;
    hsize_t  dims[DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK];
    hsize_t  retrieved_chunk_dims[DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    dcpl_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with single chunk");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_SINGLE_CHUNK_WRITE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_SINGLE_CHUNK_WRITE_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK, NULL, dims,
                                               false)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK, dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME,
                              DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, dcpl_id,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK, retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK; i++) {
        if (dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0, data_size = 1; i < DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPESIZE;

    if (NULL == (write_buf = malloc(data_size)))
        TEST_ERROR;

    for (i = 0; i < data_size / DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPESIZE; i++)
        ((int *)write_buf)[i] = (int)i;

    if (H5Dwrite(dset_id, DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME);
        goto error;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPESIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (hsize_t)space_npoints; i++)
        if (((int *)read_buf)[i] != (int)i) {
            H5_FAILED();
            printf("    data verification failed\n");
            goto error;
        }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (H5Pclose(dcpl_id) < 0)
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
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
        H5Pclose(dcpl_id);
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
 * A test to create a dataset composed of multiple chunks.
 */
static int
test_create_multi_chunk_dataset(void)
{
    hsize_t dims[DATASET_MULTI_CHUNK_TEST_SPACE_RANK]       = {100, 100};
    hsize_t chunk_dims[DATASET_MULTI_CHUNK_TEST_SPACE_RANK] = {10, 10};
    hsize_t retrieved_chunk_dims[DATASET_MULTI_CHUNK_TEST_SPACE_RANK];
    size_t  i;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id    = H5I_INVALID_HID;
    hid_t   dset_dtype = H5I_INVALID_HID;
    hid_t   dcpl_id    = H5I_INVALID_HID;
    hid_t   fspace_id  = H5I_INVALID_HID;

    TESTING("creation of dataset with multiple chunks");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_MULTI_CHUNK_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_MULTI_CHUNK_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_TEST_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_TEST_DSET_NAME, dset_dtype, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_MULTI_CHUNK_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_TEST_SPACE_RANK, retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_TEST_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    /*
     * Now close the dataset and retrieve a copy
     * of the DCPL after re-opening it.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if (H5Dclose(dset_id) < 0) {
        H5_FAILED();
        printf("    failed to close dataset\n");
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to re-open dataset\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_TEST_SPACE_RANK, retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_TEST_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly. When reading back the
 * chunks of the dataset, the file dataspace and memory dataspace
 * used are the same shape.
 */
static int
test_write_multi_chunk_dataset_same_shape_read(void)
{
    hsize_t dims[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK]       = {100, 100};
    hsize_t chunk_dims[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK] = {10, 10};
    hsize_t retrieved_chunk_dims[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t start[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t count[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t  i, data_size, chunk_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    void   *write_buf = NULL;
    int     read_buf[10][10];

    TESTING("write to dataset with multiple chunks using same shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK, dims,
                                      NULL)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK, chunk_dims) <
        0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                              DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                     retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    if (NULL == (write_buf = malloc(data_size)))
        TEST_ERROR;

    /*
     * Ensure that each underlying chunk contains the values
     *
     * chunk_index .. (chunk_nelemts - 1) + chunk_index.
     *
     * That is to say, for a chunk size of 10 x 10, chunk 0
     * contains the values
     *
     * 0 .. 99
     *
     * while the next chunk contains the values
     *
     * 1 .. 100
     *
     * and so on.
     */
    for (i = 0; i < data_size / DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE; i++) {
        size_t j;
        size_t base;
        size_t tot_adjust;

        /*
         * Calculate a starting base value by taking the index value mod
         * the size of a chunk in each dimension.
         */
        for (j = 0, base = i; j < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
            if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                base %= chunk_dims[j];

        /*
         * Calculate the adjustment in each dimension.
         */
        for (j = 0, tot_adjust = 0; j < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
            if (j == (DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                tot_adjust += (i % dims[j]) / chunk_dims[j];
            else {
                size_t k;
                size_t n_faster_elemts;

                /*
                 * Calculate the number of elements in faster dimensions.
                 */
                for (k = j + 1, n_faster_elemts = 1;
                     k < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                    n_faster_elemts *= dims[k];

                tot_adjust += (((i / n_faster_elemts) / chunk_dims[j]) * (dims[j + 1] / chunk_dims[j + 1])) +
                              (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
            }
        }

        ((int *)write_buf)[i] = (int)(base + tot_adjust);
    }

    /*
     * Write every chunk in the dataset.
     */
    if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                 H5P_DEFAULT, write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    if ((dset_id =
             H5Dopen2(group_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    /*
     * Create 2-dimensional memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {chunk_dims[0], chunk_dims[1]};

        if ((mspace_id = H5Screate_simple(2, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    /*
     * Read every chunk in the dataset, checking the data for each one.
     */
    printf("\n");
    for (i = 0; i < data_size / chunk_size; i++) {
        size_t j, k;

        printf("\r Reading chunk %zu", i);

        for (j = 0; j < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
            if (dims[j] == chunk_dims[j])
                start[j] = 0;
            else if (j == (DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                /* Fastest changing dimension */
                start[j] = (i * chunk_dims[j]) % dims[j];
            else
                start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
        }

        /*
         * Adjust file dataspace selection for next chunk.
         */
        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            H5_FAILED();
            printf("    failed to set hyperslab selection\n");
            goto error;
        }

        for (j = 0; j < chunk_dims[0]; j++)
            for (k = 0; k < chunk_dims[1]; k++)
                read_buf[j][k] = 0;

        if (H5Dread(dset_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, mspace_id, fspace_id,
                    H5P_DEFAULT, read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n",
                   DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        for (j = 0; j < chunk_dims[0]; j++) {
            for (k = 0; k < chunk_dims[1]; k++) {
                if (read_buf[j][k] != (int)((j * chunk_dims[0]) + k + i)) {
                    H5_FAILED();
                    printf("    data verification failed for chunk %lld\n", (long long)i);
                    goto error;
                }
            }
        }
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id) < 0)
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
        if (write_buf)
            free(write_buf);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
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
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly. When reading back the
 * chunks of the dataset, the file dataspace and memory dataspace
 * used are differently shaped.
 */
static int
test_write_multi_chunk_dataset_diff_shape_read(void)
{
    hsize_t dims[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK]       = {100, 100};
    hsize_t chunk_dims[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK] = {10, 10};
    hsize_t retrieved_chunk_dims[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t start[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t count[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t  i, data_size, chunk_size;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    void   *write_buf = NULL;
    void   *read_buf  = NULL;

    TESTING("write to dataset with multiple chunks using differently shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK, dims,
                                      NULL)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK, chunk_dims) <
        0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                              DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                     retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    if (NULL == (write_buf = malloc(data_size)))
        TEST_ERROR;

    /*
     * Ensure that each underlying chunk contains the values
     *
     * chunk_index .. (chunk_nelemts - 1) + chunk_index.
     *
     * That is to say, for a chunk size of 10 x 10, chunk 0
     * contains the values
     *
     * 0 .. 99
     *
     * while the next chunk contains the values
     *
     * 1 .. 100
     *
     * and so on.
     */
    for (i = 0; i < data_size / DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE; i++) {
        size_t j;
        size_t base;
        size_t tot_adjust;

        /*
         * Calculate a starting base value by taking the index value mod
         * the size of a chunk in each dimension.
         */
        for (j = 0, base = i; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
            if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                base %= chunk_dims[j];

        /*
         * Calculate the adjustment in each dimension.
         */
        for (j = 0, tot_adjust = 0; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
            if (j == (DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                tot_adjust += (i % dims[j]) / chunk_dims[j];
            else {
                size_t k;
                size_t n_faster_elemts;

                /*
                 * Calculate the number of elements in faster dimensions.
                 */
                for (k = j + 1, n_faster_elemts = 1;
                     k < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                    n_faster_elemts *= dims[k];

                tot_adjust += (((i / n_faster_elemts) / chunk_dims[j]) * (dims[j + 1] / chunk_dims[j + 1])) +
                              (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
            }
        }

        ((int *)write_buf)[i] = (int)(base + tot_adjust);
    }

    /*
     * Write every chunk in the dataset.
     */
    if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                 H5P_DEFAULT, write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    if ((dset_id =
             H5Dopen2(group_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    /*
     * Allocate single chunk-sized read buffer.
     */
    if (NULL == (read_buf = malloc(chunk_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    /*
     * Create 1-dimensional memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {chunk_size / DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE};

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    /*
     * Read every chunk in the dataset, checking the data for each one.
     */
    printf("\n");
    for (i = 0; i < data_size / chunk_size; i++) {
        size_t j;

        printf("\r Reading chunk %zu", i);

        for (j = 0; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
            if (dims[j] == chunk_dims[j])
                start[j] = 0;
            else if (j == (DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                /* Fastest changing dimension */
                start[j] = (i * chunk_dims[j]) % dims[j];
            else
                start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
        }

        /*
         * Adjust file dataspace selection for next chunk.
         */
        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            H5_FAILED();
            printf("    failed to set hyperslab selection\n");
            goto error;
        }

        memset(read_buf, 0, chunk_size);
        if (H5Dread(dset_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, mspace_id, fspace_id,
                    H5P_DEFAULT, read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n",
                   DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        for (j = 0; j < (hsize_t)chunk_size / DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;
             j++)
            if (((int *)read_buf)[j] != (int)(j + i)) {
                H5_FAILED();
                printf("    data verification failed for chunk %lld\n", (long long)i);
                goto error;
            }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id) < 0)
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
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
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
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly several times in a row.
 * When reading back the chunks of the dataset, the file
 * dataspace and memory dataspace used are the same shape.
 */
static int
test_overwrite_multi_chunk_dataset_same_shape_read(void)
{
    hsize_t dims[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK]       = {100, 100};
    hsize_t chunk_dims[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK] = {10, 10};
    hsize_t retrieved_chunk_dims[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t start[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t count[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t  i, data_size, chunk_size;
    size_t  niter;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    void   *write_buf = NULL;
    int     read_buf[10][10];

    TESTING("several overwrites to dataset with multiple chunks using same shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                                      dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                     chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                              DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n",
               DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                     retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    if (NULL == (write_buf = malloc(data_size)))
        TEST_ERROR;

    /*
     * Create 2-dimensional memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {chunk_dims[0], chunk_dims[1]};

        if ((mspace_id = H5Screate_simple(2, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    printf("\n");
    for (niter = 0; niter < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_NITERS; niter++) {
        memset(write_buf, 0, data_size);

        /*
         * Ensure that each underlying chunk contains the values
         *
         * chunk_index .. (chunk_nelemts - 1) + chunk_index.
         *
         * That is to say, for a chunk size of 10 x 10, chunk 0
         * contains the values
         *
         * 0 .. 99
         *
         * while the next chunk contains the values
         *
         * 1 .. 100
         *
         * and so on. On each iteration, we add 1 to the previous
         * values.
         */
        for (i = 0; i < data_size / DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE; i++) {
            size_t j;
            size_t base;
            size_t tot_adjust;

            /*
             * Calculate a starting base value by taking the index value mod
             * the size of a chunk in each dimension.
             */
            for (j = 0, base = i; j < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
                if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                    base %= chunk_dims[j];

            /*
             * Calculate the adjustment in each dimension.
             */
            for (j = 0, tot_adjust = 0;
                 j < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                if (j == (DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                    tot_adjust += (i % dims[j]) / chunk_dims[j];
                else {
                    size_t k;
                    size_t n_faster_elemts;

                    /*
                     * Calculate the number of elements in faster dimensions.
                     */
                    for (k = j + 1, n_faster_elemts = 1;
                         k < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                        n_faster_elemts *= dims[k];

                    tot_adjust +=
                        (((i / n_faster_elemts) / chunk_dims[j]) * (dims[j + 1] / chunk_dims[j + 1])) +
                        (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
                }
            }

            ((int *)write_buf)[i] = (int)(base + tot_adjust + niter);
        }

        /*
         * Write every chunk in the dataset.
         */
        if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, write_buf) < 0) {
            H5_FAILED();
            printf("    couldn't write to dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                                H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    couldn't get dataset dataspace\n");
            goto error;
        }

        /*
         * Read every chunk in the dataset, checking the data for each one.
         */
        for (i = 0; i < data_size / chunk_size; i++) {
            size_t j, k;

            printf("\r Reading chunk %zu", i);

            for (j = 0; j < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                if (dims[j] == chunk_dims[j])
                    start[j] = 0;
                else if (j == (DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                    /* Fastest changing dimension */
                    start[j] = (i * chunk_dims[j]) % dims[j];
                else
                    start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
            }

            /*
             * Adjust file dataspace selection for next chunk.
             */
            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
                H5_FAILED();
                printf("    failed to set hyperslab selection\n");
                goto error;
            }

            for (j = 0; j < chunk_dims[0]; j++)
                for (k = 0; k < chunk_dims[1]; k++)
                    read_buf[j][k] = 0;

            if (H5Dread(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, mspace_id,
                        fspace_id, H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
                goto error;
            }

            for (j = 0; j < chunk_dims[0]; j++) {
                for (k = 0; k < chunk_dims[1]; k++) {
                    if (read_buf[j][k] != (int)((j * chunk_dims[0]) + k + i + niter)) {
                        H5_FAILED();
                        printf("    data verification failed for chunk %lld\n", (long long)i);
                        goto error;
                    }
                }
            }
        }
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id) < 0)
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
        if (write_buf)
            free(write_buf);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
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
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly several times in a row.
 * When reading back the chunks of the dataset, the file
 * dataspace and memory dataspace used are differently shaped.
 */
static int
test_overwrite_multi_chunk_dataset_diff_shape_read(void)
{
    hsize_t dims[DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK]       = {100, 100};
    hsize_t chunk_dims[DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK] = {10, 10};
    hsize_t retrieved_chunk_dims[DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t start[DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t count[DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t  i, data_size, chunk_size;
    size_t  niter;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    void   *write_buf = NULL;
    void   *read_buf  = NULL;

    TESTING("several overwrites to dataset with multiple chunks using differently shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                                      dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                     chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                              DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, fspace_id,
                              H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n",
               DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                     retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    if (NULL == (write_buf = malloc(data_size)))
        TEST_ERROR;

    /*
     * Allocate single chunk-sized read buffer.
     */
    if (NULL == (read_buf = malloc(chunk_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    /*
     * Create 1-dimensional memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {chunk_size / DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE};

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    printf("\n");
    for (niter = 0; niter < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_NITERS; niter++) {
        memset(write_buf, 0, data_size);

        /*
         * Ensure that each underlying chunk contains the values
         *
         * chunk_index .. (chunk_nelemts - 1) + chunk_index.
         *
         * That is to say, for a chunk size of 10 x 10, chunk 0
         * contains the values
         *
         * 0 .. 99
         *
         * while the next chunk contains the values
         *
         * 1 .. 100
         *
         * and so on. On each iteration, we add 1 to the previous
         * values.
         */
        for (i = 0; i < data_size / DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE; i++) {
            size_t j;
            size_t base;
            size_t tot_adjust;

            /*
             * Calculate a starting base value by taking the index value mod
             * the size of a chunk in each dimension.
             */
            for (j = 0, base = i; j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
                if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                    base %= chunk_dims[j];

            /*
             * Calculate the adjustment in each dimension.
             */
            for (j = 0, tot_adjust = 0;
                 j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                if (j == (DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                    tot_adjust += (i % dims[j]) / chunk_dims[j];
                else {
                    size_t k;
                    size_t n_faster_elemts;

                    /*
                     * Calculate the number of elements in faster dimensions.
                     */
                    for (k = j + 1, n_faster_elemts = 1;
                         k < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                        n_faster_elemts *= dims[k];

                    tot_adjust +=
                        (((i / n_faster_elemts) / chunk_dims[j]) * (dims[j + 1] / chunk_dims[j + 1])) +
                        (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
                }
            }

            ((int *)write_buf)[i] = (int)(base + tot_adjust + niter);
        }

        /*
         * Write every chunk in the dataset.
         */
        if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, write_buf) < 0) {
            H5_FAILED();
            printf("    couldn't write to dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                                H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    couldn't get dataset dataspace\n");
            goto error;
        }

        /*
         * Read every chunk in the dataset, checking the data for each one.
         */
        for (i = 0; i < data_size / chunk_size; i++) {
            size_t j;

            printf("\r Reading chunk %zu", i);

            for (j = 0; j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                if (dims[j] == chunk_dims[j])
                    start[j] = 0;
                else if (j == (DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                    /* Fastest changing dimension */
                    start[j] = (i * chunk_dims[j]) % dims[j];
                else
                    start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
            }

            /*
             * Adjust file dataspace selection for next chunk.
             */
            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
                H5_FAILED();
                printf("    failed to set hyperslab selection\n");
                goto error;
            }

            memset(read_buf, 0, chunk_size);
            if (H5Dread(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, mspace_id,
                        fspace_id, H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
                goto error;
            }

            for (j = 0;
                 j < (hsize_t)chunk_size / DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;
                 j++)
                if (((int *)read_buf)[j] != (int)(j + i + niter)) {
                    H5_FAILED();
                    printf("    data verification failed for chunk %lld\n", (long long)i);
                    goto error;
                }
        }
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id) < 0)
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
        if (write_buf)
            free(write_buf);
        if (read_buf)
            free(read_buf);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
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
 * A test to ensure that a partial chunk can be written and
 * then read correctly when the selection used in a chunked
 * dataset's file dataspace is H5S_ALL.
 */
#define FIXED_DIMSIZE       25
#define FIXED_CHUNK_DIMSIZE 10
static int
test_read_partial_chunk_all_selection(void)
{
    DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_CTYPE write_buf[FIXED_DIMSIZE][FIXED_DIMSIZE];
    DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_CTYPE read_buf[FIXED_DIMSIZE][FIXED_DIMSIZE];
    hsize_t dims[DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK] = {FIXED_DIMSIZE, FIXED_DIMSIZE};
    hsize_t chunk_dims[DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK] = {FIXED_CHUNK_DIMSIZE,
                                                                                   FIXED_CHUNK_DIMSIZE};
    hsize_t retrieved_chunk_dims[DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK];
    size_t  i, j;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;

    TESTING("reading a partial chunk using H5S_ALL for file dataspace");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = H5Screate_simple(DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK, dims, NULL)) <
        0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_NAME,
                              DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK, retrieved_chunk_dims) <
        0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0; i < FIXED_DIMSIZE; i++)
        for (j = 0; j < FIXED_DIMSIZE; j++)
            write_buf[i][j] = (DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_CTYPE)((i * FIXED_DIMSIZE) + j);

    for (i = 0; i < FIXED_DIMSIZE; i++)
        for (j = 0; j < FIXED_DIMSIZE; j++)
            read_buf[i][j] = -1;

    if (H5Dwrite(dset_id, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 write_buf) < 0) {
        H5_FAILED();
        printf("    failed to write to dataset\n");
        goto error;
    }

    /*
     * Close and re-open the dataset to ensure that the data is written.
     */
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if ((dset_id = H5Dopen2(group_id, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to re-open dataset\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    failed to read from dataset\n");
        goto error;
    }

    for (i = 0; i < FIXED_DIMSIZE; i++)
        for (j = 0; j < FIXED_DIMSIZE; j++)
            if (read_buf[i][j] != (int)((i * FIXED_DIMSIZE) + j)) {
                H5_FAILED();
                printf("    data verification failed for read buffer element %lld: expected %lld but was "
                       "%lld\n",
                       (long long)((i * FIXED_DIMSIZE) + j), (long long)((i * FIXED_DIMSIZE) + j),
                       (long long)read_buf[i][j]);
                goto error;
            }

    if (H5Pclose(dcpl_id) < 0)
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
        H5Pclose(dcpl_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}
#undef FIXED_DIMSIZE
#undef FIXED_CHUNK_DIMSIZE

/*
 * A test to ensure that a partial chunk can be written and
 * then read correctly when the selection used in a chunked
 * dataset's file dataspace is a hyperslab selection.
 */
#define FIXED_DIMSIZE       25
#define FIXED_CHUNK_DIMSIZE 10
#define FIXED_NCHUNKS       9 /* For convenience - make sure to adjust this as necessary */
static int
test_read_partial_chunk_hyperslab_selection(void)
{
    DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_CTYPE write_buf[FIXED_CHUNK_DIMSIZE][FIXED_CHUNK_DIMSIZE];
    DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_CTYPE read_buf[FIXED_CHUNK_DIMSIZE][FIXED_CHUNK_DIMSIZE];
    hsize_t dims[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK] = {FIXED_DIMSIZE, FIXED_DIMSIZE};
    hsize_t chunk_dims[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK] = {FIXED_CHUNK_DIMSIZE,
                                                                                     FIXED_CHUNK_DIMSIZE};
    hsize_t retrieved_chunk_dims[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK];
    size_t  i, j, k;
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t   dset_id   = H5I_INVALID_HID;
    hid_t   dcpl_id   = H5I_INVALID_HID;
    hid_t   mspace_id = H5I_INVALID_HID;
    hid_t   fspace_id = H5I_INVALID_HID;

    TESTING("reading a partial chunk using a hyperslab selection in file dataspace");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or get property list aren't supported "
               "with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id =
             H5Screate_simple(DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl_id, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK, chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_NAME,
                              DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              dcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_NAME);
        goto error;
    }

    /*
     * See if a copy of the DCPL reports the correct chunking.
     */
    if (H5Pclose(dcpl_id) < 0) {
        H5_FAILED();
        printf("    failed to close DCPL\n");
        goto error;
    }

    if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
        H5_FAILED();
        printf("    failed to retrieve copy of DCPL\n");
        goto error;
    }

    memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
    if (H5Pget_chunk(dcpl_id, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK,
                     retrieved_chunk_dims) < 0) {
        H5_FAILED();
        printf("    failed to retrieve chunking info\n");
        goto error;
    }

    for (i = 0; i < DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK; i++) {
        if (chunk_dims[i] != retrieved_chunk_dims[i]) {
            H5_FAILED();
            printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                   "dimensionality\n");
            goto error;
        }
    }

    for (i = 0; i < FIXED_CHUNK_DIMSIZE; i++)
        for (j = 0; j < FIXED_CHUNK_DIMSIZE; j++)
            write_buf[i][j] =
                (DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_CTYPE)((i * FIXED_CHUNK_DIMSIZE) + j);

    for (i = 0; i < FIXED_CHUNK_DIMSIZE; i++)
        for (j = 0; j < FIXED_CHUNK_DIMSIZE; j++)
            read_buf[i][j] = -1;

    /*
     * Create chunk-sized memory dataspace for read buffer.
     */
    {
        hsize_t mdims[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK] = {FIXED_CHUNK_DIMSIZE,
                                                                                    FIXED_CHUNK_DIMSIZE};

        if ((mspace_id = H5Screate_simple(DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK, mdims,
                                          NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    /*
     * Write and read each chunk in the dataset.
     */
    for (i = 0; i < FIXED_NCHUNKS; i++) {
        hsize_t start[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK];
        hsize_t count[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK];
        bool    on_partial_edge_chunk = false;
        size_t  n_chunks_per_dim      = (dims[1] / chunk_dims[1]) + (((dims[1] % chunk_dims[1]) > 0) ? 1 : 0);

        on_partial_edge_chunk =
            (i > 0) && (((i + 1) % n_chunks_per_dim == 0) || (i / n_chunks_per_dim == n_chunks_per_dim - 1));

        for (j = 0; j < DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK; j++) {
            if (j == 0)
                start[j] = (i / n_chunks_per_dim) * chunk_dims[j];
            else
                start[j] = (i % n_chunks_per_dim) * chunk_dims[j];

            if (on_partial_edge_chunk) {
                /*
                 * Partial edge chunk
                 */
                if (j == 0) {
                    if (i / n_chunks_per_dim == n_chunks_per_dim - 1)
                        /* This partial edge chunk spans the remainder of the first dimension */
                        count[j] = dims[j] - ((i / n_chunks_per_dim) * chunk_dims[j]);
                    else
                        /* This partial edge chunk spans the whole first dimension */
                        count[j] = chunk_dims[j];
                }
                else {
                    if (i % n_chunks_per_dim == n_chunks_per_dim - 1)
                        /* This partial edge chunk spans the remainder of the second dimension */
                        count[j] = dims[j] - ((i % n_chunks_per_dim) * chunk_dims[j]);
                    else
                        /* This partial edge chunk spans the whole second dimension */
                        count[j] = chunk_dims[j];
                }
            }
            else
                count[j] = chunk_dims[j];
        }

        if (on_partial_edge_chunk) {
            hsize_t m_start[DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK] = {0, 0};

            if (H5Sselect_hyperslab(mspace_id, H5S_SELECT_SET, m_start, NULL, count, NULL) < 0) {
                H5_FAILED();
                printf("    failed to select hyperslab in memory dataspace\n");
                goto error;
            }
        }
        else {
            if (H5Sselect_all(mspace_id) < 0) {
                H5_FAILED();
                printf("    failed to select entire memory dataspace\n");
                goto error;
            }
        }

        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            H5_FAILED();
            printf("    failed to select hyperslab\n");
            goto error;
        }

        printf("\r Writing chunk %zu", i);

        if (H5Dwrite(dset_id, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id,
                     H5P_DEFAULT, write_buf) < 0) {
            H5_FAILED();
            printf("    failed to write to dataset\n");
            goto error;
        }

        /*
         * Close and re-open the dataset to ensure the data gets written.
         */
        if (H5Sclose(fspace_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_id) < 0)
            TEST_ERROR;
        if ((dset_id = H5Dopen2(group_id, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_NAME, H5P_DEFAULT)) <
            0) {
            H5_FAILED();
            printf("    failed to re-open dataset\n");
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    failed to retrieve dataspace from dataset\n");
            goto error;
        }

        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            H5_FAILED();
            printf("    failed to select hyperslab\n");
            goto error;
        }

        printf("\r Reading chunk %zu", i);

        if (H5Dread(dset_id, DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id,
                    H5P_DEFAULT, read_buf) < 0) {
            H5_FAILED();
            printf("    failed to read from dataset\n");
            goto error;
        }

        for (j = 0; j < FIXED_CHUNK_DIMSIZE; j++)
            for (k = 0; k < FIXED_CHUNK_DIMSIZE; k++)
                if (read_buf[j][k] != (int)((j * FIXED_CHUNK_DIMSIZE) + k)) {
                    H5_FAILED();
                    printf("    data verification failed for read buffer element %lld: expected %lld but "
                           "was %lld\n",
                           (long long)((j * FIXED_CHUNK_DIMSIZE) + k),
                           (long long)((j * FIXED_CHUNK_DIMSIZE) + k), (long long)read_buf[j][k]);
                    goto error;
                }
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id) < 0)
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
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}
#undef FIXED_DIMSIZE
#undef FIXED_CHUNK_DIMSIZE
#undef FIXED_NCHUNKS

/*
 * A test to ensure that a partial chunk can be written and
 * then read correctly when the selection used in a chunked
 * dataset's file dataspace is a point selection.
 */
/* #define FIXED_DIMSIZE       25 */
/* #define FIXED_CHUNK_DIMSIZE 10 */
static int
test_read_partial_chunk_point_selection(void)
{
    TESTING("reading a partial chunk using a point selection in file dataspace");
    SKIPPED();

    return 1;
}
/* #undef FIXED_DIMSIZE */
/* #undef FIXED_CHUNK_DIMSIZE */

/*
 * A test to verify that H5Dvlen_get_buf_size returns
 * correct size
 */
static int
test_get_vlen_buf_size(void)
{
    hvl_t    wdata[DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM]; /* Information to write */
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID;
    hid_t    group_id        = H5I_INVALID_HID;
    hid_t    dataset         = H5I_INVALID_HID;
    hid_t    dspace_id       = H5I_INVALID_HID;
    hid_t    dtype_id        = H5I_INVALID_HID;
    bool     freed_wdata     = false;
    hsize_t  dims1[]         = {DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM};
    hsize_t  size; /* Number of bytes which will be used */
    unsigned i, j;

    TESTING("H5Dvlen_get_buf_size");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this "
               "connector\n");
        return 0;
    }

    /* Allocate and initialize VL data to write */
    for (i = 0; i < DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM; i++) {
        wdata[i].p   = malloc((i + 1) * sizeof(unsigned int));
        wdata[i].len = i + 1;
        for (j = 0; j < (i + 1); j++)
            ((unsigned int *)wdata[i].p)[j] = i * 10 + j;
    } /* end for */

    /* Open the file */
    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_GET_VLEN_BUF_SIZE_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_GET_VLEN_BUF_SIZE_GROUP_NAME);
        goto error;
    }

    /* Create dataspace for dataset */
    if ((dspace_id = H5Screate_simple(DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_RANK, dims1, NULL)) < 0)
        TEST_ERROR;

    /* Create a datatype to refer to */
    if ((dtype_id = H5Tvlen_create(H5T_NATIVE_UINT)) < 0)
        TEST_ERROR;

    /* Create a dataset */
    if ((dataset = H5Dcreate2(group_id, DATASET_GET_VLEN_BUF_SIZE_DSET_NAME, dtype_id, dspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write dataset to disk */
    if (H5Dwrite(dataset, dtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    /* Make certain the correct amount of memory will be used */
    if (H5Dvlen_get_buf_size(dataset, dtype_id, dspace_id, &size) < 0)
        TEST_ERROR;

    /* 10 elements allocated = 1 + 2 + 3 + 4 elements for each array position */
    if (size !=
        ((DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM * (DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM + 1)) / 2) *
            sizeof(unsigned int)) {
        H5_FAILED();
        printf("    H5Dvlen_get_buf_size returned wrong size (%lu), compared to the correct size (%lu)\n",
               size,
               ((DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM * (DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM + 1)) /
                2) *
                   sizeof(unsigned int));
        goto error;
    }

    if (H5Treclaim(dtype_id, dspace_id, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;
    freed_wdata = true;

    if (H5Dclose(dataset) < 0)
        TEST_ERROR;

    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;

    if (H5Sclose(dspace_id) < 0)
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
        if (!freed_wdata)
            H5Treclaim(dtype_id, dspace_id, H5P_DEFAULT, wdata);
        H5Sclose(dspace_id);
        H5Tclose(dtype_id);
        H5Dclose(dataset);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_get_vlen_buf_size() */

int
H5_api_dataset_test(void)
{
    size_t i;
    int    nerrors;

    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*             API Dataset Tests              *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(dataset_tests); i++) {
        nerrors += (*dataset_tests[i])() ? 1 : 0;
    }

    printf("\n");

    return nerrors;
}
