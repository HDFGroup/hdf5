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

#include "H5_api_object_test.h"

static int test_open_object(void);
static int test_open_object_invalid_params(void);
static int test_object_exists(void);
static int test_object_exists_invalid_params(void);
static int test_get_object_info(void);
static int test_get_object_info_invalid_params(void);
static int test_link_object(void);
static int test_link_object_invalid_params(void);
static int test_incr_decr_object_refcount(void);
static int test_incr_decr_object_refcount_invalid_params(void);
static int test_object_copy_basic(void);
static int test_object_copy_already_existing(void);
static int test_object_copy_shallow_group_copy(void);
static int test_object_copy_no_attributes(void);
static int test_object_copy_by_soft_link(void);
static int test_object_copy_group_with_soft_links(void);
static int test_object_copy_between_files(void);
static int test_object_copy_invalid_params(void);
static int test_object_comments(void);
static int test_object_comments_invalid_params(void);
static int test_object_visit(void);
static int test_object_visit_soft_link(void);
static int test_object_visit_invalid_params(void);
static int test_close_object(void);
static int test_close_object_invalid_params(void);
static int test_close_invalid_objects(void);
static int test_flush_object(void);
static int test_flush_object_invalid_params(void);
static int test_refresh_object(void);
static int test_refresh_object_invalid_params(void);

static herr_t object_copy_attribute_iter_callback(hid_t location_id, const char *attr_name,
                                                  const H5A_info_t *ainfo, void *op_data);
static herr_t object_copy_soft_link_non_expand_callback(hid_t group, const char *name,
                                                        const H5L_info2_t *info, void *op_data);
static herr_t object_copy_soft_link_expand_callback(hid_t group, const char *name, const H5L_info2_t *info,
                                                    void *op_data);
static herr_t object_visit_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info,
                                    void *op_data);
static herr_t object_visit_simple_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info,
                                           void *op_data);
static herr_t object_visit_dset_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info,
                                         void *op_data);
static herr_t object_visit_dtype_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info,
                                          void *op_data);
static herr_t object_visit_soft_link_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info,
                                              void *op_data);
static herr_t object_visit_noop_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info,
                                         void *op_data);

/*
 * The array of object tests to be performed.
 */
static int (*object_tests[])(void) = {
    test_open_object,
    test_open_object_invalid_params,
    test_object_exists,
    test_object_exists_invalid_params,
    test_get_object_info,
    test_get_object_info_invalid_params,
    test_link_object,
    test_link_object_invalid_params,
    test_incr_decr_object_refcount,
    test_incr_decr_object_refcount_invalid_params,
    test_object_copy_basic,
    test_object_copy_already_existing,
    test_object_copy_shallow_group_copy,
    test_object_copy_no_attributes,
    test_object_copy_by_soft_link,
    test_object_copy_group_with_soft_links,
    test_object_copy_between_files,
    test_object_copy_invalid_params,
    test_object_comments,
    test_object_comments_invalid_params,
    test_object_visit,
    test_object_visit_soft_link,
    test_object_visit_invalid_params,
    test_close_object,
    test_close_object_invalid_params,
    test_close_invalid_objects,
    test_flush_object,
    test_flush_object_invalid_params,
    test_refresh_object,
    test_refresh_object_invalid_params,
};

/*
 * A test to check that various objects (group, dataset, datatype)
 * can be opened by using H5Oopen, H5Oopen_by_idx and H5Oopen_by_addr.
 *
 * XXX: create separate objects for each test part.
 *
 * XXX: Add more open by idx tests
 *
 * XXX: test opening through dangling and resolving soft links.
 */
static int
test_open_object(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t group_id2  = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t type_id    = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING_MULTIPART("object opening");

    TESTING_2("test setup");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, dataset, or stored datatype aren't "
               "supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_OPEN_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_OPEN_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(OBJECT_OPEN_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oopen_group)
        {
            TESTING_2("H5Oopen on a group");

            if ((group_id2 = H5Gcreate2(group_id, OBJECT_OPEN_TEST_GRP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create group '%s'\n", OBJECT_OPEN_TEST_GRP_NAME);
                PART_ERROR(H5Oopen_group);
            }

            H5E_BEGIN_TRY
            {
                H5Gclose(group_id2);
            }
            H5E_END_TRY

            if ((group_id2 = H5Oopen(group_id, OBJECT_OPEN_TEST_GRP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open group '%s' with H5Oopen\n", OBJECT_OPEN_TEST_GRP_NAME);
                PART_ERROR(H5Oopen_group);
            }

            if (H5Iget_type(group_id2) != H5I_GROUP) {
                H5_FAILED();
                printf("    ID is not a group\n");
                PART_ERROR(H5Oopen_group);
            }

            if (H5Gclose(group_id2) < 0) {
                H5_FAILED();
                printf("    couldn't close group opened with H5Oopen\n");
                PART_ERROR(H5Oopen_group);
            }

            PASSED();
        }
        PART_END(H5Oopen_group);

        PART_BEGIN(H5Oopen_dset)
        {
            TESTING_2("H5Oopen on a dataset");

            if ((dset_id = H5Dcreate2(group_id, OBJECT_OPEN_TEST_DSET_NAME, dset_dtype, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", OBJECT_OPEN_TEST_DSET_NAME);
                PART_ERROR(H5Oopen_dset);
            }

            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY

            if ((dset_id = H5Oopen(group_id, OBJECT_OPEN_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s' with H5Oopen\n", OBJECT_OPEN_TEST_DSET_NAME);
                PART_ERROR(H5Oopen_dset);
            }

            if (H5Iget_type(dset_id) != H5I_DATASET) {
                H5_FAILED();
                printf("    ID is not a dataset\n");
                PART_ERROR(H5Oopen_dset);
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dataset opened with H5Oopen\n");
                PART_ERROR(H5Oopen_dset);
            }

            PASSED();
        }
        PART_END(H5Oopen_dset);

        PART_BEGIN(H5Oopen_dtype)
        {
            TESTING_2("H5Oopen on a committed datatype");

            if ((type_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
                H5_FAILED();
                printf("    couldn't create datatype '%s'\n", OBJECT_OPEN_TEST_TYPE_NAME);
                PART_ERROR(H5Oopen_dtype);
            }

            if (H5Tcommit2(group_id, OBJECT_OPEN_TEST_TYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT,
                           H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't commit datatype '%s'\n", OBJECT_OPEN_TEST_TYPE_NAME);
                PART_ERROR(H5Oopen_dtype);
            }

            H5E_BEGIN_TRY
            {
                H5Tclose(type_id);
            }
            H5E_END_TRY

            if ((type_id = H5Oopen(group_id, OBJECT_OPEN_TEST_TYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open datatype '%s' with H5Oopen\n", OBJECT_OPEN_TEST_TYPE_NAME);
                PART_ERROR(H5Oopen_dtype);
            }

            if (H5Iget_type(type_id) != H5I_DATATYPE) {
                H5_FAILED();
                printf("    ID is not a dataset\n");
                PART_ERROR(H5Oopen_dtype);
            }

            if (H5Tclose(type_id) < 0) {
                H5_FAILED();
                printf("    couldn't close committed datatype opened with H5Oopen\n");
                PART_ERROR(H5Oopen_dtype);
            }

            PASSED();
        }
        PART_END(H5Oopen_dtype);

        if (group_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(group_id2);
            }
            H5E_END_TRY
            group_id2 = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }
        if (type_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(type_id);
            }
            H5E_END_TRY
            type_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Oopen_by_idx_group)
        {
            TESTING_2("H5Oopen_by_idx on a group");

            if ((group_id2 = H5Oopen_by_idx(container_group, OBJECT_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                            H5_ITER_INC, 1, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open group '%s' with H5Oopen_by_idx\n", OBJECT_OPEN_TEST_GRP_NAME);
                PART_ERROR(H5Oopen_by_idx_group);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_group);

        PART_BEGIN(H5Oopen_by_idx_dset)
        {
            TESTING_2("H5Oopen_by_idx on a dataset");

            if ((dset_id = H5Oopen_by_idx(container_group, OBJECT_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_INC, 0, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s' with H5Oopen_by_idx\n", OBJECT_OPEN_TEST_DSET_NAME);
                PART_ERROR(H5Oopen_by_idx_dset);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_dset);

        PART_BEGIN(H5Oopen_by_idx_dtype)
        {
            TESTING_2("H5Oopen_by_idx on a committed datatype");

            if ((type_id = H5Oopen_by_idx(container_group, OBJECT_OPEN_TEST_GROUP_NAME, H5_INDEX_NAME,
                                          H5_ITER_INC, 2, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open committed datatype '%s' with H5Oopen_by_idx\n",
                       OBJECT_OPEN_TEST_TYPE_NAME);
                PART_ERROR(H5Oopen_by_idx_dtype);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_dtype);

        if (group_id2 >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(group_id2);
            }
            H5E_END_TRY
            group_id2 = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }
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
        H5Tclose(type_id);
        H5Dclose(dset_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that various objects (group, dataset, datatype)
 * can't be opened when H5Oopen, H5Oopen_by_idx and H5Oopen_by_addr
 * are passed invalid parameters.
 */
static int
test_open_object_invalid_params(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t group_id2 = H5I_INVALID_HID;
    hid_t gcpl_id   = H5I_INVALID_HID;

    TESTING_MULTIPART("object opening with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or object aren't supported with "
               "this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create a GCPL\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0) {
            H5_FAILED();
            printf("    couldn't enable link creation order tracking and indexing on GCPL\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               gcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id2 = H5Gcreate2(group_id, OBJECT_OPEN_INVALID_PARAMS_TEST_GRP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_OPEN_INVALID_PARAMS_TEST_GRP_NAME);
        goto error;
    }

    if (H5Gclose(group_id2) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oopen_invalid_loc_id)
        {
            TESTING_2("H5Oopen with an invalid location ID");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen(H5I_INVALID_HID, OBJECT_OPEN_INVALID_PARAMS_TEST_GRP_NAME, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen succeeded with an invalid location ID!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Oopen_invalid_loc_id);

        PART_BEGIN(H5Oopen_invalid_obj_name)
        {
            TESTING_2("H5Oopen with an invalid object name");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen(group_id, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen succeeded with a NULL object name!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen(group_id, "", H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen succeeded with an invalid object name of ''!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Oopen_invalid_obj_name);

        PART_BEGIN(H5Oopen_invalid_lapl)
        {
            TESTING_2("H5Oopen with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen(group_id, OBJECT_OPEN_INVALID_PARAMS_TEST_GRP_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen succeeded with an invalid LAPL!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Oopen_invalid_lapl);

        PART_BEGIN(H5Oopen_by_idx_invalid_loc_id)
        {
            TESTING_2("H5Oopen_by_idx with an invalid location ID");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(H5I_INVALID_HID, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                           H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with an invalid location ID!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_invalid_loc_id);

        PART_BEGIN(H5Oopen_by_idx_invalid_grp_name)
        {
            TESTING_2("H5Oopen_by_idx with an invalid group name");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, NULL, H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with a NULL group name!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_grp_name);
            }

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, "", H5_INDEX_NAME, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with an invalid group name of ''!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_grp_name);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_invalid_grp_name);

        PART_BEGIN(H5Oopen_by_idx_invalid_index_type)
        {
            TESTING_2("H5Oopen_by_idx with an invalid index type");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                           H5_INDEX_UNKNOWN, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with invalid index type H5_INDEX_UNKNOWN!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                           H5_INDEX_N, H5_ITER_INC, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with invalid index type H5_INDEX_N!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_invalid_index_type);

        PART_BEGIN(H5Oopen_by_idx_invalid_iter_order)
        {
            TESTING_2("H5Oopen_by_idx with an invalid iteration order");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                           H5_INDEX_NAME, H5_ITER_UNKNOWN, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with an invalid iteration ordering H5_ITER_UNKNOWN!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_iter_order);
            }

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                           H5_INDEX_NAME, H5_ITER_N, 0, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with an invalid iteration ordering H5_ITER_N!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_iter_order);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_invalid_iter_order);

        PART_BEGIN(H5Oopen_by_idx_invalid_lapl)
        {
            TESTING_2("H5Oopen_by_idx with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_idx(container_group, OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME,
                                           H5_INDEX_NAME, H5_ITER_INC, 0, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_idx succeeded with an invalid LAPL!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_idx_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_idx_invalid_lapl);

        PART_BEGIN(H5Oopen_by_token_invalid_loc_id)
        {
            TESTING_2("H5Oopen_by_token with an invalid location ID");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_token(H5I_INVALID_HID, H5O_TOKEN_UNDEF);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_token succeeded with an invalid location ID!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_token_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_token_invalid_loc_id);

        PART_BEGIN(H5Oopen_by_token_invalid_token)
        {
            TESTING_2("H5Oopen_by_token with an invalid token");

            H5E_BEGIN_TRY
            {
                group_id2 = H5Oopen_by_token(file_id, H5O_TOKEN_UNDEF);
            }
            H5E_END_TRY

            if (group_id2 >= 0) {
                H5_FAILED();
                printf("    H5Oopen_by_token succeeded with an invalid token!\n");
                H5Gclose(group_id2);
                PART_ERROR(H5Oopen_by_token_invalid_token);
            }

            PASSED();
        }
        PART_END(H5Oopen_by_token_invalid_token);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

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
        H5Pclose(gcpl_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Oexists_by_name.
 */
static int
test_object_exists(void)
{
    htri_t object_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  group_id2  = H5I_INVALID_HID;
    hid_t  dset_id    = H5I_INVALID_HID;
    hid_t  dtype_id   = H5I_INVALID_HID;
    hid_t  fspace_id  = H5I_INVALID_HID;
    hid_t  dset_dtype = H5I_INVALID_HID;

    TESTING_MULTIPART("object existence");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_SOFT_LINKS)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, dataset, stored datatype or soft link "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_EXISTS_TEST_SUBGROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_EXISTS_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(OBJECT_EXISTS_TEST_DSET_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    /*
     * NOTE: H5Oexists_by_name for hard links should always succeed.
     *       H5Oexists_by_name for a soft link may fail if the link doesn't resolve.
     */
    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oexists_by_name_group)
        {
            TESTING_2("H5Oexists_by_name on a group");

            if ((group_id2 = H5Gcreate2(group_id, OBJECT_EXISTS_TEST_GRP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create group '%s'\n", OBJECT_EXISTS_TEST_GRP_NAME);
                PART_ERROR(H5Oexists_by_name_group);
            }

            if ((object_exists = H5Oexists_by_name(group_id, OBJECT_EXISTS_TEST_GRP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if object '%s' exists\n", OBJECT_EXISTS_TEST_GRP_NAME);
                PART_ERROR(H5Oexists_by_name_group);
            }

            if (!object_exists) {
                H5_FAILED();
                printf("    object '%s' didn't exist!\n", OBJECT_EXISTS_TEST_GRP_NAME);
                PART_ERROR(H5Oexists_by_name_group);
            }

            if (H5Gclose(group_id2) < 0) {
                H5_FAILED();
                printf("    couldn't close group\n");
                PART_ERROR(H5Oexists_by_name_group);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_group);

        PART_BEGIN(H5Oexists_by_name_dset)
        {
            TESTING_2("H5Oexists_by_name on a dataset");

            if ((dset_id = H5Dcreate2(group_id, OBJECT_EXISTS_TEST_DSET_NAME, dset_dtype, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", OBJECT_EXISTS_TEST_DSET_NAME);
                PART_ERROR(H5Oexists_by_name_dset);
            }

            if ((object_exists = H5Oexists_by_name(group_id, OBJECT_EXISTS_TEST_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't determine if object '%s' exists\n", OBJECT_EXISTS_TEST_DSET_NAME);
                PART_ERROR(H5Oexists_by_name_dset);
            }

            if (!object_exists) {
                H5_FAILED();
                printf("    object '%s' didn't exist!\n", OBJECT_EXISTS_TEST_DSET_NAME);
                PART_ERROR(H5Oexists_by_name_dset);
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dataset\n");
                PART_ERROR(H5Oexists_by_name_dset);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_dset);

        PART_BEGIN(H5Oexists_by_name_dtype)
        {
            TESTING_2("H5Oexists_by_name on a committed datatype");

            if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
                H5_FAILED();
                printf("    couldn't create datatype '%s'\n", OBJECT_EXISTS_TEST_TYPE_NAME);
                PART_ERROR(H5Oexists_by_name_dtype);
            }

            if (H5Tcommit2(group_id, OBJECT_EXISTS_TEST_TYPE_NAME, dtype_id, H5P_DEFAULT, H5P_DEFAULT,
                           H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't commit datatype '%s'\n", OBJECT_EXISTS_TEST_TYPE_NAME);
                PART_ERROR(H5Oexists_by_name_dtype);
            }

            if ((object_exists = H5Oexists_by_name(group_id, OBJECT_EXISTS_TEST_TYPE_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    couldn't determine if object '%s' exists\n", OBJECT_EXISTS_TEST_TYPE_NAME);
                PART_ERROR(H5Oexists_by_name_dtype);
            }

            if (!object_exists) {
                H5_FAILED();
                printf("    object '%s' didn't exist!\n", OBJECT_EXISTS_TEST_TYPE_NAME);
                PART_ERROR(H5Oexists_by_name_dtype);
            }

            if (H5Tclose(dtype_id) < 0) {
                H5_FAILED();
                printf("    couldn't close datatype\n");
                PART_ERROR(H5Oexists_by_name_dtype);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_dtype);

        PART_BEGIN(H5Oexists_by_name_soft_link)
        {
            TESTING_2("H5Oexists_by_name for a soft link");

            if (H5Lcreate_soft("/" OBJECT_TEST_GROUP_NAME "/" OBJECT_EXISTS_TEST_SUBGROUP_NAME, group_id,
                               OBJECT_EXISTS_TEST_SOFT_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't create soft link '%s'\n", OBJECT_EXISTS_TEST_SOFT_LINK_NAME);
                PART_ERROR(H5Oexists_by_name_soft_link);
            }

            if ((object_exists =
                     H5Oexists_by_name(group_id, OBJECT_EXISTS_TEST_SOFT_LINK_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if object '%s' exists\n", OBJECT_EXISTS_TEST_SOFT_LINK_NAME);
                PART_ERROR(H5Oexists_by_name_soft_link);
            }

            if (!object_exists) {
                H5_FAILED();
                printf("    object '%s' didn't exist!\n", OBJECT_EXISTS_TEST_SOFT_LINK_NAME);
                PART_ERROR(H5Oexists_by_name_soft_link);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_soft_link);

        PART_BEGIN(H5Oexists_by_name_dangling_soft_link)
        {
            TESTING_2("H5Oexists_by_name for a dangling soft link");

            if (H5Lcreate_soft(
                    "/" OBJECT_TEST_GROUP_NAME "/" OBJECT_EXISTS_TEST_SUBGROUP_NAME "/non_existent_object",
                    group_id, OBJECT_EXISTS_TEST_DANGLING_LINK_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't create soft link '%s'\n", OBJECT_EXISTS_TEST_DANGLING_LINK_NAME);
                PART_ERROR(H5Oexists_by_name_dangling_soft_link);
            }

            if ((object_exists =
                     H5Oexists_by_name(group_id, OBJECT_EXISTS_TEST_DANGLING_LINK_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if object '%s' exists\n",
                       "/" OBJECT_TEST_GROUP_NAME "/" OBJECT_EXISTS_TEST_SUBGROUP_NAME
                       "/non_existent_object");
                PART_ERROR(H5Oexists_by_name_dangling_soft_link);
            }

            if (object_exists) {
                H5_FAILED();
                printf("    object pointed to by dangling soft link should not have existed!\n");
                PART_ERROR(H5Oexists_by_name_dangling_soft_link);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_dangling_soft_link);
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
        H5Tclose(dtype_id);
        H5Dclose(dset_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Oexists_by_name fails
 * when it is passed invalid parameters.
 */
static int
test_object_exists_invalid_params(void)
{
    htri_t object_exists;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  group_id2 = H5I_INVALID_HID;

    TESTING_MULTIPART("object existence with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or object aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_EXISTS_INVALID_PARAMS_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n",
               OBJECT_EXISTS_INVALID_PARAMS_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((group_id2 = H5Gcreate2(group_id, OBJECT_EXISTS_INVALID_PARAMS_TEST_GRP_NAME, H5P_DEFAULT,
                                H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_EXISTS_INVALID_PARAMS_TEST_GRP_NAME);
        goto error;
    }

    if (H5Gclose(group_id2) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oexists_by_name_invalid_loc_id)
        {
            TESTING_2("H5Oexists_by_name with an invalid location ID");

            H5E_BEGIN_TRY
            {
                object_exists = H5Oexists_by_name(H5I_INVALID_HID, OBJECT_EXISTS_INVALID_PARAMS_TEST_GRP_NAME,
                                                  H5P_DEFAULT);
            }
            H5E_END_TRY

            if (object_exists >= 0) {
                H5_FAILED();
                printf("    H5Oexists_by_name succeeded with an invalid location ID!\n");
                PART_ERROR(H5Oexists_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_invalid_loc_id);

        PART_BEGIN(H5Oexists_by_name_invalid_obj_name)
        {
            TESTING_2("H5Oexists_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                object_exists = H5Oexists_by_name(group_id, NULL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (object_exists >= 0) {
                H5_FAILED();
                printf("    H5Oexists_by_name succeeded with a NULL object name!\n");
                PART_ERROR(H5Oexists_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                object_exists = H5Oexists_by_name(group_id, "", H5P_DEFAULT);
            }
            H5E_END_TRY

            if (object_exists >= 0) {
                H5_FAILED();
                printf("    H5Oexists_by_name succeeded with an invalid object name of ''!\n");
                PART_ERROR(H5Oexists_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_invalid_obj_name);

        PART_BEGIN(H5Oexists_by_name_invalid_lapl)
        {
            TESTING_2("H5Oexists_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                object_exists =
                    H5Oexists_by_name(group_id, OBJECT_EXISTS_INVALID_PARAMS_TEST_GRP_NAME, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (object_exists >= 0) {
                H5_FAILED();
                printf("    H5Oexists_by_name succeeded with an invalid LAPL!\n");
                PART_ERROR(H5Oexists_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Oexists_by_name_invalid_lapl);
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
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Oget_info(_by_name/_by_idx).
 */
static int
test_get_object_info(void)
{
    TESTING("object info retrieval");

    SKIPPED();

    return 0;
}

/*
 * A test to check that an object's info can't be retrieved
 * when H5Oget_info(_by_name/_by_idx) are passed invalid
 * parameters.
 */
static int
test_get_object_info_invalid_params(void)
{
    TESTING("object info retrieval with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test for H5Olink.
 */
static int
test_link_object(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t group_id2  = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t type_id    = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING_MULTIPART("object linking");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, dataset, or stored datatype aren't "
               "supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_LINK_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_LINK_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(OBJECT_LINK_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Olink_group)
        {
            TESTING_2("H5Olink an anonymous group");

            if ((group_id2 = H5Gcreate_anon(group_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create an anonymous group\n");
                PART_ERROR(H5Olink_group);
            }

            if (H5Olink(group_id2, group_id, OBJECT_LINK_TEST_GROUP_NAME2, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't link the anonymous group\n");
                PART_ERROR(H5Olink_group);
            }

            PASSED();
        }
        PART_END(H5Olink_group);

        PART_BEGIN(H5Olink_dataset)
        {
            TESTING_2("H5Olink an anonymous dataset");

            if ((dset_id = H5Dcreate_anon(group_id, dset_dtype, fspace_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create an anonymous dataset\n");
                PART_ERROR(H5Olink_dataset);
            }

            if (H5Olink(dset_id, group_id, OBJECT_LINK_TEST_DSET_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't link the anonymous dataset\n");
                PART_ERROR(H5Olink_dataset);
            }

            PASSED();
        }
        PART_END(H5Olink_dataset);

        PART_BEGIN(H5Olink_datatype)
        {
            TESTING_2("H5Olink an anonymous datatype");

            if (H5Tcommit_anon(group_id, dset_dtype, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't create an anonymous datatype\n");
                PART_ERROR(H5Olink_datatype);
            }

            if (H5Olink(dset_dtype, group_id, OBJECT_LINK_TEST_DTYPE_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't link the anonymous datatype\n");
                PART_ERROR(H5Olink_datatype);
            }

            PASSED();
        }
        PART_END(H5Olink_datatype);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Tclose(type_id);
        H5Dclose(dset_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that an object can't be linked into
 * the file structure when H5Olink is passed invalid
 * parameters.
 */
static int
test_link_object_invalid_params(void)
{
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  group_id2 = H5I_INVALID_HID;
    herr_t status;

    TESTING_MULTIPART("object linking with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or object aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_LINK_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_LINK_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id2 = H5Gcreate_anon(group_id, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create an anonymous group\n");
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Olink_invalid_object_id)
        {
            TESTING_2("H5Olink with an invalid object ID");

            H5E_BEGIN_TRY
            {
                status = H5Olink(H5I_INVALID_HID, group_id, OBJECT_LINK_TEST_GROUP_NAME2, H5P_DEFAULT,
                                 H5P_DEFAULT);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Olink succeeded with an invalid object ID!\n");
                PART_ERROR(H5Olink_invalid_object_id);
            }

            PASSED();
        }
        PART_END(H5Olink_invalid_object_id);

        PART_BEGIN(H5Olink_invalid_location)
        {
            TESTING_2("H5Olink with an invalid location ID");

            H5E_BEGIN_TRY
            {
                status = H5Olink(group_id2, H5I_INVALID_HID, OBJECT_LINK_TEST_GROUP_NAME2, H5P_DEFAULT,
                                 H5P_DEFAULT);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Olink succeeded with an invalid location ID!\n");
                PART_ERROR(H5Olink_invalid_location);
            }

            PASSED();
        }
        PART_END(H5Olink_invalid_location);

        PART_BEGIN(H5Olink_invalid_name)
        {
            TESTING_2("H5Olink with an invalid name");

            H5E_BEGIN_TRY
            {
                status = H5Olink(group_id2, group_id, NULL, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Olink succeeded with NULL as the object name!\n");
                PART_ERROR(H5Olink_invalid_name);
            }

            H5E_BEGIN_TRY
            {
                status = H5Olink(group_id2, group_id, "", H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Olink succeeded with an invalid object name of ''!\n");
                PART_ERROR(H5Olink_invalid_name);
            }

            PASSED();
        }
        PART_END(H5Olink_invalid_name);

        PART_BEGIN(H5Olink_invalid_lcpl)
        {
            TESTING_2("H5Olink with an invalid LCPL");

            H5E_BEGIN_TRY
            {
                status =
                    H5Olink(group_id2, group_id, OBJECT_LINK_TEST_GROUP_NAME2, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Olink succeeded with an invalid LCPL!\n");
                PART_ERROR(H5Olink_invalid_lcpl);
            }

            PASSED();
        }
        PART_END(H5Olink_invalid_lcpl);

        PART_BEGIN(H5Olink_invalid_lapl)
        {
            TESTING_2("H5Olink with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                status =
                    H5Olink(group_id2, group_id, OBJECT_LINK_TEST_GROUP_NAME2, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Olink succeeded with an invalid LAPL!\n");
                PART_ERROR(H5Olink_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Olink_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Oincr_refcount/H5Odecr_refcount.
 */
static int
test_incr_decr_object_refcount(void)
{
    H5O_info2_t oinfo; /* Object info struct */
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t       group_id2  = H5I_INVALID_HID;
    hid_t       dset_id    = H5I_INVALID_HID;
    hid_t       fspace_id  = H5I_INVALID_HID;
    hid_t       dset_dtype = H5I_INVALID_HID;

    TESTING_MULTIPART("increment/decrement the reference count of object");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, stored datatype, or object  "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_REF_COUNT_TEST_SUBGROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_REF_COUNT_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(OBJECT_REF_COUNT_TEST_DSET_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oincr_decr_refcount_group)
        {
            TESTING_2("H5Oincr_refcount/H5Odecr_refcount on a group");

            if ((group_id2 = H5Gcreate2(group_id, OBJECT_REF_COUNT_TEST_GRP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create group '%s'\n", OBJECT_REF_COUNT_TEST_GRP_NAME);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            /* Increment the reference count */
            if (H5Oincr_refcount(group_id2) < 0) {
                H5_FAILED();
                printf("    couldn't increment reference count for the group '%s' \n",
                       OBJECT_REF_COUNT_TEST_GRP_NAME);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            /* Verify that reference count is 2 now */
            if (H5Oget_info_by_name3(group_id, OBJECT_REF_COUNT_TEST_GRP_NAME, &oinfo, H5O_INFO_BASIC,
                                     H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get reference count for the group '%s' \n",
                       OBJECT_REF_COUNT_TEST_GRP_NAME);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            if (oinfo.rc != 2) {
                H5_FAILED();
                printf("    the reference count for the group '%s' isn't 2: %d\n",
                       OBJECT_REF_COUNT_TEST_GRP_NAME, oinfo.rc);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            /* Decrement the reference count */
            if (H5Odecr_refcount(group_id2) < 0) {
                H5_FAILED();
                printf("    couldn't decrement reference count for the group '%s' \n",
                       OBJECT_REF_COUNT_TEST_GRP_NAME);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            /* Verify that reference count is 1 now */
            if (H5Oget_info_by_name3(group_id, OBJECT_REF_COUNT_TEST_GRP_NAME, &oinfo, H5O_INFO_BASIC,
                                     H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get reference count for the group '%s' \n",
                       OBJECT_REF_COUNT_TEST_GRP_NAME);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            if (oinfo.rc != 1) {
                H5_FAILED();
                printf("    the reference count for the group '%s' isn't 1: %d\n",
                       OBJECT_REF_COUNT_TEST_GRP_NAME, oinfo.rc);
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            if (H5Gclose(group_id2) < 0) {
                H5_FAILED();
                printf("    couldn't close group\n");
                PART_ERROR(H5Oincr_decr_refcount_group);
            }

            PASSED();
        }
        PART_END(H5Oincr_decr_refcount_group);

        PART_BEGIN(H5Oincr_decr_refcount_dset)
        {
            TESTING_2("H5Oincr_refcount/H5Odecr_refcount on a dataset");

            if ((dset_id = H5Dcreate2(group_id, OBJECT_REF_COUNT_TEST_DSET_NAME, dset_dtype, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", OBJECT_REF_COUNT_TEST_DSET_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            /* Increment the reference count */
            if (H5Oincr_refcount(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't increment reference count for the dataset '%s' \n",
                       OBJECT_REF_COUNT_TEST_DSET_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            /* Verify that reference count is 2 now */
            if (H5Oget_info_by_name3(group_id, OBJECT_REF_COUNT_TEST_DSET_NAME, &oinfo, H5O_INFO_BASIC,
                                     H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get reference count for the dataset '%s' \n",
                       OBJECT_REF_COUNT_TEST_DSET_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            if (oinfo.rc != 2) {
                H5_FAILED();
                printf("    the reference count for the dataset '%s' isn't 2: %d\n",
                       OBJECT_REF_COUNT_TEST_DSET_NAME, oinfo.rc);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            /* Decrement the reference count */
            if (H5Odecr_refcount(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't decrement reference count for the dataset '%s' \n",
                       OBJECT_REF_COUNT_TEST_DSET_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            /* Verify that reference count is 1 now */
            if (H5Oget_info_by_name3(group_id, OBJECT_REF_COUNT_TEST_DSET_NAME, &oinfo, H5O_INFO_BASIC,
                                     H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get reference count for the dataset '%s' \n",
                       OBJECT_REF_COUNT_TEST_DSET_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            if (oinfo.rc != 1) {
                H5_FAILED();
                printf("    the reference count for the dataset '%s' isn't 1: %d\n",
                       OBJECT_REF_COUNT_TEST_DSET_NAME, oinfo.rc);
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            if (H5Dclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dataset\n");
                PART_ERROR(H5Oincr_decr_refcount_dset);
            }

            PASSED();
        }
        PART_END(H5Oincr_decr_refcount_dset);

        PART_BEGIN(H5Oincr / decr_refcount_dtype)
        {
            TESTING_2("H5Oincr_refcount/H5Odecr_refcount on a committed datatype");

            if (H5Tcommit2(group_id, OBJECT_REF_COUNT_TEST_TYPE_NAME, dset_dtype, H5P_DEFAULT, H5P_DEFAULT,
                           H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't commit datatype '%s'\n", OBJECT_REF_COUNT_TEST_TYPE_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            /* Increment the reference count */
            if (H5Oincr_refcount(dset_dtype) < 0) {
                H5_FAILED();
                printf("    couldn't increment reference count for the datatype '%s' \n",
                       OBJECT_REF_COUNT_TEST_TYPE_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            /* Verify that reference count is 2 now */
            if (H5Oget_info_by_name3(group_id, OBJECT_REF_COUNT_TEST_TYPE_NAME, &oinfo, H5O_INFO_BASIC,
                                     H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get reference count for the datatype '%s' \n",
                       OBJECT_REF_COUNT_TEST_TYPE_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            if (oinfo.rc != 2) {
                H5_FAILED();
                printf("    the reference count for the datatype '%s' isn't 2: %d\n",
                       OBJECT_REF_COUNT_TEST_TYPE_NAME, oinfo.rc);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            /* Decrement the reference count */
            if (H5Odecr_refcount(dset_dtype) < 0) {
                H5_FAILED();
                printf("    couldn't decrement reference count for the datatype '%s' \n",
                       OBJECT_REF_COUNT_TEST_TYPE_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            /* Verify that reference count is 1 now */
            if (H5Oget_info_by_name3(group_id, OBJECT_REF_COUNT_TEST_TYPE_NAME, &oinfo, H5O_INFO_BASIC,
                                     H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't get reference count for the datatype '%s' \n",
                       OBJECT_REF_COUNT_TEST_TYPE_NAME);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            if (oinfo.rc != 1) {
                H5_FAILED();
                printf("    the reference count for the datatype '%s' isn't 1: %d\n",
                       OBJECT_REF_COUNT_TEST_TYPE_NAME, oinfo.rc);
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            if (H5Tclose(dset_dtype) < 0) {
                H5_FAILED();
                printf("    couldn't close datatype\n");
                PART_ERROR(H5Oincr_decr_refcount_dtype);
            }

            PASSED();
        }
        PART_END(H5Oincr_decr_refcount_dtype);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

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
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Dclose(dset_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
} /* test_incr_decr_object_refcount */

/*
 * A test to check that H5Oincr_refcount/H5Odecr_refcount
 * fail when passed invalid parameters.
 */
static int
test_incr_decr_object_refcount_invalid_params(void)
{
    herr_t status;

    TESTING_MULTIPART("object reference count incr./decr. with an invalid parameter");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE)) {
        SKIPPED();
        printf("    API functions for more object aren't supported with this connector\n");
        return 0;
    }

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oincr_refcount_invalid_param)
        {
            TESTING_2("H5Oincr_refcount with invalid object ID");

            H5E_BEGIN_TRY
            {
                status = H5Oincr_refcount(H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    incremented the reference count for an invalid object ID\n");
                PART_ERROR(H5Oincr_refcount_invalid_param);
            }

            PASSED();
        }
        PART_END(H5Oincr_refcount_invalid_param);

        PART_BEGIN(H5Odecr_refcount_invalid_param)
        {
            TESTING_2("H5Odecr_refcount with invalid object ID");

            H5E_BEGIN_TRY
            {
                status = H5Odecr_refcount(H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    decremented the reference count for an invalid object ID\n");
                PART_ERROR(H5Odecr_refcount_invalid_param);
            }

            PASSED();
        }
        PART_END(H5Odecr_refcount_invalid_param);
    }
    END_MULTIPART;

    return 0;

error:
    return 1;
}

/*
 * Basic tests for H5Ocopy.
 */
static int
test_object_copy_basic(void)
{
    H5O_info2_t object_info;
    H5G_info_t  group_info;
    htri_t      object_link_exists;
    size_t      i;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID;
    hid_t       group_id        = H5I_INVALID_HID;
    hid_t       group_id2       = H5I_INVALID_HID;
    hid_t       tmp_group_id    = H5I_INVALID_HID;
    hid_t       dset_id         = H5I_INVALID_HID;
    hid_t       tmp_dset_id     = H5I_INVALID_HID;
    hid_t       dtype_id        = H5I_INVALID_HID;
    hid_t       tmp_dtype_id    = H5I_INVALID_HID;
    hid_t       tmp_attr_id     = H5I_INVALID_HID;
    hid_t       dset_dtype      = H5I_INVALID_HID;
    hid_t       attr_space_id   = H5I_INVALID_HID;
    hid_t       space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("basic object copying");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, link, dataset, attribute, iterate, or "
               "stored datatype aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_BASIC_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_COPY_BASIC_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(OBJECT_COPY_BASIC_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;
    if ((attr_space_id = generate_random_dataspace(OBJECT_COPY_BASIC_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    /* Create the test group object, along with its nested members and the attributes attached to it. */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_BASIC_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_BASIC_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BASIC_TEST_NUM_NESTED_OBJS; i++) {
        char grp_name[OBJECT_COPY_BASIC_TEST_BUF_SIZE];

        snprintf(grp_name, OBJECT_COPY_BASIC_TEST_BUF_SIZE, "grp%d", (int)i);

        if ((tmp_group_id = H5Gcreate2(group_id2, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create group '%s' under group '%s'\n", grp_name,
                   OBJECT_COPY_BASIC_TEST_GROUP_NAME);
            goto error;
        }

        /* Create a further nested group under the last group added */
        if (i == (OBJECT_COPY_BASIC_TEST_NUM_NESTED_OBJS - 1)) {
            if (H5Gclose(H5Gcreate2(tmp_group_id, OBJECT_COPY_BASIC_TEST_DEEP_NESTED_GROUP_NAME, H5P_DEFAULT,
                                    H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create nested group '%s' under group '%s'\n",
                       OBJECT_COPY_BASIC_TEST_DEEP_NESTED_GROUP_NAME, grp_name);
                goto error;
            }
        }

        if (H5Gclose(tmp_group_id) < 0) {
            H5_FAILED();
            printf("    couldn't close group '%s'\n", grp_name);
            goto error;
        }
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BASIC_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_BASIC_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_BASIC_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((tmp_attr_id = H5Acreate2(group_id2, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on group '%s'\n", attr_name,
                   OBJECT_COPY_BASIC_TEST_GROUP_NAME);
            goto error;
        }

        if (H5Aclose(tmp_attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    /* Create the test dataset object, along with the attributes attached to it. */
    if ((dset_id = H5Dcreate2(group_id, OBJECT_COPY_BASIC_TEST_DSET_NAME, dset_dtype, space_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", OBJECT_COPY_BASIC_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BASIC_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_BASIC_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_BASIC_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((tmp_attr_id = H5Acreate2(dset_id, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on dataset '%s'\n", attr_name,
                   OBJECT_COPY_BASIC_TEST_DSET_NAME);
            goto error;
        }

        if (H5Aclose(tmp_attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    /* Create the test committed datatype object, along with the attributes attached to it. */
    if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, OBJECT_COPY_BASIC_TEST_DTYPE_NAME, dtype_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", OBJECT_COPY_BASIC_TEST_DTYPE_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BASIC_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_BASIC_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_BASIC_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((tmp_attr_id = H5Acreate2(dtype_id, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on committed datatype '%s'\n", attr_name,
                   OBJECT_COPY_BASIC_TEST_DTYPE_NAME);
            goto error;
        }

        if (H5Aclose(tmp_attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_group)
        {
            TESTING_2("H5Ocopy on a group (default copy options)");

            if (H5Ocopy(group_id, OBJECT_COPY_BASIC_TEST_GROUP_NAME, group_id,
                        OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy group '%s' to '%s'\n", OBJECT_COPY_BASIC_TEST_GROUP_NAME,
                       OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied group exists\n",
                       OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied group didn't exist!\n",
                       OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group);
            }

            /* Ensure that the new group has all the members of the copied group, and all its attributes */
            if ((tmp_group_id = H5Gopen2(group_id, OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open group copy '%s'\n", OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group);
            }

            memset(&group_info, 0, sizeof(group_info));

            /*
             * Set link count to zero in case the connector doesn't support
             * retrieval of group info.
             */
            group_info.nlinks = 0;

            if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                H5_FAILED();
                printf("    failed to retrieve group info\n");
                PART_ERROR(H5Ocopy_group);
            }

            if (group_info.nlinks != OBJECT_COPY_BASIC_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    copied group contained %d members instead of %d members after a deep copy!\n",
                       (int)group_info.nlinks, OBJECT_COPY_BASIC_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_group);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_group_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_group);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied group didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_group);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_group_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied group's attributes\n");
                PART_ERROR(H5Ocopy_group);
            }

            if (i != OBJECT_COPY_BASIC_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf(
                    "    number of attributes on copied group (%llu) didn't match expected number (%llu)!\n",
                    (unsigned long long)i, (unsigned long long)OBJECT_COPY_BASIC_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_group);
            }

            if (H5Gclose(tmp_group_id) < 0) {
                H5_FAILED();
                printf("    failed to close group copy\n");
                PART_ERROR(H5Ocopy_group);
            }

            /*
             * Ensure that the last immediate member of the copied group
             * contains its single member after the deep copy.
             */
            {
                char grp_name[OBJECT_COPY_BASIC_TEST_BUF_SIZE];

                snprintf(grp_name, OBJECT_COPY_BASIC_TEST_BUF_SIZE,
                         OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME "/grp%d",
                         OBJECT_COPY_BASIC_TEST_NUM_NESTED_OBJS - 1);

                if ((tmp_group_id = H5Gopen2(group_id, grp_name, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    failed to open group '%s'\n", OBJECT_COPY_BASIC_TEST_DEEP_NESTED_GROUP_NAME);
                    PART_ERROR(H5Ocopy_group);
                }

                memset(&group_info, 0, sizeof(group_info));

                /*
                 * Set link count to zero in case the connector doesn't support
                 * retrieval of group info.
                 */
                group_info.nlinks = 0;

                if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                    H5_FAILED();
                    printf("    failed to retrieve group info\n");
                    PART_ERROR(H5Ocopy_group);
                }

                if (group_info.nlinks != 1) {
                    H5_FAILED();
                    printf("    copied group's immediate members didn't contain nested members after a "
                           "deep copy!\n");
                    PART_ERROR(H5Ocopy_group);
                }

                if (H5Gclose(tmp_group_id) < 0) {
                    H5_FAILED();
                    printf("    failed to close group '%s'\n", OBJECT_COPY_BASIC_TEST_DEEP_NESTED_GROUP_NAME);
                    PART_ERROR(H5Ocopy_group);
                }
            }

            PASSED();
        }
        PART_END(H5Ocopy_group);

        if (tmp_group_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(tmp_group_id);
            }
            H5E_END_TRY
            tmp_group_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_dset)
        {
            TESTING_2("H5Ocopy on a dataset (default copy options)");

            if (H5Ocopy(group_id, OBJECT_COPY_BASIC_TEST_DSET_NAME, group_id,
                        OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy dataset '%s' to '%s'\n", OBJECT_COPY_BASIC_TEST_DSET_NAME,
                       OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied dataset exists\n",
                       OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied dataset didn't exist!\n",
                       OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset);
            }

            /* Ensure that the new dataset has all of the attributes of the copied dataset */
            if ((tmp_dset_id = H5Dopen2(group_id, OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open dataset copy '%s'\n", OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_dset_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_dset);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied dataset didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_dset);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_dset_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied dataset's attributes\n");
                PART_ERROR(H5Ocopy_dset);
            }

            if (i != OBJECT_COPY_BASIC_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    number of attributes on copied dataset (%llu) didn't match expected number "
                       "(%llu)!\n",
                       (unsigned long long)i, (unsigned long long)OBJECT_COPY_BASIC_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_dset);
            }

            if (H5Dclose(tmp_dset_id) < 0) {
                H5_FAILED();
                printf("    failed to close dataset copy\n");
                PART_ERROR(H5Ocopy_dset);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dset);

        if (tmp_dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(tmp_dset_id);
            }
            H5E_END_TRY
            tmp_dset_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_dtype)
        {
            TESTING_2("H5Ocopy on a committed datatype (default copy options)");

            if (H5Ocopy(group_id, OBJECT_COPY_BASIC_TEST_DTYPE_NAME, group_id,
                        OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy datatype '%s' to '%s'\n", OBJECT_COPY_BASIC_TEST_DTYPE_NAME,
                       OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied datatype exists\n",
                       OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied datatype didn't exist!\n",
                       OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype);
            }

            /* Ensure that the new committed datatype has all the attributes of the copied datatype */
            if ((tmp_dtype_id = H5Topen2(group_id, OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open datatype copy '%s'\n", OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_dtype_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_dtype);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied committed datatype didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_dtype);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_dtype_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied datatype's attributes\n");
                PART_ERROR(H5Ocopy_dtype);
            }

            if (i != OBJECT_COPY_BASIC_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    number of attributes on copied datatype (%llu) didn't match expected number "
                       "(%llu)!\n",
                       (unsigned long long)i, (unsigned long long)OBJECT_COPY_BASIC_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_dtype);
            }

            if (H5Tclose(tmp_dtype_id) < 0) {
                H5_FAILED();
                printf("    failed to close datatype copy\n");
                PART_ERROR(H5Ocopy_dtype);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dtype);

        if (tmp_dtype_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(tmp_dtype_id);
            }
            H5E_END_TRY
            tmp_dtype_id = H5I_INVALID_HID;
        }
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Sclose(space_id);
        H5Aclose(tmp_attr_id);
        H5Tclose(dset_dtype);
        H5Tclose(tmp_dtype_id);
        H5Tclose(dtype_id);
        H5Dclose(tmp_dset_id);
        H5Dclose(dset_id);
        H5Gclose(tmp_group_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Tests to ensure that H5Ocopy fails when attempting to copy
 * an object to a destination where the object already exists.
 */
static int
test_object_copy_already_existing(void)
{
    herr_t err_ret;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  group_id2       = H5I_INVALID_HID;
    hid_t  dset_id         = H5I_INVALID_HID;
    hid_t  dtype_id        = H5I_INVALID_HID;
    hid_t  dset_dtype      = H5I_INVALID_HID;
    hid_t  space_id        = H5I_INVALID_HID;

    TESTING_MULTIPART("object copying to location where objects already exist");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, dataset, or stored datatype aren't "
               "supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_ALREADY_EXISTING_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n",
               OBJECT_COPY_ALREADY_EXISTING_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((space_id =
             generate_random_dataspace(OBJECT_COPY_ALREADY_EXISTING_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;
    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    /* Create the test group object */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_ALREADY_EXISTING_TEST_GROUP_NAME, H5P_DEFAULT,
                                H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_ALREADY_EXISTING_TEST_GROUP_NAME);
        goto error;
    }

    /* Create the test dataset object */
    if ((dset_id = H5Dcreate2(group_id, OBJECT_COPY_ALREADY_EXISTING_TEST_DSET_NAME, dset_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", OBJECT_COPY_ALREADY_EXISTING_TEST_DSET_NAME);
        goto error;
    }

    /* Create the test committed datatype object */
    if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, OBJECT_COPY_ALREADY_EXISTING_TEST_DTYPE_NAME, dtype_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", OBJECT_COPY_ALREADY_EXISTING_TEST_DTYPE_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_already_existing_group)
        {
            TESTING_2("H5Ocopy group to location where group already exists");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_ALREADY_EXISTING_TEST_GROUP_NAME, group_id,
                                  OBJECT_COPY_ALREADY_EXISTING_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    group copy succeeded in location where group already exists!\n");
                PART_ERROR(H5Ocopy_already_existing_group);
            }

            PASSED();
        }
        PART_END(H5Ocopy_already_existing_group);

        PART_BEGIN(H5Ocopy_already_existing_dset)
        {
            TESTING_2("H5Ocopy dataset to location where dataset already exists");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_ALREADY_EXISTING_TEST_DSET_NAME, group_id,
                                  OBJECT_COPY_ALREADY_EXISTING_TEST_DSET_NAME, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    dataset copy succeeded in location where dataset already exists!\n");
                PART_ERROR(H5Ocopy_already_existing_dset);
            }

            PASSED();
        }
        PART_END(H5Ocopy_already_existing_dset);

        PART_BEGIN(H5Ocopy_already_existing_dtype)
        {
            TESTING_2("H5Ocopy committed datatype to location where committed datatype already exists");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_ALREADY_EXISTING_TEST_DTYPE_NAME, group_id,
                                  OBJECT_COPY_ALREADY_EXISTING_TEST_DTYPE_NAME, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    committed datatype copy succeeded in location where committed datatype already "
                       "exists!\n");
                PART_ERROR(H5Ocopy_already_existing_dtype);
            }

            PASSED();
        }
        PART_END(H5Ocopy_already_existing_dtype);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Tclose(dtype_id);
        H5Dclose(dset_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to exercise the H5O_COPY_SHALLOW_HIERARCHY_FLAG flag
 * for H5Ocopy.
 */
static int
test_object_copy_shallow_group_copy(void)
{
    H5G_info_t group_info;
    htri_t     object_link_exists;
    size_t     i;
    hid_t      file_id         = H5I_INVALID_HID;
    hid_t      container_group = H5I_INVALID_HID;
    hid_t      group_id        = H5I_INVALID_HID;
    hid_t      group_id2       = H5I_INVALID_HID;
    hid_t      tmp_group_id    = H5I_INVALID_HID;
    hid_t      ocpypl_id       = H5I_INVALID_HID;

    TESTING("object copying with H5O_COPY_SHALLOW_HIERARCHY_FLAG flag");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_MORE) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, or link aren't supported with this "
               "connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_SHALLOW_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_COPY_SHALLOW_TEST_SUBGROUP_NAME);
        goto error;
    }

    /* Create the test group object, along with its nested members. */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_SHALLOW_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_SHALLOW_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_SHALLOW_TEST_NUM_NESTED_OBJS; i++) {
        char grp_name[OBJECT_COPY_SHALLOW_TEST_BUF_SIZE];

        snprintf(grp_name, OBJECT_COPY_SHALLOW_TEST_BUF_SIZE, "grp%d", (int)i);

        if ((tmp_group_id = H5Gcreate2(group_id2, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create group '%s' under group '%s'\n", grp_name,
                   OBJECT_COPY_SHALLOW_TEST_GROUP_NAME);
            goto error;
        }

        /* Create a further nested group under the last group added */
        if (i == (OBJECT_COPY_SHALLOW_TEST_NUM_NESTED_OBJS - 1)) {
            if (H5Gclose(H5Gcreate2(tmp_group_id, OBJECT_COPY_SHALLOW_TEST_DEEP_NESTED_GROUP_NAME,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create nested group '%s' under group '%s'\n",
                       OBJECT_COPY_SHALLOW_TEST_DEEP_NESTED_GROUP_NAME, grp_name);
                goto error;
            }
        }

        if (H5Gclose(tmp_group_id) < 0) {
            H5_FAILED();
            printf("    couldn't close group '%s'\n", grp_name);
            goto error;
        }
    }

    if ((ocpypl_id = H5Pcreate(H5P_OBJECT_COPY)) < 0) {
        H5_FAILED();
        printf("    couldn't create OCopyPL\n");
        goto error;
    }

    if (H5Pset_copy_object(ocpypl_id, H5O_COPY_SHALLOW_HIERARCHY_FLAG) < 0) {
        H5_FAILED();
        printf("    couldn't set object copying options\n");
        goto error;
    }

    if (H5Ocopy(group_id, OBJECT_COPY_SHALLOW_TEST_GROUP_NAME, group_id,
                OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME, ocpypl_id, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    failed to copy group '%s' to '%s'\n", OBJECT_COPY_SHALLOW_TEST_GROUP_NAME,
               OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME);
        goto error;
    }

    if ((object_link_exists = H5Lexists(group_id, OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't determine if link '%s' to copied group exists\n",
               OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME);
        goto error;
    }

    if (!object_link_exists) {
        H5_FAILED();
        printf("    link '%s' to copied group didn't exist!\n", OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME);
        goto error;
    }

    /*
     * Ensure that the new group has only the immediate members of the copied group.
     */
    if ((tmp_group_id = H5Gopen2(group_id, OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to open group copy '%s'\n", OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME);
        goto error;
    }

    memset(&group_info, 0, sizeof(group_info));

    /*
     * Set link count to zero in case the connector doesn't support
     * retrieval of group info.
     */
    group_info.nlinks = 0;

    if (H5Gget_info(tmp_group_id, &group_info) < 0) {
        H5_FAILED();
        printf("    failed to retrieve group info\n");
        goto error;
    }

    if (group_info.nlinks != OBJECT_COPY_SHALLOW_TEST_NUM_NESTED_OBJS) {
        H5_FAILED();
        printf("    copied group contained %d members instead of %d members after a shallow copy!\n",
               (int)group_info.nlinks, OBJECT_COPY_SHALLOW_TEST_NUM_NESTED_OBJS);
        goto error;
    }

    if (H5Gclose(tmp_group_id) < 0)
        TEST_ERROR;

    /*
     * Ensure that the last immediate member of the copied group doesn't
     * contain any members after the shallow copy.
     */
    {
        char grp_name[OBJECT_COPY_SHALLOW_TEST_BUF_SIZE];

        snprintf(grp_name, OBJECT_COPY_SHALLOW_TEST_BUF_SIZE,
                 OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME "/grp%d",
                 OBJECT_COPY_SHALLOW_TEST_NUM_NESTED_OBJS - 1);

        if ((tmp_group_id = H5Gopen2(group_id, grp_name, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    failed to open group '%s'\n", grp_name);
            goto error;
        }

        memset(&group_info, 0, sizeof(group_info));

        /*
         * Set link count to non-zero in case the connector doesn't support
         * retrieval of group info.
         */
        group_info.nlinks = 1;

        if (H5Gget_info(tmp_group_id, &group_info) < 0) {
            H5_FAILED();
            printf("    failed to retrieve group info\n");
            goto error;
        }

        if (group_info.nlinks != 0) {
            H5_FAILED();
            printf("    copied group's immediate members contained nested members after a shallow copy!\n");
            goto error;
        }

        if (H5Gclose(tmp_group_id) < 0) {
            H5_FAILED();
            printf("    failed to close group '%s'\n", grp_name);
            goto error;
        }
    }

    if (H5Pclose(ocpypl_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Pclose(ocpypl_id);
        H5Gclose(tmp_group_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Tests to exercise the H5O_COPY_WITHOUT_ATTR_FLAG flag
 * of H5Ocopy.
 */
static int
test_object_copy_no_attributes(void)
{
    H5O_info2_t object_info;
    htri_t      object_link_exists;
    size_t      i;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID;
    hid_t       group_id        = H5I_INVALID_HID;
    hid_t       group_id2       = H5I_INVALID_HID;
    hid_t       tmp_group_id    = H5I_INVALID_HID;
    hid_t       dset_id         = H5I_INVALID_HID;
    hid_t       tmp_dset_id     = H5I_INVALID_HID;
    hid_t       dset_dtype      = H5I_INVALID_HID;
    hid_t       dtype_id        = H5I_INVALID_HID;
    hid_t       tmp_dtype_id    = H5I_INVALID_HID;
    hid_t       attr_id         = H5I_INVALID_HID;
    hid_t       attr_space_id   = H5I_INVALID_HID;
    hid_t       space_id        = H5I_INVALID_HID;
    hid_t       ocpypl_id       = H5I_INVALID_HID;

    TESTING_MULTIPART("object copying with H5O_COPY_WITHOUT_ATTR_FLAG flag");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, link, dataset, attribute, or stored "
               "datatype aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_NO_ATTRS_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(OBJECT_COPY_NO_ATTRS_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;
    if ((attr_space_id = generate_random_dataspace(OBJECT_COPY_NO_ATTRS_TEST_SPACE_RANK, NULL, NULL, true)) <
        0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    /* Create the test group object, along with the attributes attached to it. */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_NO_ATTRS_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_NO_ATTRS_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(group_id2, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on group '%s'\n", attr_name,
                   OBJECT_COPY_NO_ATTRS_TEST_GROUP_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    /* Create the test dataset object, along with the attributes attached to it. */
    if ((dset_id = H5Dcreate2(group_id, OBJECT_COPY_NO_ATTRS_TEST_DSET_NAME, dset_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_NO_ATTRS_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(dset_id, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on dataset '%s'\n", attr_name,
                   OBJECT_COPY_NO_ATTRS_TEST_DSET_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    /* Create the test committed datatype object, along with the attributes attached to it. */
    if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, OBJECT_COPY_NO_ATTRS_TEST_DTYPE_NAME, dtype_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_DTYPE_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_NO_ATTRS_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(dtype_id, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on committed datatype '%s'\n", attr_name,
                   OBJECT_COPY_NO_ATTRS_TEST_DTYPE_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_group_no_attributes)
        {
            TESTING_2("H5Ocopy on a group (without attributes)");

            if ((ocpypl_id = H5Pcreate(H5P_OBJECT_COPY)) < 0) {
                H5_FAILED();
                printf("    couldn't create OCopyPL\n");
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if (H5Pset_copy_object(ocpypl_id, H5O_COPY_WITHOUT_ATTR_FLAG) < 0) {
                H5_FAILED();
                printf("    couldn't set object copying options\n");
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if (H5Ocopy(group_id, OBJECT_COPY_NO_ATTRS_TEST_GROUP_NAME, group_id,
                        OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME, ocpypl_id, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy group '%s' to '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_GROUP_NAME,
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied group exists\n",
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied group didn't exist!\n",
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            /* Ensure that the new group has no attributes */
            if ((tmp_group_id = H5Gopen2(group_id, OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    failed to open group copy '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to non-zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 1;

            if (H5Oget_info3(tmp_group_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if (object_info.num_attrs != 0) {
                H5_FAILED();
                printf("    copied group contained attributes after a non-attribute copy!\n");
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if (H5Pclose(ocpypl_id) < 0) {
                H5_FAILED();
                printf("    failed to close OCopyPL\n");
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            if (H5Gclose(tmp_group_id) < 0) {
                H5_FAILED();
                printf("    failed to close group copy\n");
                PART_ERROR(H5Ocopy_group_no_attributes);
            }

            PASSED();
        }
        PART_END(H5Ocopy_group_no_attributes);

        if (ocpypl_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(ocpypl_id);
            }
            H5E_END_TRY
            ocpypl_id = H5I_INVALID_HID;
        }
        if (tmp_group_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(tmp_group_id);
            }
            H5E_END_TRY
            tmp_group_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_dset_no_attributes)
        {
            TESTING_2("H5Ocopy on a dataset (without attributes)");

            if ((ocpypl_id = H5Pcreate(H5P_OBJECT_COPY)) < 0) {
                H5_FAILED();
                printf("    couldn't create OCopyPL\n");
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if (H5Pset_copy_object(ocpypl_id, H5O_COPY_WITHOUT_ATTR_FLAG) < 0) {
                H5_FAILED();
                printf("    couldn't set object copying options\n");
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if (H5Ocopy(group_id, OBJECT_COPY_NO_ATTRS_TEST_DSET_NAME, group_id,
                        OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME, ocpypl_id, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy dataset '%s' to '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_DSET_NAME,
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied dataset exists\n",
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied dataset didn't exist!\n",
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            /* Ensure that the new dataset doesn't have any attributes */
            if ((tmp_dset_id = H5Dopen2(group_id, OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    failed to open dataset copy '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to non-zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 1;

            if (H5Oget_info3(tmp_dset_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if (object_info.num_attrs != 0) {
                H5_FAILED();
                printf("    copied dataset contained attributes after a non-attribute copy!\n");
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if (H5Pclose(ocpypl_id) < 0) {
                H5_FAILED();
                printf("    failed to close OCopyPL\n");
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            if (H5Dclose(tmp_dset_id) < 0) {
                H5_FAILED();
                printf("    failed to close dataset copy\n");
                PART_ERROR(H5Ocopy_dset_no_attributes);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dset_no_attributes);

        if (ocpypl_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(ocpypl_id);
            }
            H5E_END_TRY
            ocpypl_id = H5I_INVALID_HID;
        }
        if (tmp_dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(tmp_dset_id);
            }
            H5E_END_TRY
            tmp_dset_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_dtype_no_attributes)
        {
            TESTING_2("H5Ocopy on a committed datatype (without attributes)");

            if ((ocpypl_id = H5Pcreate(H5P_OBJECT_COPY)) < 0) {
                H5_FAILED();
                printf("    couldn't create OCopyPL\n");
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if (H5Pset_copy_object(ocpypl_id, H5O_COPY_WITHOUT_ATTR_FLAG) < 0) {
                H5_FAILED();
                printf("    couldn't set object copying options\n");
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if (H5Ocopy(group_id, OBJECT_COPY_NO_ATTRS_TEST_DTYPE_NAME, group_id,
                        OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME, ocpypl_id, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy datatype '%s' to '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_DTYPE_NAME,
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied datatype exists\n",
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied datatype didn't exist!\n",
                       OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            /* Ensure that the new committed datatype doesn't have any attributes */
            if ((tmp_dtype_id = H5Topen2(group_id, OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    failed to open dataset copy '%s'\n", OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to non-zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 1;

            if (H5Oget_info3(tmp_dtype_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if (object_info.num_attrs != 0) {
                H5_FAILED();
                printf("    copied committed datatype contained attributes after a non-attribute copy!\n");
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if (H5Pclose(ocpypl_id) < 0) {
                H5_FAILED();
                printf("    failed to close OCopyPL\n");
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            if (H5Tclose(tmp_dtype_id) < 0) {
                H5_FAILED();
                printf("    failed to close datatype copy\n");
                PART_ERROR(H5Ocopy_dtype_no_attributes);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dtype_no_attributes);

        if (ocpypl_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Pclose(ocpypl_id);
            }
            H5E_END_TRY
            ocpypl_id = H5I_INVALID_HID;
        }
        if (tmp_dtype_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(tmp_dtype_id);
            }
            H5E_END_TRY
            tmp_dtype_id = H5I_INVALID_HID;
        }
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Pclose(ocpypl_id);
        H5Sclose(attr_space_id);
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Tclose(dset_dtype);
        H5Tclose(tmp_dtype_id);
        H5Tclose(dtype_id);
        H5Dclose(tmp_dset_id);
        H5Dclose(dset_id);
        H5Gclose(tmp_group_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Tests to exercise the behavior of H5Ocopy when the source
 * object specified is a soft link or dangling soft link.
 */
static int
test_object_copy_by_soft_link(void)
{
    H5O_info2_t object_info;
    H5G_info_t  group_info;
    H5L_info2_t link_info;
    htri_t      object_link_exists;
    size_t      i;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID;
    hid_t       group_id        = H5I_INVALID_HID;
    hid_t       group_id2       = H5I_INVALID_HID;
    hid_t       tmp_group_id    = H5I_INVALID_HID;
    hid_t       attr_id         = H5I_INVALID_HID;
    hid_t       attr_space_id   = H5I_INVALID_HID;

    TESTING_MULTIPART("object copying through use of soft links");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_SOFT_LINKS)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, link, dataset, attribute, iterate, or "
               "soft link aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_SOFT_LINK_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_COPY_SOFT_LINK_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((attr_space_id = generate_random_dataspace(OBJECT_COPY_SOFT_LINK_TEST_SPACE_RANK, NULL, NULL, true)) <
        0)
        TEST_ERROR;

    /* Create the test group object, along with its nested members and the attributes attached to it. */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_SOFT_LINK_TEST_NUM_NESTED_OBJS; i++) {
        char grp_name[OBJECT_COPY_SOFT_LINK_TEST_BUF_SIZE];

        snprintf(grp_name, OBJECT_COPY_SOFT_LINK_TEST_BUF_SIZE, "grp%d", (int)i);

        if ((tmp_group_id = H5Gcreate2(group_id2, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create group '%s' under group '%s'\n", grp_name,
                   OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME);
            goto error;
        }

        /* Create a further nested group under the last group added */
        if (i == (OBJECT_COPY_SOFT_LINK_TEST_NUM_NESTED_OBJS - 1)) {
            if (H5Gclose(H5Gcreate2(tmp_group_id, OBJECT_COPY_SOFT_LINK_TEST_DEEP_NESTED_GROUP_NAME,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create nested group '%s' under group '%s'\n",
                       OBJECT_COPY_SOFT_LINK_TEST_DEEP_NESTED_GROUP_NAME, grp_name);
                goto error;
            }
        }

        if (H5Gclose(tmp_group_id) < 0) {
            H5_FAILED();
            printf("    couldn't close group '%s'\n", grp_name);
            goto error;
        }
    }

    for (i = 0; i < (size_t)OBJECT_COPY_SOFT_LINK_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_SOFT_LINK_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_SOFT_LINK_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(group_id2, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on group '%s'\n", attr_name,
                   OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_through_soft_link)
        {
            TESTING_2("H5Ocopy through use of a soft link");

            if (H5Lcreate_soft("/" OBJECT_TEST_GROUP_NAME "/" OBJECT_COPY_SOFT_LINK_TEST_SUBGROUP_NAME
                               "/" OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME,
                               group_id, OBJECT_COPY_SOFT_LINK_TEST_SOFT_LINK_NAME, H5P_DEFAULT,
                               H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to create soft link '%s' to group for copying\n",
                       OBJECT_COPY_SOFT_LINK_TEST_SOFT_LINK_NAME);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (H5Ocopy(group_id, OBJECT_COPY_SOFT_LINK_TEST_SOFT_LINK_NAME, group_id,
                        OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy group '%s' to '%s'\n", OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME,
                       OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied group exists\n",
                       OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied group didn't exist!\n",
                       OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            /* Make sure the new object is an actual group and not another soft link */
            memset(&link_info, 0, sizeof(link_info));
            if (H5Lget_info2(group_id, OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME, &link_info, H5P_DEFAULT) <
                0) {
                H5_FAILED();
                printf("    failed to retrieve info for link '%s'\n",
                       OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (link_info.type != H5L_TYPE_HARD) {
                H5_FAILED();
                printf(
                    "    after group copy through soft link, group's new link type wasn't H5L_TYPE_HARD!\n");
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            /*
             * Ensure that the new group doesn't have any attributes and only the
             * immediate members of the copied group.
             */
            if ((tmp_group_id = H5Gopen2(group_id, OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) <
                0) {
                H5_FAILED();
                printf("    failed to open group copy '%s'\n", OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            memset(&group_info, 0, sizeof(group_info));

            /*
             * Set link count to zero in case the connector doesn't support
             * retrieval of group info.
             */
            group_info.nlinks = 0;

            if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                H5_FAILED();
                printf("    failed to retrieve group info\n");
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (group_info.nlinks != OBJECT_COPY_SOFT_LINK_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    copied group contained %d members instead of %d members after a shallow copy!\n",
                       (int)group_info.nlinks, OBJECT_COPY_SOFT_LINK_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_group_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied group didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_group_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied group's attributes\n");
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (i != OBJECT_COPY_SOFT_LINK_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf(
                    "    number of attributes on copied group (%llu) didn't match expected number (%llu)!\n",
                    (unsigned long long)i, (unsigned long long)OBJECT_COPY_SOFT_LINK_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            if (H5Gclose(tmp_group_id) < 0) {
                H5_FAILED();
                printf("    failed to close group copy\n");
                PART_ERROR(H5Ocopy_through_soft_link);
            }

            PASSED();
        }
        PART_END(H5Ocopy_through_soft_link);

        if (tmp_group_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(tmp_group_id);
            }
            H5E_END_TRY
            tmp_group_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_through_dangling_soft_link)
        {
            herr_t err_ret;

            TESTING_2("H5Ocopy through use of a dangling soft link");

            if (H5Lcreate_soft("/" OBJECT_TEST_GROUP_NAME "/" OBJECT_COPY_SOFT_LINK_TEST_SUBGROUP_NAME
                               "/nonexistent_object",
                               group_id, OBJECT_COPY_SOFT_LINK_TEST_DANGLING_LINK_NAME, H5P_DEFAULT,
                               H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to create dangling soft link '%s'\n",
                       OBJECT_COPY_SOFT_LINK_TEST_DANGLING_LINK_NAME);
                PART_ERROR(H5Ocopy_through_dangling_soft_link);
            }

            H5E_BEGIN_TRY
            {
                err_ret =
                    H5Ocopy(group_id, OBJECT_COPY_SOFT_LINK_TEST_DANGLING_LINK_NAME, group_id,
                            OBJECT_COPY_SOFT_LINK_TEST_DANGLING_LINK_NAME "2", H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    copied non-existent object through use of a dangling soft link!\n");
                PART_ERROR(H5Ocopy_through_dangling_soft_link);
            }

            PASSED();
        }
        PART_END(H5Ocopy_through_dangling_soft_link);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
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
        H5Aclose(attr_id);
        H5Gclose(tmp_group_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Tests for copying groups that contain soft links with
 * H5Ocopy. Also tested is the H5O_COPY_EXPAND_SOFT_LINK_FLAG
 * flag.
 */
static int
test_object_copy_group_with_soft_links(void)
{
    H5G_info_t group_info;
    htri_t     object_link_exists;
    size_t     i;
    hid_t      file_id         = H5I_INVALID_HID;
    hid_t      container_group = H5I_INVALID_HID;
    hid_t      group_id        = H5I_INVALID_HID;
    hid_t      group_id2       = H5I_INVALID_HID;
    hid_t      tmp_group_id    = H5I_INVALID_HID;
    hid_t      ocpypl_id       = H5I_INVALID_HID;

    TESTING_MULTIPART("group copying when group contains soft links");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) || !(vol_cap_flags_g & H5VL_CAP_FLAG_SOFT_LINKS)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, link, iterate, or soft link aren't "
               "supported with "
               "this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_SUBGROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n",
               OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_SUBGROUP_NAME);
        goto error;
    }

    /* Create the test group object. */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME, H5P_DEFAULT,
                                H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME);
        goto error;
    }

    /* Create several groups at the root level and add soft links pointing to them inside
     * the test group object.
     */
    for (i = 0; i < (size_t)OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS; i++) {
        char grp_name[OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE];
        char lnk_name[OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE];
        char lnk_target[2 * OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE];

        snprintf(grp_name, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE, "grp%d", (int)i);
        snprintf(lnk_name, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE, "link%d", (int)i);
        snprintf(lnk_target, 2 * OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE,
                 "/" OBJECT_TEST_GROUP_NAME "/" OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_SUBGROUP_NAME "/%s",
                 grp_name);

        if ((tmp_group_id = H5Gcreate2(group_id, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create group '%s' under group '%s'\n", grp_name,
                   OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_SUBGROUP_NAME);
            goto error;
        }

        if (H5Lcreate_soft(lnk_target, group_id2, lnk_name, H5P_DEFAULT, H5P_DEFAULT) < 0) {
            H5_FAILED();
            printf("    failed to create soft link '%s'\n", lnk_name);
            goto error;
        }

        if (H5Gclose(tmp_group_id) < 0) {
            H5_FAILED();
            printf("    couldn't close group '%s'\n", grp_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_dont_expand_soft_links)
        {
            TESTING_2("H5Ocopy on group with soft links (soft links not expanded)");

            if (H5Ocopy(group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME, group_id,
                        OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME, H5P_DEFAULT,
                        H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy group '%s' to '%s'\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME,
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            if ((object_link_exists =
                     H5Lexists(group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME,
                               H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied group exists\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied group didn't exist!\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            /* Ensure that the number of links is the same */
            if ((tmp_group_id =
                     H5Gopen2(group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME,
                              H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open group copy '%s'\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            memset(&group_info, 0, sizeof(group_info));

            /*
             * Set link count to zero in case the connector doesn't support
             * retrieval of group info.
             */
            group_info.nlinks = 0;

            if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                H5_FAILED();
                printf("    failed to retrieve group info\n");
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            if (group_info.nlinks != OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    copied group contained %d members instead of %d members after copy!\n",
                       (int)group_info.nlinks, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            /*
             * Iterate over the links in the copied group and ensure that they're all
             * still soft links with their original values.
             */
            i = 0;
            if (H5Literate2(tmp_group_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_soft_link_non_expand_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over links in group '%s'\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            if (i != OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    number of links in copied group (%llu) didn't match expected number (%llu)!\n",
                       (unsigned long long)i,
                       (unsigned long long)OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            if (H5Gclose(tmp_group_id) < 0) {
                H5_FAILED();
                printf("    failed to close group copy\n");
                PART_ERROR(H5Ocopy_dont_expand_soft_links);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dont_expand_soft_links);

        if (tmp_group_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(tmp_group_id);
            }
            H5E_END_TRY
            tmp_group_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_expand_soft_links)
        {
            TESTING_2("H5Ocopy on group with soft links (soft links expanded)");

            if ((ocpypl_id = H5Pcreate(H5P_OBJECT_COPY)) < 0) {
                H5_FAILED();
                printf("    couldn't create OCopyPL\n");
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (H5Pset_copy_object(ocpypl_id, H5O_COPY_EXPAND_SOFT_LINK_FLAG) < 0) {
                H5_FAILED();
                printf("    couldn't set object copying options\n");
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (H5Ocopy(group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME, group_id,
                        OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME, ocpypl_id,
                        H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy group '%s' to '%s'\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME,
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if ((object_link_exists = H5Lexists(
                     group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied group exists\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied group didn't exist!\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            /* Ensure that the number of links is the same */
            if ((tmp_group_id = H5Gopen2(group_id, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME,
                                         H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open group copy '%s'\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            memset(&group_info, 0, sizeof(group_info));

            /*
             * Set link count to zero in case the connector doesn't support
             * retrieval of group info.
             */
            group_info.nlinks = 0;

            if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                H5_FAILED();
                printf("    failed to retrieve group info\n");
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (group_info.nlinks != OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    copied group contained %d members instead of %d members after copy!\n",
                       (int)group_info.nlinks, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            /*
             * Iterate over the links in the copied group and ensure that they've all
             * been expanded into hard links corresponding to the top-level groups
             * created.
             */
            i = 0;
            if (H5Literate2(tmp_group_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_soft_link_expand_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over links in group '%s'\n",
                       OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (i != OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    number of links in copied group (%llu) didn't match expected number (%llu)!\n",
                       (unsigned long long)i,
                       (unsigned long long)OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (H5Pclose(ocpypl_id) < 0) {
                H5_FAILED();
                printf("    failed to close OCopyPL\n");
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            if (H5Gclose(tmp_group_id) < 0) {
                H5_FAILED();
                printf("    failed to close group copy\n");
                PART_ERROR(H5Ocopy_expand_soft_links);
            }

            PASSED();
        }
        PART_END(H5Ocopy_expand_soft_links);

        if (ocpypl_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(ocpypl_id);
            }
            H5E_END_TRY
            ocpypl_id = H5I_INVALID_HID;
        }
        if (tmp_group_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(tmp_group_id);
            }
            H5E_END_TRY
            tmp_group_id = H5I_INVALID_HID;
        }
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Gclose(group_id2) < 0)
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
        H5Pclose(ocpypl_id);
        H5Gclose(tmp_group_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Tests for copying objects between two different files using
 * H5Ocopy.
 */
static int
test_object_copy_between_files(void)
{
    H5O_info2_t object_info;
    H5G_info_t  group_info;
    htri_t      object_link_exists;
    size_t      i;
    hid_t       file_id         = H5I_INVALID_HID;
    hid_t       file_id2        = H5I_INVALID_HID;
    hid_t       container_group = H5I_INVALID_HID;
    hid_t       group_id        = H5I_INVALID_HID;
    hid_t       group_id2       = H5I_INVALID_HID;
    hid_t       tmp_group_id    = H5I_INVALID_HID;
    hid_t       dset_id         = H5I_INVALID_HID;
    hid_t       tmp_dset_id     = H5I_INVALID_HID;
    hid_t       dset_dtype      = H5I_INVALID_HID;
    hid_t       dtype_id        = H5I_INVALID_HID;
    hid_t       tmp_dtype_id    = H5I_INVALID_HID;
    hid_t       attr_id         = H5I_INVALID_HID;
    hid_t       attr_space_id   = H5I_INVALID_HID;
    hid_t       space_id        = H5I_INVALID_HID;
    hid_t       ocpypl_id       = H5I_INVALID_HID;

    TESTING_MULTIPART("object copying between files");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, link, dataset, attribute, stored "
               "datatype, or iterate aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    /*
     * Create the second file for the between file copying tests.
     */
    if ((file_id2 = H5Fcreate(OBJECT_COPY_BETWEEN_FILES_TEST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_BETWEEN_FILES_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_COPY_BETWEEN_FILES_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((space_id = generate_random_dataspace(OBJECT_COPY_BETWEEN_FILES_TEST_SPACE_RANK, NULL, NULL, false)) <
        0)
        TEST_ERROR;
    if ((attr_space_id =
             generate_random_dataspace(OBJECT_COPY_BETWEEN_FILES_TEST_SPACE_RANK, NULL, NULL, true)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    /* Create the test group object, along with its nested members and the attributes attached to it. */
    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_NESTED_OBJS; i++) {
        char grp_name[OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE];

        snprintf(grp_name, OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE, "grp%d", (int)i);

        if ((tmp_group_id = H5Gcreate2(group_id2, grp_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create group '%s' under group '%s'\n", grp_name,
                   OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME);
            goto error;
        }

        /* Create a further nested group under the last group added */
        if (i == (OBJECT_COPY_BETWEEN_FILES_TEST_NUM_NESTED_OBJS - 1)) {
            if (H5Gclose(H5Gcreate2(tmp_group_id, OBJECT_COPY_BETWEEN_FILES_TEST_DEEP_NESTED_GROUP_NAME,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create nested group '%s' under group '%s'\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_DEEP_NESTED_GROUP_NAME, grp_name);
                goto error;
            }
        }

        if (H5Gclose(tmp_group_id) < 0) {
            H5_FAILED();
            printf("    couldn't close group '%s'\n", grp_name);
            goto error;
        }
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(group_id2, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on group '%s'\n", attr_name,
                   OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    /* Create the test dataset object, along with the attributes attached to it. */
    if ((dset_id = H5Dcreate2(group_id, OBJECT_COPY_BETWEEN_FILES_TEST_DSET_NAME, dset_dtype, space_id,
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", OBJECT_COPY_BETWEEN_FILES_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(dset_id, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on dataset '%s'\n", attr_name,
                   OBJECT_COPY_BETWEEN_FILES_TEST_DSET_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    /* Create the test committed datatype object, along with the attributes attached to it. */
    if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype\n");
        goto error;
    }

    if (H5Tcommit2(group_id, OBJECT_COPY_BETWEEN_FILES_TEST_DTYPE_NAME, dtype_id, H5P_DEFAULT, H5P_DEFAULT,
                   H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", OBJECT_COPY_BETWEEN_FILES_TEST_DTYPE_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS; i++) {
        char attr_name[OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE];

        snprintf(attr_name, OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE, "attr%d", (int)i);

        if ((attr_id = H5Acreate2(dtype_id, attr_name, H5T_NATIVE_INT, attr_space_id, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't create attribute '%s' on committed datatype '%s'\n", attr_name,
                   OBJECT_COPY_BETWEEN_FILES_TEST_DTYPE_NAME);
            goto error;
        }

        if (H5Aclose(attr_id) < 0) {
            H5_FAILED();
            printf("    couldn't close attribute '%s'\n", attr_name);
            goto error;
        }
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_group_between_files)
        {
            TESTING_2("H5Ocopy on group between different files");

            if (H5Ocopy(group_id, OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME, file_id2,
                        OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy group '%s' to second file '%s'\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME,
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_between_files);
            }

            if ((object_link_exists =
                     H5Lexists(file_id2, OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied group exists\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_between_files);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied group in second file '%s' didn't exist!\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME,
                       OBJECT_COPY_BETWEEN_FILES_TEST_FILE_NAME);
                PART_ERROR(H5Ocopy_group_between_files);
            }

            /* Ensure that the new group has all the members of the copied group, and all its attributes */
            if ((tmp_group_id =
                     H5Gopen2(file_id2, OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open group copy '%s'\n", OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME);
                PART_ERROR(H5Ocopy_group_between_files);
            }

            memset(&group_info, 0, sizeof(group_info));

            /*
             * Set link count to zero in case the connector doesn't support
             * retrieval of group info.
             */
            group_info.nlinks = 0;

            if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                H5_FAILED();
                printf("    failed to retrieve group info\n");
                PART_ERROR(H5Ocopy_group_between_files);
            }

            if (group_info.nlinks != OBJECT_COPY_BETWEEN_FILES_TEST_NUM_NESTED_OBJS) {
                H5_FAILED();
                printf("    copied group contained %d members instead of %d members after a deep copy!\n",
                       (int)group_info.nlinks, OBJECT_COPY_BETWEEN_FILES_TEST_NUM_NESTED_OBJS);
                PART_ERROR(H5Ocopy_group_between_files);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_group_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_group_between_files);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied group didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_group_between_files);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_group_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied group's attributes\n");
                PART_ERROR(H5Ocopy_group_between_files);
            }

            if (i != OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf(
                    "    number of attributes on copied group (%llu) didn't match expected number (%llu)!\n",
                    (unsigned long long)i, (unsigned long long)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_group_between_files);
            }

            if (H5Gclose(tmp_group_id) < 0) {
                H5_FAILED();
                printf("    failed to close group copy\n");
                PART_ERROR(H5Ocopy_group_between_files);
            }

            /*
             * Ensure that the last immediate member of the copied group
             * contains its single member after the deep copy.
             */
            {
                char grp_name[OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE];

                snprintf(grp_name, OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE,
                         "/" OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME "/grp%d",
                         OBJECT_COPY_BETWEEN_FILES_TEST_NUM_NESTED_OBJS - 1);

                if ((tmp_group_id = H5Gopen2(file_id2, grp_name, H5P_DEFAULT)) < 0) {
                    H5_FAILED();
                    printf("    failed to open group '%s'\n", grp_name);
                    PART_ERROR(H5Ocopy_group_between_files);
                }

                memset(&group_info, 0, sizeof(group_info));

                /*
                 * Set link count to zero in case the connector doesn't support
                 * retrieval of group info.
                 */
                group_info.nlinks = 0;

                if (H5Gget_info(tmp_group_id, &group_info) < 0) {
                    H5_FAILED();
                    printf("    failed to retrieve group info\n");
                    PART_ERROR(H5Ocopy_group_between_files);
                }

                if (group_info.nlinks != 1) {
                    H5_FAILED();
                    printf("    copied group's immediate members didn't contain nested members after a "
                           "deep copy!\n");
                    PART_ERROR(H5Ocopy_group_between_files);
                }

                if (H5Gclose(tmp_group_id) < 0) {
                    H5_FAILED();
                    printf("    failed to close group '%s'\n", grp_name);
                    PART_ERROR(H5Ocopy_group_between_files);
                }
            }

            PASSED();
        }
        PART_END(H5Ocopy_group_between_files);

        if (tmp_group_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Gclose(tmp_group_id);
            }
            H5E_END_TRY
            tmp_group_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_dset_between_files)
        {
            TESTING_2("H5Ocopy on dataset between different files");

            if (H5Ocopy(group_id, OBJECT_COPY_BETWEEN_FILES_TEST_DSET_NAME, file_id2,
                        OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy dataset '%s' to second file '%s'\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_DSET_NAME,
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            if ((object_link_exists =
                     H5Lexists(file_id2, OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied dataset exists\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied dataset in second file '%s' didn't exist!\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME,
                       OBJECT_COPY_BETWEEN_FILES_TEST_FILE_NAME);
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            /* Ensure that the new dataset has all the attributes of the copied dataset */
            if ((tmp_dset_id =
                     H5Dopen2(file_id2, OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open dataset copy '%s'\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME);
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_dset_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied dataset didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_dset_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied dataset's attributes\n");
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            if (i != OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    number of attributes on copied dataset (%llu) didn't match expected number "
                       "(%llu)!\n",
                       (unsigned long long)i, (unsigned long long)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            if (H5Dclose(tmp_dset_id) < 0) {
                H5_FAILED();
                printf("    failed to close dataset copy\n");
                PART_ERROR(H5Ocopy_dset_between_files);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dset_between_files);

        if (tmp_dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(tmp_dset_id);
            }
            H5E_END_TRY
            tmp_dset_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Ocopy_dtype_between_files)
        {
            TESTING_2("H5Ocopy on committed datatype between different files");

            if (H5Ocopy(group_id, OBJECT_COPY_BETWEEN_FILES_TEST_DTYPE_NAME, file_id2,
                        OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME, H5P_DEFAULT, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    failed to copy committed datatype '%s' to second file '%s'\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_DTYPE_NAME,
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            if ((object_link_exists =
                     H5Lexists(file_id2, OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't determine if link '%s' to copied committed datatype exists\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            if (!object_link_exists) {
                H5_FAILED();
                printf("    link '%s' to copied committed datatype in second file '%s' didn't exist!\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME,
                       OBJECT_COPY_BETWEEN_FILES_TEST_FILE_NAME);
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            /* Ensure that the new committed datatype has all the attributes of the copied committed datatype
             */
            if ((tmp_dtype_id =
                     H5Topen2(file_id2, OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    failed to open committed datatype copy '%s'\n",
                       OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME);
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            memset(&object_info, 0, sizeof(object_info));

            /*
             * Set attribute count to zero in case the connector doesn't
             * support retrieval of object info.
             */
            object_info.num_attrs = 0;

            if (H5Oget_info3(tmp_dtype_id, &object_info, H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    failed to retrieve object info\n");
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            if (object_info.num_attrs == 0) {
                H5_FAILED();
                printf("    copied committed datatype didn't contain any attributes after copy operation!\n");
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            /* Check the attribute names, types, etc. */
            i = 0;
            if (H5Aiterate2(tmp_dtype_id, H5_INDEX_NAME, H5_ITER_INC, NULL,
                            object_copy_attribute_iter_callback, &i) < 0) {
                H5_FAILED();
                printf("    failed to iterate over copied datatype's attributes\n");
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            if (i != OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS) {
                H5_FAILED();
                printf("    number of attributes on copied datatype (%llu) didn't match expected number "
                       "(%llu)!\n",
                       (unsigned long long)i, (unsigned long long)OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS);
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            if (H5Tclose(tmp_dtype_id) < 0) {
                H5_FAILED();
                printf("    failed to close committed datatype copy\n");
                PART_ERROR(H5Ocopy_dtype_between_files);
            }

            PASSED();
        }
        PART_END(H5Ocopy_dtype_between_files);

        if (tmp_dtype_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Tclose(tmp_dtype_id);
            }
            H5E_END_TRY
            tmp_dtype_id = H5I_INVALID_HID;
        }
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id2) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(ocpypl_id);
        H5Sclose(attr_space_id);
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Tclose(dset_dtype);
        H5Tclose(tmp_dtype_id);
        H5Tclose(dtype_id);
        H5Dclose(tmp_dset_id);
        H5Dclose(dset_id);
        H5Gclose(tmp_group_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id2);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Ocopy fails when it
 * is passed invalid parameters.
 */
static int
test_object_copy_invalid_params(void)
{
    herr_t err_ret         = -1;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  group_id2 = H5I_INVALID_HID;

    TESTING_MULTIPART("object copying with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or object aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_COPY_INVALID_PARAMS_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n",
               OBJECT_COPY_INVALID_PARAMS_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((group_id2 = H5Gcreate2(group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                                H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ocopy_invalid_src_loc_id)
        {
            TESTING_2("H5Ocopy with an invalid source location ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(H5I_INVALID_HID, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, group_id,
                                  OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with an invalid source location ID!\n");
                PART_ERROR(H5Ocopy_invalid_src_loc_id);
            }

            PASSED();
        }
        PART_END(H5Ocopy_invalid_src_loc_id);

        PART_BEGIN(H5Ocopy_invalid_src_obj_name)
        {
            TESTING_2("H5Ocopy with an invalid source object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, NULL, group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2,
                                  H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with a NULL source object name!\n");
                PART_ERROR(H5Ocopy_invalid_src_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, "", group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2,
                                  H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with an invalid source object name of ''!\n");
                PART_ERROR(H5Ocopy_invalid_src_obj_name);
            }

            PASSED();
        }
        PART_END(H5Ocopy_invalid_src_obj_name);

        PART_BEGIN(H5Ocopy_invalid_dst_loc_id)
        {
            TESTING_2("H5Ocopy with an invalid destination location ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, H5I_INVALID_HID,
                                  OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2, H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with an invalid destination location ID!\n");
                PART_ERROR(H5Ocopy_invalid_dst_loc_id);
            }

            PASSED();
        }
        PART_END(H5Ocopy_invalid_dst_loc_id);

        PART_BEGIN(H5Ocopy_invalid_dst_obj_name)
        {
            TESTING_2("H5Ocopy with an invalid destination object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, group_id, NULL,
                                  H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with a NULL destination object name!\n");
                PART_ERROR(H5Ocopy_invalid_dst_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, group_id, "",
                                  H5P_DEFAULT, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with an invalid destination object name of ''!\n");
                PART_ERROR(H5Ocopy_invalid_dst_obj_name);
            }

            PASSED();
        }
        PART_END(H5Ocopy_invalid_dst_obj_name);

        PART_BEGIN(H5Ocopy_invalid_ocpypl)
        {
            TESTING_2("H5Ocopy with an invalid OcpyPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, group_id,
                                  OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2, H5I_INVALID_HID, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with an invalid OcpyPL!\n");
                PART_ERROR(H5Ocopy_invalid_ocpypl);
            }

            PASSED();
        }
        PART_END(H5Ocopy_invalid_ocpypl);

        PART_BEGIN(H5Ocopy_invalid_lcpl)
        {
            TESTING_2("H5Ocopy with an invalid LCPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ocopy(group_id, OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME, group_id,
                                  OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2, H5P_DEFAULT, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ocopy succeeded with an invalid LCPL!\n");
                PART_ERROR(H5Ocopy_invalid_lcpl);
            }

            PASSED();
        }
        PART_END(H5Ocopy_invalid_lcpl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Gclose(group_id2) < 0)
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
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Oset_comment(_by_name)/H5Oget_comment(_by_name).
 */
static int
test_object_comments(void)
{
    TESTING("object comments");

    SKIPPED();

    return 0;
}

/*
 * A test to check that H5Oset_comment(_by_name)/H5Oget_comment(_by_name)
 * fail when passed invalid parameters.
 */
static int
test_object_comments_invalid_params(void)
{
    TESTING("object comment ");

    SKIPPED();

    return 0;
}

/*
 * A test for H5Ovisit(_by_name).
 *
 * XXX: Should have test for checking nested object's names/paths.
 */
static int
test_object_visit(void)
{
    size_t   i;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    file_id2        = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    group_id2  = H5I_INVALID_HID;
    hid_t    gcpl_id    = H5I_INVALID_HID;
    hid_t    type_id    = H5I_INVALID_HID;
    hid_t    dset_id    = H5I_INVALID_HID;
    hid_t    dset_dtype = H5I_INVALID_HID;
    hid_t    fspace_id  = H5I_INVALID_HID;
    hid_t    attr_id    = H5I_INVALID_HID;
    hid_t    group_id3  = H5I_INVALID_HID;
    hid_t    group_id4  = H5I_INVALID_HID;
    hid_t    group_id5  = H5I_INVALID_HID;
    hssize_t num_elems  = 0;
    size_t   elem_size  = 0;
    char     visit_filename[H5_API_TEST_FILENAME_MAX_LENGTH];

    TESTING_MULTIPART("object visiting");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, dataset, attribute, stored datatype, or "
               "iterate aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    snprintf(visit_filename, H5_API_TEST_FILENAME_MAX_LENGTH, "%s%s", test_path_prefix,
             OBJECT_VISIT_TEST_FILE_NAME);

    if ((file_id2 = H5Fcreate(visit_filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", OBJECT_VISIT_TEST_FILE_NAME);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create a GCPL\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0) {
            H5_FAILED();
            printf("    couldn't enable link creation order tracking and indexing on GCPL\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_VISIT_TEST_SUBGROUP_NAME, H5P_DEFAULT, gcpl_id,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_VISIT_TEST_SUBGROUP_NAME);
        goto error;
    }

    /* Make sure not to generate too much data for an attribute to hold */
    do {
        if (fspace_id != H5I_INVALID_HID)
            H5Sclose(fspace_id);

        if (dset_dtype != H5I_INVALID_HID)
            H5Tclose(dset_dtype);

        if ((fspace_id = generate_random_dataspace(OBJECT_VISIT_TEST_SPACE_RANK, NULL, NULL, FALSE)) < 0) {
            TEST_ERROR;
        }

        if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, FALSE)) < 0) {
            TEST_ERROR;
        }

        if ((num_elems = H5Sget_simple_extent_npoints(fspace_id)) < 0)
            TEST_ERROR;

        if ((elem_size = H5Tget_size(dset_dtype)) == 0)
            TEST_ERROR;

    } while (((long unsigned int)num_elems * elem_size) > OBJECT_VISIT_TEST_TOTAL_DATA_SIZE_LIMIT);

    if ((type_id = generate_random_datatype(H5T_NO_CLASS, FALSE)) < 0) {
        H5_FAILED();
        printf("    couldn't create datatype '%s'\n", OBJECT_VISIT_TEST_TYPE_NAME);
        goto error;
    }

    if ((attr_id = H5Acreate2(group_id, OBJECT_VISIT_TEST_ATTR_NAME, dset_dtype, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT)) == H5I_INVALID_HID) {
        H5_FAILED();
        printf("    couldn't create attribute '%s' on group '%s'\n", OBJECT_VISIT_TEST_ATTR_NAME,
               OBJECT_VISIT_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((group_id2 = H5Gcreate2(group_id, OBJECT_VISIT_TEST_GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id3 = H5Gcreate2(file_id2, OBJECT_VISIT_TEST_GROUP_NAME_PARENT, H5P_DEFAULT, gcpl_id,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_TEST_GROUP_NAME_PARENT);
        goto error;
    }

    if ((group_id4 = H5Gcreate2(group_id3, OBJECT_VISIT_TEST_GROUP_NAME_CHILD, H5P_DEFAULT, gcpl_id,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_TEST_GROUP_NAME_CHILD);
        goto error;
    }

    if ((group_id5 = H5Gcreate2(group_id4, OBJECT_VISIT_TEST_GROUP_NAME_GRANDCHILD, H5P_DEFAULT, gcpl_id,
                                H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_TEST_GROUP_NAME_GRANDCHILD);
        goto error;
    }

    if ((dset_id = H5Dcreate2(group_id, OBJECT_VISIT_TEST_DSET_NAME, dset_dtype, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", OBJECT_VISIT_TEST_DSET_NAME);
        goto error;
    }

    if (H5Tcommit2(group_id, OBJECT_VISIT_TEST_TYPE_NAME, type_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) <
        0) {
        H5_FAILED();
        printf("    couldn't commit datatype '%s'\n", OBJECT_VISIT_TEST_TYPE_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /*
         * NOTE: A counter is passed to the iteration callback to try to match up
         * the expected objects with a given step throughout all of the following
         * iterations. This is to try and check that the objects are indeed being
         * returned in the correct order.
         */

        PART_BEGIN(H5Ovisit_obj_name_increasing)
        {
            TESTING_2("H5Ovisit by object name in increasing order");

            i = 0;

            if (H5Ovisit3(group_id, H5_INDEX_NAME, H5_ITER_INC, object_visit_callback, &i, H5O_INFO_ALL) <
                0) {
                H5_FAILED();
                printf("    H5Ovisit by object name in increasing order failed\n");
                PART_ERROR(H5Ovisit_obj_name_increasing);
            }

            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_obj_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_obj_name_increasing);

        PART_BEGIN(H5Ovisit_obj_name_decreasing)
        {
            TESTING_2("H5Ovisit by object name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            i = OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit3(group_id, H5_INDEX_NAME, H5_ITER_DEC, object_visit_callback, &i, H5O_INFO_ALL) <
                0) {
                H5_FAILED();
                printf("    H5Ovisit by object name in decreasing order failed\n");
                PART_ERROR(H5Ovisit_obj_name_decreasing);
            }

            if (i != 2 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_obj_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_obj_name_decreasing);

        PART_BEGIN(H5Ovisit_create_order_increasing)
        {
            TESTING_2("H5Ovisit by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_create_order_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 2 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit3(group_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit by creation order in increasing order failed\n");
                PART_ERROR(H5Ovisit_create_order_increasing);
            }

            if (i != 3 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_create_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_create_order_increasing);

        PART_BEGIN(H5Ovisit_create_order_decreasing)
        {
            TESTING_2("H5Ovisit by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_create_order_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 3 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit3(group_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, object_visit_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit by creation order in decreasing order failed\n");
                PART_ERROR(H5Ovisit_create_order_decreasing);
            }

            if (i != 4 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_create_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_create_order_decreasing);

        PART_BEGIN(H5Ovisit_group)
        {
            TESTING_2("H5Ovisit on a group");

            i = 0;

            if (H5Ovisit3(group_id3, H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_simple_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit on a group failed!\n");
                PART_ERROR(H5Ovisit_group);
            }

            if (i != OBJECT_VISIT_TEST_SUBGROUP_LAYERS) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_group);
            }

            PASSED();
        }
        PART_END(H5Ovisit_group);

        PART_BEGIN(H5Ovisit_file)
        {
            TESTING_2("H5Ovisit on a file ID");

            i = 0;

            if (H5Ovisit3(file_id2, H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_simple_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit on a file ID failed!\n");
                PART_ERROR(H5Ovisit_file);
            }

            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_file);
            }

            PASSED();
        }
        PART_END(H5Ovisit_file);

        PART_BEGIN(H5Ovisit_dset)
        {
            TESTING_2("H5Ovisit on a dataset ID");

            if (H5Ovisit3(dset_id, H5_INDEX_NAME, H5_ITER_INC, object_visit_dset_callback, NULL,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit failed\n");
                PART_ERROR(H5Ovisit_dset);
            }

            PASSED();
        }
        PART_END(H5Ovisit_dset);

        PART_BEGIN(H5Ovisit_dtype)
        {
            TESTING_2("H5Ovisit on a committed datatype ID");

            if (H5Ovisit3(type_id, H5_INDEX_NAME, H5_ITER_INC, object_visit_dtype_callback, NULL,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit failed\n");
                PART_ERROR(H5Ovisit_dtype);
            }

            PASSED();
        }
        PART_END(H5Ovisit_dtype);

        PART_BEGIN(H5Ovisit_attr)
        {
            TESTING_2("H5Ovisit on an attribute");

            i = 0;

            if (H5Ovisit3(attr_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_simple_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit on an attribute failed!\n");
                PART_ERROR(H5Ovisit_attr);
            }

            /* Should have same effect as calling H5Ovisit on group_id */
            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_attr);
            }

            PASSED();
        }
        PART_END(H5Ovisit_attr);

        PART_BEGIN(H5Ovisit_by_name_obj_name_increasing)
        {
            TESTING_2("H5Ovisit_by_name by object name in increasing order");

            /* Reset the counter to the appropriate value for the next test */
            i = 0;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(group_id, ".", H5_INDEX_NAME, H5_ITER_INC, object_visit_callback, &i,
                                  H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = 0;

            if (H5Ovisit_by_name3(container_group, OBJECT_VISIT_TEST_SUBGROUP_NAME, H5_INDEX_NAME,
                                  H5_ITER_INC, object_visit_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_obj_name_increasing);

        PART_BEGIN(H5Ovisit_by_name_obj_name_decreasing)
        {
            TESTING_2("H5Ovisit_by_name by object name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            i = OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(group_id, ".", H5_INDEX_NAME, H5_ITER_DEC, object_visit_callback, &i,
                                  H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            if (i != 2 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit_by_name3(container_group, OBJECT_VISIT_TEST_SUBGROUP_NAME, H5_INDEX_NAME,
                                  H5_ITER_DEC, object_visit_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            if (i != 2 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_obj_name_decreasing);

        PART_BEGIN(H5Ovisit_by_name_create_order_increasing)
        {
            TESTING_2("H5Ovisit_by_name by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_by_name_create_order_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 2 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_callback, &i,
                                  H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            if (i != 3 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = 2 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit_by_name3(container_group, OBJECT_VISIT_TEST_SUBGROUP_NAME, H5_INDEX_CRT_ORDER,
                                  H5_ITER_INC, object_visit_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            if (i != 3 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_create_order_increasing);

        PART_BEGIN(H5Ovisit_by_name_create_order_decreasing)
        {
            TESTING_2("H5Ovisit_by_name by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_by_name_create_order_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 3 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC, object_visit_callback, &i,
                                  H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            if (i != 4 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = 3 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit_by_name3(container_group, OBJECT_VISIT_TEST_SUBGROUP_NAME, H5_INDEX_CRT_ORDER,
                                  H5_ITER_DEC, object_visit_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            if (i != 4 * OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_create_order_decreasing);

        PART_BEGIN(H5Ovisit_by_name_file)
        {
            TESTING_2("H5Ovisit_by_name on a file ID");

            i = 0;

            if (H5Ovisit_by_name3(file_id2, "/", H5_INDEX_CRT_ORDER, H5_ITER_INC,
                                  object_visit_simple_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit on a file ID failed!\n");
                PART_ERROR(H5Ovisit_by_name_file);
            }

            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_file);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_file);

        PART_BEGIN(H5Ovisit_by_name_dset)
        {
            TESTING_2("H5Ovisit_by_name on a dataset ID");

            if (H5Ovisit_by_name3(group_id, OBJECT_VISIT_TEST_DSET_NAME, H5_INDEX_NAME, H5_ITER_INC,
                                  object_visit_dset_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name failed\n");
                PART_ERROR(H5Ovisit_by_name_dset);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_dset);

        PART_BEGIN(H5Ovisit_by_name_dtype)
        {
            TESTING_2("H5Ovisit_by_name on a committed datatype ID");

            if (H5Ovisit_by_name3(group_id, OBJECT_VISIT_TEST_TYPE_NAME, H5_INDEX_NAME, H5_ITER_INC,
                                  object_visit_dtype_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name failed\n");
                PART_ERROR(H5Ovisit_by_name_dtype);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_dtype);

        PART_BEGIN(H5Ovisit_by_name_attr)
        {
            TESTING_2("H5Ovisit_by_name on an attribute");

            i = 0;

            if (H5Ovisit_by_name(attr_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_simple_callback,
                                 &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name on an attribute failed!\n");
                PART_ERROR(H5Ovisit_by_name_attr);
            }

            /* Should have same effect as calling H5Ovisit on group_id */
            if (i != OBJECT_VISIT_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_attr);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_attr);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dset_dtype) < 0)
        TEST_ERROR;
    if (H5Tclose(type_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id2) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id3) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id4) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id5) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id2) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(fspace_id);
        H5Tclose(dset_dtype);
        H5Tclose(type_id);
        H5Dclose(dset_id);
        H5Pclose(gcpl_id);
        H5Gclose(group_id2);
        H5Gclose(group_id3);
        H5Gclose(group_id4);
        H5Gclose(group_id5);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
        H5Fclose(file_id2);
    }
    H5E_END_TRY;

    return 1;
}

/*
 * A test for H5Ovisit(_by_name) on soft links. Since
 * H5Ovisit(_by_name) ignores soft links, this test is
 * meant to verify that behavior by placing objects and
 * the soft links pointing to those objects in separate
 * groups. Visiting is done only on the group containing
 * the links to ensure that the objects in the other group
 * do not get visited.
 */
static int
test_object_visit_soft_link(void)
{
    size_t i;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  subgroup_id = H5I_INVALID_HID, subgroup_id2 = H5I_INVALID_HID;
    hid_t  linked_group_id = H5I_INVALID_HID;
    hid_t  gcpl_id         = H5I_INVALID_HID;

    TESTING_MULTIPART("object visiting with soft links");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_SOFT_LINKS)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, soft link, or iterate "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0) {
        H5_FAILED();
        printf("    couldn't create a GCPL\n");
        goto error;
    }

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0) {
            H5_FAILED();
            printf("    couldn't enable link creation order tracking and indexing on GCPL\n");
            goto error;
        }
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_VISIT_SOFT_LINK_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               gcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_SUBGROUP_NAME);
        goto error;
    }

    /* Create group to hold soft links */
    if ((subgroup_id = H5Gcreate2(group_id, OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1, H5P_DEFAULT, gcpl_id,
                                  H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1);
        goto error;
    }

    /* Create group to hold objects pointed to by soft links */
    if ((subgroup_id2 = H5Gcreate2(group_id, OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2, H5P_DEFAULT, gcpl_id,
                                   H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2);
        goto error;
    }

    /* Create objects under subgroup 2 */
    if ((linked_group_id = H5Gcreate2(subgroup_id2, OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME1, H5P_DEFAULT,
                                      gcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME1);
        goto error;
    }

    if (H5Gclose(linked_group_id) < 0) {
        H5_FAILED();
        printf("    couldn't close group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME1);
        goto error;
    }

    if ((linked_group_id = H5Gcreate2(subgroup_id2, OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME2, H5P_DEFAULT,
                                      gcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME2);
        goto error;
    }

    if (H5Gclose(linked_group_id) < 0) {
        H5_FAILED();
        printf("    couldn't close group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME2);
        goto error;
    }

    if ((linked_group_id = H5Gcreate2(subgroup_id2, OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME3, H5P_DEFAULT,
                                      gcpl_id, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME3);
        goto error;
    }

    if (H5Gclose(linked_group_id) < 0) {
        H5_FAILED();
        printf("    couldn't close group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME3);
        goto error;
    }

    if (H5Gclose(subgroup_id2) < 0) {
        H5_FAILED();
        printf("    couldn't close group '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2);
        goto error;
    }

    /* Create soft links under subgroup 1 to point to the previously-created objects */
    if (H5Lcreate_soft("/" OBJECT_TEST_GROUP_NAME "/" OBJECT_VISIT_SOFT_LINK_TEST_SUBGROUP_NAME
                       "/" OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2 "/" OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME1,
                       subgroup_id, OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME1, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't create soft link '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME1);
        goto error;
    }

    if (H5Lcreate_soft("/" OBJECT_TEST_GROUP_NAME "/" OBJECT_VISIT_SOFT_LINK_TEST_SUBGROUP_NAME
                       "/" OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2 "/" OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME2,
                       subgroup_id, OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME2, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't create soft link '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME2);
        goto error;
    }

    if (H5Lcreate_soft("/" OBJECT_TEST_GROUP_NAME "/" OBJECT_VISIT_SOFT_LINK_TEST_SUBGROUP_NAME
                       "/" OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2 "/" OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME3,
                       subgroup_id, OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME3, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("    couldn't create soft link '%s'\n", OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME3);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        /*
         * NOTE: A counter is passed to the iteration callback to try to match up
         * the expected objects with a given step throughout all of the following
         * iterations. This is to try and check that the objects are indeed being
         * returned in the correct order.
         */

        PART_BEGIN(H5Ovisit_obj_name_increasing)
        {
            TESTING_2("H5Ovisit by object name in increasing order");

            i = 0;

            if (H5Ovisit3(subgroup_id, H5_INDEX_NAME, H5_ITER_INC, object_visit_soft_link_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit by object name in increasing order failed\n");
                PART_ERROR(H5Ovisit_obj_name_increasing);
            }

            if (i != OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_obj_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_obj_name_increasing);

        PART_BEGIN(H5Ovisit_obj_name_decreasing)
        {
            TESTING_2("H5Ovisit by object name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            i = OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit3(subgroup_id, H5_INDEX_NAME, H5_ITER_DEC, object_visit_soft_link_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit by object name in decreasing order failed\n");
                PART_ERROR(H5Ovisit_obj_name_decreasing);
            }

            if (i != 2 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_obj_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_obj_name_decreasing);

        PART_BEGIN(H5Ovisit_create_order_increasing)
        {
            TESTING_2("H5Ovisit by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_create_order_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 2 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit3(subgroup_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, object_visit_soft_link_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit by creation order in increasing order failed\n");
                PART_ERROR(H5Ovisit_create_order_increasing);
            }

            if (i != 3 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_create_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_create_order_increasing);

        PART_BEGIN(H5Ovisit_create_order_decreasing)
        {
            TESTING_2("H5Ovisit by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_create_order_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 3 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            if (H5Ovisit3(subgroup_id, H5_INDEX_CRT_ORDER, H5_ITER_DEC, object_visit_soft_link_callback, &i,
                          H5O_INFO_ALL) < 0) {
                H5_FAILED();
                printf("    H5Ovisit by creation order in decreasing order failed\n");
                PART_ERROR(H5Ovisit_create_order_decreasing);
            }

            if (i != 4 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_create_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_create_order_decreasing);

        PART_BEGIN(H5Ovisit_by_name_obj_name_increasing)
        {
            TESTING_2("H5Ovisit_by_name by object name in increasing order");

            /* Reset the counter to the appropriate value for the next test */
            i = 0;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(subgroup_id, ".", H5_INDEX_NAME, H5_ITER_INC,
                                  object_visit_soft_link_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            if (i != OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = 0;

            /* Repeat the test using an indirect object name */
            if (H5Ovisit_by_name3(group_id, OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1, H5_INDEX_NAME,
                                  H5_ITER_INC, object_visit_soft_link_callback, &i, H5O_INFO_ALL,
                                  H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            if (i != OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_obj_name_increasing);

        PART_BEGIN(H5Ovisit_by_name_obj_name_decreasing)
        {
            TESTING_2("H5Ovisit_by_name by object name in decreasing order");

            /* Reset the counter to the appropriate value for the next test */
            i = OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(subgroup_id, ".", H5_INDEX_NAME, H5_ITER_DEC,
                                  object_visit_soft_link_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            if (i != 2 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            /* Repeat the test using an indirect object name */
            if (H5Ovisit_by_name3(group_id, OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1, H5_INDEX_NAME,
                                  H5_ITER_DEC, object_visit_soft_link_callback, &i, H5O_INFO_ALL,
                                  H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by object name in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            if (i != 2 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_obj_name_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_obj_name_decreasing);

        PART_BEGIN(H5Ovisit_by_name_create_order_increasing)
        {
            TESTING_2("H5Ovisit_by_name by creation order in increasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_by_name_create_order_increasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 2 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(subgroup_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC,
                                  object_visit_soft_link_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            if (i != 3 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = 2 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            /* Repeat the test using an indirect object name */
            if (H5Ovisit_by_name3(group_id, OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1, H5_INDEX_CRT_ORDER,
                                  H5_ITER_INC, object_visit_soft_link_callback, &i, H5O_INFO_ALL,
                                  H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in increasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            if (i != 3 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_increasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_create_order_increasing);

        PART_BEGIN(H5Ovisit_by_name_create_order_decreasing)
        {
            TESTING_2("H5Ovisit_by_name by creation order in decreasing order");

            if (!(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
                SKIPPED();
                printf("    creation order tracking isn't supported with this VOL connector\n");
                PART_EMPTY(H5Ovisit_by_name_create_order_decreasing);
            }

            /* Reset the counter to the appropriate value for the next test */
            i = 3 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            /* First, test visiting using "." for the object name */
            if (H5Ovisit_by_name3(subgroup_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_DEC,
                                  object_visit_soft_link_callback, &i, H5O_INFO_ALL, H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            if (i != 4 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            /* Reset the special counter and repeat the test using an indirect object name. */
            i = 3 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED;

            /* Repeat the test using an indirect object name */
            if (H5Ovisit_by_name3(group_id, OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1, H5_INDEX_CRT_ORDER,
                                  H5_ITER_DEC, object_visit_soft_link_callback, &i, H5O_INFO_ALL,
                                  H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name by creation order in decreasing order failed\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            if (i != 4 * OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED) {
                H5_FAILED();
                printf("    some objects were not visited!\n");
                PART_ERROR(H5Ovisit_by_name_create_order_decreasing);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_create_order_decreasing);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(gcpl_id) < 0)
        TEST_ERROR;
    if (H5Gclose(subgroup_id) < 0)
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
        H5Pclose(gcpl_id);
        H5Gclose(linked_group_id);
        H5Gclose(subgroup_id);
        H5Gclose(subgroup_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Ovisit(_by_name) fails when
 * it is passed invalid parameters.
 */
static int
test_object_visit_invalid_params(void)
{
    herr_t err_ret         = -1;
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  group_id2 = H5I_INVALID_HID;

    TESTING_MULTIPART("object visiting with invalid parameters");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ITERATE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or iterate aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_VISIT_INVALID_PARAMS_TEST_SUBGROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               OBJECT_VISIT_INVALID_PARAMS_TEST_SUBGROUP_NAME);
        goto error;
    }

    if ((group_id2 = H5Gcreate2(group_id, OBJECT_VISIT_INVALID_PARAMS_TEST_GROUP_NAME, H5P_DEFAULT,
                                H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create group '%s'\n", OBJECT_VISIT_INVALID_PARAMS_TEST_GROUP_NAME);
        goto error;
    }

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Ovisit_invalid_obj_id)
        {
            TESTING_2("H5Ovisit with an invalid object ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit3(H5I_INVALID_HID, H5_INDEX_NAME, H5_ITER_INC, object_visit_noop_callback,
                                    NULL, H5O_INFO_ALL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit succeeded with an invalid object ID!\n");
                PART_ERROR(H5Ovisit_invalid_obj_id);
            }

            PASSED();
        }
        PART_END(H5Ovisit_invalid_obj_id);

        PART_BEGIN(H5Ovisit_invalid_index_type)
        {
            TESTING_2("H5Ovisit with an invalid index type");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit3(group_id, H5_INDEX_UNKNOWN, H5_ITER_INC, object_visit_noop_callback, NULL,
                                    H5O_INFO_ALL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit succeeded with invalid index type H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Ovisit_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit3(group_id, H5_INDEX_N, H5_ITER_INC, object_visit_noop_callback, NULL,
                                    H5O_INFO_ALL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit succeeded with invalid index type H5_INDEX_N!\n");
                PART_ERROR(H5Ovisit_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Ovisit_invalid_index_type);

        PART_BEGIN(H5Ovisit_invalid_iter_order)
        {
            TESTING_2("H5Ovisit with an invalid iteration ordering");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit3(group_id, H5_INDEX_NAME, H5_ITER_UNKNOWN, object_visit_noop_callback,
                                    NULL, H5O_INFO_ALL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit succeeded with invalid iteration ordering H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Ovisit_invalid_iter_order);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit3(group_id, H5_INDEX_NAME, H5_ITER_N, object_visit_noop_callback, NULL,
                                    H5O_INFO_ALL);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit succeeded with invalid iteration ordering H5_ITER_N!\n");
                PART_ERROR(H5Ovisit_invalid_iter_order);
            }

            PASSED();
        }
        PART_END(H5Ovisit_invalid_iter_order);

        PART_BEGIN(H5Ovisit_by_name_invalid_loc_id)
        {
            TESTING_2("H5Ovisit_by_name with an invalid location ID");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(H5I_INVALID_HID, ".", H5_INDEX_NAME, H5_ITER_N,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with an invalid location ID!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_loc_id);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_invalid_loc_id);

        PART_BEGIN(H5Ovisit_by_name_invalid_obj_name)
        {
            TESTING_2("H5Ovisit_by_name with an invalid object name");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, NULL, H5_INDEX_NAME, H5_ITER_N,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with a NULL object name!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_obj_name);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, "", H5_INDEX_NAME, H5_ITER_N,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with an invalid object name of ''!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_obj_name);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_invalid_obj_name);

        PART_BEGIN(H5Ovisit_by_name_invalid_index_type)
        {
            TESTING_2("H5Ovisit_by_name with an invalid index type");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, ".", H5_INDEX_UNKNOWN, H5_ITER_N,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with invalid index type H5_INDEX_UNKNOWN!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_index_type);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, ".", H5_INDEX_N, H5_ITER_N, object_visit_noop_callback,
                                            NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with invalid index type H5_INDEX_N!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_index_type);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_invalid_index_type);

        PART_BEGIN(H5Ovisit_by_name_invalid_iter_order)
        {
            TESTING_2("H5Ovisit_by_name with an invalid iteration ordering");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, ".", H5_INDEX_NAME, H5_ITER_UNKNOWN,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with invalid iteration ordering H5_ITER_UNKNOWN!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_iter_order);
            }

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, ".", H5_INDEX_NAME, H5_ITER_N,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5P_DEFAULT);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with invalid iteration ordering H5_ITER_N!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_iter_order);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_invalid_iter_order);

        PART_BEGIN(H5Ovisit_by_name_invalid_lapl)
        {
            TESTING_2("H5Ovisit_by_name with an invalid LAPL");

            H5E_BEGIN_TRY
            {
                err_ret = H5Ovisit_by_name3(group_id, ".", H5_INDEX_NAME, H5_ITER_INC,
                                            object_visit_noop_callback, NULL, H5O_INFO_ALL, H5I_INVALID_HID);
            }
            H5E_END_TRY

            if (err_ret >= 0) {
                H5_FAILED();
                printf("    H5Ovisit_by_name succeeded with an invalid LAPL!\n");
                PART_ERROR(H5Ovisit_by_name_invalid_lapl);
            }

            PASSED();
        }
        PART_END(H5Ovisit_by_name_invalid_lapl);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Gclose(group_id2) < 0)
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
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test for H5Oclose.
 */
static int
test_close_object(void)
{
    hid_t file_id         = H5I_INVALID_HID;
    hid_t container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t group_id2  = H5I_INVALID_HID;
    hid_t dtype_id   = H5I_INVALID_HID;
    hid_t dset_id    = H5I_INVALID_HID;
    hid_t dset_dtype = H5I_INVALID_HID;
    hid_t fspace_id  = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Oclose");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_STORED_DATATYPES)) {
        SKIPPED();
        printf("    API functions for basic file, group, object, dataset, attribute, or stored datatype "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_CLOSE_TEST_GROUP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container subgroup '%s'\n", OBJECT_CLOSE_TEST_GROUP_NAME);
        goto error;
    }

    if ((fspace_id = generate_random_dataspace(OBJECT_CLOSE_TEST_SPACE_RANK, NULL, NULL, false)) < 0)
        TEST_ERROR;

    if ((dset_dtype = generate_random_datatype(H5T_NO_CLASS, false)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oclose_group)
        {
            TESTING_2("H5Oclose on a group");

            if ((group_id2 = H5Gcreate2(group_id, OBJECT_CLOSE_TEST_GRP_NAME, H5P_DEFAULT, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create group '%s'\n", OBJECT_CLOSE_TEST_GRP_NAME);
                PART_ERROR(H5Oclose_group);
            }

            H5E_BEGIN_TRY
            {
                H5Gclose(group_id2);
            }
            H5E_END_TRY

            if ((group_id2 = H5Oopen(group_id, OBJECT_CLOSE_TEST_GRP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open group '%s' with H5Oopen\n", OBJECT_CLOSE_TEST_GRP_NAME);
                PART_ERROR(H5Oclose_group);
            }

            if (H5Oclose(group_id2) < 0) {
                H5_FAILED();
                printf("    couldn't close group '%s' with H5Oclose\n", OBJECT_CLOSE_TEST_GRP_NAME);
                PART_ERROR(H5Oclose_group);
            }

            PASSED();
        }
        PART_END(H5Oclose_group);

        PART_BEGIN(H5Oclose_dset)
        {
            TESTING_2("H5Oclose on a dataset");

            if ((dset_id = H5Dcreate2(group_id, OBJECT_CLOSE_TEST_DSET_NAME, dset_dtype, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", OBJECT_CLOSE_TEST_DSET_NAME);
                PART_ERROR(H5Oclose_dset);
            }

            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY

            if ((dset_id = H5Oopen(group_id, OBJECT_CLOSE_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s' with H5Oopen\n", OBJECT_CLOSE_TEST_DSET_NAME);
                PART_ERROR(H5Oclose_dset);
            }

            if (H5Oclose(dset_id) < 0) {
                H5_FAILED();
                printf("    couldn't close dataset '%s' with H5Oclose\n", OBJECT_CLOSE_TEST_DSET_NAME);
                PART_ERROR(H5Oclose_dset);
            }

            PASSED();
        }
        PART_END(H5Oclose_dset);

        PART_BEGIN(H5Oclose_dtype)
        {
            TESTING_2("H5Oclose on a committed datatype");

            if ((dtype_id = generate_random_datatype(H5T_NO_CLASS, false)) < 0) {
                H5_FAILED();
                printf("    couldn't create datatype '%s'\n", OBJECT_CLOSE_TEST_TYPE_NAME);
                PART_ERROR(H5Oclose_dtype);
            }

            if (H5Tcommit2(group_id, OBJECT_CLOSE_TEST_TYPE_NAME, dtype_id, H5P_DEFAULT, H5P_DEFAULT,
                           H5P_DEFAULT) < 0) {
                H5_FAILED();
                printf("    couldn't commit datatype '%s'\n", OBJECT_CLOSE_TEST_TYPE_NAME);
                PART_ERROR(H5Oclose_dtype);
            }

            H5E_BEGIN_TRY
            {
                H5Tclose(dtype_id);
            }
            H5E_END_TRY

            if ((dtype_id = H5Oopen(group_id, OBJECT_CLOSE_TEST_TYPE_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open datatype '%s' with H5Oopen\n", OBJECT_CLOSE_TEST_TYPE_NAME);
                PART_ERROR(H5Oclose_dtype);
            }

            if (H5Oclose(dtype_id) < 0) {
                H5_FAILED();
                printf("    couldn't close datatype '%s' with H5Oclose\n", OBJECT_CLOSE_TEST_TYPE_NAME);
                PART_ERROR(H5Oclose_dtype);
            }

            PASSED();
        }
        PART_END(H5Oclose_dtype);
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
        H5Tclose(dtype_id);
        H5Dclose(dset_id);
        H5Gclose(group_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that H5Oclose fails when it
 * is passed invalid parameters.
 */
static int
test_close_object_invalid_params(void)
{
    herr_t err_ret = -1;
    hid_t  file_id = H5I_INVALID_HID;

    TESTING("H5Oclose with an invalid object ID");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file or object aren't supported with this connector\n");
        return 0;
    }

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    H5E_BEGIN_TRY
    {
        err_ret = H5Oclose(H5I_INVALID_HID);
    }
    H5E_END_TRY

    if (err_ret >= 0) {
        H5_FAILED();
        printf("    H5Oclose succeeded with an invalid object ID!\n");
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
 * A test to check that various objects (file, dataspace, property list,
 * and attribute) can't be closed with H5Oclose.
 */
static int
test_close_invalid_objects(void)
{
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t  attr_dtype    = H5I_INVALID_HID;
    hid_t  attr_space_id = H5I_INVALID_HID;
    hid_t  fapl_id       = H5I_INVALID_HID;
    hid_t  attr_id       = H5I_INVALID_HID;
    herr_t status;

    TESTING_MULTIPART("H5Oclose invalid objects");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & (H5VL_CAP_FLAG_FILE_BASIC)) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or object "
               "aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, OBJECT_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", OBJECT_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, OBJECT_CLOSE_INVALID_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", OBJECT_OPEN_TEST_GROUP_NAME);
        goto error;
    }

    if ((attr_space_id = generate_random_dataspace(OBJECT_CLOSE_INVALID_TEST_SPACE_RANK, NULL, NULL, true)) <
        0)
        TEST_ERROR;

    if ((attr_dtype = generate_random_datatype(H5T_NO_CLASS, true)) < 0)
        TEST_ERROR;

    if ((attr_id = H5Acreate2(group_id, OBJECT_CLOSE_INVALID_TEST_ATTRIBUTE_NAME, attr_dtype, attr_space_id,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Oclose_file)
        {
            TESTING_2("H5Oclose with an invalid object - file");

            H5E_BEGIN_TRY
            {
                status = H5Oclose(file_id);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Oclose succeeded with an invalid object (file)!\n");
                PART_ERROR(H5Oclose_file);
            }

            PASSED();
        }
        PART_END(H5Oclose_file);

        PART_BEGIN(H5Oclose_plist)
        {
            TESTING_2("H5Oclose with an invalid object - property list");

            H5E_BEGIN_TRY
            {
                status = H5Oclose(fapl_id);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Oclose succeeded with an invalid object (property list)!\n");
                PART_ERROR(H5Oclose_plist);
            }

            PASSED();
        }
        PART_END(H5Oclose_plist);

        PART_BEGIN(H5Oclose_dspace)
        {
            TESTING_2("H5Oclose with an invalid object - data space");

            H5E_BEGIN_TRY
            {
                status = H5Oclose(attr_space_id);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Oclose succeeded with an invalid object (data space)!\n");
                PART_ERROR(H5Oclose_dspace);
            }

            PASSED();
        }
        PART_END(H5Oclose_dspace);

        PART_BEGIN(H5Oclose_attribute)
        {
            TESTING_2("H5Oclose with an invalid object - attribute");

            H5E_BEGIN_TRY
            {
                status = H5Oclose(attr_id);
            }
            H5E_END_TRY

            if (status >= 0) {
                H5_FAILED();
                printf("    H5Oclose succeeded with an invalid object (attribute)!\n");
                PART_ERROR(H5Oclose_attribute);
            }

            PASSED();
        }
        PART_END(H5Oclose_attribute);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Tclose(attr_dtype) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(attr_dtype);
        H5Sclose(attr_space_id);
        H5Aclose(attr_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Fclose(file_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return 1;
} /* test_close_invalid_objects */

/*
 * A test for H5Oflush.
 */
static int
test_flush_object(void)
{
    TESTING("H5Oflush");

    SKIPPED();

    return 0;
}

/*
 * A test to check that H5Oflush fails when
 * it is passed invalid parameters.
 */
static int
test_flush_object_invalid_params(void)
{
    TESTING("H5Oflush with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * A test for H5Orefresh.
 */
static int
test_refresh_object(void)
{
    TESTING("H5Orefresh");

    SKIPPED();

    return 0;
}

/*
 * A test to check that H5Orefresh fails when
 * it is passed invalid parameters.
 */
static int
test_refresh_object_invalid_params(void)
{
    TESTING("H5Orefresh with invalid parameters");

    SKIPPED();

    return 0;
}

/*
 * H5Ocopy test callback to check that an object's attributes got copied
 * over successfully to the new object.
 */
static herr_t
object_copy_attribute_iter_callback(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo,
                                    void *op_data)
{
    size_t *counter = (size_t *)op_data;
    htri_t  types_equal;
    char    expected_name[256];
    hid_t   attr_id   = H5I_INVALID_HID;
    hid_t   attr_type = H5I_INVALID_HID;
    herr_t  ret_value = H5_ITER_CONT;

    UNUSED(ainfo);
    UNUSED(op_data);

    snprintf(expected_name, 256, "attr%d", (int)(*counter));

    if (strncmp(attr_name, expected_name, 256)) {
        printf("    attribute name '%s' did not match expected name '%s'\n", attr_name, expected_name);
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    if ((attr_id = H5Aopen(location_id, attr_name, H5P_DEFAULT)) < 0) {
        printf("    failed to open attribute '%s'\n", attr_name);
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    if ((attr_type = H5Aget_type(attr_id)) < 0) {
        printf("    failed to retrieve attribute's datatype\n");
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    if ((types_equal = H5Tequal(attr_type, H5T_NATIVE_INT)) < 0) {
        printf("    failed to determine if attribute's datatype matched what is expected\n");
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    if (!types_equal) {
        printf("    attribute datatype did not match expected H5T_NATIVE_INT\n");
        ret_value = H5_ITER_ERROR;
        goto done;
    }

done:
    if (attr_type >= 0)
        H5Tclose(attr_type);
    if (attr_id >= 0)
        H5Aclose(attr_id);

    (*counter)++;

    return ret_value;
}

/*
 * H5Ocopy callback to check that a copied group's soft links
 * have not been expanded when the default copy options are
 * used.
 */
static herr_t
object_copy_soft_link_non_expand_callback(hid_t group, const char *name, const H5L_info2_t *info,
                                          void *op_data)
{
    size_t *counter      = (size_t *)op_data;
    void   *link_val_buf = NULL;
    char    expected_link_val[OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE];
    herr_t  ret_value = H5_ITER_CONT;

    /* Make sure the link type is soft */
    if (H5L_TYPE_SOFT != info->type) {
        printf("    link type was not H5L_TYPE_SOFT; link must have been expanded!\n");
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    if (NULL == (link_val_buf = calloc(1, info->u.val_size))) {
        printf("    failed to allocate buffer for link value\n");
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    /* Retrieve the link's value */
    if (H5Lget_val(group, name, link_val_buf, info->u.val_size, H5P_DEFAULT) < 0) {
        printf("    failed to retrieve value of link '%s'\n", name);
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    /* Make sure link's value matches what is expected */
    snprintf(expected_link_val, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE,
             "/" OBJECT_TEST_GROUP_NAME "/" OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_SUBGROUP_NAME "/grp%d",
             (int)(*counter));

    if (strncmp(link_val_buf, expected_link_val, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE)) {
        printf("    value '%s' for link '%s' did not match expected value '%s'\n", (char *)link_val_buf, name,
               expected_link_val);
        ret_value = H5_ITER_ERROR;
        goto done;
    }

done:
    if (link_val_buf)
        free(link_val_buf);

    (*counter)++;

    return ret_value;
}

/*
 * H5Ocopy callback to check that a copied group's soft links
 * have been expanded when the H5O_COPY_EXPAND_SOFT_LINK_FLAG
 * flag is specified.
 */
static herr_t
object_copy_soft_link_expand_callback(hid_t group, const char *name, const H5L_info2_t *info, void *op_data)
{
    size_t *counter = (size_t *)op_data;
    char    expected_link_name[OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE];
    herr_t  ret_value = H5_ITER_CONT;

    UNUSED(group);

    /* Make sure the link type is hard */
    if (H5L_TYPE_HARD != info->type) {
        printf("    link type was not H5L_TYPE_HARD; link must not have been expanded!\n");
        ret_value = H5_ITER_ERROR;
        goto done;
    }

    /* Ensure that the link's name still follows the 'link1', 'link2', etc. pattern */
    snprintf(expected_link_name, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE, "link%d", (int)(*counter));

    if (strncmp(name, expected_link_name, OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE)) {
        printf("    link name '%s' did not match expected name '%s'\n", name, expected_link_name);
        ret_value = H5_ITER_ERROR;
        goto done;
    }

done:
    (*counter)++;

    return ret_value;
}

/*
 * H5Ovisit callback to simply iterate recursively through all of the objects in a
 * group and check to make sure their names match what is expected.
 */
static herr_t
object_visit_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info, void *op_data)
{
    size_t *i           = (size_t *)op_data;
    size_t  counter_val = *((size_t *)op_data);
    herr_t  ret_val     = 0;

    UNUSED(o_id);

    if (!strncmp(name, ".", strlen(".") + 1) &&
        (counter_val == 0 || counter_val == 4 || counter_val == 8 || counter_val == 12)) {
        if (H5O_TYPE_GROUP == object_info->type)
            goto done;
        else
            printf("    type for object '%s' was not H5O_TYPE_GROUP\n", name);
    }
    else if (!strncmp(name, OBJECT_VISIT_TEST_GROUP_NAME, strlen(OBJECT_VISIT_TEST_GROUP_NAME) + 1) &&
             (counter_val == 2 || counter_val == 6 || counter_val == 9 || counter_val == 15)) {
        if (H5O_TYPE_GROUP == object_info->type)
            goto done;
        else
            printf("    type for object '%s' was not H5O_TYPE_GROUP\n", name);
    }
    else if (!strncmp(name, OBJECT_VISIT_TEST_DSET_NAME, strlen(OBJECT_VISIT_TEST_DSET_NAME) + 1) &&
             (counter_val == 1 || counter_val == 7 || counter_val == 10 || counter_val == 14)) {
        if (H5O_TYPE_DATASET == object_info->type)
            goto done;
        else
            printf("    type for object '%s' was not H5O_TYPE_DATASET\n", name);
    }
    else if (!strncmp(name, OBJECT_VISIT_TEST_TYPE_NAME, strlen(OBJECT_VISIT_TEST_TYPE_NAME) + 1) &&
             (counter_val == 3 || counter_val == 5 || counter_val == 11 || counter_val == 13)) {
        if (H5O_TYPE_NAMED_DATATYPE == object_info->type)
            goto done;
        else
            printf("    type for object '%s' was not H5O_TYPE_NAMED_DATATYPE\n", name);
    }
    else
        printf("    object '%s' didn't match known names or came in an incorrect order\n", name);

    ret_val = -1;

done:
    (*i)++;

    return ret_val;
}

/*
 * H5Ovisit callback to count the number of visited objects
 */
static herr_t
object_visit_simple_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info, void *op_data)
{
    size_t *i       = (size_t *)op_data;
    herr_t  ret_val = 0;

    UNUSED(o_id);
    UNUSED(object_info);

    if (name)
        goto done;

    ret_val = -1;

done:
    (*i)++;

    return ret_val;
}

/*
 * H5Ovisit callback for visiting a singular dataset.
 */
static herr_t
object_visit_dset_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info, void *op_data)
{
    herr_t ret_val = 0;

    UNUSED(o_id);
    UNUSED(op_data);

    if (strncmp(name, ".", strlen(".") + 1)) {
        printf("    object '%s' didn't match known names\n", name);
        return -1;
    }

    if (H5O_TYPE_DATASET != object_info->type) {
        printf("    object type was not H5O_TYPE_DATASET\n");
        return -1;
    }

    return ret_val;
}

/*
 * H5Ovisit callback for visiting a singular committed datatype.
 */
static herr_t
object_visit_dtype_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info, void *op_data)
{
    herr_t ret_val = 0;

    UNUSED(o_id);
    UNUSED(op_data);

    if (strncmp(name, ".", strlen(".") + 1)) {
        printf("    object '%s' didn't match known names\n", name);
        return -1;
    }

    if (H5O_TYPE_NAMED_DATATYPE != object_info->type) {
        printf("    object type was not H5O_TYPE_NAMED_DATATYPE\n");
        return -1;
    }

    return ret_val;
}

/*
 * H5Ovisit callback for testing ignoring of
 * soft links during object visiting.
 */
static herr_t
object_visit_soft_link_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info, void *op_data)
{
    size_t *i           = (size_t *)op_data;
    size_t  counter_val = *((size_t *)op_data);
    herr_t  ret_val     = 0;

    UNUSED(o_id);

    if (!strcmp(name, OBJECT_VISIT_TEST_GROUP_NAME_PARENT) ||
        !strcmp(name, OBJECT_VISIT_TEST_GROUP_NAME_PARENT "/" OBJECT_VISIT_TEST_GROUP_NAME_CHILD) ||
        !strcmp(name, OBJECT_VISIT_TEST_GROUP_NAME_PARENT "/" OBJECT_VISIT_TEST_GROUP_NAME_CHILD
                                                          "/" OBJECT_VISIT_TEST_GROUP_NAME_GRANDCHILD)) {
        (*i)--;
        goto done;
    }

    if (!strncmp(name, ".", strlen(".") + 1) && (counter_val <= 5)) {
        if (H5O_TYPE_GROUP == object_info->type)
            goto done;
        else
            printf("    type for object '%s' was not H5O_TYPE_GROUP\n", name);
    }
    else
        printf("    object '%s' didn't match known names or came in an incorrect order\n", name);

    ret_val = -1;

done:
    (*i)++;

    return ret_val;
}

/*
 * H5Ovisit callback to simply iterate through all of the objects in a given
 * group.
 */
static herr_t
object_visit_noop_callback(hid_t o_id, const char *name, const H5O_info2_t *object_info, void *op_data)
{
    UNUSED(o_id);
    UNUSED(name);
    UNUSED(object_info);
    UNUSED(op_data);

    return 0;
}

/*
 * Cleanup temporary test files
 */
static void
cleanup_files(void)
{
    char filename[H5_API_TEST_FILENAME_MAX_LENGTH];

    snprintf(filename, H5_API_TEST_FILENAME_MAX_LENGTH, "%s%s", test_path_prefix,
             OBJECT_COPY_BETWEEN_FILES_TEST_FILE_NAME);
    H5Fdelete(filename, H5P_DEFAULT);

    snprintf(filename, H5_API_TEST_FILENAME_MAX_LENGTH, "%s%s", test_path_prefix,
             OBJECT_VISIT_TEST_FILE_NAME);
    H5Fdelete(filename, H5P_DEFAULT);
}

int
H5_api_object_test(void)
{
    size_t i;
    int    nerrors;

    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*              API Object Tests              *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(object_tests); i++) {
        nerrors += (*object_tests[i])() ? 1 : 0;
    }

    printf("\n");

    printf("Cleaning up testing files\n");
    cleanup_files();

    return nerrors;
}
