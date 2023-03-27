/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef VOL_ATTRIBUTE_TEST_H
#define VOL_ATTRIBUTE_TEST_H

#include "vol_test.h"

int vol_attribute_test(void);

/**************************************************
 *                                                *
 *      VOL connector Attribute test defines      *
 *                                                *
 **************************************************/

#define ATTRIBUTE_CREATE_ON_ROOT_SPACE_RANK 1
#define ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME  "attr_on_root"
#define ATTRIBUTE_CREATE_ON_ROOT_ATTR_NAME2 "attr_on_root2"

#define ATTRIBUTE_CREATE_ON_DATASET_DSET_SPACE_RANK 2
#define ATTRIBUTE_CREATE_ON_DATASET_ATTR_SPACE_RANK 1
#define ATTRIBUTE_CREATE_ON_DATASET_GROUP_NAME      "attr_on_dataset_test"
#define ATTRIBUTE_CREATE_ON_DATASET_DSET_NAME       "dataset_with_attr"
#define ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME       "attr_on_dataset"
#define ATTRIBUTE_CREATE_ON_DATASET_ATTR_NAME2      "attr_on_dataset2"

#define ATTRIBUTE_CREATE_ON_DATATYPE_SPACE_RANK 1
#define ATTRIBUTE_CREATE_ON_DATATYPE_DTYPE_NAME "datatype_with_attr"
#define ATTRIBUTE_CREATE_ON_DATATYPE_GROUP_NAME "attr_on_datatype_test"
#define ATTRIBUTE_CREATE_ON_DATATYPE_ATTR_NAME  "attr_on_datatype"
#define ATTRIBUTE_CREATE_ON_DATATYPE_ATTR_NAME2 "attr_on_datatype2"

#define ATTRIBUTE_CREATE_NULL_DATASPACE_TEST_SUBGROUP_NAME "attr_with_null_space_test"
#define ATTRIBUTE_CREATE_NULL_DATASPACE_TEST_ATTR_NAME     "attr_with_null_space"

#define ATTRIBUTE_CREATE_SCALAR_DATASPACE_TEST_SUBGROUP_NAME "attr_with_scalar_space_test"
#define ATTRIBUTE_CREATE_SCALAR_DATASPACE_TEST_ATTR_NAME     "attr_with_scalar_space"

#define ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_SPACE_RANK 1
#define ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_GROUP_NAME "attr_with_space_in_name_test"
#define ATTRIBUTE_CREATE_WITH_SPACE_IN_NAME_ATTR_NAME  "attr with space in name"

#define ATTRIBUTE_CREATE_INVALID_PARAMS_SPACE_RANK 1
#define ATTRIBUTE_CREATE_INVALID_PARAMS_GROUP_NAME "attribute_create_invalid_params_test"
#define ATTRIBUTE_CREATE_INVALID_PARAMS_ATTR_NAME  "invalid_params_attr"

#define ATTRIBUTE_OPEN_TEST_SPACE_RANK 1
#define ATTRIBUTE_OPEN_TEST_GROUP_NAME "attribute_open_test"
#define ATTRIBUTE_OPEN_TEST_ATTR_NAME  "attribute_open_test_attr"
#define ATTRIBUTE_OPEN_TEST_ATTR_NAME2 ATTRIBUTE_OPEN_TEST_ATTR_NAME "2"
#define ATTRIBUTE_OPEN_TEST_ATTR_NAME3 ATTRIBUTE_OPEN_TEST_ATTR_NAME "3"

#define ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME "attribute_open_invalid_params_test"
#define ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_SPACE_RANK 1
#define ATTRIBUTE_OPEN_INVALID_PARAMS_TEST_ATTR_NAME  "attribute_open_invalid_params_attr"

#define ATTRIBUTE_WRITE_TEST_ATTR_DTYPE_SIZE sizeof(int)
#define ATTRIBUTE_WRITE_TEST_ATTR_DTYPE      H5T_NATIVE_INT
#define ATTRIBUTE_WRITE_TEST_SPACE_RANK      1
#define ATTRIBUTE_WRITE_TEST_GROUP_NAME      "attr_write_test"
#define ATTRIBUTE_WRITE_TEST_ATTR_NAME       "write_test_attr"

#define ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE_SIZE sizeof(int)
#define ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_DTYPE      H5T_NATIVE_INT
#define ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_SPACE_RANK      1
#define ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_GROUP_NAME      "attr_write_invalid_params_test"
#define ATTRIBUTE_WRITE_INVALID_PARAMS_TEST_ATTR_NAME       "invalid_params_write_test_attr"

#define ATTRIBUTE_READ_TEST_ATTR_DTYPE_SIZE sizeof(int)
#define ATTRIBUTE_READ_TEST_ATTR_DTYPE      H5T_NATIVE_INT
#define ATTRIBUTE_READ_TEST_SPACE_RANK      1
#define ATTRIBUTE_READ_TEST_GROUP_NAME      "attr_read_test"
#define ATTRIBUTE_READ_TEST_ATTR_NAME       "read_test_attr"

#define ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE_SIZE sizeof(int)
#define ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_DTYPE      H5T_NATIVE_INT
#define ATTRIBUTE_READ_INVALID_PARAMS_TEST_SPACE_RANK      1
#define ATTRIBUTE_READ_INVALID_PARAMS_TEST_GROUP_NAME      "attr_read_invalid_params_test"
#define ATTRIBUTE_READ_INVALID_PARAMS_TEST_ATTR_NAME       "invalid_params_read_test_attr"

#define ATTRIBUTE_READ_EMPTY_SPACE_RANK      1
#define ATTRIBUTE_READ_EMPTY_ATTR_GROUP_NAME "read_empty_attr_test"
#define ATTRIBUTE_READ_EMPTY_ATTR_NAME       "read_empty_attr"
#define ATTRIBUTE_READ_EMPTY_DTYPE           H5T_NATIVE_INT
#define ATTRIBUTE_READ_EMPTY_DTYPE_SIZE      sizeof(int)

#define ATTRIBUTE_GET_SPACE_TYPE_TEST_SPACE_RANK 1
#define ATTRIBUTE_GET_SPACE_TYPE_TEST_GROUP_NAME "get_attr_space_type_test"
#define ATTRIBUTE_GET_SPACE_TYPE_TEST_ATTR_NAME  "get_space_type_test_attr"

#define ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_SPACE_RANK 1
#define ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_GROUP_NAME "get_attr_space_type_invalid_params_test"
#define ATTRIBUTE_GET_SPACE_TYPE_INVALID_PARAMS_TEST_ATTR_NAME  "get_space_type_invalid_params_test_attr"

#define ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME1 "property_list_test_attribute1"
#define ATTRIBUTE_PROPERTY_LIST_TEST_ATTRIBUTE_NAME2 "property_list_test_attribute2"
#define ATTRIBUTE_PROPERTY_LIST_TEST_SUBGROUP_NAME   "attribute_property_list_test_group"
#define ATTRIBUTE_PROPERTY_LIST_TEST_SPACE_RANK      1

#define ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME  "attr_name_retrieval_attr"
#define ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME2 ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME "2"
#define ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME3 ATTRIBUTE_GET_NAME_TEST_ATTRIBUTE_NAME "3"
#define ATTRIBUTE_GET_NAME_TEST_SPACE_RANK      1
#define ATTRIBUTE_GET_NAME_TEST_GROUP_NAME      "retrieve_attr_name_test"

#define ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_ATTRIBUTE_NAME "invalid_params_attr_name_retrieval_attr"
#define ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_SPACE_RANK     1
#define ATTRIBUTE_GET_NAME_INVALID_PARAMS_TEST_GROUP_NAME     "retrieve_attr_name_invalid_params_test"

#define ATTRIBUTE_GET_INFO_TEST_SPACE_RANK 1
#define ATTRIBUTE_GET_INFO_TEST_GROUP_NAME "attr_get_info_test"
#define ATTRIBUTE_GET_INFO_TEST_ATTR_NAME  "get_info_test_attr"
#define ATTRIBUTE_GET_INFO_TEST_ATTR_NAME2 ATTRIBUTE_GET_INFO_TEST_ATTR_NAME "2"
#define ATTRIBUTE_GET_INFO_TEST_ATTR_NAME3 ATTRIBUTE_GET_INFO_TEST_ATTR_NAME "3"

#define ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_SPACE_RANK 1
#define ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_GROUP_NAME "attr_get_info_invalid_params_test"
#define ATTRIBUTE_GET_INFO_INVALID_PARAMS_TEST_ATTR_NAME  "invalid_params_get_info_test_attr"

#define ATTRIBUTE_RENAME_TEST_SPACE_RANK 1
#define ATTRIBUTE_RENAME_TEST_GROUP_NAME "attr_rename_test"
#define ATTRIBUTE_RENAME_TEST_ATTR_NAME  "rename_test_attr"
#define ATTRIBUTE_RENAME_TEST_ATTR_NAME2 "rename_test_attr2"
#define ATTRIBUTE_RENAME_TEST_NEW_NAME   "renamed_attr"
#define ATTRIBUTE_RENAME_TEST_NEW_NAME2  "renamed_attr2"

#define ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_SPACE_RANK 1
#define ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_GROUP_NAME "attr_rename_invalid_params_test"
#define ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME  "invalid_params_rename_test_attr"
#define ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_ATTR_NAME2 "invalid_params_rename_test_attr2"
#define ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME   "invalid_params_renamed_attr"
#define ATTRIBUTE_RENAME_INVALID_PARAMS_TEST_NEW_NAME2  "invalid_params_renamed_attr2"

#define ATTRIBUTE_ITERATE_TEST_ATTR_NAME_BUF_SIZE  256
#define ATTRIBUTE_ITERATE_TEST_DSET_SPACE_RANK     2
#define ATTRIBUTE_ITERATE_TEST_ATTR_SPACE_RANK     1
#define ATTRIBUTE_ITERATE_TEST_GRP_SUBGROUP_NAME   "attribute_iterate_group_test"
#define ATTRIBUTE_ITERATE_TEST_DSET_SUBGROUP_NAME  "attribute_iterate_dset_test"
#define ATTRIBUTE_ITERATE_TEST_DTYPE_SUBGROUP_NAME "attribute_iterate_datatype_test"
#define ATTRIBUTE_ITERATE_TEST_DSET_NAME           "attribute_iterate_dset"
#define ATTRIBUTE_ITERATE_TEST_DTYPE_NAME          "attribute_iterate_dtype"
#define ATTRIBUTE_ITERATE_TEST_ATTR_NAME           "iter_attr"
#define ATTRIBUTE_ITERATE_TEST_NUM_ATTRS           4

#define ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_SPACE_RANK 2
#define ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_SUBGROUP_NAME   "attribute_iterate_test_0_attributes"
#define ATTRIBUTE_ITERATE_TEST_0_ATTRIBUTES_DSET_NAME       "attribute_iterate_dset"

#define ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_SPACE_RANK 1
#define ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_SUBGROUP_NAME   "attribute_iterate_invalid_params_test"
#define ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME       "invalid_params_iter_attr1"
#define ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME2      "invalid_params_iter_attr2"
#define ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME3      "invalid_params_iter_attr3"
#define ATTRIBUTE_ITERATE_INVALID_PARAMS_TEST_ATTR_NAME4      "invalid_params_iter_attr4"

#define ATTRIBUTE_DELETION_TEST_SPACE_RANK 1
#define ATTRIBUTE_DELETION_TEST_GROUP_NAME "attr_deletion_test"
#define ATTRIBUTE_DELETION_TEST_ATTR_NAME  "attr_to_be_deleted"
#define ATTRIBUTE_DELETION_TEST_ATTR_NAME2 ATTRIBUTE_DELETION_TEST_ATTR_NAME "2"
#define ATTRIBUTE_DELETION_TEST_ATTR_NAME3 ATTRIBUTE_DELETION_TEST_ATTR_NAME "3"

#define ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_SPACE_RANK 1
#define ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_GROUP_NAME "attr_deletion_invalid_params_test"
#define ATTRIBUTE_DELETION_INVALID_PARAMS_TEST_ATTR_NAME  "invalid_params_attr_to_be_deleted"

#define ATTRIBUTE_EXISTS_TEST_GROUP_NAME "attr_exists_test"
#define ATTRIBUTE_EXISTS_TEST_SPACE_RANK 1
#define ATTRIBUTE_EXISTS_TEST_ATTR_NAME  "attr_exists"

#define ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_SPACE_RANK 1
#define ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_GROUP_NAME "attr_exists_invalid_params_test"
#define ATTRIBUTE_EXISTS_INVALID_PARAMS_TEST_ATTR_NAME  "invalid_params_attr_exists"

#define ATTRIBUTE_MANY_GROUP_NAME    "group_for_many_attributes"
#define ATTRIBUTE_MANY_NAME_BUF_SIZE 32U
#define ATTRIBUTE_MANY_NUMB          64U
#define ATTRIBUTE_MANY_SPACE_RANK    1

#define ATTRIBUTE_DUPLICATE_ID_GRP_NAME   "attr_duplicate_open_test"
#define ATTRIBUTE_DUPLICATE_ID_ATTR_NAME  "attr_duplicated_id"
#define ATTRIBUTE_DUPLICATE_ID_SPACE_RANK 1

#define ATTRIBUTE_GET_NUM_ATTRS_TEST_GRP_NAME   "get_num_attrs_test"
#define ATTRIBUTE_GET_NUM_ATTRS_TEST_ATTR_NAME  "get_num_attrs_test_attribute"
#define ATTRIBUTE_GET_NUM_ATTRS_TEST_SPACE_RANK 1

#define ATTRIBUTE_SHARED_DTYPE_NAME       "Datatype"
#define ATTRIBUTE_SHARED_DTYPE_GROUP_NAME "shared_dtype_group"
#define ATTRIBUTE_SHARED_DTYPE_ATTR_NAME  "shared_dtype_attr"
#define ATTRIBUTE_SHARED_DTYPE_DSET_NAME  "shared_dtype_dset"
#define ATTRIBUTE_SHARED_DTYPE_SPACE_RANK 1

#endif
