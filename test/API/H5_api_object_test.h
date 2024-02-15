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

#ifndef H5_API_OBJECT_TEST_H
#define H5_API_OBJECT_TEST_H

#include "H5_api_test.h"

int H5_api_object_test(void);

/***********************************************
 *                                             *
 *           API Object test defines           *
 *                                             *
 ***********************************************/

#define OBJECT_OPEN_TEST_SPACE_RANK 2
#define OBJECT_OPEN_TEST_GROUP_NAME "object_open_test"
#define OBJECT_OPEN_TEST_GRP_NAME   "object_open_test_group"
#define OBJECT_OPEN_TEST_DSET_NAME  "object_open_test_dset"
#define OBJECT_OPEN_TEST_TYPE_NAME  "object_open_test_type"

#define OBJECT_OPEN_INVALID_PARAMS_TEST_GROUP_NAME "object_open_invalid_params_test"
#define OBJECT_OPEN_INVALID_PARAMS_TEST_GRP_NAME   "object_open_invalid_params_test_group"

#define OBJECT_CLOSE_INVALID_TEST_GROUP_NAME     "object_close_invalid_params_test"
#define OBJECT_CLOSE_INVALID_TEST_ATTRIBUTE_NAME "object_close_invalid_test_attribute"
#define OBJECT_CLOSE_INVALID_TEST_SPACE_RANK     2

#define OBJECT_EXISTS_TEST_DSET_SPACE_RANK    2
#define OBJECT_EXISTS_TEST_SUBGROUP_NAME      "object_exists_test"
#define OBJECT_EXISTS_TEST_DANGLING_LINK_NAME "object_exists_test_dangling_soft_link"
#define OBJECT_EXISTS_TEST_SOFT_LINK_NAME     "object_exists_test_soft_link"
#define OBJECT_EXISTS_TEST_GRP_NAME           "object_exists_test_group"
#define OBJECT_EXISTS_TEST_TYPE_NAME          "object_exists_test_type"
#define OBJECT_EXISTS_TEST_DSET_NAME          "object_exists_test_dset"

#define OBJECT_EXISTS_INVALID_PARAMS_TEST_SUBGROUP_NAME "object_exists_invalid_params_test"
#define OBJECT_EXISTS_INVALID_PARAMS_TEST_GRP_NAME      "object_exists_invalid_params_test_group"

#define OBJECT_COPY_BASIC_TEST_DEEP_NESTED_GROUP_NAME "deep_nested_group"
#define OBJECT_COPY_BASIC_TEST_NUM_NESTED_OBJS        3
#define OBJECT_COPY_BASIC_TEST_NEW_GROUP_NAME         "copied_group"
#define OBJECT_COPY_BASIC_TEST_NEW_DSET_NAME          "copied_dset"
#define OBJECT_COPY_BASIC_TEST_NEW_DTYPE_NAME         "copied_dtype"
#define OBJECT_COPY_BASIC_TEST_SUBGROUP_NAME          "object_copy_basic_test"
#define OBJECT_COPY_BASIC_TEST_GROUP_NAME             "group_to_copy"
#define OBJECT_COPY_BASIC_TEST_DSET_NAME              "dset_to_copy"
#define OBJECT_COPY_BASIC_TEST_DTYPE_NAME             "dtype_to_copy"
#define OBJECT_COPY_BASIC_TEST_SPACE_RANK             2
#define OBJECT_COPY_BASIC_TEST_NUM_ATTRS              3
#define OBJECT_COPY_BASIC_TEST_BUF_SIZE               256

#define OBJECT_COPY_ALREADY_EXISTING_TEST_SUBGROUP_NAME "object_copy_existing_objects_test"
#define OBJECT_COPY_ALREADY_EXISTING_TEST_GROUP_NAME    "group_to_copy"
#define OBJECT_COPY_ALREADY_EXISTING_TEST_DSET_NAME     "dset_to_copy"
#define OBJECT_COPY_ALREADY_EXISTING_TEST_DTYPE_NAME    "dtype_to_copy"
#define OBJECT_COPY_ALREADY_EXISTING_TEST_SPACE_RANK    2

#define OBJECT_COPY_SHALLOW_TEST_DEEP_NESTED_GROUP_NAME "deep_nested_group"
#define OBJECT_COPY_SHALLOW_TEST_NUM_NESTED_OBJS        3
#define OBJECT_COPY_SHALLOW_TEST_SUBGROUP_NAME          "object_copy_shallow_group_copy_test"
#define OBJECT_COPY_SHALLOW_TEST_NEW_GROUP_NAME         "copied_group"
#define OBJECT_COPY_SHALLOW_TEST_GROUP_NAME             "group_to_copy"
#define OBJECT_COPY_SHALLOW_TEST_BUF_SIZE               256

#define OBJECT_COPY_NO_ATTRS_TEST_SUBGROUP_NAME  "object_copy_no_attributes_test"
#define OBJECT_COPY_NO_ATTRS_TEST_NEW_GROUP_NAME "copied_group"
#define OBJECT_COPY_NO_ATTRS_TEST_NEW_DSET_NAME  "copied_dset"
#define OBJECT_COPY_NO_ATTRS_TEST_NEW_DTYPE_NAME "copied_dtype"
#define OBJECT_COPY_NO_ATTRS_TEST_GROUP_NAME     "group_to_copy"
#define OBJECT_COPY_NO_ATTRS_TEST_DSET_NAME      "dset_to_copy"
#define OBJECT_COPY_NO_ATTRS_TEST_DTYPE_NAME     "dtype_to_copy"
#define OBJECT_COPY_NO_ATTRS_TEST_SPACE_RANK     2
#define OBJECT_COPY_NO_ATTRS_TEST_NUM_ATTRS      3
#define OBJECT_COPY_NO_ATTRS_TEST_BUF_SIZE       256

#define OBJECT_COPY_SOFT_LINK_TEST_DEEP_NESTED_GROUP_NAME "deep_nested_group"
#define OBJECT_COPY_SOFT_LINK_TEST_DANGLING_LINK_NAME     "dangling_link"
#define OBJECT_COPY_SOFT_LINK_TEST_NUM_NESTED_OBJS        3
#define OBJECT_COPY_SOFT_LINK_TEST_SUBGROUP_NAME          "object_copy_soft_link_test"
#define OBJECT_COPY_SOFT_LINK_TEST_SOFT_LINK_NAME         "soft_link_to_group_to_copy"
#define OBJECT_COPY_SOFT_LINK_TEST_NEW_GROUP_NAME         "copied_group"
#define OBJECT_COPY_SOFT_LINK_TEST_GROUP_NAME             "group_to_copy"
#define OBJECT_COPY_SOFT_LINK_TEST_SPACE_RANK             2
#define OBJECT_COPY_SOFT_LINK_TEST_NUM_ATTRS              3
#define OBJECT_COPY_SOFT_LINK_TEST_BUF_SIZE               256

#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_DANGLING_GROUP_NAME "expanded_dangling_soft_links_group"
#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NON_EXPAND_GROUP_NAME      "non_expanded_soft_links_group"
#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_EXPAND_GROUP_NAME          "expanded_soft_links_group"
#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_NUM_NESTED_OBJS            3
#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_SUBGROUP_NAME              "object_copy_group_with_soft_links_test"
#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_GROUP_NAME                 "group_to_copy"
#define OBJECT_COPY_GROUP_WITH_SOFT_LINKS_TEST_BUF_SIZE                   256

#define OBJECT_COPY_BETWEEN_FILES_TEST_DEEP_NESTED_GROUP_NAME "deep_nested_group"
#define OBJECT_COPY_BETWEEN_FILES_TEST_NUM_NESTED_OBJS        3
#define OBJECT_COPY_BETWEEN_FILES_TEST_SUBGROUP_NAME          "object_copy_between_files_test"
#define OBJECT_COPY_BETWEEN_FILES_TEST_NEW_GROUP_NAME         "copied_group"
#define OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DSET_NAME          "copied_dset"
#define OBJECT_COPY_BETWEEN_FILES_TEST_NEW_DTYPE_NAME         "copied_dtype"
#define OBJECT_COPY_BETWEEN_FILES_TEST_FILE_NAME              "object_copy_test_file.h5"
#define OBJECT_COPY_BETWEEN_FILES_TEST_GROUP_NAME             "group_to_copy"
#define OBJECT_COPY_BETWEEN_FILES_TEST_DSET_NAME              "dset_to_copy"
#define OBJECT_COPY_BETWEEN_FILES_TEST_DTYPE_NAME             "dtype_to_copy"
#define OBJECT_COPY_BETWEEN_FILES_TEST_SPACE_RANK             2
#define OBJECT_COPY_BETWEEN_FILES_TEST_NUM_ATTRS              3
#define OBJECT_COPY_BETWEEN_FILES_TEST_BUF_SIZE               256

#define OBJECT_COPY_INVALID_PARAMS_TEST_SUBGROUP_NAME "object_copy_invalid_params_test"
#define OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME    "object_copy_invalid_params_group"
#define OBJECT_COPY_INVALID_PARAMS_TEST_GROUP_NAME2   "object_copy_invalid_params_group_copy"

#define OBJECT_VISIT_TEST_NUM_OBJS_VISITED      4
#define OBJECT_VISIT_TEST_SUBGROUP_NAME         "object_visit_test"
#define OBJECT_VISIT_TEST_SPACE_RANK            2
#define OBJECT_VISIT_TEST_GROUP_NAME            "object_visit_test_group"
#define OBJECT_VISIT_TEST_DSET_NAME             "object_visit_test_dset"
#define OBJECT_VISIT_TEST_TYPE_NAME             "object_visit_test_type"
#define OBJECT_VISIT_TEST_ATTR_NAME             "object_visit_test_attr"
#define OBJECT_VISIT_TEST_FILE_NAME             "object_visit_test_file"
#define OBJECT_VISIT_TEST_SUBGROUP_LAYERS       3
#define OBJECT_VISIT_TEST_GROUP_NAME_PARENT     "object_visit_test_group_parent"
#define OBJECT_VISIT_TEST_GROUP_NAME_CHILD      "object_visit_test_group_child"
#define OBJECT_VISIT_TEST_GROUP_NAME_GRANDCHILD "object_visit_test_group_grandchild"
#define OBJECT_VISIT_TEST_TOTAL_DATA_SIZE_LIMIT 32000

#define OBJECT_VISIT_SOFT_LINK_TEST_NUM_OBJS_VISITED 1
#define OBJECT_VISIT_SOFT_LINK_TEST_SUBGROUP_NAME    "object_visit_soft_link"
#define OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME1      "links_group"
#define OBJECT_VISIT_SOFT_LINK_TEST_GROUP_NAME2      "objects_group"
#define OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME1       "soft_link1"
#define OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME2       "soft_link2"
#define OBJECT_VISIT_SOFT_LINK_TEST_LINK_NAME3       "soft_link3"
#define OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME1        "group1"
#define OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME2        "group2"
#define OBJECT_VISIT_SOFT_LINK_TEST_OBJ_NAME3        "group3"

#define OBJECT_VISIT_DANGLING_LINK_TEST_SUBGROUP_NAME "object_visit_dangling_link_test"
#define OBJECT_VISIT_DANGLING_LINK_TEST_LINK_NAME1    "dangling_link1"
#define OBJECT_VISIT_DANGLING_LINK_TEST_LINK_NAME2    "dangling_link2"
#define OBJECT_VISIT_DANGLING_LINK_TEST_LINK_NAME3    "dangling_link3"

#define OBJECT_VISIT_INVALID_PARAMS_TEST_SUBGROUP_NAME "object_visit_invalid_params_test"
#define OBJECT_VISIT_INVALID_PARAMS_TEST_GROUP_NAME    "object_visit_invalid_params_group"

#define OBJECT_CLOSE_TEST_SPACE_RANK 2
#define OBJECT_CLOSE_TEST_GROUP_NAME "object_close_test"
#define OBJECT_CLOSE_TEST_GRP_NAME   "object_close_test_group"
#define OBJECT_CLOSE_TEST_DSET_NAME  "object_close_test_dset"
#define OBJECT_CLOSE_TEST_TYPE_NAME  "object_close_test_type"

#define OBJECT_LINK_TEST_GROUP_NAME  "object_link_test_group"
#define OBJECT_LINK_TEST_GROUP_NAME2 "object_link_test_group_link"
#define OBJECT_LINK_TEST_DSET_NAME   "object_link_test_dataset"
#define OBJECT_LINK_TEST_DTYPE_NAME  "object_link_test_datatype"
#define OBJECT_LINK_TEST_SPACE_RANK  2

#define OBJECT_LINK_INVALID_PARAMS_TEST_GROUP_NAME "object_link_invalid_params_test_group"

#define OBJ_REF_GET_TYPE_TEST_SUBGROUP_NAME "obj_ref_get_obj_type_test"
#define OBJ_REF_GET_TYPE_TEST_DSET_NAME     "ref_dset"
#define OBJ_REF_GET_TYPE_TEST_TYPE_NAME     "ref_dtype"
#define OBJ_REF_GET_TYPE_TEST_SPACE_RANK    2

#define OBJ_REF_DATASET_WRITE_TEST_SUBGROUP_NAME "obj_ref_write_test"
#define OBJ_REF_DATASET_WRITE_TEST_REF_DSET_NAME "ref_dset"
#define OBJ_REF_DATASET_WRITE_TEST_REF_TYPE_NAME "ref_dtype"
#define OBJ_REF_DATASET_WRITE_TEST_SPACE_RANK    1
#define OBJ_REF_DATASET_WRITE_TEST_DSET_NAME     "obj_ref_dset"

#define OBJ_REF_DATASET_READ_TEST_SUBGROUP_NAME "obj_ref_read_test"
#define OBJ_REF_DATASET_READ_TEST_REF_DSET_NAME "ref_dset"
#define OBJ_REF_DATASET_READ_TEST_REF_TYPE_NAME "ref_dtype"
#define OBJ_REF_DATASET_READ_TEST_SPACE_RANK    1
#define OBJ_REF_DATASET_READ_TEST_DSET_NAME     "obj_ref_dset"

#define OBJ_REF_DATASET_EMPTY_WRITE_TEST_SUBGROUP_NAME "obj_ref_empty_write_test"
#define OBJ_REF_DATASET_EMPTY_WRITE_TEST_SPACE_RANK    1
#define OBJ_REF_DATASET_EMPTY_WRITE_TEST_DSET_NAME     "obj_ref_dset"

#define OBJECT_REF_COUNT_TEST_SUBGROUP_NAME   "ref_count_test"
#define OBJECT_REF_COUNT_TEST_GRP_NAME        "ref_count_test_group"
#define OBJECT_REF_COUNT_TEST_DSET_NAME       "ref_count_dset"
#define OBJECT_REF_COUNT_TEST_TYPE_NAME       "ref_count_dtype"
#define OBJECT_REF_COUNT_TEST_DSET_SPACE_RANK 2

#endif
