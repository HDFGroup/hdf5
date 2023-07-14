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

#ifndef H5_API_DATATYPE_TEST_H
#define H5_API_DATATYPE_TEST_H

#include "H5_api_test.h"

int H5_api_datatype_test(void);

/*************************************************
 *                                               *
 *           API Datatype test defines           *
 *                                               *
 *************************************************/

#define DATATYPE_CREATE_TEST_DATASET_DIMS 2
#define DATATYPE_CREATE_TEST_GROUP_NAME   "committed_datatype_creation_test"
#define DATATYPE_CREATE_TEST_TYPE_NAME    "test_type"

#define DATATYPE_CREATE_INVALID_PARAMS_TEST_SPACE_RANK 2
#define DATATYPE_CREATE_INVALID_PARAMS_TEST_GROUP_NAME "committed_datatype_creation_invalid_params_test"
#define DATATYPE_CREATE_INVALID_PARAMS_TEST_TYPE_NAME  "committed_datatype_creation_invalid_params_datatype"

#define DATATYPE_CREATE_ANONYMOUS_GROUP_NAME "anonymous_type_creation_test"
#define DATATYPE_CREATE_ANONYMOUS_TYPE_NAME  "anon_type"

#define DATATYPE_CREATE_ANONYMOUS_INVALID_PARAMS_GROUP_NAME "anonymous_type_creation_invalid_params_test"

#define DATATYPE_CREATE_EMPTY_TYPES_TEST_CMPD_TYPE_NAME "compound_type"
#define DATATYPE_CREATE_EMPTY_TYPES_TEST_ENUM_TYPE_NAME "enum_type"
#define DATATYPE_CREATE_EMPTY_TYPES_TEST_GROUP_NAME     "committed_datatype_empty_types_test"

#define RECOMMIT_COMMITTED_TYPE_TEST_GROUP_NAME "recommit_committed_type_test"

#define DATATYPE_OPEN_TEST_GROUP_NAME "datatype_open_test"
#define DATATYPE_OPEN_TEST_TYPE_NAME  "open_test_datatype"

#define DATATYPE_OPEN_INVALID_PARAMS_TEST_GROUP_NAME "datatype_open_invalid_params_test"
#define DATATYPE_OPEN_INVALID_PARAMS_TEST_TYPE_NAME  "open_invalid_params_test_datatype"

#define DATATYPE_REOPEN_TEST_SPACE_RANK 2
#define DATATYPE_REOPEN_TEST_GROUP_NAME "datatype_reopen_test"

#define DATASET_CREATE_WITH_DATATYPE_TEST_DATASET_DIMS 2
#define DATASET_CREATE_WITH_DATATYPE_TEST_GROUP_NAME   "dataset_create_with_committed_type_test"
#define DATASET_CREATE_WITH_DATATYPE_TEST_TYPE_NAME    "committed_type_test_dtype1"
#define DATASET_CREATE_WITH_DATATYPE_TEST_DSET_NAME    "committed_type_test_dset"

#define ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_SPACE_RANK 2
#define ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_GROUP_NAME "attribute_create_with_committed_type_test"
#define ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_DTYPE_NAME "committed_type_test_dtype2"
#define ATTRIBUTE_CREATE_WITH_DATATYPE_TEST_ATTR_NAME  "committed_type_test_attr"

#define DATATYPE_DELETE_TEST_GROUP_NAME "datatype_deletion_test"
#define DATATYPE_DELETE_TEST_DTYPE_NAME "delete_test_dtype"

#define DATATYPE_RESURRECT_TEST_GROUP_NAME  "datatype_resurrection_test"
#define DATATYPE_RESURRECT_TEST_DTYPE_NAME  "delete_test_dtype"
#define DATATYPE_RESURRECT_TEST_DTYPE_NAME2 "resurrected_dtype"

#define DATATYPE_PROPERTY_LIST_TEST_SUBGROUP_NAME  "datatype_property_list_test_group"
#define DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME1 "property_list_test_datatype1"
#define DATATYPE_PROPERTY_LIST_TEST_DATATYPE_NAME2 "property_list_test_datatype2"

#define PREDEFINED_TYPE_COMMIT_TEST_GROUP_NAME "predefined_type_commit_test"

#define MODIFY_COMMITTED_TYPE_TEST_GROUP_NAME "modify_committed_type_test"

#endif
