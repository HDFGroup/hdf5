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

#ifndef VOL_DATASET_TEST_H
#define VOL_DATASET_TEST_H

#include "vol_test.h"

int vol_dataset_test(void);

/************************************************
 *                                              *
 *      VOL connector Dataset test defines      *
 *                                              *
 ************************************************/

#define DATASET_CREATE_UNDER_ROOT_DSET_NAME  "/dset_under_root"
#define DATASET_CREATE_UNDER_ROOT_SPACE_RANK 2

#define DATASET_CREATE_UNDER_EXISTING_SPACE_RANK 2
#define DATASET_CREATE_UNDER_EXISTING_GROUP_NAME "dset_under_group_test"
#define DATASET_CREATE_UNDER_EXISTING_DSET_NAME  "nested_dset"

#define DATASET_CREATE_INVALID_PARAMS_SPACE_RANK 2
#define DATASET_CREATE_INVALID_PARAMS_GROUP_NAME "dset_create_invalid_params_test"
#define DATASET_CREATE_INVALID_PARAMS_DSET_NAME  "invalid_params_dset"

#define DATASET_CREATE_ANONYMOUS_DATASET_NAME "anon_dset"
#define DATASET_CREATE_ANONYMOUS_GROUP_NAME   "anon_dset_creation_test"
#define DATASET_CREATE_ANONYMOUS_SPACE_RANK   2

#define DATASET_CREATE_ANONYMOUS_INVALID_PARAMS_DATASET_NAME "invalid_params_anon_dset"
#define DATASET_CREATE_ANONYMOUS_INVALID_PARAMS_GROUP_NAME   "anon_dset_creation_invalid_params_test"
#define DATASET_CREATE_ANONYMOUS_INVALID_PARAMS_SPACE_RANK   2

#define DATASET_CREATE_NULL_DATASPACE_TEST_SUBGROUP_NAME "dataset_with_null_space_test"
#define DATASET_CREATE_NULL_DATASPACE_TEST_DSET_NAME     "dataset_with_null_space"

#define DATASET_CREATE_SCALAR_DATASPACE_TEST_SUBGROUP_NAME "dataset_with_scalar_space_test"
#define DATASET_CREATE_SCALAR_DATASPACE_TEST_DSET_NAME     "dataset_with_scalar_space"

#define ZERO_DIM_DSET_TEST_GROUP_NAME "zero_dim_dset_test"
#define ZERO_DIM_DSET_TEST_SPACE_RANK 1
#define ZERO_DIM_DSET_TEST_DSET_NAME  "zero_dim_dset"

#define DATASET_MANY_CREATE_GROUP_NAME "group_for_many_datasets"
#define DSET_NAME_BUF_SIZE             64u
#define DATASET_NUMB                   100u

#define DATASET_SHAPE_TEST_DSET_BASE_NAME "dataset_shape_test"
#define DATASET_SHAPE_TEST_SUBGROUP_NAME  "dataset_shape_test"
#define DATASET_SHAPE_TEST_NUM_ITERATIONS 5
#define DATASET_SHAPE_TEST_MAX_DIMS       5

#define DATASET_PREDEFINED_TYPE_TEST_SPACE_RANK    2
#define DATASET_PREDEFINED_TYPE_TEST_BASE_NAME     "predefined_type_dset"
#define DATASET_PREDEFINED_TYPE_TEST_SUBGROUP_NAME "predefined_type_dataset_test"

#define DATASET_STRING_TYPE_TEST_STRING_LENGTH 40
#define DATASET_STRING_TYPE_TEST_SPACE_RANK    2
#define DATASET_STRING_TYPE_TEST_DSET_NAME1    "fixed_length_string_dset"
#define DATASET_STRING_TYPE_TEST_DSET_NAME2    "variable_length_string_dset"
#define DATASET_STRING_TYPE_TEST_SUBGROUP_NAME "string_type_dataset_test"

#define DATASET_COMPOUND_TYPE_TEST_SUBGROUP_NAME "compound_type_dataset_test"
#define DATASET_COMPOUND_TYPE_TEST_DSET_NAME     "compound_type_test"
#define DATASET_COMPOUND_TYPE_TEST_MAX_SUBTYPES  5
#define DATASET_COMPOUND_TYPE_TEST_MAX_PASSES    5
#define DATASET_COMPOUND_TYPE_TEST_DSET_RANK     2

#define DATASET_ENUM_TYPE_TEST_VAL_BASE_NAME "INDEX"
#define DATASET_ENUM_TYPE_TEST_SUBGROUP_NAME "enum_type_dataset_test"
#define DATASET_ENUM_TYPE_TEST_NUM_MEMBERS   16
#define DATASET_ENUM_TYPE_TEST_SPACE_RANK    2
#define DATASET_ENUM_TYPE_TEST_DSET_NAME1    "enum_native_dset"
#define DATASET_ENUM_TYPE_TEST_DSET_NAME2    "enum_non_native_dset"

#define DATASET_ARRAY_TYPE_TEST_SUBGROUP_NAME "array_type_dataset_test"
#define DATASET_ARRAY_TYPE_TEST_DSET_NAME1    "array_type_test1"
#define DATASET_ARRAY_TYPE_TEST_DSET_NAME2    "array_type_test2"
#define DATASET_ARRAY_TYPE_TEST_DSET_NAME3    "array_type_test3"
#define DATASET_ARRAY_TYPE_TEST_SPACE_RANK    2
#define DATASET_ARRAY_TYPE_TEST_RANK1         2
#define DATASET_ARRAY_TYPE_TEST_RANK2         2
#define DATASET_ARRAY_TYPE_TEST_RANK3         2

#define DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_YES_DSET_NAME "track_times_true_test"
#define DATASET_CREATION_PROPERTIES_TEST_TRACK_TIMES_NO_DSET_NAME  "track_times_false_test"
#define DATASET_CREATION_PROPERTIES_TEST_PHASE_CHANGE_DSET_NAME    "attr_phase_change_test"
#define DATASET_CREATION_PROPERTIES_TEST_ALLOC_TIMES_BASE_NAME     "alloc_time_test"
#define DATASET_CREATION_PROPERTIES_TEST_FILL_TIMES_BASE_NAME      "fill_times_test"
#define DATASET_CREATION_PROPERTIES_TEST_CRT_ORDER_BASE_NAME       "creation_order_test"
#define DATASET_CREATION_PROPERTIES_TEST_LAYOUTS_BASE_NAME         "layout_test"
#define DATASET_CREATION_PROPERTIES_TEST_FILTERS_DSET_NAME         "filters_test"
#define DATASET_CREATION_PROPERTIES_TEST_GROUP_NAME                "creation_properties_test"
#define DATASET_CREATION_PROPERTIES_TEST_CHUNK_DIM_RANK            DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK
#define DATASET_CREATION_PROPERTIES_TEST_MAX_COMPACT               12
#define DATASET_CREATION_PROPERTIES_TEST_MIN_DENSE                 8
#define DATASET_CREATION_PROPERTIES_TEST_SHAPE_RANK                3

#define DATASET_OPEN_INVALID_PARAMS_SPACE_RANK 2
#define DATASET_OPEN_INVALID_PARAMS_GROUP_NAME "dataset_open_test"
#define DATASET_OPEN_INVALID_PARAMS_DSET_NAME  "open_test_dataset"

#define DATASET_GET_SPACE_TYPE_TEST_SPACE_RANK 2
#define DATASET_GET_SPACE_TYPE_TEST_GROUP_NAME "get_dset_space_type_test"
#define DATASET_GET_SPACE_TYPE_TEST_DSET_NAME  "get_space_type_test_dset"

#define DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_SPACE_RANK 2
#define DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_GROUP_NAME "get_dset_space_type_invalid_params_test"
#define DATASET_GET_SPACE_TYPE_INVALID_PARAMS_TEST_DSET_NAME  "get_space_type_test_invalid_params_dset"

#define DATASET_PROPERTY_LIST_TEST_SUBGROUP_NAME "dataset_property_list_test_group"
#define DATASET_PROPERTY_LIST_TEST_SPACE_RANK    2
#define DATASET_PROPERTY_LIST_TEST_DSET_NAME1    "property_list_test_dataset1"
#define DATASET_PROPERTY_LIST_TEST_DSET_NAME2    "property_list_test_dataset2"
#define DATASET_PROPERTY_LIST_TEST_DSET_NAME3    "property_list_test_dataset3"
#define DATASET_PROPERTY_LIST_TEST_DSET_NAME4    "property_list_test_dataset4"

#define DATASET_SMALL_READ_TEST_ALL_DSET_SPACE_RANK 3
#define DATASET_SMALL_READ_TEST_ALL_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SMALL_READ_TEST_ALL_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SMALL_READ_TEST_ALL_GROUP_NAME      "dataset_small_read_all_test"
#define DATASET_SMALL_READ_TEST_ALL_DSET_NAME       "dataset_small_read_all_dset"

#define DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_SPACE_RANK 3
#define DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SMALL_READ_TEST_HYPERSLAB_GROUP_NAME      "dataset_small_read_hyperslab_test"
#define DATASET_SMALL_READ_TEST_HYPERSLAB_DSET_NAME       "dataset_small_read_hyperslab_dset"

#define DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK 3
#define DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SMALL_READ_TEST_POINT_SELECTION_NUM_POINTS      10
#define DATASET_SMALL_READ_TEST_POINT_SELECTION_GROUP_NAME      "dataset_small_read_point_selection_test"
#define DATASET_SMALL_READ_TEST_POINT_SELECTION_DSET_NAME       "dataset_small_read_point_selection_dset"

#define DATASET_IO_POINT_GROUP_NAME        "dataset_io_point_selection_test"
#define DATASET_IO_POINT_DSET_NAME_NOCHUNK "dataset_io_point_selection_dset_nochunk"
#define DATASET_IO_POINT_DSET_NAME_CHUNK   "dataset_io_point_selection_dset_chunk"

#ifndef NO_LARGE_TESTS
#define DATASET_LARGE_READ_TEST_ALL_DSET_SPACE_RANK 3
#define DATASET_LARGE_READ_TEST_ALL_DSET_DTYPESIZE  sizeof(int)
#define DATASET_LARGE_READ_TEST_ALL_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_LARGE_READ_TEST_ALL_GROUP_NAME      "dataset_large_read_all_test"
#define DATASET_LARGE_READ_TEST_ALL_DSET_NAME       "dataset_large_read_all_dset"

#define DATASET_LARGE_READ_TEST_HYPERSLAB_DSET_SPACE_RANK 3
#define DATASET_LARGE_READ_TEST_HYPERSLAB_DSET_DTYPESIZE  sizeof(int)
#define DATASET_LARGE_READ_TEST_HYPERSLAB_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_LARGE_READ_TEST_HYPERSLAB_GROUP_NAME      "dataset_large_read_hyperslab_test"
#define DATASET_LARGE_READ_TEST_HYPERSLAB_DSET_NAME       "dataset_large_read_hyperslab_dset"

#define DATASET_LARGE_READ_TEST_POINT_SELECTION_DSET_SPACE_RANK 1
#define DATASET_LARGE_READ_TEST_POINT_SELECTION_DSET_DTYPESIZE  sizeof(int)
#define DATASET_LARGE_READ_TEST_POINT_SELECTION_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_LARGE_READ_TEST_POINT_SELECTION_GROUP_NAME      "dataset_large_read_point_selection_test"
#define DATASET_LARGE_READ_TEST_POINT_SELECTION_DSET_NAME       "dataset_large_read_point_selection_dset"
#endif

#define DATASET_READ_INVALID_PARAMS_TEST_DSET_SPACE_RANK 3
#define DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_READ_INVALID_PARAMS_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_READ_INVALID_PARAMS_TEST_GROUP_NAME      "dataset_read_invalid_params_test"
#define DATASET_READ_INVALID_PARAMS_TEST_DSET_NAME       "dataset_read_invalid_params_dset"

#define DATASET_SMALL_WRITE_TEST_ALL_DSET_SPACE_RANK 3
#define DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SMALL_WRITE_TEST_ALL_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SMALL_WRITE_TEST_ALL_GROUP_NAME      "dataset_small_write_all_test"
#define DATASET_SMALL_WRITE_TEST_ALL_DSET_NAME       "dataset_small_write_all_dset"

#define DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK 3
#define DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SMALL_WRITE_TEST_HYPERSLAB_GROUP_NAME      "dataset_small_write_hyperslab_test"
#define DATASET_SMALL_WRITE_TEST_HYPERSLAB_DSET_NAME       "dataset_small_write_hyperslab_dset"

#define DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK 3
#define DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SMALL_WRITE_TEST_POINT_SELECTION_NUM_POINTS      10
#define DATASET_SMALL_WRITE_TEST_POINT_SELECTION_GROUP_NAME      "dataset_small_write_point_selection_test"
#define DATASET_SMALL_WRITE_TEST_POINT_SELECTION_DSET_NAME       "dataset_small_write_point_selection_dset"

#ifndef NO_LARGE_TESTS
#define DATASET_LARGE_WRITE_TEST_ALL_DSET_SPACE_RANK 3
#define DATASET_LARGE_WRITE_TEST_ALL_DSET_DTYPESIZE  sizeof(int)
#define DATASET_LARGE_WRITE_TEST_ALL_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_LARGE_WRITE_TEST_ALL_GROUP_NAME      "dataset_large_write_all_test"
#define DATASET_LARGE_WRITE_TEST_ALL_DSET_NAME       "dataset_large_write_all_dset"

#define DATASET_LARGE_WRITE_TEST_HYPERSLAB_DSET_SPACE_RANK 3
#define DATASET_LARGE_WRITE_TEST_HYPERSLAB_DSET_DTYPESIZE  sizeof(int)
#define DATASET_LARGE_WRITE_TEST_HYPERSLAB_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_LARGE_WRITE_TEST_HYPERSLAB_GROUP_NAME      "dataset_large_write_hyperslab_test"
#define DATASET_LARGE_WRITE_TEST_HYPERSLAB_DSET_NAME       "dataset_large_write_hyperslab_dset"

#define DATASET_LARGE_WRITE_TEST_POINT_SELECTION_DSET_SPACE_RANK 3
#define DATASET_LARGE_WRITE_TEST_POINT_SELECTION_DSET_DTYPESIZE  sizeof(int)
#define DATASET_LARGE_WRITE_TEST_POINT_SELECTION_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_LARGE_WRITE_TEST_POINT_SELECTION_GROUP_NAME      "dataset_large_write_point_selection_test"
#define DATASET_LARGE_WRITE_TEST_POINT_SELECTION_DSET_NAME       "dataset_large_write_point_selection_dset"
#endif

#define DATASET_DATA_VERIFY_WRITE_TEST_DSET_SPACE_RANK 3
#define DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_DATA_VERIFY_WRITE_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_DATA_VERIFY_WRITE_TEST_NUM_POINTS      10
#define DATASET_DATA_VERIFY_WRITE_TEST_GROUP_NAME      "dataset_data_write_verification_test"
#define DATASET_DATA_VERIFY_WRITE_TEST_DSET_NAME       "dataset_data_write_verification_dset"

#define DATASET_WRITE_INVALID_PARAMS_TEST_DSET_SPACE_RANK 3
#define DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_WRITE_INVALID_PARAMS_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_WRITE_INVALID_PARAMS_TEST_GROUP_NAME      "dataset_write_invalid_params_test"
#define DATASET_WRITE_INVALID_PARAMS_TEST_DSET_NAME       "dataset_write_invalid_params_dset"

#define DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_SPACE_RANK 3
#define DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPESIZE   sizeof(int)
#define DATASET_DATA_BUILTIN_CONVERSION_TEST_MEM_DTYPE       H5T_NATIVE_INT
#define DATASET_DATA_BUILTIN_CONVERSION_TEST_NUM_POINTS      10
#define DATASET_DATA_BUILTIN_CONVERSION_TEST_GROUP_NAME      "dataset_builtin_conversion_verification_test"
#define DATASET_DATA_BUILTIN_CONVERSION_TEST_DSET_NAME       "dataset_builtin_conversion_verification_dset"

#define DATASET_COMPOUND_PARTIAL_IO_DSET_DIMS            10
#define DATASET_DATA_COMPOUND_PARTIAL_IO_TEST_GROUP_NAME "dataset_compound_partial_io_test"
#define DATASET_DATA_COMPOUND_PARTIAL_IO_TEST_DSET_NAME  "dataset_compound_partial_io_test"

#define DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_SPACE_RANK 2
#define DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_NUM_PASSES 3
#define DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_GROUP_NAME "set_extent_chunked_unlimited_test"
#define DATASET_SET_EXTENT_CHUNKED_UNLIMITED_TEST_DSET_NAME  "set_extent_chunked_unlimited_test_dset"

#define DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_SPACE_RANK 2
#define DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_NUM_PASSES 3
#define DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_GROUP_NAME "set_extent_chunked_fixed_test"
#define DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME  "set_extent_chunked_fixed_test_dset"
#define DATASET_SET_EXTENT_CHUNKED_FIXED_TEST_DSET_NAME2 "set_extent_chunked_fixed_test_dset2"

#define DATASET_SET_EXTENT_DATA_TEST_SPACE_RANK 2
#define DATASET_SET_EXTENT_DATA_TEST_GROUP_NAME "set_extent_chunked_data_test"
#define DATASET_SET_EXTENT_DATA_TEST_DSET_NAME  "set_extent_chunked_data_test_dset"
#define DATASET_SET_EXTENT_DATA_TEST_SPACE_DIM  8

#define DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_RANK 2
#define DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_GROUP_NAME "set_extent_chunked_double_handles_test"
#define DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_DSET_NAME  "set_extent_chunked_double_handles_test_dset"
#define DATASET_SET_EXTENT_DOUBLE_HANDLES_TEST_SPACE_DIM  8

#define DATASET_SET_EXTENT_INVALID_PARAMS_TEST_SPACE_RANK        2
#define DATASET_SET_EXTENT_INVALID_PARAMS_TEST_GROUP_NAME        "set_extent_invalid_params_test"
#define DATASET_SET_EXTENT_INVALID_PARAMS_TEST_DSET_NAME         "set_extent_invalid_params_test_dset"
#define DATASET_SET_EXTENT_INVALID_LAYOUT_TEST_COMPACT_DSET_NAME "set_extent_invalid_layout_test_compact_dset"
#define DATASET_SET_EXTENT_INVALID_LAYOUT_TEST_CONTIGUOUS_DSET_NAME                                          \
    "set_extent_invalid_layout_test_contiguous_dset"

#define DATASET_SINGLE_CHUNK_TEST_SPACE_RANK 2
#define DATASET_SINGLE_CHUNK_TEST_GROUP_NAME "single_chunk_dataset_test"
#define DATASET_SINGLE_CHUNK_TEST_DSET_NAME  "single_chunk_dataset"

#define DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_SPACE_RANK 2
#define DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_SINGLE_CHUNK_WRITE_TEST_GROUP_NAME      "single_chunk_dataset_write_test"
#define DATASET_SINGLE_CHUNK_WRITE_TEST_DSET_NAME       "single_chunk_dataset"

#define DATASET_MULTI_CHUNK_TEST_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_TEST_GROUP_NAME "multi_chunk_dataset_test"
#define DATASET_MULTI_CHUNK_TEST_DSET_NAME  "multi_chunk_dataset"

#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME                                            \
    "multi_chunk_dataset_write_same_space_read_test"
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"

#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME                                            \
    "multi_chunk_dataset_write_diff_space_read_test"
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"

#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME                                        \
    "multi_chunk_dataset_same_space_overwrite_test"
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_NITERS    10

#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME                                        \
    "multi_chunk_dataset_diff_space_overwrite_test"
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_NITERS    10

#define DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_SPACE_RANK 2
#define DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_CTYPE      int
#define DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_GROUP_NAME      "read_partial_chunk_all_sel_test"
#define DATASET_PARTIAL_CHUNK_READ_ALL_SEL_TEST_DSET_NAME       "read_partial_chunk_all_sel_dset"

#define DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_SPACE_RANK 2
#define DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_CTYPE      int
#define DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_GROUP_NAME      "read_partial_chunk_hyper_sel_test"
#define DATASET_PARTIAL_CHUNK_READ_HYPER_SEL_TEST_DSET_NAME       "read_partial_chunk_hyper_sel_dset"

#define DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_RANK 1
#define DATASET_GET_VLEN_BUF_SIZE_DSET_SPACE_DIM  4
#define DATASET_GET_VLEN_BUF_SIZE_GROUP_NAME      "get_vlen_buffer_size_group"
#define DATASET_GET_VLEN_BUF_SIZE_DSET_NAME       "get_vlen_buffer_size_dset"
#endif
