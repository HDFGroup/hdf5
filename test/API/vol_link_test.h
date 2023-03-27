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

#ifndef VOL_LINK_TEST_H
#define VOL_LINK_TEST_H

#include "vol_test.h"

int vol_link_test(void);

/*********************************************
 *                                           *
 *      VOL connector Link test defines      *
 *                                           *
 *********************************************/

#define HARD_LINK_TEST_GROUP_NAME "hard_link_creation_test"
#define HARD_LINK_TEST_LINK_NAME  "hard_link"

#define HARD_LINK_TEST_GROUP_LONG_NAME "hard_link_long_name"
#define MAX_NAME_LEN                   ((64 * 1024) + 1024)

#define HARD_LINK_TEST_GROUP_MANY_NAME          "hard_link_many_name"
#define HARD_LINK_TEST_GROUP_MANY_FINAL_NAME    "hard_link_final"
#define HARD_LINK_TEST_GROUP_MANY_NAME_BUF_SIZE 1024

#define H5L_SAME_LOC_TEST_GROUP_NAME "h5l_same_loc_test_group"
#if 0
#define H5L_SAME_LOC_TEST_LINK_NAME1 "h5l_same_loc_test_link1"
#endif
#define H5L_SAME_LOC_TEST_LINK_NAME2 "h5l_same_loc_test_link2"

#define HARD_LINK_INVALID_PARAMS_TEST_GROUP_NAME "hard_link_creation_invalid_params_test"
#define HARD_LINK_INVALID_PARAMS_TEST_LINK_NAME  "hard_link"

#define SOFT_LINK_EXISTING_RELATIVE_TEST_SUBGROUP_NAME "soft_link_to_existing_relative_path_test"
#define SOFT_LINK_EXISTING_RELATIVE_TEST_OBJECT_NAME   "group"
#define SOFT_LINK_EXISTING_RELATIVE_TEST_LINK_NAME     "soft_link_to_existing_relative_path"

#define SOFT_LINK_EXISTING_ABSOLUTE_TEST_SUBGROUP_NAME "soft_link_to_existing_absolute_path_test"
#define SOFT_LINK_EXISTING_ABSOLUTE_TEST_LINK_NAME     "soft_link_to_existing_absolute_path"

#define SOFT_LINK_DANGLING_RELATIVE_TEST_SUBGROUP_NAME "soft_link_dangling_relative_path_test"
#define SOFT_LINK_DANGLING_RELATIVE_TEST_OBJECT_NAME   "group"
#define SOFT_LINK_DANGLING_RELATIVE_TEST_LINK_NAME     "soft_link_dangling_relative_path"

#define SOFT_LINK_DANGLING_ABSOLUTE_TEST_SUBGROUP_NAME "soft_link_dangling_absolute_path_test"
#define SOFT_LINK_DANGLING_ABSOLUTE_TEST_OBJECT_NAME   "group"
#define SOFT_LINK_DANGLING_ABSOLUTE_TEST_LINK_NAME     "soft_link_dangling_absolute_path"

#define SOFT_LINK_TEST_GROUP_LONG_NAME  "soft_link_long_name"
#define SOFT_LINK_TEST_LONG_OBJECT_NAME "soft_link_object_name"

#define SOFT_LINK_TEST_GROUP_MANY_NAME          "soft_link_many_name"
#define SOFT_LINK_TEST_GROUP_MANY_FINAL_NAME    "soft_link_final"
#define SOFT_LINK_TEST_GROUP_MANY_NAME_BUF_SIZE 1024

#define SOFT_LINK_INVALID_PARAMS_TEST_GROUP_NAME "soft_link_creation_invalid_params_test"
#define SOFT_LINK_INVALID_PARAMS_TEST_LINK_NAME  "soft_link_to_root"

#define EXTERNAL_LINK_TEST_SUBGROUP_NAME "external_link_test"
#define EXTERNAL_LINK_TEST_FILE_NAME     "ext_link_file.h5"
#define EXTERNAL_LINK_TEST_LINK_NAME     "ext_link"

#define EXTERNAL_LINK_TEST_DANGLING_SUBGROUP_NAME "external_link_dangling_test"
#define EXTERNAL_LINK_TEST_DANGLING_LINK_NAME     "dangling_ext_link"
#define EXTERNAL_LINK_TEST_DANGLING_OBJECT_NAME   "external_group"

#define EXTERNAL_LINK_TEST_MULTI_NAME          "external_link_multi_test"
#define EXTERNAL_LINK_TEST_MULTI_NAME_BUF_SIZE 1024
#define EXTERNAL_LINK_TEST_FILE_NAME2          "ext_link_file_2.h5"
#define EXTERNAL_LINK_TEST_FILE_NAME3          "ext_link_file_3.h5"
#define EXTERNAL_LINK_TEST_FILE_NAME4          "ext_link_file_4.h5"

#define EXTERNAL_LINK_TEST_PING_PONG_NAME1         "ext_link_file_ping_pong_1.h5"
#define EXTERNAL_LINK_TEST_PING_PONG_NAME2         "ext_link_file_ping_pong_2.h5"
#define EXTERNAL_LINK_TEST_PING_PONG_NAME_BUF_SIZE 1024

#define EXTERNAL_LINK_INVALID_PARAMS_TEST_GROUP_NAME "external_link_creation_invalid_params_test"
#define EXTERNAL_LINK_INVALID_PARAMS_TEST_FILE_NAME  "ext_link_invalid_params_file.h5"
#define EXTERNAL_LINK_INVALID_PARAMS_TEST_LINK_NAME  "external_link"

#define UD_LINK_TEST_UDATA_MAX_SIZE 256
#define UD_LINK_TEST_GROUP_NAME     "ud_link_creation_test"
#define UD_LINK_TEST_LINK_NAME      "ud_link"

#define UD_LINK_INVALID_PARAMS_TEST_UDATA_MAX_SIZE 256
#define UD_LINK_INVALID_PARAMS_TEST_GROUP_NAME     "ud_link_creation_invalid_params_test"
#define UD_LINK_INVALID_PARAMS_TEST_LINK_NAME      "ud_link"

#define LINK_DELETE_TEST_NESTED_GRP_NAME "nested_grp"
#define LINK_DELETE_TEST_HARD_LINK_NAME  "hard_link"
#define LINK_DELETE_TEST_NESTED_HARD_LINK_NAME                                                               \
    LINK_DELETE_TEST_NESTED_GRP_NAME "/" LINK_DELETE_TEST_HARD_LINK_NAME
#define LINK_DELETE_TEST_HARD_LINK_NAME2       LINK_DELETE_TEST_HARD_LINK_NAME "2"
#define LINK_DELETE_TEST_HARD_LINK_NAME3       LINK_DELETE_TEST_HARD_LINK_NAME "3"
#define LINK_DELETE_TEST_SOFT_LINK_NAME        "soft_link"
#define LINK_DELETE_TEST_SOFT_LINK_NAME2       LINK_DELETE_TEST_SOFT_LINK_NAME "2"
#define LINK_DELETE_TEST_SOFT_LINK_NAME3       LINK_DELETE_TEST_SOFT_LINK_NAME "3"
#define LINK_DELETE_TEST_EXTERNAL_LINK_NAME    "external_link"
#define LINK_DELETE_TEST_EXTERNAL_LINK_NAME2   LINK_DELETE_TEST_EXTERNAL_LINK_NAME "2"
#define LINK_DELETE_TEST_EXTERNAL_LINK_NAME3   LINK_DELETE_TEST_EXTERNAL_LINK_NAME "3"
#define LINK_DELETE_TEST_SUBGROUP_NAME         "link_delete_test"
#define LINK_DELETE_TEST_SUBGROUP1_NAME        "H5Ldelete_hard_link"
#define LINK_DELETE_TEST_NESTED_SUBGROUP_NAME1 "H5Ldelete_nested_hard_link"
#define LINK_DELETE_TEST_SUBGROUP2_NAME        "H5Ldelete_soft_link"
#define LINK_DELETE_TEST_SUBGROUP3_NAME        "H5Ldelete_external_link"
#define LINK_DELETE_TEST_SUBGROUP4_NAME        "H5Ldelete_ud_link"
#define LINK_DELETE_TEST_SUBGROUP5_NAME        "H5Ldelete_by_idx_hard_link_crt_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP6_NAME        "H5Ldelete_by_idx_hard_link_crt_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP7_NAME        "H5Ldelete_by_idx_hard_link_name_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP8_NAME        "H5Ldelete_by_idx_hard_link_name_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP9_NAME        "H5Ldelete_by_idx_soft_link_crt_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP10_NAME       "H5Ldelete_by_idx_soft_link_crt_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP11_NAME       "H5Ldelete_by_idx_soft_link_name_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP12_NAME       "H5Ldelete_by_idx_soft_link_name_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP13_NAME       "H5Ldelete_by_idx_external_link_crt_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP14_NAME       "H5Ldelete_by_idx_external_link_crt_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP15_NAME       "H5Ldelete_by_idx_external_link_name_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP16_NAME       "H5Ldelete_by_idx_external_link_name_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP17_NAME       "H5Ldelete_by_idx_ud_link_crt_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP18_NAME       "H5Ldelete_by_idx_ud_link_crt_order_decreasing"
#define LINK_DELETE_TEST_SUBGROUP19_NAME       "H5Ldelete_by_idx_ud_link_name_order_increasing"
#define LINK_DELETE_TEST_SUBGROUP20_NAME       "H5Ldelete_by_idx_ud_link_name_order_decreasing"

#define LINK_DELETE_RESET_MAX_CRT_ORDER_TEST_SUBGROUP_NAME  "H5Ldelete_reset_grp_max_crt_order_test"
#define LINK_DELETE_RESET_MAX_CRT_ORDER_TEST_SUBGROUP1_NAME "H5Ldelete_bottom_up"
#define LINK_DELETE_RESET_MAX_CRT_ORDER_TEST_SUBGROUP2_NAME "H5Ldelete_top_down"
#define LINK_DELETE_RESET_MAX_CRT_ORDER_TEST_NUM_LINKS      5
#define LINK_DELETE_RESET_MAX_CRT_ORDER_TEST_BUF_SIZE       1024

#define LINK_DELETE_INVALID_PARAMS_TEST_HARD_LINK_NAME "hard_link"
#define LINK_DELETE_INVALID_PARAMS_TEST_GROUP_NAME     "link_deletion_invalid_params_test"

#define COPY_LINK_TEST_LINK_VAL_BUF_SIZE            1024
#define COPY_LINK_TEST_EXTERNAL_LINK_NAME           "external_link"
#define COPY_LINK_TEST_EXTERNAL_LINK_NAME2          COPY_LINK_TEST_EXTERNAL_LINK_NAME "2"
#define COPY_LINK_TEST_EXTERNAL_LINK_NAME3          COPY_LINK_TEST_EXTERNAL_LINK_NAME "3"
#define COPY_LINK_TEST_EXTERNAL_LINK_COPY_NAME      "external_link_copy"
#define COPY_LINK_TEST_EXTERNAL_LINK_COPY_NAME2     COPY_LINK_TEST_EXTERNAL_LINK_COPY_NAME "2"
#define COPY_LINK_TEST_EXTERNAL_LINK_SAME_LOC_NAME  "external_link_same_loc"
#define COPY_LINK_TEST_EXTERNAL_LINK_SAME_LOC_NAME2 COPY_LINK_TEST_EXTERNAL_LINK_SAME_LOC_NAME "2"
#define COPY_LINK_TEST_HARD_LINK_NAME               "hard_link"
#define COPY_LINK_TEST_HARD_LINK_NAME2              COPY_LINK_TEST_HARD_LINK_NAME "2"
#define COPY_LINK_TEST_HARD_LINK_NAME3              COPY_LINK_TEST_HARD_LINK_NAME "3"
#define COPY_LINK_TEST_HARD_LINK_COPY_NAME          "hard_link_copy"
#define COPY_LINK_TEST_HARD_LINK_COPY_NAME2         COPY_LINK_TEST_HARD_LINK_COPY_NAME "2"
#define COPY_LINK_TEST_HARD_LINK_SAME_LOC_NAME      "hard_link_same_loc"
#define COPY_LINK_TEST_HARD_LINK_SAME_LOC_NAME2     COPY_LINK_TEST_HARD_LINK_SAME_LOC_NAME "2"
#define COPY_LINK_TEST_SOFT_LINK_TARGET_PATH        "/" LINK_TEST_GROUP_NAME "/" COPY_LINK_TEST_SUBGROUP_NAME
#define COPY_LINK_TEST_SOFT_LINK_NAME               "soft_link"
#define COPY_LINK_TEST_SOFT_LINK_NAME2              COPY_LINK_TEST_SOFT_LINK_NAME "2"
#define COPY_LINK_TEST_SOFT_LINK_NAME3              COPY_LINK_TEST_SOFT_LINK_NAME "3"
#define COPY_LINK_TEST_SOFT_LINK_COPY_NAME          "soft_link_copy"
#define COPY_LINK_TEST_SOFT_LINK_COPY_NAME2         COPY_LINK_TEST_SOFT_LINK_COPY_NAME "2"
#define COPY_LINK_TEST_SOFT_LINK_SAME_LOC_NAME      "soft_link_same_loc"
#define COPY_LINK_TEST_SOFT_LINK_SAME_LOC_NAME2     COPY_LINK_TEST_SOFT_LINK_SAME_LOC_NAME "2"
#define COPY_LINK_TEST_SRC_GROUP_NAME               "src_group"
#define COPY_LINK_TEST_DST_GROUP_NAME               "dst_group"
#define COPY_LINK_TEST_SUBGROUP_NAME                "link_copy_test"

#define COPY_LINK_INVALID_PARAMS_TEST_HARD_LINK_COPY_NAME "hard_link_copy"
#define COPY_LINK_INVALID_PARAMS_TEST_HARD_LINK_NAME      "hard_link"
#define COPY_LINK_INVALID_PARAMS_TEST_HARD_LINK_NEW_NAME  "hard_link_new"
#define COPY_LINK_INVALID_PARAMS_TEST_SRC_GROUP_NAME      "src_group"
#define COPY_LINK_INVALID_PARAMS_TEST_DST_GROUP_NAME      "dst_group"
#define COPY_LINK_INVALID_PARAMS_TEST_SUBGROUP_NAME       "link_copy_invalid_params_test"

#define MOVE_LINK_TEST_LINK_VAL_BUF_SIZE         1024
#define MOVE_LINK_TEST_EXTERN_LINK_NAME          "extern_link"
#define MOVE_LINK_TEST_EXTERN_LINK_NAME2         MOVE_LINK_TEST_EXTERN_LINK_NAME "2"
#define MOVE_LINK_TEST_EXTERN_LINK_NAME3         MOVE_LINK_TEST_EXTERN_LINK_NAME "3"
#define MOVE_LINK_TEST_EXTERN_LINK_NAME4         MOVE_LINK_TEST_EXTERN_LINK_NAME "4"
#define MOVE_LINK_TEST_EXTERN_LINK_NEW_NAME      "extern_link_renamed"
#define MOVE_LINK_TEST_EXTERN_LINK_SAME_LOC_NAME "extern_link_same_loc"
#define MOVE_LINK_TEST_HARD_LINK_NAME            "hard_link"
#define MOVE_LINK_TEST_HARD_LINK_NAME2           MOVE_LINK_TEST_HARD_LINK_NAME "2"
#define MOVE_LINK_TEST_HARD_LINK_NAME3           MOVE_LINK_TEST_HARD_LINK_NAME "3"
#define MOVE_LINK_TEST_HARD_LINK_NAME4           MOVE_LINK_TEST_HARD_LINK_NAME "4"
#define MOVE_LINK_TEST_HARD_LINK_NEW_NAME        "hard_link_renamed"
#define MOVE_LINK_TEST_HARD_LINK_SAME_LOC_NAME   "hard_link_same_loc"
#define MOVE_LINK_TEST_SOFT_LINK_TARGET_PATH     "/" LINK_TEST_GROUP_NAME "/" MOVE_LINK_TEST_SUBGROUP_NAME
#define MOVE_LINK_TEST_SOFT_LINK_NAME            "soft_link"
#define MOVE_LINK_TEST_SOFT_LINK_NAME2           MOVE_LINK_TEST_SOFT_LINK_NAME "2"
#define MOVE_LINK_TEST_SOFT_LINK_NAME3           MOVE_LINK_TEST_SOFT_LINK_NAME "3"
#define MOVE_LINK_TEST_SOFT_LINK_NAME4           MOVE_LINK_TEST_SOFT_LINK_NAME "4"
#define MOVE_LINK_TEST_SOFT_LINK_NEW_NAME        "soft_link_renamed"
#define MOVE_LINK_TEST_SOFT_LINK_SAME_LOC_NAME   "soft_link_same_loc"
#define MOVE_LINK_TEST_SRC_GROUP_NAME            "src_group"
#define MOVE_LINK_TEST_DST_GROUP_NAME            "dst_group"
#define MOVE_LINK_TEST_SUBGROUP_NAME             "link_move_test"

#define MOVE_LINK_INTO_GRP_WITH_LINKS_TEST_SUBGROUP_NAME "link_move_into_group_with_links_test"
#define MOVE_LINK_INTO_GRP_WITH_LINKS_TEST_SRC_GRP_NAME  "source_group"
#define MOVE_LINK_INTO_GRP_WITH_LINKS_TEST_DST_GRP_NAME  "dest_group"
#define MOVE_LINK_INTO_GRP_WITH_LINKS_TEST_NUM_LINKS     5
#define MOVE_LINK_INTO_GRP_WITH_LINKS_TEST_BUF_SIZE      1024

#define MOVE_LINK_RESET_MAX_CRT_ORDER_TEST_SUBGROUP_NAME "H5Lmove_reset_grp_max_crt_order_test"
#define MOVE_LINK_RESET_MAX_CRT_ORDER_TEST_SRC_GRP_NAME  "source_group"
#define MOVE_LINK_RESET_MAX_CRT_ORDER_TEST_DST_GRP_NAME  "dest_group"
#define MOVE_LINK_RESET_MAX_CRT_ORDER_TEST_NUM_LINKS     5
#define MOVE_LINK_RESET_MAX_CRT_ORDER_TEST_BUF_SIZE      1024

#define MOVE_LINK_INVALID_PARAMS_TEST_HARD_LINK_NAME "hard_link"
#define MOVE_LINK_INVALID_PARAMS_TEST_SRC_GROUP_NAME "src_grp"
#define MOVE_LINK_INVALID_PARAMS_TEST_DST_GROUP_NAME "dst_grp"
#define MOVE_LINK_INVALID_PARAMS_TEST_SUBGROUP_NAME  "link_move_invalid_params_test"

#define GET_LINK_VAL_TEST_LINK_VAL_BUF_SIZE 1024
#define GET_LINK_VAL_TEST_SUBGROUP_NAME     "get_link_val_test"
#define GET_LINK_VAL_TEST_SOFT_LINK_NAME    "soft_link"
#define GET_LINK_VAL_TEST_SOFT_LINK_NAME2   GET_LINK_VAL_TEST_SOFT_LINK_NAME "2"
#define GET_LINK_VAL_TEST_SOFT_LINK_NAME3   GET_LINK_VAL_TEST_SOFT_LINK_NAME "3"
#define GET_LINK_VAL_TEST_EXT_LINK_NAME     "ext_link"
#define GET_LINK_VAL_TEST_EXT_LINK_NAME2    GET_LINK_VAL_TEST_EXT_LINK_NAME "2"
#define GET_LINK_VAL_TEST_EXT_LINK_NAME3    GET_LINK_VAL_TEST_EXT_LINK_NAME "3"
#define GET_LINK_VAL_TEST_SUBGROUP1_NAME    "H5Lget_val_soft_link"
#define GET_LINK_VAL_TEST_SUBGROUP2_NAME    "H5Lget_val_external_link"
#define GET_LINK_VAL_TEST_SUBGROUP3_NAME    "H5Lget_val_ud_link"
#define GET_LINK_VAL_TEST_SUBGROUP4_NAME    "H5Lget_val_by_idx_soft_link_crt_order_increasing"
#define GET_LINK_VAL_TEST_SUBGROUP5_NAME    "H5Lget_val_by_idx_soft_link_crt_order_decreasing"
#define GET_LINK_VAL_TEST_SUBGROUP6_NAME    "H5Lget_val_by_idx_soft_link_name_order_increasing"
#define GET_LINK_VAL_TEST_SUBGROUP7_NAME    "H5Lget_val_by_idx_soft_link_name_order_decreasing"
#define GET_LINK_VAL_TEST_SUBGROUP8_NAME    "H5Lget_val_by_idx_external_link_crt_order_increasing"
#define GET_LINK_VAL_TEST_SUBGROUP9_NAME    "H5Lget_val_by_idx_external_link_crt_order_decreasing"
#define GET_LINK_VAL_TEST_SUBGROUP10_NAME   "H5Lget_val_by_idx_external_link_name_order_increasing"
#define GET_LINK_VAL_TEST_SUBGROUP11_NAME   "H5Lget_val_by_idx_external_link_name_order_decreasing"
#define GET_LINK_VAL_TEST_SUBGROUP12_NAME   "H5Lget_val_by_idx_ud_link_crt_order_increasing"
#define GET_LINK_VAL_TEST_SUBGROUP13_NAME   "H5Lget_val_by_idx_ud_link_crt_order_decreasing"
#define GET_LINK_VAL_TEST_SUBGROUP14_NAME   "H5Lget_val_by_idx_ud_link_name_order_increasing"
#define GET_LINK_VAL_TEST_SUBGROUP15_NAME   "H5Lget_val_by_idx_ud_link_name_order_decreasing"

#define GET_LINK_VAL_INVALID_PARAMS_TEST_SOFT_LINK_NAME "soft_link"
#define GET_LINK_VAL_INVALID_PARAMS_TEST_GROUP_NAME     "get_link_val_invalid_params_test"

#define GET_LINK_INFO_TEST_HARD_LINK_NAME  "hard_link"
#define GET_LINK_INFO_TEST_HARD_LINK_NAME2 GET_LINK_INFO_TEST_HARD_LINK_NAME "2"
#define GET_LINK_INFO_TEST_HARD_LINK_NAME3 GET_LINK_INFO_TEST_HARD_LINK_NAME "3"
#define GET_LINK_INFO_TEST_SOFT_LINK_NAME  "soft_link"
#define GET_LINK_INFO_TEST_SOFT_LINK_NAME2 GET_LINK_INFO_TEST_SOFT_LINK_NAME "2"
#define GET_LINK_INFO_TEST_SOFT_LINK_NAME3 GET_LINK_INFO_TEST_SOFT_LINK_NAME "3"
#define GET_LINK_INFO_TEST_EXT_LINK_NAME   "ext_link"
#define GET_LINK_INFO_TEST_EXT_LINK_NAME2  GET_LINK_INFO_TEST_EXT_LINK_NAME "2"
#define GET_LINK_INFO_TEST_EXT_LINK_NAME3  GET_LINK_INFO_TEST_EXT_LINK_NAME "3"
#define GET_LINK_INFO_TEST_GROUP_NAME      "get_link_info_test"
#define GET_LINK_INFO_TEST_SUBGROUP1_NAME  "H5Lget_info_hard_link"
#define GET_LINK_INFO_TEST_SUBGROUP2_NAME  "H5Lget_info_soft_link"
#define GET_LINK_INFO_TEST_SUBGROUP3_NAME  "H5Lget_info_external_link"
#define GET_LINK_INFO_TEST_SUBGROUP4_NAME  "H5Lget_info_ud_link"
#define GET_LINK_INFO_TEST_SUBGROUP5_NAME  "H5Lget_info_by_idx_hard_link_crt_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP6_NAME  "H5Lget_info_by_idx_hard_link_crt_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP7_NAME  "H5Lget_info_by_idx_hard_link_name_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP8_NAME  "H5Lget_info_by_idx_hard_link_name_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP9_NAME  "H5Lget_info_by_idx_soft_link_crt_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP10_NAME "H5Lget_info_by_idx_soft_link_crt_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP11_NAME "H5Lget_info_by_idx_soft_link_name_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP12_NAME "H5Lget_info_by_idx_soft_link_name_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP13_NAME "H5Lget_info_by_idx_external_link_crt_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP14_NAME "H5Lget_info_by_idx_external_link_crt_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP15_NAME "H5Lget_info_by_idx_external_link_name_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP16_NAME "H5Lget_info_by_idx_external_link_name_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP17_NAME "H5Lget_info_by_idx_ud_link_crt_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP18_NAME "H5Lget_info_by_idx_ud_link_crt_order_decreasing"
#define GET_LINK_INFO_TEST_SUBGROUP19_NAME "H5Lget_info_by_idx_ud_link_name_order_increasing"
#define GET_LINK_INFO_TEST_SUBGROUP20_NAME "H5Lget_info_by_idx_ud_link_name_order_decreasing"

#define GET_LINK_INFO_INVALID_PARAMS_TEST_HARD_LINK_NAME "hard_link"
#define GET_LINK_INFO_INVALID_PARAMS_TEST_GROUP_NAME     "get_link_info_invalid_params_test"

#define GET_LINK_NAME_TEST_EXTERNAL_SUBGROUP_NAME  "get_external_link_name_crt_order_increasing"
#define GET_LINK_NAME_TEST_EXTERNAL_SUBGROUP_NAME2 "get_external_link_name_crt_order_decreasing"
#define GET_LINK_NAME_TEST_EXTERNAL_SUBGROUP_NAME3 "get_external_link_name_alpha_order_increasing"
#define GET_LINK_NAME_TEST_EXTERNAL_SUBGROUP_NAME4 "get_external_link_name_alpha_order_decreasing"
#define GET_LINK_NAME_TEST_EXTERNAL_LINK_NAME      "external_link"
#define GET_LINK_NAME_TEST_EXTERNAL_LINK_NAME2     GET_LINK_NAME_TEST_EXTERNAL_LINK_NAME "2"
#define GET_LINK_NAME_TEST_EXTERNAL_LINK_NAME3     GET_LINK_NAME_TEST_EXTERNAL_LINK_NAME "3"
#define GET_LINK_NAME_TEST_HARD_SUBGROUP_NAME      "get_hard_link_name_crt_order_increasing"
#define GET_LINK_NAME_TEST_HARD_SUBGROUP_NAME2     "get_hard_link_name_crt_order_decreasing"
#define GET_LINK_NAME_TEST_HARD_SUBGROUP_NAME3     "get_hard_link_name_alpha_order_increasing"
#define GET_LINK_NAME_TEST_HARD_SUBGROUP_NAME4     "get_hard_link_name_alpha_order_decreasing"
#define GET_LINK_NAME_TEST_HARD_LINK_NAME          "hard_link"
#define GET_LINK_NAME_TEST_HARD_LINK_NAME2         GET_LINK_NAME_TEST_HARD_LINK_NAME "2"
#define GET_LINK_NAME_TEST_HARD_LINK_NAME3         GET_LINK_NAME_TEST_HARD_LINK_NAME "3"
#define GET_LINK_NAME_TEST_SOFT_SUBGROUP_NAME      "get_soft_link_name_crt_order_increasing"
#define GET_LINK_NAME_TEST_SOFT_SUBGROUP_NAME2     "get_soft_link_name_crt_order_decreasing"
#define GET_LINK_NAME_TEST_SOFT_SUBGROUP_NAME3     "get_soft_link_name_alpha_order_increasing"
#define GET_LINK_NAME_TEST_SOFT_SUBGROUP_NAME4     "get_soft_link_name_alpha_order_decreasing"
#define GET_LINK_NAME_TEST_SOFT_LINK_NAME          "soft_link"
#define GET_LINK_NAME_TEST_SOFT_LINK_NAME2         GET_LINK_NAME_TEST_SOFT_LINK_NAME "2"
#define GET_LINK_NAME_TEST_SOFT_LINK_NAME3         GET_LINK_NAME_TEST_SOFT_LINK_NAME "3"
#define GET_LINK_NAME_TEST_GROUP_NAME              "get_link_name_test"
#define GET_LINK_NAME_TEST_BUF_SIZE                256

#define GET_LINK_NAME_INVALID_PARAMS_TEST_HARD_LINK_NAME "test_link1"
#define GET_LINK_NAME_INVALID_PARAMS_TEST_GROUP_NAME     "get_link_name_invalid_params_test"

#define LINK_ITER_HARD_LINKS_TEST_DSET_SPACE_RANK 2
#define LINK_ITER_HARD_LINKS_TEST_SUBGROUP_NAME   "link_iter_hard_links_test"
#define LINK_ITER_HARD_LINKS_TEST_LINK_NAME       "hard_link"
#define LINK_ITER_HARD_LINKS_TEST_NUM_LINKS       10
#define LINK_ITER_HARD_LINKS_TEST_BUF_SIZE        64

#define LINK_ITER_SOFT_LINKS_TEST_SUBGROUP_NAME "link_iter_soft_links_test"
#define LINK_ITER_SOFT_LINKS_TEST_LINK_NAME     "soft_link"
#define LINK_ITER_SOFT_LINKS_TEST_NUM_LINKS     10
#define LINK_ITER_SOFT_LINKS_TEST_BUF_SIZE      64

#define LINK_ITER_EXT_LINKS_TEST_SUBGROUP_NAME "link_iter_ext_links_test"
#define LINK_ITER_EXT_LINKS_TEST_LINK_NAME     "external_link"
#define LINK_ITER_EXT_LINKS_TEST_NUM_LINKS     10
#define LINK_ITER_EXT_LINKS_TEST_BUF_SIZE      64

#define LINK_ITER_MIXED_LINKS_TEST_DSET_SPACE_RANK 2
#define LINK_ITER_MIXED_LINKS_TEST_HARD_LINK_NAME  "hard_link1"
#define LINK_ITER_MIXED_LINKS_TEST_SOFT_LINK_NAME  "soft_link1"
#define LINK_ITER_MIXED_LINKS_TEST_EXT_LINK_NAME   "ext_link1"
#define LINK_ITER_MIXED_LINKS_TEST_SUBGROUP_NAME   "link_iter_mixed_links_test"
#define LINK_ITER_MIXED_LINKS_TEST_NUM_LINKS       3

#define LINK_ITER_INVALID_PARAMS_TEST_DSET_SPACE_RANK 2
#define LINK_ITER_INVALID_PARAMS_TEST_HARD_LINK_NAME  "hard_link1"
#define LINK_ITER_INVALID_PARAMS_TEST_SOFT_LINK_NAME  "soft_link1"
#define LINK_ITER_INVALID_PARAMS_TEST_EXT_LINK_NAME   "ext_link1"
#define LINK_ITER_INVALID_PARAMS_TEST_SUBGROUP_NAME   "link_iter_invalid_params_test"

#define LINK_ITER_0_LINKS_TEST_SUBGROUP_NAME "link_iter_0_links_test"

#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_TEST                                               \
    ((LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_GROUP *                                              \
      LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS) +                                                   \
     LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS)
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_GROUP 10
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_DSET_SPACE_RANK     2
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS       5
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_NESTED_GRP_NAME     "subgroup"
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_SUBGROUP_NAME       "link_visit_hard_links_no_cycle_test"
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_LINK_NAME           "hard_link"
#define LINK_VISIT_HARD_LINKS_NO_CYCLE_TEST_BUF_SIZE            256

#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_TEST                                               \
    ((LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_GROUP *                                              \
      LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS) +                                                   \
     LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS)
#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_GROUP 10
#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS       5
#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_NESTED_GRP_NAME     "subgroup"
#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_SUBGROUP_NAME       "link_visit_soft_links_no_cycle_test"
#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_LINK_NAME           "soft_link"
#define LINK_VISIT_SOFT_LINKS_NO_CYCLE_TEST_BUF_SIZE            256

#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_TEST                                                \
    ((LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_GROUP *                                               \
      LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS) +                                                    \
     LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS)
#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NUM_LINKS_PER_GROUP 10
#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NUM_SUBGROUPS       5
#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_NESTED_GRP_NAME     "subgroup"
#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_SUBGROUP_NAME       "link_visit_ext_links_no_cycle_test"
#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_LINK_NAME           "external_link"
#define LINK_VISIT_EXT_LINKS_NO_CYCLE_TEST_BUF_SIZE            256

#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_DSET_SPACE_RANK 2
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_DSET_NAME       "dset"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_DSET_NAME2      "dset2"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_SUBGROUP_NAME   "link_visit_mixed_links_no_cycle_test"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_SUBGROUP_NAME2  "link_visit_subgroup1"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_SUBGROUP_NAME3  "link_visit_subgroup2"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_LINK_NAME1      "hard_link1"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_LINK_NAME2      "soft_link1"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_LINK_NAME3      "ext_link1"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_LINK_NAME4      "hard_link2"
#define LINK_VISIT_MIXED_LINKS_NO_CYCLE_TEST_NUM_LINKS       8

#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_NUM_LINKS_PER_TEST                                                  \
    ((LINK_VISIT_HARD_LINKS_CYCLE_TEST_NUM_LINKS_PER_GROUP *                                                 \
      LINK_VISIT_HARD_LINKS_CYCLE_TEST_NUM_SUBGROUPS) +                                                      \
     LINK_VISIT_HARD_LINKS_CYCLE_TEST_NUM_SUBGROUPS)
#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_NUM_LINKS_PER_GROUP 10
#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_NUM_SUBGROUPS       5
#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_NESTED_GRP_NAME     "subgroup"
#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_SUBGROUP_NAME       "link_visit_hard_links_cycle_test"
#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_LINK_NAME           "hard_link"
#define LINK_VISIT_HARD_LINKS_CYCLE_TEST_BUF_SIZE            256

#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NUM_LINKS_PER_TEST                                                  \
    ((LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NUM_LINKS_PER_GROUP *                                                 \
      LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NUM_SUBGROUPS) +                                                      \
     LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NUM_SUBGROUPS)
#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NUM_LINKS_PER_GROUP 10
#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NUM_SUBGROUPS       5
#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_NESTED_GRP_NAME     "subgroup"
#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_SUBGROUP_NAME       "link_visit_soft_links_cycle_test"
#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_LINK_NAME           "soft_link"
#define LINK_VISIT_SOFT_LINKS_CYCLE_TEST_BUF_SIZE            256

#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_NUM_LINKS_PER_TEST                                                   \
    ((LINK_VISIT_EXT_LINKS_CYCLE_TEST_NUM_LINKS_PER_GROUP * LINK_VISIT_EXT_LINKS_CYCLE_TEST_NUM_SUBGROUPS) + \
     LINK_VISIT_EXT_LINKS_CYCLE_TEST_NUM_SUBGROUPS)
#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_NUM_LINKS_PER_GROUP 10
#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_NUM_SUBGROUPS       5
#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_NESTED_GRP_NAME     "subgroup"
#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_SUBGROUP_NAME       "link_visit_ext_links_cycle_test"
#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_LINK_NAME           "external_link"
#define LINK_VISIT_EXT_LINKS_CYCLE_TEST_BUF_SIZE            256

#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_SUBGROUP_NAME  "link_visit_mixed_links_cycle_test"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_SUBGROUP_NAME2 "link_visit_subgroup1"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_SUBGROUP_NAME3 "link_visit_subgroup2"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_LINK_NAME1     "hard_link1"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_LINK_NAME2     "soft_link1"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_LINK_NAME3     "ext_link1"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_LINK_NAME4     "hard_link2"
#define LINK_VISIT_MIXED_LINKS_CYCLE_TEST_NUM_LINKS      6

#define LINK_VISIT_INVALID_PARAMS_TEST_DSET_SPACE_RANK 2
#define LINK_VISIT_INVALID_PARAMS_TEST_DSET_NAME       "dset"
#define LINK_VISIT_INVALID_PARAMS_TEST_SUBGROUP_NAME   "link_visit_invalid_params_test"
#define LINK_VISIT_INVALID_PARAMS_TEST_SUBGROUP_NAME2  "link_visit_subgroup1"
#define LINK_VISIT_INVALID_PARAMS_TEST_SUBGROUP_NAME3  "link_visit_subgroup2"
#define LINK_VISIT_INVALID_PARAMS_TEST_LINK_NAME1      "hard_link1"
#define LINK_VISIT_INVALID_PARAMS_TEST_LINK_NAME2      "soft_link1"
#define LINK_VISIT_INVALID_PARAMS_TEST_LINK_NAME3      "ext_link1"
#define LINK_VISIT_INVALID_PARAMS_TEST_LINK_NAME4      "hard_link2"

#define LINK_VISIT_0_LINKS_TEST_SUBGROUP_NAME  "link_visit_0_links_test"
#define LINK_VISIT_0_LINKS_TEST_SUBGROUP_NAME2 "link_visit_0_links_test_subgroup1"
#define LINK_VISIT_0_LINKS_TEST_SUBGROUP_NAME3 "link_visit_0_links_test_subgroup2"

#endif
