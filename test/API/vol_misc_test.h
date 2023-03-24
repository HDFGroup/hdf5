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

#ifndef VOL_MISC_TEST_H
#define VOL_MISC_TEST_H

#include "vol_test.h"

int vol_misc_test(void);

/******************************************************
 *                                                    *
 *      VOL connector Miscellaneous test defines      *
 *                                                    *
 ******************************************************/

#define OPEN_LINK_WITHOUT_SLASH_DSET_SPACE_RANK 2
#define OPEN_LINK_WITHOUT_SLASH_DSET_NAME       "link_without_slash_test_dset"

#define OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_CONTAINER_GROUP_NAME "absolute_path_test_container_group"
#define OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_SUBGROUP_NAME        "absolute_path_test_subgroup"
#define OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DTYPE_NAME           "absolute_path_test_dtype"
#define OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DSET_NAME            "absolute_path_test_dset"
#define OBJECT_CREATE_BY_ABSOLUTE_PATH_TEST_DSET_SPACE_RANK      3

#define ABSOLUTE_VS_RELATIVE_PATH_TEST_CONTAINER_GROUP_NAME "absolute_vs_relative_test_container_group"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET1_NAME           "absolute_vs_relative_test_dset1"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET2_NAME           "absolute_vs_relative_test_dset2"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET3_NAME           "absolute_vs_relative_test_dset3"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET4_NAME           "absolute_vs_relative_test_dset4"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET5_NAME           "absolute_vs_relative_test_dset5"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET6_NAME           "absolute_vs_relative_test_dset6"
#define ABSOLUTE_VS_RELATIVE_PATH_TEST_DSET_SPACE_RANK      3

#define DOT_AS_OBJECT_NAME_TEST_SUBGROUP_NAME "dot_as_object_name_test"

#define COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_SUBGROUP_NAME                                             \
    "compound_type_with_symbols_in_member_names_test"
#define COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_NUM_SUBTYPES 9
#define COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_DSET_RANK    2
#define COMPOUND_WITH_SYMBOLS_IN_MEMBER_NAMES_TEST_DSET_NAME    "dset"

#endif
