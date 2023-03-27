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

#ifndef VOL_TESTS_DISABLED_H
#define VOL_TESTS_DISABLED_H

#include "h5vl_test_config.h"

/* Contains #defines to temporarily disable VOL tests based
 * on problematic or unsupported functionality */

#define NO_LARGE_TESTS
#define NO_ATTR_FILL_VALUE_SUPPORT
#define NO_DECREASING_ALPHA_ITER_ORDER
#define NO_USER_DEFINED_LINKS
#define NO_EXTERNAL_LINKS
#define NO_ITERATION_RESTART
#define NO_FILE_MOUNTS
#define NO_CLEAR_ON_SHRINK
#define NO_DOUBLE_OBJECT_OPENS
#define NO_OBJECT_GET_NAME
#define WRONG_DATATYPE_OBJ_COUNT
#define NO_SHARED_DATATYPES
#define NO_INVALID_PROPERTY_LIST_TESTS
#define NO_MAX_LINK_CRT_ORDER_RESET
#define NO_PREVENT_HARD_LINKS_ACROSS_FILES
#define NO_SOFT_LINK_MANY_DANGLING
#define NO_ID_PREVENTS_OBJ_DELETE
#define NO_WRITE_SAME_ELEMENT_TWICE
#define NO_PREVENT_CREATE_SAME_ATTRIBUTE_TWICE
#define NO_DELETE_NONEXISTENT_ATTRIBUTE
#define NO_TRUNCATE_OPEN_FILE
#define NO_CHECK_SELECTION_BOUNDS
#define NO_VALIDATE_DATASPACE
#define NO_REFERENCE_TO_DELETED

#endif /* VOL_TESTS_DISABLED_H */
