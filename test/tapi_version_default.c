/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Generated automatically by bin/make_vers -- do not edit */
/* Add new versioned symbols to H5vers.txt file */

/*
 * Purpose: Tests that versioned API function macros default to the earliest
 *          version for functions introduced after the configured global API
 *          version. For example, with H5_USE_16_API, H5Sencode (introduced
 *          in v1.8) should map to H5Sencode1, not H5Sencode2.
 *
 *          This test is compiled multiple times with different
 *          -DH5_USE_*_API and -DTEST_API_VERSION=* flags to verify each
 *          API version level.
 */
/* Include config header first to set its include guard */
#include "H5pubconf.h"

/* Clear all API version macros that may have been set by the
 * global default configuration, so only the version under test
 * is active when H5version.h is processed.
 */
#undef H5_USE_16_API_DEFAULT
#undef H5_USE_16_API
#undef H5_USE_18_API_DEFAULT
#undef H5_USE_18_API
#undef H5_USE_110_API_DEFAULT
#undef H5_USE_110_API
#undef H5_USE_112_API_DEFAULT
#undef H5_USE_112_API
#undef H5_USE_114_API_DEFAULT
#undef H5_USE_114_API
#undef H5_USE_200_API_DEFAULT
#undef H5_USE_200_API

/* Re-establish only the API version under test */
#if TEST_API_VERSION == 16
#define H5_USE_16_API 1
#elif TEST_API_VERSION == 18
#define H5_USE_18_API 1
#elif TEST_API_VERSION == 110
#define H5_USE_110_API 1
#elif TEST_API_VERSION == 112
#define H5_USE_112_API 1
#elif TEST_API_VERSION == 114
#define H5_USE_114_API 1
#elif TEST_API_VERSION == 200
#define H5_USE_200_API 1
#else
#error "TEST_API_VERSION not set to a valid value"
#endif

#include "h5test.h"

/*
 * Helper macro: check that a _vers macro equals an expected value.
 */
#define CHECK_VERS(func_name, expected)                                                                      \
    do {                                                                                                     \
        if (func_name##_vers != (expected)) {                                                                \
            fprintf(stderr, "FAIL: %s_vers = %d, expected %d\n", #func_name, func_name##_vers, (expected));  \
            nerrors++;                                                                                       \
        }                                                                                                    \
    } while (0)

#define CHECK_VERS_T(type_name, expected)                                                                    \
    do {                                                                                                     \
        if (type_name##_t_vers != (expected)) {                                                              \
            fprintf(stderr, "FAIL: %s_t_vers = %d, expected %d\n", #type_name, type_name##_t_vers,           \
                    (expected));                                                                             \
            nerrors++;                                                                                       \
        }                                                                                                    \
    } while (0)

int
main(void)
{
    int nerrors = 0;

    TESTING("API version defaulting for versioned functions");

#if TEST_API_VERSION == 16
    printf("Configured with H5_USE_16_API\n");

    CHECK_VERS(H5Acreate, 1);
    CHECK_VERS(H5Aiterate, 1);
    CHECK_VERS(H5Dcreate, 1);
    CHECK_VERS(H5Dopen, 1);
    CHECK_VERS(H5Dread_chunk, 1);
    CHECK_VERS(H5Eclear, 1);
    CHECK_VERS(H5Eget_auto, 1);
    CHECK_VERS(H5Eprint, 1);
    CHECK_VERS(H5Epush, 1);
    CHECK_VERS(H5Eset_auto, 1);
    CHECK_VERS(H5Ewalk, 1);
    CHECK_VERS(H5Fget_info, 1);
    CHECK_VERS(H5Gcreate, 1);
    CHECK_VERS(H5Gopen, 1);
    CHECK_VERS(H5Iregister_type, 1);
    CHECK_VERS(H5Lget_info, 1);
    CHECK_VERS(H5Lget_info_by_idx, 1);
    CHECK_VERS(H5Literate, 1);
    CHECK_VERS(H5Literate_by_name, 1);
    CHECK_VERS(H5Lvisit, 1);
    CHECK_VERS(H5Lvisit_by_name, 1);
    CHECK_VERS(H5Oget_info, 1);
    CHECK_VERS(H5Oget_info_by_idx, 1);
    CHECK_VERS(H5Oget_info_by_name, 1);
    CHECK_VERS(H5Ovisit, 1);
    CHECK_VERS(H5Ovisit_by_name, 1);
    CHECK_VERS(H5Pencode, 1);
    CHECK_VERS(H5Pget_filter, 1);
    CHECK_VERS(H5Pget_filter_by_id, 1);
    CHECK_VERS(H5Pinsert, 1);
    CHECK_VERS(H5Pregister, 1);
    CHECK_VERS(H5Rdereference, 1);
    CHECK_VERS(H5Rget_obj_type, 1);
    CHECK_VERS(H5Sencode, 1);
    CHECK_VERS(H5Tarray_create, 1);
    CHECK_VERS(H5Tcommit, 1);
    CHECK_VERS(H5Tdecode, 1);
    CHECK_VERS(H5Tget_array_dims, 1);
    CHECK_VERS(H5Topen, 1);

    CHECK_VERS_T(H5E_auto, 1);
    CHECK_VERS_T(H5O_info, 1);
    CHECK_VERS_T(H5O_iterate, 1);
    CHECK_VERS_T(H5Z_class, 1);

#elif TEST_API_VERSION == 18
    printf("Configured with H5_USE_18_API\n");

    CHECK_VERS(H5Acreate, 2);
    CHECK_VERS(H5Aiterate, 2);
    CHECK_VERS(H5Dcreate, 2);
    CHECK_VERS(H5Dopen, 2);
    CHECK_VERS(H5Dread_chunk, 1);
    CHECK_VERS(H5Eclear, 2);
    CHECK_VERS(H5Eget_auto, 2);
    CHECK_VERS(H5Eprint, 2);
    CHECK_VERS(H5Epush, 2);
    CHECK_VERS(H5Eset_auto, 2);
    CHECK_VERS(H5Ewalk, 2);
    CHECK_VERS(H5Fget_info, 1);
    CHECK_VERS(H5Gcreate, 2);
    CHECK_VERS(H5Gopen, 2);
    CHECK_VERS(H5Iregister_type, 1);
    CHECK_VERS(H5Lget_info, 1);
    CHECK_VERS(H5Lget_info_by_idx, 1);
    CHECK_VERS(H5Literate, 1);
    CHECK_VERS(H5Literate_by_name, 1);
    CHECK_VERS(H5Lvisit, 1);
    CHECK_VERS(H5Lvisit_by_name, 1);
    CHECK_VERS(H5Oget_info, 1);
    CHECK_VERS(H5Oget_info_by_idx, 1);
    CHECK_VERS(H5Oget_info_by_name, 1);
    CHECK_VERS(H5Ovisit, 1);
    CHECK_VERS(H5Ovisit_by_name, 1);
    CHECK_VERS(H5Pencode, 1);
    CHECK_VERS(H5Pget_filter, 2);
    CHECK_VERS(H5Pget_filter_by_id, 2);
    CHECK_VERS(H5Pinsert, 2);
    CHECK_VERS(H5Pregister, 2);
    CHECK_VERS(H5Rdereference, 1);
    CHECK_VERS(H5Rget_obj_type, 2);
    CHECK_VERS(H5Sencode, 1);
    CHECK_VERS(H5Tarray_create, 2);
    CHECK_VERS(H5Tcommit, 2);
    CHECK_VERS(H5Tdecode, 1);
    CHECK_VERS(H5Tget_array_dims, 2);
    CHECK_VERS(H5Topen, 2);

    CHECK_VERS_T(H5E_auto, 2);
    CHECK_VERS_T(H5O_info, 1);
    CHECK_VERS_T(H5O_iterate, 1);
    CHECK_VERS_T(H5Z_class, 2);

#elif TEST_API_VERSION == 110
    printf("Configured with H5_USE_110_API\n");

    CHECK_VERS(H5Acreate, 2);
    CHECK_VERS(H5Aiterate, 2);
    CHECK_VERS(H5Dcreate, 2);
    CHECK_VERS(H5Dopen, 2);
    CHECK_VERS(H5Dread_chunk, 1);
    CHECK_VERS(H5Eclear, 2);
    CHECK_VERS(H5Eget_auto, 2);
    CHECK_VERS(H5Eprint, 2);
    CHECK_VERS(H5Epush, 2);
    CHECK_VERS(H5Eset_auto, 2);
    CHECK_VERS(H5Ewalk, 2);
    CHECK_VERS(H5Fget_info, 2);
    CHECK_VERS(H5Gcreate, 2);
    CHECK_VERS(H5Gopen, 2);
    CHECK_VERS(H5Iregister_type, 1);
    CHECK_VERS(H5Lget_info, 1);
    CHECK_VERS(H5Lget_info_by_idx, 1);
    CHECK_VERS(H5Literate, 1);
    CHECK_VERS(H5Literate_by_name, 1);
    CHECK_VERS(H5Lvisit, 1);
    CHECK_VERS(H5Lvisit_by_name, 1);
    CHECK_VERS(H5Oget_info, 1);
    CHECK_VERS(H5Oget_info_by_idx, 1);
    CHECK_VERS(H5Oget_info_by_name, 1);
    CHECK_VERS(H5Ovisit, 1);
    CHECK_VERS(H5Ovisit_by_name, 1);
    CHECK_VERS(H5Pencode, 1);
    CHECK_VERS(H5Pget_filter, 2);
    CHECK_VERS(H5Pget_filter_by_id, 2);
    CHECK_VERS(H5Pinsert, 2);
    CHECK_VERS(H5Pregister, 2);
    CHECK_VERS(H5Rdereference, 2);
    CHECK_VERS(H5Rget_obj_type, 2);
    CHECK_VERS(H5Sencode, 1);
    CHECK_VERS(H5Tarray_create, 2);
    CHECK_VERS(H5Tcommit, 2);
    CHECK_VERS(H5Tdecode, 1);
    CHECK_VERS(H5Tget_array_dims, 2);
    CHECK_VERS(H5Topen, 2);

    CHECK_VERS_T(H5E_auto, 2);
    CHECK_VERS_T(H5O_info, 1);
    CHECK_VERS_T(H5O_iterate, 1);
    CHECK_VERS_T(H5Z_class, 2);

#elif TEST_API_VERSION == 112
    printf("Configured with H5_USE_112_API\n");

    CHECK_VERS(H5Acreate, 2);
    CHECK_VERS(H5Aiterate, 2);
    CHECK_VERS(H5Dcreate, 2);
    CHECK_VERS(H5Dopen, 2);
    CHECK_VERS(H5Dread_chunk, 1);
    CHECK_VERS(H5Eclear, 2);
    CHECK_VERS(H5Eget_auto, 2);
    CHECK_VERS(H5Eprint, 2);
    CHECK_VERS(H5Epush, 2);
    CHECK_VERS(H5Eset_auto, 2);
    CHECK_VERS(H5Ewalk, 2);
    CHECK_VERS(H5Fget_info, 2);
    CHECK_VERS(H5Gcreate, 2);
    CHECK_VERS(H5Gopen, 2);
    CHECK_VERS(H5Iregister_type, 1);
    CHECK_VERS(H5Lget_info, 2);
    CHECK_VERS(H5Lget_info_by_idx, 2);
    CHECK_VERS(H5Literate, 2);
    CHECK_VERS(H5Literate_by_name, 2);
    CHECK_VERS(H5Lvisit, 2);
    CHECK_VERS(H5Lvisit_by_name, 2);
    CHECK_VERS(H5Oget_info, 3);
    CHECK_VERS(H5Oget_info_by_idx, 3);
    CHECK_VERS(H5Oget_info_by_name, 3);
    CHECK_VERS(H5Ovisit, 3);
    CHECK_VERS(H5Ovisit_by_name, 3);
    CHECK_VERS(H5Pencode, 2);
    CHECK_VERS(H5Pget_filter, 2);
    CHECK_VERS(H5Pget_filter_by_id, 2);
    CHECK_VERS(H5Pinsert, 2);
    CHECK_VERS(H5Pregister, 2);
    CHECK_VERS(H5Rdereference, 2);
    CHECK_VERS(H5Rget_obj_type, 2);
    CHECK_VERS(H5Sencode, 2);
    CHECK_VERS(H5Tarray_create, 2);
    CHECK_VERS(H5Tcommit, 2);
    CHECK_VERS(H5Tdecode, 1);
    CHECK_VERS(H5Tget_array_dims, 2);
    CHECK_VERS(H5Topen, 2);

    CHECK_VERS_T(H5E_auto, 2);
    CHECK_VERS_T(H5O_info, 2);
    CHECK_VERS_T(H5O_iterate, 2);
    CHECK_VERS_T(H5Z_class, 2);

#elif TEST_API_VERSION == 114
    printf("Configured with H5_USE_114_API\n");

    CHECK_VERS(H5Acreate, 2);
    CHECK_VERS(H5Aiterate, 2);
    CHECK_VERS(H5Dcreate, 2);
    CHECK_VERS(H5Dopen, 2);
    CHECK_VERS(H5Dread_chunk, 1);
    CHECK_VERS(H5Eclear, 2);
    CHECK_VERS(H5Eget_auto, 2);
    CHECK_VERS(H5Eprint, 2);
    CHECK_VERS(H5Epush, 2);
    CHECK_VERS(H5Eset_auto, 2);
    CHECK_VERS(H5Ewalk, 2);
    CHECK_VERS(H5Fget_info, 2);
    CHECK_VERS(H5Gcreate, 2);
    CHECK_VERS(H5Gopen, 2);
    CHECK_VERS(H5Iregister_type, 1);
    CHECK_VERS(H5Lget_info, 2);
    CHECK_VERS(H5Lget_info_by_idx, 2);
    CHECK_VERS(H5Literate, 2);
    CHECK_VERS(H5Literate_by_name, 2);
    CHECK_VERS(H5Lvisit, 2);
    CHECK_VERS(H5Lvisit_by_name, 2);
    CHECK_VERS(H5Oget_info, 3);
    CHECK_VERS(H5Oget_info_by_idx, 3);
    CHECK_VERS(H5Oget_info_by_name, 3);
    CHECK_VERS(H5Ovisit, 3);
    CHECK_VERS(H5Ovisit_by_name, 3);
    CHECK_VERS(H5Pencode, 2);
    CHECK_VERS(H5Pget_filter, 2);
    CHECK_VERS(H5Pget_filter_by_id, 2);
    CHECK_VERS(H5Pinsert, 2);
    CHECK_VERS(H5Pregister, 2);
    CHECK_VERS(H5Rdereference, 2);
    CHECK_VERS(H5Rget_obj_type, 2);
    CHECK_VERS(H5Sencode, 2);
    CHECK_VERS(H5Tarray_create, 2);
    CHECK_VERS(H5Tcommit, 2);
    CHECK_VERS(H5Tdecode, 1);
    CHECK_VERS(H5Tget_array_dims, 2);
    CHECK_VERS(H5Topen, 2);

    CHECK_VERS_T(H5E_auto, 2);
    CHECK_VERS_T(H5O_info, 2);
    CHECK_VERS_T(H5O_iterate, 2);
    CHECK_VERS_T(H5Z_class, 2);

#elif TEST_API_VERSION == 200
    printf("Configured with H5_USE_200_API\n");

    CHECK_VERS(H5Acreate, 2);
    CHECK_VERS(H5Aiterate, 2);
    CHECK_VERS(H5Dcreate, 2);
    CHECK_VERS(H5Dopen, 2);
    CHECK_VERS(H5Dread_chunk, 2);
    CHECK_VERS(H5Eclear, 2);
    CHECK_VERS(H5Eget_auto, 2);
    CHECK_VERS(H5Eprint, 2);
    CHECK_VERS(H5Epush, 2);
    CHECK_VERS(H5Eset_auto, 2);
    CHECK_VERS(H5Ewalk, 2);
    CHECK_VERS(H5Fget_info, 2);
    CHECK_VERS(H5Gcreate, 2);
    CHECK_VERS(H5Gopen, 2);
    CHECK_VERS(H5Iregister_type, 2);
    CHECK_VERS(H5Lget_info, 2);
    CHECK_VERS(H5Lget_info_by_idx, 2);
    CHECK_VERS(H5Literate, 2);
    CHECK_VERS(H5Literate_by_name, 2);
    CHECK_VERS(H5Lvisit, 2);
    CHECK_VERS(H5Lvisit_by_name, 2);
    CHECK_VERS(H5Oget_info, 3);
    CHECK_VERS(H5Oget_info_by_idx, 3);
    CHECK_VERS(H5Oget_info_by_name, 3);
    CHECK_VERS(H5Ovisit, 3);
    CHECK_VERS(H5Ovisit_by_name, 3);
    CHECK_VERS(H5Pencode, 2);
    CHECK_VERS(H5Pget_filter, 2);
    CHECK_VERS(H5Pget_filter_by_id, 2);
    CHECK_VERS(H5Pinsert, 2);
    CHECK_VERS(H5Pregister, 2);
    CHECK_VERS(H5Rdereference, 2);
    CHECK_VERS(H5Rget_obj_type, 2);
    CHECK_VERS(H5Sencode, 2);
    CHECK_VERS(H5Tarray_create, 2);
    CHECK_VERS(H5Tcommit, 2);
    CHECK_VERS(H5Tdecode, 2);
    CHECK_VERS(H5Tget_array_dims, 2);
    CHECK_VERS(H5Topen, 2);

    CHECK_VERS_T(H5E_auto, 2);
    CHECK_VERS_T(H5O_info, 2);
    CHECK_VERS_T(H5O_iterate, 2);
    CHECK_VERS_T(H5Z_class, 2);

#else
#error "TEST_API_VERSION not set to a valid value"
#endif

    if (nerrors) {
        H5_FAILED();
        fprintf(stderr, "    %d version check%s failed\n", nerrors, nerrors > 1 ? "s" : "");
        return 1;
    }

    PASSED();
    return 0;
}
