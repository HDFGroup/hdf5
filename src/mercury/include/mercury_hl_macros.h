/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_HL_MACROS_H
#define MERCURY_HL_MACROS_H

#include "mercury_hl.h"
#include "mercury_macros.h"

/**
 * The purpose of these macros is to generate boilerplate code in order
 * to send and execute HG RPC calls.
 * Since these macros make use of the mercury high-level interface, applications
 * using these macros must link to the mercury_hl library.
 * HG_XXX macros are private macros / MERCURY_XXX are public macros.
 * Macros defined in this file are:
 *   - MERCURY_GEN_LOG_MESSAGE
 *   - MERCURY_GEN_RPC_STUB
 *   - MERCURY_GEN_CALLBACK_STUB
 */

/****************/
/* Local Macros */
/****************/

/* Return parameter with fixed name */
#define HG_GEN_RET_PARAM(ret_type) ((ret_type)(ret))

/* Generate ((param) (datai)) element */
#define HG_GEN_PARAM_NAME(r, prefix, i, param) ((param)(BOOST_PP_CAT(prefix, i)))

/* Generate parameter names and ((type) (name)) sequence */
#define HG_GEN_PARAM_NAME_SEQ(prefix, type_seq) BOOST_PP_SEQ_FOR_EACH_I(HG_GEN_PARAM_NAME, prefix, type_seq)

/* Extract parameter (type name) element */
#define HG_GEN_DECL_FUNC_PARAM(r, is_ref, param)                                                             \
    (HG_GEN_GET_TYPE(param) BOOST_PP_IF(is_ref, *, BOOST_PP_EMPTY()) HG_GEN_GET_NAME(param))

/* Extract (type name) sequence */
#define HG_GEN_DECL_FUNC_PARAM_SEQ(is_ref, param_seq)                                                        \
    BOOST_PP_SEQ_FOR_EACH(HG_GEN_DECL_FUNC_PARAM, is_ref, param_seq)

/* Extract function parameter declarations */
#define HG_GEN_DECL_FUNC_PARAMS(with_input, in_params, extra_in_params, with_output, out_params,             \
                                extra_out_params)                                                            \
    BOOST_PP_SEQ_TO_TUPLE(BOOST_PP_IF(                                                                       \
        BOOST_PP_OR(with_input, with_output),                                                                \
        HG_GEN_DECL_FUNC_PARAM_SEQ(0, in_params) HG_GEN_DECL_FUNC_PARAM_SEQ(0, extra_in_params)              \
            HG_GEN_DECL_FUNC_PARAM_SEQ(1, out_params) HG_GEN_DECL_FUNC_PARAM_SEQ(1, extra_out_params),       \
        (void)))

/* Extract parameter (get_name(param)) element */
#define HG_GEN_FUNC_PARAM(r, is_ref, param) (BOOST_PP_IF(is_ref, &, BOOST_PP_EMPTY()) HG_GEN_GET_NAME(param))

/* Extract (name) sequence */
#define HG_GEN_FUNC_PARAM_SEQ(is_ref, param_seq) BOOST_PP_SEQ_FOR_EACH(HG_GEN_FUNC_PARAM, is_ref, param_seq)

/* Extract function parameters */
#define HG_GEN_FUNC_PARAMS(with_input, in_params, extra_in_params, with_output, out_params,                  \
                           extra_out_params)                                                                 \
    BOOST_PP_SEQ_TO_TUPLE(                                                                                   \
        BOOST_PP_IF(BOOST_PP_OR(with_input, with_output),                                                    \
                    HG_GEN_FUNC_PARAM_SEQ(0, in_params) HG_GEN_FUNC_PARAM_SEQ(0, extra_in_params)            \
                        HG_GEN_FUNC_PARAM_SEQ(1, out_params) HG_GEN_FUNC_PARAM_SEQ(1, extra_out_params),     \
                    ()))

/* Generate declaration of parameters --> type name; */
#define HG_GEN_DECL_PARAMS(param_seq) BOOST_PP_SEQ_FOR_EACH(HG_GEN_STRUCT_FIELD, , param_seq)

/* Assign param to struct field ( e.g., struct_name.param_1 = param_1; ) */
#define HG_SET_STRUCT_PARAM(r, struct_name, param)                                                           \
    struct_name.HG_GEN_GET_NAME(param) = HG_GEN_GET_NAME(param);

/* Assign param ((type) (name)) sequence to struct_name */
#define HG_SET_STRUCT_PARAMS(struct_name, params)                                                            \
    BOOST_PP_SEQ_FOR_EACH(HG_SET_STRUCT_PARAM, struct_name, params)

/* Assign struct_name field to param ( e.g., param_1 = struct_name.param_1; ) */
#define HG_GET_STRUCT_PARAM(r, struct_name, param)                                                           \
    HG_GEN_GET_NAME(param) = struct_name.HG_GEN_GET_NAME(param);

/* Assign struct_name fields to param ((type) (name)) sequence */
#define HG_GET_STRUCT_PARAMS(struct_name, params)                                                            \
    BOOST_PP_SEQ_FOR_EACH(HG_GET_STRUCT_PARAM, struct_name, params)

/* Assign struct_name field to out param ( e.g., *param_1 = struct_name.param_1;
 * ) */
#define HG_GET_OUT_STRUCT_PARAM(r, struct_name, param)                                                       \
    *HG_GEN_GET_NAME(param) = struct_name.HG_GEN_GET_NAME(param);

/* Assign struct_name fields to out parame ((type) (name)) sequence */
#define HG_GET_OUT_STRUCT_PARAMS(struct_name, params)                                                        \
    BOOST_PP_SEQ_FOR_EACH(HG_GET_OUT_STRUCT_PARAM, struct_name, params)

/**
 * Get/free output boilerplate code
 */

/* Get output */
#define HG_GET_OUTPUT(with_ret, ret_fail)                                                                    \
    hg_ret = HG_Get_output(handle, &out_struct);                                                             \
    if (hg_ret != HG_SUCCESS) {                                                                              \
        BOOST_PP_IF(with_ret, ret = ret_fail;, BOOST_PP_EMPTY())                                             \
        goto done;                                                                                           \
    }

/* Free output */
#define HG_FREE_OUTPUT(with_ret, ret_fail)                                                                   \
    hg_ret = HG_Free_output(handle, &out_struct);                                                            \
    if (hg_ret != HG_SUCCESS) {                                                                              \
        BOOST_PP_IF(with_ret, ret = ret_fail;, BOOST_PP_EMPTY())                                             \
        goto done;                                                                                           \
    }

/**
 * Bulk data support boilerplate code
 */

/* Extra input parameters for bulk data */
#define HG_BULK_CONST_BUF      ((const void *)(bulk_buf))
#define HG_BULK_BUF            ((void *)(bulk_buf))
#define HG_BULK_COUNT          ((hg_uint64_t)(bulk_count))
#define HG_BULK_EXTRA_IN_PARAM HG_BULK_BUF HG_BULK_COUNT

/* Bulk handle parameter */
#define HG_BULK_PARAM ((hg_bulk_t)(bulk_handle))

/* Local bulk handle parameter */
#define HG_BULK_LOCAL_PARAM ((hg_bulk_t)(local_bulk_handle))

/* Create bulk handle */
#define HG_BULK_REGISTER(handle, bulk_handle, with_ret, fail_ret, bulk_read)                                 \
    hg_ret = HG_Bulk_create(HG_Get_info(handle)->hg_bulk_class, 1,                                           \
                            &HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_BUF)),                                \
                            &HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_COUNT)),                              \
                            BOOST_PP_IF(bulk_read, HG_BULK_READ_ONLY, HG_BULK_READWRITE), &bulk_handle);     \
    if (hg_ret != HG_SUCCESS) {                                                                              \
        BOOST_PP_IF(with_ret, ret = fail_ret;, BOOST_PP_EMPTY())                                             \
        goto done;                                                                                           \
    }

/* Free bulk handle */
#define HG_BULK_FREE(bulk_handle, with_ret, fail_ret)                                                        \
    hg_ret = HG_Bulk_free(bulk_handle);                                                                      \
    if (hg_ret != HG_SUCCESS) {                                                                              \
        BOOST_PP_IF(with_ret, ret = fail_ret;, BOOST_PP_EMPTY())                                             \
        goto done;                                                                                           \
    }

/* Declare variables required for bulk transfers */
#define HG_GEN_DECL_BULK_PARAMS HG_GEN_DECL_PARAMS(HG_BULK_PARAM HG_BULK_LOCAL_PARAM HG_BULK_EXTRA_IN_PARAM)

/* Allocate memory and create local bulk handle */
#define HG_BULK_LOCAL_ALLOCATE(origin_bulk_handle, local_bulk_handle)                                        \
    HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_COUNT)) = HG_Bulk_get_size(origin_bulk_handle);                \
    HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_BUF)) =                                                        \
        malloc(HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_COUNT)));                                           \
    HG_Bulk_create(HG_Get_info(handle)->hg_bulk_class, 1, &HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_BUF)),  \
                   &HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_COUNT)), HG_BULK_READWRITE,                    \
                   &local_bulk_handle);

/* Free memory and local handle */
#define HG_BULK_LOCAL_FREE(local_bulk_handle)                                                                \
    hg_ret = HG_Bulk_free(local_bulk_handle);                                                                \
    if (hg_ret != HG_SUCCESS) {                                                                              \
        goto done;                                                                                           \
    }                                                                                                        \
    free(HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_BUF)));

/* Transfer bulk data using origin/local bulk handles (pull or push) */
#define HG_BULK_TRANSFER(handle, origin_bulk_handle, local_bulk_handle, bulk_read)                           \
    hg_ret = HG_Hl_bulk_transfer_wait(                                                                       \
        HG_Get_info(handle)->bulk_context, BOOST_PP_IF(bulk_read, HG_BULK_PULL, HG_BULK_PUSH),               \
        HG_Get_info(handle)->addr, HG_Get_info(handle)->target_id, origin_bulk_handle, 0, local_bulk_handle, \
        0, HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_COUNT)));                                               \
    if (hg_ret != HG_SUCCESS) {                                                                              \
        goto done;                                                                                           \
    }

/*****************/
/* Public Macros */
/*****************/

/**
 * Advanced BOOST macros:
 *   - MERCURY_GEN_RPC_STUB
 *   - MERCURY_GEN_CALLBACK_STUB
 */

/* Custom function that applications can define for log purposes (none by
 * default) */
#ifndef MERCURY_GEN_LOG_MESSAGE
#define MERCURY_GEN_LOG_MESSAGE(x)
#endif

/* Booleans for MERCURY_GEN_MACROS */
#define MERCURY_GEN_FALSE 0
#define MERCURY_GEN_TRUE  1

/* Generate RPC stub */
#define MERCURY_GEN_RPC_STUB(gen_func_name, func_name, with_ret, ret_type_name, ret_fail, with_input,        \
                             in_struct_type_name, in_params, with_output, out_struct_type_name, out_params,  \
                             with_bulk, bulk_read)                                                           \
    BOOST_PP_IF(with_ret, ret_type_name, void)                                                               \
    gen_func_name HG_GEN_DECL_FUNC_PARAMS(with_input, in_params,                                             \
                                          BOOST_PP_IF(with_bulk, HG_BULK_EXTRA_IN_PARAM, BOOST_PP_EMPTY()),  \
                                          with_output, out_params, )                                         \
    {                                                                                                        \
        BOOST_PP_IF(with_input, in_struct_type_name in_struct;, BOOST_PP_EMPTY())                            \
        BOOST_PP_IF(BOOST_PP_OR(with_output, with_ret), out_struct_type_name out_struct;, BOOST_PP_EMPTY())  \
        BOOST_PP_IF(with_ret, ret_type_name ret;, BOOST_PP_EMPTY())                                          \
        hg_id_t     id;                                                                                      \
        hg_handle_t handle;                                                                                  \
        BOOST_PP_IF(with_bulk, HG_GEN_DECL_PARAMS(HG_BULK_PARAM), BOOST_PP_EMPTY())                          \
        hg_bool_t   func_registered;                                                                         \
        hg_return_t hg_ret;                                                                                  \
                                                                                                             \
        /* Init stack if not initialized */                                                                  \
        HG_Hl_init(NULL, 0);                                                                                 \
                                                                                                             \
        /* Check whether call has already been registered or not */                                          \
        HG_Registered_rpc(HG_CLASS_DEFAULT, BOOST_PP_STRINGIZE(func_name), &func_registered, &id);           \
        if (!func_registered) {                                                                              \
            id = MERCURY_REGISTER(                                                                           \
                HG_CLASS_DEFAULT, BOOST_PP_STRINGIZE(func_name),                                             \
                BOOST_PP_IF(with_input, in_struct_type_name, void),                                          \
                BOOST_PP_IF(BOOST_PP_OR(with_output, with_ret), out_struct_type_name, void), NULL);          \
        }                                                                                                    \
                                                                                                             \
        /* Create HG handle */                                                                               \
        hg_ret = HG_Create(HG_CLASS_DEFAULT, HG_CONTEXT_DEFAULT, NA_ADDR_DEFAULT, id, &handle);              \
        if (hg_ret != HG_SUCCESS) {                                                                          \
            BOOST_PP_IF(with_ret, ret = ret_fail;, BOOST_PP_EMPTY())                                         \
            goto done;                                                                                       \
        }                                                                                                    \
                                                                                                             \
        /* Create bulk handle */                                                                             \
        BOOST_PP_IF(with_bulk,                                                                               \
                    HG_BULK_REGISTER(handle, HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_PARAM)), with_ret,    \
                                     ret_fail, bulk_read),                                                   \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Fill input structure */                                                                           \
        BOOST_PP_IF(with_input,                                                                              \
                    HG_SET_STRUCT_PARAMS(in_struct,                                                          \
                                         in_params BOOST_PP_IF(with_bulk, HG_BULK_PARAM, BOOST_PP_EMPTY())), \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Forward call to default target */                                                                 \
        hg_ret = HG_Hl_forward_wait(handle, BOOST_PP_IF(with_input, &in_struct, NULL));                      \
        if (hg_ret != HG_SUCCESS) {                                                                          \
            BOOST_PP_IF(with_ret, ret = ret_fail;, BOOST_PP_EMPTY())                                         \
            goto done;                                                                                       \
        }                                                                                                    \
                                                                                                             \
        /* Free bulk handle */                                                                               \
        BOOST_PP_IF(with_bulk,                                                                               \
                    HG_BULK_FREE(HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_PARAM)), with_ret, ret_fail),     \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Get output */                                                                                     \
        BOOST_PP_IF(BOOST_PP_OR(with_output, with_ret), HG_GET_OUTPUT(with_ret, ret_fail), BOOST_PP_EMPTY()) \
                                                                                                             \
        /* Get output parameters */                                                                          \
        BOOST_PP_IF(with_ret, HG_GET_STRUCT_PARAMS(out_struct, ((ret_type)(ret))), BOOST_PP_EMPTY())         \
        BOOST_PP_IF(with_output, HG_GET_OUT_STRUCT_PARAMS(out_struct, out_params), BOOST_PP_EMPTY())         \
                                                                                                             \
        /* Free output */                                                                                    \
        BOOST_PP_IF(BOOST_PP_OR(with_output, with_ret), HG_FREE_OUTPUT(with_ret, ret_fail),                  \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Destroy handle */                                                                                 \
        hg_ret = HG_Destroy(handle);                                                                         \
        if (hg_ret != HG_SUCCESS) {                                                                          \
            BOOST_PP_IF(with_ret, ret = ret_fail;, BOOST_PP_EMPTY())                                         \
            goto done;                                                                                       \
        }                                                                                                    \
                                                                                                             \
done:                                                                                                        \
                                                                                                             \
        return BOOST_PP_IF(with_ret, ret, BOOST_PP_EMPTY());                                                 \
    }

/* Generate callback stub */
#define MERCURY_GEN_CALLBACK_STUB(gen_func_name, func_name, with_ret, ret_type, with_input,                  \
                                  in_struct_type_name, in_params, with_output, out_struct_type_name,         \
                                  out_params, with_bulk, bulk_read, with_thread, thread_pool)                \
    static BOOST_PP_IF(with_thread, HG_THREAD_RETURN_TYPE BOOST_PP_CAT(gen_func_name, _thread),              \
                       hg_return_t gen_func_name)(BOOST_PP_IF(with_thread, void *arg, hg_handle_t handle))   \
    {                                                                                                        \
        BOOST_PP_IF(with_thread, hg_handle_t handle = (hg_handle_t)arg;                                      \
                    hg_thread_ret_t thread_ret = (hg_thread_ret_t)0;, BOOST_PP_EMPTY())                      \
        hg_return_t hg_ret = HG_SUCCESS;                                                                     \
        BOOST_PP_IF(with_input, in_struct_type_name in_struct;, BOOST_PP_EMPTY())                            \
        BOOST_PP_IF(BOOST_PP_OR(with_output, with_ret), out_struct_type_name out_struct;, BOOST_PP_EMPTY())  \
        BOOST_PP_IF(with_input, HG_GEN_DECL_PARAMS(in_params), BOOST_PP_EMPTY())                             \
        BOOST_PP_IF(with_output, HG_GEN_DECL_PARAMS(out_params), BOOST_PP_EMPTY())                           \
        BOOST_PP_IF(with_ret, ret_type ret;, BOOST_PP_EMPTY())                                               \
        BOOST_PP_IF(with_bulk, HG_GEN_DECL_BULK_PARAMS, BOOST_PP_EMPTY())                                    \
                                                                                                             \
        /* Get input */                                                                                      \
        BOOST_PP_IF(                                                                                         \
            with_input, hg_ret = HG_Get_input(handle, &in_struct);                                           \
            if (hg_ret != HG_SUCCESS) { goto done; }                                                         \
                                                                                                             \
            /* Get parameters */                                                                             \
            HG_GET_STRUCT_PARAMS(in_struct,                                                                  \
                                 in_params BOOST_PP_IF(with_bulk, HG_BULK_PARAM, BOOST_PP_EMPTY())),         \
            BOOST_PP_EMPTY())                                                                                \
                                                                                                             \
        /* Allocate bulk handle */                                                                           \
        BOOST_PP_IF(with_bulk,                                                                               \
                    HG_BULK_LOCAL_ALLOCATE(HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_PARAM)),                \
                                           HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_LOCAL_PARAM))),         \
                    BOOST_PP_EMPTY())                                                                        \
        BOOST_PP_IF(with_bulk,                                                                               \
                    BOOST_PP_IF(bulk_read,                                                                   \
                                HG_BULK_TRANSFER(handle, HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_PARAM)),  \
                                                 HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_LOCAL_PARAM)),    \
                                                 bulk_read),                                                 \
                                BOOST_PP_EMPTY()),                                                           \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Call function */                                                                                  \
        MERCURY_GEN_LOG_MESSAGE(BOOST_PP_STRINGIZE(func_name));                                              \
        BOOST_PP_IF(with_ret, ret =, BOOST_PP_EMPTY())                                                       \
        func_name HG_GEN_FUNC_PARAMS(with_input, in_params,                                                  \
                                     BOOST_PP_IF(with_bulk, HG_BULK_EXTRA_IN_PARAM, BOOST_PP_EMPTY()),       \
                                     with_output, out_params, );                                             \
                                                                                                             \
        BOOST_PP_IF(with_bulk,                                                                               \
                    BOOST_PP_IF(bulk_read, BOOST_PP_EMPTY(),                                                 \
                                HG_BULK_TRANSFER(handle, HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_PARAM)),  \
                                                 HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_LOCAL_PARAM)),    \
                                                 bulk_read)),                                                \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Free bulk handle */                                                                               \
        BOOST_PP_IF(with_bulk, HG_BULK_LOCAL_FREE(HG_GEN_GET_NAME(BOOST_PP_SEQ_HEAD(HG_BULK_LOCAL_PARAM))),  \
                    BOOST_PP_EMPTY())                                                                        \
                                                                                                             \
        /* Fill output structure */                                                                          \
        BOOST_PP_IF(with_ret, HG_SET_STRUCT_PARAMS(out_struct, ((ret_type)(ret))), BOOST_PP_EMPTY())         \
        BOOST_PP_IF(with_output, HG_SET_STRUCT_PARAMS(out_struct, out_params), BOOST_PP_EMPTY())             \
                                                                                                             \
        /* Respond back */                                                                                   \
        hg_ret = HG_Respond(handle, NULL, NULL,                                                              \
                            BOOST_PP_IF(BOOST_PP_OR(with_output, with_ret), &out_struct, NULL));             \
        if (hg_ret != HG_SUCCESS) {                                                                          \
            goto done;                                                                                       \
        }                                                                                                    \
                                                                                                             \
        /* Free input */                                                                                     \
        BOOST_PP_IF(                                                                                         \
            with_input, hg_ret = HG_Free_input(handle, &in_struct);                                          \
            if (hg_ret != HG_SUCCESS) { goto done; }, BOOST_PP_EMPTY())                                      \
                                                                                                             \
        /* Destroy handle */                                                                                 \
        hg_ret = HG_Destroy(handle);                                                                         \
        if (hg_ret != HG_SUCCESS) {                                                                          \
            goto done;                                                                                       \
        }                                                                                                    \
                                                                                                             \
done:                                                                                                        \
                                                                                                             \
        BOOST_PP_IF(with_thread, return thread_ret;, return hg_ret;)                                         \
    }                                                                                                        \
    BOOST_PP_IF(                                                                                             \
        with_thread,                                                                                         \
        static hg_return_t gen_func_name(hg_handle_t handle) {                                               \
            hg_return_t ret = HG_SUCCESS;                                                                    \
            hg_thread_pool_post(thread_pool, &BOOST_PP_CAT(gen_func_name, _thread), handle);                 \
            return ret;                                                                                      \
        },                                                                                                   \
        BOOST_PP_EMPTY())

#endif /* MERCURY_HL_MACROS_H */
