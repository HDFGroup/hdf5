/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_HL_H
#define MERCURY_HL_H

#include "mercury.h"
#include "mercury_bulk.h"
#include "mercury_request.h"

/*****************/
/* Public Macros */
/*****************/

/**
 * Define macros so that default classes/contexts can be easily renamed
 * if we ever need to. Users should use macros and not global variables
 * directly.
 */
#define HG_CLASS_DEFAULT         hg_class_default_g
#define HG_CONTEXT_DEFAULT       hg_context_default_g
#define HG_REQUEST_CLASS_DEFAULT hg_request_class_default_g

#ifdef __cplusplus
extern "C" {
#endif

/********************/
/* Public Variables */
/********************/

/* HG default */
extern HG_PUBLIC hg_class_t *HG_CLASS_DEFAULT;
extern HG_PUBLIC hg_context_t *HG_CONTEXT_DEFAULT;
extern HG_PUBLIC hg_request_class_t *HG_REQUEST_CLASS_DEFAULT;

/*********************/
/* Public Prototypes */
/*********************/

/**
 * Initialize Mercury high-level layer and create default classes/contexts.
 * If no info_string is passed, the HG HL layer will attempt to initialize
 * NA by using the value contained in the environment variable called
 * MERCURY_PORT_NAME.
 * \remark HG_Hl_finalize() is registered with atexit() so that default
 * classes/contexts are freed at process termination.
 *
 * \param na_info_string [IN]   host address with port number (e.g.,
 *                              "tcp://localhost:3344" or
 *                              "bmi+tcp://localhost:3344")
 * \param na_listen [IN]        listen for incoming connections
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Hl_init(const char *na_info_string, hg_bool_t na_listen);

/**
 * Initialize Mercury high-level layer with options provided by init_info.
 * Must be finalized with HG_Hl_finalize().
 * \remark HG_Hl_finalize() is registered with atexit() so that default
 * classes/contexts are freed at process termination.
 * \remark HG_Hl_init_opt() may become HG_Hl_init() in the future.
 *
 * \param na_info_string [IN]   host address with port number (e.g.,
 *                              "tcp://localhost:3344" or
 *                              "bmi+tcp://localhost:3344")
 * \param na_listen [IN]        listen for incoming connections
 * \param hg_init_info [IN]     (Optional) HG init info, NULL if no info
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Hl_init_opt(const char *na_info_string, hg_bool_t na_listen,
                                     const struct hg_init_info *hg_init_info);

/**
 * Finalize Mercury high-level layer.
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Hl_finalize(void);

/**
 * Lookup an address and wait for its completion. Address must be freed
 * using HG_Addr_free().
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Hl_addr_lookup_wait(hg_context_t *context, hg_request_class_t *request_class,
                                             const char *name, hg_addr_t *addr, unsigned int timeout);

/**
 * Forward a call and wait for its completion. A HG handle must have been
 * previously created. Output can be queried using HG_Get_output() and freed
 * using HG_Free_output().
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Hl_forward_wait(hg_request_class_t *request_class, hg_handle_t handle,
                                         void *in_struct, unsigned int timeout);

/**
 * Initiate a bulk data transfer and wait for its completion.
 *
 * \param context [IN]          pointer to HG context
 * \param op [IN]               transfer operation:
 *                                  - HG_BULK_PUSH
 *                                  - HG_BULK_PULL
 * \param origin_addr [IN]      abstract address of origin
 * \param origin_handle [IN]    abstract bulk handle
 * \param origin_offset [IN]    offset
 * \param local_handle [IN]     abstract bulk handle
 * \param local_offset [IN]     offset
 * \param size [IN]             size of data to be transferred
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Hl_bulk_transfer_wait(hg_context_t *context, hg_request_class_t *request_class,
                                               hg_bulk_op_t op, hg_addr_t origin_addr,
                                               hg_bulk_t origin_handle, hg_size_t origin_offset,
                                               hg_bulk_t local_handle, hg_size_t local_offset, hg_size_t size,
                                               unsigned int timeout);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_HL_H */
