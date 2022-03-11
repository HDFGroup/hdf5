/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_MEM_POOL_H
#define MERCURY_MEM_POOL_H

#include "mercury_util_config.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/**
 * Register memory block.
 *
 * \param buf [IN]              pointer to buffer
 * \param size [IN]             buffer size
 * \param handle [OUT]          handle
 * \param arg [IN/OUT]          optional arguments
 *
 * \return HG_UTIL_SUCCESS if successful / error code otherwise
 */
typedef int (*hg_mem_pool_register_func_t)(const void *buf, size_t size, void **handle, void *arg);

/**
 * Deregister memory block.
 *
 * \param handle [IN/OUT]       handle
 * \param arg [IN/OUT]          optional arguments
 *
 * \return HG_UTIL_SUCCESS if successful / error code otherwise
 */
typedef int (*hg_mem_pool_deregister_func_t)(void *handle, void *arg);

/*****************/
/* Public Macros */
/*****************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Create a memory pool with \block_count of size \chunk_count x \chunk_size
 * bytes. Optionally register and deregister memory for each block using
 * \register_func and \deregister_func respectively.
 *
 * \param chunk_size [IN]       size of chunks
 * \param chunk_count [IN]      number of chunks
 * \param block_count [IN]      number of blocks
 * \param register_func [IN]    pointer to register function
 * \param deregister_func [IN]  pointer to deregister function
 * \param arg [IN/OUT]          optional arguments passed to register functions
 *
 * \return HG_UTIL_SUCCESS if successful / error code otherwise
 */
HG_UTIL_PUBLIC struct hg_mem_pool *hg_mem_pool_create(size_t chunk_size, size_t chunk_count,
                                                      size_t                        block_count,
                                                      hg_mem_pool_register_func_t   register_func,
                                                      hg_mem_pool_deregister_func_t deregister_func,
                                                      void *                        arg);

/**
 * Destroy a memory pool.
 *
 * \param hg_mem_pool [IN/OUT]  pointer to memory pool
 *
 */
HG_UTIL_PUBLIC void hg_mem_pool_destroy(struct hg_mem_pool *hg_mem_pool);

/**
 * Allocate \size bytes and optionally return a memory handle
 * \mr_handle if registration functions were provided.
 *
 * \param hg_mem_pool [IN/OUT]  pointer to memory pool
 * \param size [IN]             requested size
 * \param mr_handle [OUT]       pointer to memory handle
 *
 * \return pointer to memory block
 */
HG_UTIL_PUBLIC void *hg_mem_pool_alloc(struct hg_mem_pool *hg_mem_pool, size_t size, void **mr_handle);

/**
 * Release memory at address \mem_ptr.
 *
 * \param hg_mem_pool [IN/OUT]  pointer to memory pool
 * \param mem_ptr [IN]          pointer to memory
 * \param mr_handle [INT]       pointer to memory handle
 *
 */
HG_UTIL_PUBLIC void hg_mem_pool_free(struct hg_mem_pool *hg_mem_pool, void *mem_ptr, void *mr_handle);

/**
 * Retrieve chunk offset relative to the address used for registering
 * the memory block it belongs to.
 *
 * \param hg_mem_pool [IN/OUT]  pointer to memory pool
 * \param mem_ptr [IN]          pointer to memory
 * \param mr_handle [INT]       pointer to memory handle
 *
 * \return offset within registered block.
 */
HG_UTIL_PUBLIC size_t hg_mem_pool_chunk_offset(struct hg_mem_pool *hg_mem_pool, void *mem_ptr,
                                               void *mr_handle);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_MEM_POOL_H */
