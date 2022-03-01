/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_MEM_H
#define MERCURY_MEM_H

#include "mercury_util_config.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/*****************/
/* Public Macros */
/*****************/

#define HG_MEM_CACHE_LINE_SIZE 64
#define HG_MEM_PAGE_SIZE       4096

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Get system default page size.
 *
 * \return page size on success or negative on failure
 */
HG_UTIL_PUBLIC long hg_mem_get_page_size(void);

/**
 * Allocate size bytes and return a pointer to the allocated memory.
 * The memory address will be a multiple of alignment, which must be a power of
 * two, and size should be a multiple of alignment.
 *
 * \param alignment [IN]        alignment size
 * \param size [IN]             total requested size
 *
 * \return a pointer to the allocated memory, or NULL in case of failure
 */
HG_UTIL_PUBLIC void *hg_mem_aligned_alloc(size_t alignment, size_t size);

/**
 * Free memory allocated from hg_aligned_alloc().
 *
 * \param mem_ptr [IN]          pointer to allocated memory
 */
HG_UTIL_PUBLIC void hg_mem_aligned_free(void *mem_ptr);

/**
 * Allocate a buffer with a `size`-bytes, `alignment`-aligned payload
 * preceded by a `header_size` header, padding the allocation with up
 * to `alignment - 1` bytes to ensure that the payload is properly aligned.
 *
 * If `alignment` is 0, do not try to align the payload.  It's ok if
 * `size` is 0, however, behavior is undefined if both `header_size`
 * and `size` are 0.
 *
 * \param header_size [IN]      size of header
 * \param alignment [IN]        alignment size
 * \param size [IN]             requested payload size
 *
 * \return a pointer to the payload or NULL on failure
 */
HG_UTIL_PUBLIC void *hg_mem_header_alloc(size_t header_size, size_t alignment, size_t size);

/**
 * Free the memory that was returned previously by a call to
 * `hg_mem_header_alloc()`.
 *
 * \param header_size [IN]      size of header
 * \param alignment [IN]        alignment size
 * \param mem_ptr [IN]          memory pointer
 */
HG_UTIL_PUBLIC void hg_mem_header_free(size_t header_size, size_t alignment, void *mem_ptr);

/**
 * Create/open a shared-memory mapped file of size \size with name \name.
 *
 * \param name [IN]             name of mapped file
 * \param size [IN]             total requested size
 * \param create [IN]           create file if not existing
 *
 * \return a pointer to the mapped memory region, or NULL in case of failure
 */
HG_UTIL_PUBLIC void *hg_mem_shm_map(const char *name, size_t size, hg_util_bool_t create);

/**
 * Unmap a previously mapped region and close the file.
 *
 * \param name [IN]             name of mapped file
 * \param mem_ptr [IN]          pointer to mapped memory region
 * \param size [IN]             size range of the mapped region
 *
 * \return non-negative on success, or negative in case of failure
 */
HG_UTIL_PUBLIC int hg_mem_shm_unmap(const char *name, void *mem_ptr, size_t size);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_MEM_H */
