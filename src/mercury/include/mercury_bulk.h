/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_BULK_H
#define MERCURY_BULK_H

#include "mercury_types.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/*****************/
/* Public Macros */
/*****************/

/* The memory attributes associated with the bulk handle
 * can be defined as read only, write only or read-write */
#define HG_BULK_READ_ONLY  (1 << 0)
#define HG_BULK_WRITE_ONLY (1 << 1)
#define HG_BULK_READWRITE  (HG_BULK_READ_ONLY | HG_BULK_WRITE_ONLY)

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Create an abstract bulk handle from specified memory segments.
 * Memory allocated is then freed when HG_Bulk_free() is called.
 * \remark If NULL is passed to buf_ptrs, i.e.,
 * \verbatim HG_Bulk_create(count, NULL, buf_sizes, flags, &handle) \endverbatim
 * memory for the missing buf_ptrs array will be internally allocated.
 *
 * \param hg_class [IN]         pointer to HG class
 * \param count [IN]            number of segments
 * \param buf_ptrs [IN]         array of pointers
 * \param buf_sizes [IN]        array of sizes
 * \param flags [IN]            permission flag:
 *                                - HG_BULK_READWRITE
 *                                - HG_BULK_READ_ONLY
 *                                - HG_BULK_WRITE_ONLY
 * \param handle [OUT]          pointer to returned abstract bulk handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_create(hg_class_t *hg_class, hg_uint32_t count, void **buf_ptrs,
                                     const hg_size_t *buf_sizes, hg_uint8_t flags, hg_bulk_t *handle);

/**
 * Free bulk handle.
 *
 * \param handle [IN/OUT]       abstract bulk handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_free(hg_bulk_t handle);

/**
 * Increment ref count on bulk handle.
 *
 * \param handle [IN]           abstract bulk handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_ref_incr(hg_bulk_t handle);

/**
 * Bind an existing bulk handle to a local HG context and associate its local
 * address. This function can be used to forward and share a bulk handle
 * between targets, which would not have direct access to the origin without
 * extra RPCs. In that case, the origin address of the bulk handle is embedded
 * and serialized/deserialized with HG_Bulk_serialize()/HG_Bulk_deserialize().
 * Users should note that binding a handle adds an extra overhead on
 * serialization, therefore it is recommended to use it with care.
 * When binding a handle on origin, HG_Bulk_bind_transfer() can be used since
 * origin information is embedded in the handle.
 *
 * Usage example:
 * Origin sends an RPC request with a bulk handle attached to target A, target A
 * forwards the origin's bulk handle to another target B. When target B receives
 * the deserialized bulk handle, it has the address/info required to initiate a
 * bulk transfer to/from the origin.
 * For that usage, the origin will have called this function to bind the bulk
 * handle to its local context, prior to sending the RPC request to target A.
 *
 * \param context [IN]          pointer to HG context
 * \param handle [IN]           abstract bulk handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_bind(hg_bulk_t handle, hg_context_t *context);

/**
 * Return attached addressing information from a handle that was previously
 * bound to a context using HG_Bulk_bind().
 *
 * \param handle [IN]           abstract bulk handle
 *
 * \return abstract HG address or HG_ADDR_NULL in case of error
 */
HG_PUBLIC hg_addr_t HG_Bulk_get_addr(hg_bulk_t handle);

/**
 * Return attached context ID from a handle that was previously bound to a
 * context using HG_Bulk_bind().
 *
 * \param handle [IN]           abstract bulk handle
 *
 * \return valid context ID or 0 by default
 */
HG_PUBLIC hg_uint8_t HG_Bulk_get_context_id(hg_bulk_t handle);

/**
 * Access bulk handle to retrieve memory segments abstracted by handle.
 * \remark When using mercury in co-resident mode (i.e., when addr passed is
 * self addr), this function allows to avoid copy of bulk data by directly
 * accessing pointers from an existing HG bulk handle.
 *
 * \param handle [IN]            abstract bulk handle
 * \param offset [IN]            bulk offset
 * \param size [IN]              bulk size
 * \param flags [IN]             permission flag:
 *                                 - HG_BULK_READWRITE
 *                                 - HG_BULK_READ_ONLY
 * \param max_count [IN]         maximum number of segments to be returned
 * \param buf_ptrs [IN/OUT]      array of buffer pointers
 * \param buf_sizes [IN/OUT]     array of buffer sizes
 * \param actual_count [OUT]     actual number of segments returned
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_access(hg_bulk_t handle, hg_size_t offset, hg_size_t size, hg_uint8_t flags,
                                     hg_uint32_t max_count, void **buf_ptrs, hg_size_t *buf_sizes,
                                     hg_uint32_t *actual_count);

/**
 * Get total size of data abstracted by bulk handle.
 *
 * \param handle [IN]           abstract bulk handle
 *
 * \return Non-negative value
 */
static HG_INLINE hg_size_t HG_Bulk_get_size(hg_bulk_t handle);

/**
 * Get total number of segments abstracted by bulk handle.
 *
 * \param handle [IN]           abstract bulk handle
 *
 * \return Non-negative value
 */
static HG_INLINE hg_uint32_t HG_Bulk_get_segment_count(hg_bulk_t handle);

/**
 * Get permission flags set on an existing bulk handle.
 *
 * \param handle [IN]           abstract bulk handle
 *
 * \return Non-negative value
 */
static HG_INLINE hg_uint8_t HG_Bulk_get_flags(hg_bulk_t handle);

/**
 * Get size required to serialize bulk handle.
 *
 * \param handle [IN]           abstract bulk handle
 * \param flags [IN]            option flags, valid flags are:
 *                                HG_BULK_SM, HG_BULK_EAGER
 *
 * \return Non-negative value
 */
HG_PUBLIC hg_size_t HG_Bulk_get_serialize_size(hg_bulk_t handle, unsigned long flags);

/**
 * Serialize bulk handle into a buffer.
 *
 * \param buf [IN/OUT]          pointer to buffer
 * \param buf_size [IN]         buffer size
 * \param flags [IN]            option flags, valid flags are:
 *                                HG_BULK_SM, HG_BULK_EAGER
 * \param handle [IN]           abstract bulk handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_serialize(void *buf, hg_size_t buf_size, unsigned long flags, hg_bulk_t handle);

/**
 * Deserialize bulk handle from an existing buffer.
 *
 * \param hg_class [IN]         pointer to HG class
 * \param handle [OUT]          abstract bulk handle
 * \param buf [IN]              pointer to buffer
 * \param buf_size [IN]         buffer size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_deserialize(hg_class_t *hg_class, hg_bulk_t *handle, const void *buf,
                                          hg_size_t buf_size);

/**
 * Transfer data to/from origin using abstract bulk handles and explicit origin
 * address information. After completion, user callback is placed into a
 * completion queue and can be triggered using HG_Trigger().
 *
 * \param context [IN]          pointer to HG context
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 * \param op [IN]               transfer operation:
 *                                  - HG_BULK_PUSH
 *                                  - HG_BULK_PULL
 * \param origin_addr [IN]      abstract address of origin
 * \param origin_handle [IN]    abstract bulk handle
 * \param origin_offset [IN]    offset
 * \param local_handle [IN]     abstract bulk handle
 * \param local_offset [IN]     offset
 * \param size [IN]             size of data to be transferred
 * \param op_id [OUT]           pointer to returned operation ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_transfer(hg_context_t *context, hg_cb_t callback, void *arg, hg_bulk_op_t op,
                                       hg_addr_t origin_addr, hg_bulk_t origin_handle,
                                       hg_size_t origin_offset, hg_bulk_t local_handle,
                                       hg_size_t local_offset, hg_size_t size, hg_op_id_t *op_id);

/**
 * Transfer data to/from origin using abstract bulk handles and implicit origin
 * information (embedded in the origin handle). After completion, user callback
 * is placed into a completion queue and can be triggered using HG_Trigger().
 *
 * \param context [IN]          pointer to HG context
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 * \param op [IN]               transfer operation:
 *                                  - HG_BULK_PUSH
 *                                  - HG_BULK_PULL
 * \param origin_handle [IN]    abstract bulk handle
 * \param origin_offset [IN]    offset
 * \param local_handle [IN]     abstract bulk handle
 * \param local_offset [IN]     offset
 * \param size [IN]             size of data to be transferred
 * \param op_id [OUT]           pointer to returned operation ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_bind_transfer(hg_context_t *context, hg_cb_t callback, void *arg,
                                            hg_bulk_op_t op, hg_bulk_t origin_handle, hg_size_t origin_offset,
                                            hg_bulk_t local_handle, hg_size_t local_offset, hg_size_t size,
                                            hg_op_id_t *op_id);

/**
 * Transfer data to/from origin using abstract bulk handles, explicit origin
 * address information and origin context ID (associating the transfer to a
 * remote context ID). After completion, user callback is placed into a
 * completion queue and can be triggered using HG_Trigger().
 *
 * \param context [IN]          pointer to HG context
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 * \param op [IN]               transfer operation:
 *                                  - HG_BULK_PUSH
 *                                  - HG_BULK_PULL
 * \param origin_addr [IN]      abstract address of origin
 * \param origin_id [IN]        context ID of origin
 * \param origin_handle [IN]    abstract bulk handle
 * \param origin_offset [IN]    offset
 * \param local_handle [IN]     abstract bulk handle
 * \param local_offset [IN]     offset
 * \param size [IN]             size of data to be transferred
 * \param op_id [OUT]           pointer to returned operation ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_transfer_id(hg_context_t *context, hg_cb_t callback, void *arg, hg_bulk_op_t op,
                                          hg_addr_t origin_addr, hg_uint8_t origin_id,
                                          hg_bulk_t origin_handle, hg_size_t origin_offset,
                                          hg_bulk_t local_handle, hg_size_t local_offset, hg_size_t size,
                                          hg_op_id_t *op_id);

/**
 * Cancel an ongoing operation.
 *
 * \param op_id [IN]            operation ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Bulk_cancel(hg_op_id_t op_id);

/************************************/
/* Local Type and Struct Definition */
/************************************/

/* HG bulk descriptor info */
struct hg_bulk_desc_info {
    hg_size_t   len;           /* Size of region */
    hg_uint32_t segment_count; /* Segment count */
    hg_uint8_t  flags;         /* Flags of operation access */
};

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
HG_Bulk_get_size(hg_bulk_t handle)
{
    return ((struct hg_bulk_desc_info *)handle)->len;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_uint32_t
HG_Bulk_get_segment_count(hg_bulk_t handle)
{
    return ((struct hg_bulk_desc_info *)handle)->segment_count;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_uint8_t
HG_Bulk_get_flags(hg_bulk_t handle)
{
    return ((struct hg_bulk_desc_info *)handle)->flags;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_BULK_H */
