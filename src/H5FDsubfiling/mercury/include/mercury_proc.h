/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_PROC_H
#define MERCURY_PROC_H

#include "mercury_types.h"

#include <string.h>
#ifdef HG_HAS_XDR
#include <limits.h>
#include <rpc/types.h>
#include <rpc/xdr.h>
#ifdef __APPLE__
#define xdr_int8_t   xdr_char
#define xdr_uint8_t  xdr_u_char
#define xdr_uint16_t xdr_u_int16_t
#define xdr_uint32_t xdr_u_int32_t
#define xdr_uint64_t xdr_u_int64_t
#endif
#define xdr_hg_int8_t   xdr_int8_t
#define xdr_hg_uint8_t  xdr_uint8_t
#define xdr_hg_int16_t  xdr_int16_t
#define xdr_hg_uint16_t xdr_uint16_t
#define xdr_hg_int32_t  xdr_int32_t
#define xdr_hg_uint32_t xdr_uint32_t
#define xdr_hg_int64_t  xdr_int64_t
#define xdr_hg_uint64_t xdr_uint64_t
#endif

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

/**
 * Hash methods available for proc.
 */
typedef enum { HG_CRC16, HG_CRC32, HG_CRC64, HG_NOHASH } hg_proc_hash_t;

/*****************/
/* Public Macros */
/*****************/

/* Encode/decode version number into uint32 */
#define HG_GET_MAJOR(value) ((value >> 24) & 0xFF)
#define HG_GET_MINOR(value) ((value >> 16) & 0xFF)
#define HG_GET_PATCH(value) (value & 0xFFFF)
#define HG_VERSION          ((HG_VERSION_MAJOR << 24) | (HG_VERSION_MINOR << 16) | HG_VERSION_PATCH)

/**
 * Operation flags.
 */
#define HG_PROC_SM         (1 << 0)
#define HG_PROC_BULK_EAGER (1 << 1)

/* Branch predictor hints */
#ifndef _WIN32
#ifndef likely
#define likely(x) __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#define unlikely(x) __builtin_expect(!!(x), 0)
#endif
#else
#ifndef likely
#define likely(x) (x)
#endif
#ifndef unlikely
#define unlikely(x) (x)
#endif
#endif

/* Check whether size exceeds current proc size left */
#ifdef HG_HAS_XDR
#define HG_PROC_CHECK_SIZE(proc, size, label, ret)                                                           \
    do {                                                                                                     \
        if (unlikely(((struct hg_proc *)proc)->current_buf->size_left < size)) {                             \
            ret = HG_OVERFLOW;                                                                               \
            goto label;                                                                                      \
        }                                                                                                    \
    } while (0)
#else
#define HG_PROC_CHECK_SIZE(proc, size, label, ret)                                                           \
    do {                                                                                                     \
        if (unlikely(((struct hg_proc *)proc)->current_buf->size_left < size)) {                             \
            ret = hg_proc_set_size(proc, hg_proc_get_size(proc) + size);                                     \
            if (ret != HG_SUCCESS)                                                                           \
                goto label;                                                                                  \
        }                                                                                                    \
    } while (0)
#endif

/* Encode type */
#define HG_PROC_TYPE_ENCODE(proc, data, size)                                                                \
    memcpy(((struct hg_proc *)proc)->current_buf->buf_ptr, data, size)

/* Decode type */
#define HG_PROC_TYPE_DECODE(proc, data, size)                                                                \
    memcpy(data, ((struct hg_proc *)proc)->current_buf->buf_ptr, size)

/* Update proc pointers */
#define HG_PROC_UPDATE(proc, size)                                                                           \
    do {                                                                                                     \
        ((struct hg_proc *)proc)->current_buf->buf_ptr =                                                     \
            (char *)((struct hg_proc *)proc)->current_buf->buf_ptr + size;                                   \
        ((struct hg_proc *)proc)->current_buf->size_left -= size;                                            \
    } while (0)

/* Update checksum */
#ifdef HG_HAS_CHECKSUMS
#define HG_PROC_CHECKSUM_UPDATE(proc, data, size) hg_proc_checksum_update(proc, data, size)
#else
#define HG_PROC_CHECKSUM_UPDATE(proc, data, size)
#endif

/* Base proc function */
#ifdef HG_HAS_XDR
#define HG_PROC_TYPE(proc, type, data, label, ret)                                                           \
    do {                                                                                                     \
        HG_PROC_CHECK_SIZE(proc, sizeof(type), label, ret);                                                  \
                                                                                                             \
        if (xdr_##type(hg_proc_get_xdr_ptr(proc), data) == 0) {                                              \
            ret = HG_PROTOCOL_ERROR;                                                                         \
            goto label;                                                                                      \
        }                                                                                                    \
                                                                                                             \
        HG_PROC_UPDATE(proc, sizeof(type));                                                                  \
        HG_PROC_CHECKSUM_UPDATE(proc, data, sizeof(type));                                                   \
    } while (0)
#else
#define HG_PROC_TYPE(proc, type, data, label, ret)                                                           \
    do {                                                                                                     \
        /* Do nothing in HG_FREE for basic types */                                                          \
        if (hg_proc_get_op(proc) == HG_FREE)                                                                 \
            goto label;                                                                                      \
                                                                                                             \
        /* If not enough space allocate extra space if encoding or just */                                   \
        /* get extra buffer if decoding */                                                                   \
        HG_PROC_CHECK_SIZE(proc, sizeof(type), label, ret);                                                  \
                                                                                                             \
        /* Encode, decode type */                                                                            \
        if (hg_proc_get_op(proc) == HG_ENCODE)                                                               \
            HG_PROC_TYPE_ENCODE(proc, data, sizeof(type));                                                   \
        else                                                                                                 \
            HG_PROC_TYPE_DECODE(proc, data, sizeof(type));                                                   \
                                                                                                             \
        /* Update proc pointers etc */                                                                       \
        HG_PROC_UPDATE(proc, sizeof(type));                                                                  \
        HG_PROC_CHECKSUM_UPDATE(proc, data, sizeof(type));                                                   \
    } while (0)
#endif

/* Base proc function */
#ifdef HG_HAS_XDR
#define HG_PROC_BYTES(proc, data, size, label, ret)                                                          \
    do {                                                                                                     \
        HG_PROC_CHECK_SIZE(proc, size, label, ret);                                                          \
                                                                                                             \
        if (xdr_bytes(hg_proc_get_xdr_ptr(proc), (char **)&data, (u_int *)&size, UINT_MAX) == 0) {           \
            ret = HG_PROTOCOL_ERROR;                                                                         \
            goto label;                                                                                      \
        }                                                                                                    \
                                                                                                             \
        HG_PROC_UPDATE(proc, size);                                                                          \
        HG_PROC_CHECKSUM_UPDATE(proc, data, size);                                                           \
    } while (0)
#else
#define HG_PROC_BYTES(proc, data, size, label, ret)                                                          \
    do {                                                                                                     \
        /* Do nothing in HG_FREE for basic types */                                                          \
        if (hg_proc_get_op(proc) == HG_FREE)                                                                 \
            goto label;                                                                                      \
                                                                                                             \
        /* If not enough space allocate extra space if encoding or just */                                   \
        /* get extra buffer if decoding */                                                                   \
        HG_PROC_CHECK_SIZE(proc, size, label, ret);                                                          \
                                                                                                             \
        /* Encode, decode type */                                                                            \
        if (hg_proc_get_op(proc) == HG_ENCODE)                                                               \
            HG_PROC_TYPE_ENCODE(proc, data, size);                                                           \
        else                                                                                                 \
            HG_PROC_TYPE_DECODE(proc, data, size);                                                           \
                                                                                                             \
        /* Update proc pointers etc */                                                                       \
        HG_PROC_UPDATE(proc, size);                                                                          \
        HG_PROC_CHECKSUM_UPDATE(proc, data, size);                                                           \
    } while (0)
#endif

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Create a new encoding/decoding processor.
 *
 * \param hg_class [IN]         HG class
 * \param hash [IN]             hash method used for computing checksum
 *                              (if NULL, checksum is not computed)
 *                              hash method: HG_CRC16, HG_CRC64, HG_NOHASH
 * \param proc [OUT]            pointer to abstract processor object
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_create(hg_class_t *hg_class, hg_proc_hash_t hash, hg_proc_t *proc);

/**
 * Create a new encoding/decoding processor.
 *
 * \param hg_class [IN]         HG class
 * \param buf [IN]              pointer to buffer that will be used for
 *                              serialization/deserialization
 * \param buf_size [IN]         buffer size
 * \param op [IN]               operation type: HG_ENCODE / HG_DECODE /
 * HG_FREE \param hash [IN]             hash method used for computing
 * checksum (if NULL, checksum is not computed) hash method: HG_CRC16,
 * HG_CRC64, HG_NOHASH \param proc [OUT]            pointer to abstract
 * processor object
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_create_set(hg_class_t *hg_class, void *buf, hg_size_t buf_size, hg_proc_op_t op,
                                         hg_proc_hash_t hash, hg_proc_t *proc);

/**
 * Free the processor.
 *
 * \param proc [IN/OUT]         abstract processor object
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_free(hg_proc_t proc);

/**
 * Reset the processor.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param buf [IN]              pointer to buffer that will be used for
 *                              serialization/deserialization
 * \param buf_size [IN]         buffer size
 * \param op [IN]               operation type: HG_ENCODE / HG_DECODE /
 * HG_FREE
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_reset(hg_proc_t proc, void *buf, hg_size_t buf_size, hg_proc_op_t op);

/**
 * Get the HG class associated to the processor.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return HG class
 */
static HG_INLINE hg_class_t *hg_proc_get_class(hg_proc_t proc);

/**
 * Get the operation type associated to the processor.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Operation type
 */
static HG_INLINE hg_proc_op_t hg_proc_get_op(hg_proc_t proc);

/**
 * Set flags to be associated with the processor.
 * Flags are reset after a call to hg_proc_reset().
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Non-negative flag value
 */
static HG_INLINE void hg_proc_set_flags(hg_proc_t proc, hg_uint8_t flags);

/**
 * Get the flags associated to the processor.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Non-negative flag value
 */
static HG_INLINE hg_uint8_t hg_proc_get_flags(hg_proc_t proc);

/**
 * Get buffer size available for processing.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Non-negative size value
 */
static HG_INLINE hg_size_t hg_proc_get_size(hg_proc_t proc);

/**
 * Get amount of buffer space that has actually been consumed
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Non-negative size value
 */
static HG_INLINE hg_size_t hg_proc_get_size_used(hg_proc_t proc);

/**
 * Request a new buffer size. This will modify the size of the buffer
 * attached to the processor or create an extra processing buffer.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param buf_size [IN]         buffer size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_set_size(hg_proc_t proc, hg_size_t buf_size);

/**
 * Get size left for processing.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Non-negative size value
 */
static HG_INLINE hg_size_t hg_proc_get_size_left(hg_proc_t proc);

/**
 * Get pointer to current buffer. Will reserve data_size for manual
 * encoding.
 *
 * \param proc [IN]             abstract processor object
 * \param data_size [IN]        data size
 *
 * \return Buffer pointer
 */
HG_PUBLIC void *hg_proc_save_ptr(hg_proc_t proc, hg_size_t data_size);

/**
 * Restore pointer from current buffer.
 *
 * \param proc [IN]             abstract processor object
 * \param data [IN]             pointer to data
 * \param data_size [IN]        data size
 *
 * \return Buffer pointer
 */
HG_PUBLIC hg_return_t hg_proc_restore_ptr(hg_proc_t proc, void *data, hg_size_t data_size);

#ifdef HG_HAS_XDR
/**
 * Get pointer to current XDR stream (for manual encoding).
 *
 * \param proc [IN]             abstract processor object
 *
 * \return XDR stream pointer
 */
static HG_INLINE XDR *hg_proc_get_xdr_ptr(hg_proc_t proc);
#endif

/**
 * Get eventual extra buffer used by processor.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Pointer to buffer or NULL if no extra buffer has been used
 */
static HG_INLINE void *hg_proc_get_extra_buf(hg_proc_t proc);

/**
 * Get eventual size of the extra buffer used by processor.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return Size of buffer or 0 if no extra buffer has been used
 */
static HG_INLINE hg_size_t hg_proc_get_extra_size(hg_proc_t proc);

/**
 * Set extra buffer to mine (if other calls mine, buffer is no longer freed
 * after hg_proc_free())
 *
 * \param proc [IN]             abstract processor object
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_set_extra_buf_is_mine(hg_proc_t proc, hg_bool_t mine);

/**
 * Flush the proc after data has been encoded or decoded and finalize
 * internal checksum if checksum of data processed was initially requested.
 *
 * \param proc [IN]             abstract processor object
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_flush(hg_proc_t proc);

#ifdef HG_HAS_CHECKSUMS
/**
 * Retrieve internal proc checksum hash.
 * \remark Must be used after hg_proc_flush() has been called so that the
 * internally computed checksum is in a finalized state.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param hash [IN/OUT]         pointer to hash
 * \param hash_size [IN]        hash size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_checksum_get(hg_proc_t proc, void *hash, hg_size_t hash_size);

/**
 * Verify that the hash passed matches the internal proc checksum.
 * \remark Must be used after hg_proc_flush() has been called so that the
 * internally computed checksum is in a finalized state.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param hash [IN]             pointer to hash
 * \param hash_size [IN]        hash size
 *
 * \return HG_SUCCESS if matches or corresponding HG error code
 */
HG_PUBLIC hg_return_t hg_proc_checksum_verify(hg_proc_t proc, const void *hash, hg_size_t hash_size);
#endif

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_int8_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_uint8_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_int16_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_uint16_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_int32_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_uint32_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_int64_t(hg_proc_t proc, void *data);

/**
 * Generic processing routine.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_hg_uint64_t(hg_proc_t proc, void *data);

/* Note: float types are not supported but can be built on top of the existing
 * proc routines; encoding floats using XDR could modify checksum */

/**
 * Generic processing routine for encoding stream of bytes.
 *
 * \param proc [IN/OUT]         abstract processor object
 * \param data [IN/OUT]         pointer to data
 * \param data_size [IN]        data size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t hg_proc_bytes(hg_proc_t proc, void *data, hg_size_t data_size);

/**
 * For convenience map stdint types to hg types
 */
#define hg_proc_int8_t   hg_proc_hg_int8_t
#define hg_proc_uint8_t  hg_proc_hg_uint8_t
#define hg_proc_int16_t  hg_proc_hg_int16_t
#define hg_proc_uint16_t hg_proc_hg_uint16_t
#define hg_proc_int32_t  hg_proc_hg_int32_t
#define hg_proc_uint32_t hg_proc_hg_uint32_t
#define hg_proc_int64_t  hg_proc_hg_int64_t
#define hg_proc_uint64_t hg_proc_hg_uint64_t

/* Map mercury common types */
#define hg_proc_hg_bool_t hg_proc_hg_uint8_t
#define hg_proc_hg_ptr_t  hg_proc_hg_uint64_t
#define hg_proc_hg_size_t hg_proc_hg_uint64_t
#define hg_proc_hg_id_t   hg_proc_hg_uint32_t

/* Map hg_proc_raw/hg_proc_memcpy to hg_proc_bytes */
#define hg_proc_memcpy hg_proc_raw
#define hg_proc_raw    hg_proc_bytes

/* Update checksum */
#ifdef HG_HAS_CHECKSUMS
HG_PUBLIC void hg_proc_checksum_update(hg_proc_t proc, void *data, hg_size_t data_size);
#endif

/************************************/
/* Local Type and Struct Definition */
/************************************/

/* HG proc buf */
struct hg_proc_buf {
    void *    buf;       /* Pointer to allocated buffer */
    void *    buf_ptr;   /* Pointer to current position */
    hg_size_t size;      /* Total buffer size */
    hg_size_t size_left; /* Available size for user */
    hg_bool_t is_mine;
#ifdef HG_HAS_XDR
    XDR xdr;
#endif
};

/* HG proc */
struct hg_proc {
    struct hg_proc_buf  proc_buf;
    struct hg_proc_buf  extra_buf;
    hg_class_t *        hg_class; /* HG class */
    struct hg_proc_buf *current_buf;
#ifdef HG_HAS_CHECKSUMS
    void * checksum;      /* Checksum */
    void * checksum_hash; /* Base checksum buf */
    size_t checksum_size; /* Checksum size */
#endif
    hg_proc_op_t op;
    hg_uint8_t   flags;
};

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_class_t *
                 hg_proc_get_class(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->hg_class;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_proc_op_t
hg_proc_get_op(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->op;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE void
hg_proc_set_flags(hg_proc_t proc, hg_uint8_t flags)
{
    ((struct hg_proc *)proc)->flags = flags;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_uint8_t
hg_proc_get_flags(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->flags;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
hg_proc_get_size(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->proc_buf.size + ((struct hg_proc *)proc)->extra_buf.size;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
hg_proc_get_size_used(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->current_buf->size - ((struct hg_proc *)proc)->current_buf->size_left;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
hg_proc_get_size_left(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->current_buf->size_left;
}

/*---------------------------------------------------------------------------*/
#ifdef HG_HAS_XDR
static HG_INLINE XDR *
                 hg_proc_get_xdr_ptr(hg_proc_t proc)
{
    return &((struct hg_proc *)proc)->current_buf->xdr;
}
#endif

/*---------------------------------------------------------------------------*/
static HG_INLINE void *
hg_proc_get_extra_buf(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->extra_buf.buf;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
hg_proc_get_extra_size(hg_proc_t proc)
{
    return ((struct hg_proc *)proc)->extra_buf.size;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_int8_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_int8_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_uint8_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_uint8_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_int16_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_int16_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_uint16_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_uint16_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_int32_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_int32_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_uint32_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_uint32_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_int64_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_int64_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_hg_uint64_t(hg_proc_t proc, void *data)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_TYPE(proc, hg_uint64_t, data, done, ret);

done:
    return ret;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
hg_proc_bytes(hg_proc_t proc, void *data, hg_size_t data_size)
{
    hg_return_t ret = HG_SUCCESS;

    HG_PROC_BYTES(proc, data, data_size, done, ret);

done:
    return ret;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_PROC_H */
