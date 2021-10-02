/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_CORE_HEADER_H
#define MERCURY_CORE_HEADER_H

#include "mercury_core_types.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

#if defined(__GNUC__) || defined(_WIN32)
#pragma pack(push, 1)
#else
#warning "Proc header struct padding may not be consistent across platforms."
#endif
#ifdef HG_HAS_CHECKSUMS
union hg_core_header_hash {
    hg_uint16_t header; /* Header checksum (16-bits checksum) */
    hg_uint32_t pad;
};
#endif

struct hg_core_header_request {
    hg_uint8_t  hg;       /* Mercury identifier */
    hg_uint8_t  protocol; /* Version number */
    hg_uint64_t id;       /* RPC request identifier */
    hg_uint8_t  flags;    /* Flags */
    hg_uint8_t  cookie;   /* Cookie */
    /* 96 bits here */
#ifdef HG_HAS_CHECKSUMS
    union hg_core_header_hash hash; /* Hash */
    /* 128 bits here */
#endif
};

struct hg_core_header_response {
    hg_int8_t   ret_code; /* Return code */
    hg_uint8_t  flags;    /* Flags */
    hg_uint16_t cookie;   /* Cookie */
    hg_uint64_t pad;      /* Pad */
    /* 96 bits here */
#ifdef HG_HAS_CHECKSUMS
    union hg_core_header_hash hash; /* Hash */
    /* 128 bits here */
#endif
};
#if defined(__GNUC__) || defined(_WIN32)
#pragma pack(pop)
#endif

/* Common header struct request/response */
struct hg_core_header {
    union {
        struct hg_core_header_request  request;
        struct hg_core_header_response response;
    } msg;
#ifdef HG_HAS_CHECKSUMS
    void *checksum; /* Checksum of header */
#endif
};

/*
 * 0        HG_CORE_HEADER_SIZE             size
 * |______________|__________________________|
 * |    Header    |        Encoded Data      |
 * |______________|__________________________|
 *
 *
 * Request:
 * mercury byte / protocol version number / rpc id / flags / cookie / checksum
 *
 * Response:
 * flags / return code / cookie / checksum
 */

/*****************/
/* Public Macros */
/*****************/

/* Mercury identifier for packets sent */
#define HG_CORE_IDENTIFIER (('H' << 1) | ('G')) /* 0xD7 */

/* Mercury protocol version number */
#define HG_CORE_PROTOCOL_VERSION 0x05

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

static HG_INLINE size_t hg_core_header_request_get_size(void);
static HG_INLINE size_t hg_core_header_response_get_size(void);

/**
 * Get size reserved for request header (separate user data stored in payload).
 *
 * \return Non-negative size value
 */
static HG_INLINE size_t
hg_core_header_request_get_size(void)
{
    return sizeof(struct hg_core_header_request);
}

/**
 * Get size reserved for response header (separate user data stored in payload).
 *
 * \return Non-negative size value
 */
static HG_INLINE size_t
hg_core_header_response_get_size(void)
{
    return sizeof(struct hg_core_header_response);
}

/**
 * Initialize RPC request header.
 *
 * \param hg_core_header [IN/OUT]   pointer to request header structure
 *
 */
HG_PRIVATE void hg_core_header_request_init(struct hg_core_header *hg_core_header);

/**
 * Initialize RPC response header.
 *
 * \param hg_core_header [IN/OUT]   pointer to response header structure
 *
 */
HG_PRIVATE void hg_core_header_response_init(struct hg_core_header *hg_core_header);

/**
 * Finalize RPC request header.
 *
 * \param hg_core_header [IN/OUT]   pointer to request header structure
 *
 */
HG_PRIVATE void hg_core_header_request_finalize(struct hg_core_header *hg_core_header);

/**
 * Finalize RPC response header.
 *
 * \param hg_core_header [IN/OUT]   pointer to response header structure
 *
 */
HG_PRIVATE void hg_core_header_response_finalize(struct hg_core_header *hg_core_header);

/**
 * Reset RPC request header.
 *
 * \param hg_core_header [IN/OUT]   pointer to request header structure
 *
 */
HG_PRIVATE void hg_core_header_request_reset(struct hg_core_header *hg_core_header);

/**
 * Reset RPC response header.
 *
 * \param hg_core_header [IN/OUT]   pointer to response header structure
 *
 */
HG_PRIVATE void hg_core_header_response_reset(struct hg_core_header *hg_core_header);

/**
 * Process private information for sending/receiving RPC request.
 *
 * \param op [IN]                   operation type: HG_ENCODE / HG_DECODE
 * \param buf [IN/OUT]              buffer
 * \param buf_size [IN]             buffer size
 * \param hg_core_header [IN/OUT]   pointer to header structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PRIVATE hg_return_t hg_core_header_request_proc(hg_proc_op_t op, void *buf, size_t buf_size,
                                                   struct hg_core_header *hg_core_header);

/**
 * Process private information for sending/receiving response.
 *
 * \param op [IN]               operation type: HG_ENCODE / HG_DECODE
 * \param buf [IN/OUT]          buffer
 * \param buf_size [IN]         buffer size
 * \param header [IN/OUT]       pointer to header structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PRIVATE hg_return_t hg_core_header_response_proc(hg_proc_op_t op, void *buf, size_t buf_size,
                                                    struct hg_core_header *hg_core_header);

/**
 * Verify private information from request header.
 *
 * \param hg_core_header [IN]   pointer to request header structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PRIVATE hg_return_t hg_core_header_request_verify(const struct hg_core_header *hg_core_header);

/**
 * Verify private information from response header.
 *
 * \param hg_core_header [IN]   pointer to response header structure
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PRIVATE hg_return_t hg_core_header_response_verify(const struct hg_core_header *hg_core_header);

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_CORE_HEADER_H */
