/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_CORE_TYPES_H
#define MERCURY_CORE_TYPES_H

#include "mercury_config.h"
#include "na_types.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef hg_uint64_t hg_size_t; /* Size */
typedef hg_uint64_t hg_id_t;   /* RPC ID */

/**
 * HG init info struct
 * NB. should be initialized using HG_INIT_INFO_INITIALIZER
 */
struct hg_init_info {
    /* NA init info struct, see na_types.h for documentation */
    struct na_init_info na_init_info;

    /* Optional NA class that can be used for initializing an HG class. Using
     * that option makes the init string passed to HG_Init() ignored.
     * Default is: NULL */
    na_class_t *na_class;

    /* Controls the initial number of requests that are posted on context
     * creation when the HG class is initialized with listen set to true.
     * A value of zero is equivalent to using the internal default value.
     * Default value is: 256 */
    hg_uint32_t request_post_init;

    /* Controls the number of requests that are incrementally posted when the
     * initial number of requests is exhausted, a value of 0 means that only the
     * initial number of requests will be re-used after they complete. Note that
     * if the number of requests that are posted reaches 0, the underlying
     * NA transport is responsible for queueing incoming requests. This value is
     * used only if \request_post_init is set to a non-zero value.
     * Default value is: 256 */
    hg_uint32_t request_post_incr;

    /* Controls whether the NA shared-memory interface should be automatically
     * used if/when the RPC target address shares the same node as its origin.
     * Default is: false */
    hg_bool_t auto_sm;

    /* Controls whether mercury should _NOT_ attempt to transfer small bulk data
     * along with the RPC request.
     * Default is: false */
    hg_bool_t no_bulk_eager;

    /* Disable internal loopback interface that enables forwarding of RPC
     * requests to self addresses. Doing so will force traffic to be routed
     * through NA. For performance reasons, users should be cautious when using
     * that option.
     * Default is: false */
    hg_bool_t no_loopback;

    /* (Debug) Print stats at exit.
     * Default is: false */
    hg_bool_t stats;
};

/* Error return codes:
 * Functions return 0 for success or corresponding return code */
#define HG_RETURN_VALUES                                                                                     \
    X(HG_SUCCESS)        /*!< operation succeeded */                                                         \
    X(HG_PERMISSION)     /*!< operation not permitted */                                                     \
    X(HG_NOENTRY)        /*!< no such file or directory */                                                   \
    X(HG_INTERRUPT)      /*!< operation interrupted */                                                       \
    X(HG_AGAIN)          /*!< operation must be retried */                                                   \
    X(HG_NOMEM)          /*!< out of memory */                                                               \
    X(HG_ACCESS)         /*!< permission denied */                                                           \
    X(HG_FAULT)          /*!< bad address */                                                                 \
    X(HG_BUSY)           /*!< device or resource busy */                                                     \
    X(HG_EXIST)          /*!< entry already exists */                                                        \
    X(HG_NODEV)          /*!< no such device */                                                              \
    X(HG_INVALID_ARG)    /*!< invalid argument */                                                            \
    X(HG_PROTOCOL_ERROR) /*!< protocol error */                                                              \
    X(HG_OVERFLOW)       /*!< value too large */                                                             \
    X(HG_MSGSIZE)        /*!< message size too long */                                                       \
    X(HG_PROTONOSUPPORT) /*!< protocol not supported */                                                      \
    X(HG_OPNOTSUPPORTED) /*!< operation not supported on endpoint */                                         \
    X(HG_ADDRINUSE)      /*!< address already in use */                                                      \
    X(HG_ADDRNOTAVAIL)   /*!< cannot assign requested address */                                             \
    X(HG_HOSTUNREACH)    /*!< cannot reach host during operation */                                          \
    X(HG_TIMEOUT)        /*!< operation reached timeout */                                                   \
    X(HG_CANCELED)       /*!< operation canceled */                                                          \
    X(HG_CHECKSUM_ERROR) /*!< checksum error */                                                              \
    X(HG_NA_ERROR)       /*!< generic NA error */                                                            \
    X(HG_OTHER_ERROR)    /*!< generic HG error */                                                            \
    X(HG_RETURN_MAX)

#define X(a) a,
typedef enum hg_return { HG_RETURN_VALUES } hg_return_t;
#undef X

/* Compat return codes */
#define HG_INVALID_PARAM HG_INVALID_ARG
#define HG_SIZE_ERROR    HG_MSGSIZE
#define HG_NOMEM_ERROR   HG_NOMEM
#define HG_NO_MATCH      HG_NOENTRY

/* Callback operation type */
typedef enum hg_cb_type {
    HG_CB_LOOKUP,  /*!< lookup callback */
    HG_CB_FORWARD, /*!< forward callback */
    HG_CB_RESPOND, /*!< respond callback */
    HG_CB_BULK     /*!< bulk transfer callback */
} hg_cb_type_t;

/* Input / output operation type */
typedef enum { HG_UNDEF, HG_INPUT, HG_OUTPUT } hg_op_t;

/**
 * Encode/decode operations.
 */
typedef enum {
    HG_ENCODE, /*!< causes the type to be encoded into the stream */
    HG_DECODE, /*!< causes the type to be extracted from the stream */
    HG_FREE    /*!< can be used to release the space allocated by an HG_DECODE
                  request */
} hg_proc_op_t;

/**
 * Encode/decode operation flags.
 */
#define HG_CORE_SM (1 << 0)

/*****************/
/* Public Macros */
/*****************/

/* Max timeout */
#define HG_MAX_IDLE_TIME (3600 * 1000)

/* HG size max */
#define HG_SIZE_MAX (UINT64_MAX)

/* HG init info initializer */
#define HG_INIT_INFO_INITIALIZER                                                                             \
    {                                                                                                        \
        NA_INIT_INFO_INITIALIZER, NULL, 0, 0, HG_FALSE, HG_FALSE, HG_FALSE, HG_FALSE                         \
    }

#endif /* MERCURY_CORE_TYPES_H */
