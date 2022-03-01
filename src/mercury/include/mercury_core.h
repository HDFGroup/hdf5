/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_CORE_H
#define MERCURY_CORE_H

#include "mercury_core_header.h"
#include "mercury_core_types.h"

#include "na.h"

/*************************************/
/* Public Type and Struct Definition */
/*************************************/

typedef struct hg_core_class   hg_core_class_t;   /* Opaque HG core class */
typedef struct hg_core_context hg_core_context_t; /* Opaque HG core context */
typedef struct hg_core_addr *  hg_core_addr_t;    /* Abstract HG address */
typedef struct hg_core_handle *hg_core_handle_t;  /* Abstract RPC handle */
typedef struct hg_core_op_id * hg_core_op_id_t;   /* Abstract operation id */

/* HG info struct */
struct hg_core_info {
    hg_core_class_t *  core_class; /* HG core class */
    hg_core_context_t *context;    /* HG core context */
    hg_core_addr_t     addr;       /* HG address at target/origin */
    hg_id_t            id;         /* RPC ID */
    hg_uint8_t         context_id; /* Context ID at target/origin */
};

/* Callback info structs */
struct hg_core_cb_info_lookup {
    hg_core_addr_t addr; /* HG address */
};

struct hg_core_cb_info_forward {
    hg_core_handle_t handle; /* HG handle */
};

struct hg_core_cb_info_respond {
    hg_core_handle_t handle; /* HG handle */
};

struct hg_core_cb_info {
    union { /* Union of callback info structures */
        struct hg_core_cb_info_lookup  lookup;
        struct hg_core_cb_info_forward forward;
        struct hg_core_cb_info_respond respond;
    } info;
    void *       arg;  /* User data */
    hg_cb_type_t type; /* Callback type */
    hg_return_t  ret;  /* Return value */
};

/* RPC / HG callbacks */
typedef hg_return_t (*hg_core_rpc_cb_t)(hg_core_handle_t handle);
typedef hg_return_t (*hg_core_cb_t)(const struct hg_core_cb_info *callback_info);

/*****************/
/* Public Macros */
/*****************/

/* Constant values */
#define HG_CORE_ADDR_NULL    ((hg_core_addr_t)0)
#define HG_CORE_HANDLE_NULL  ((hg_core_handle_t)0)
#define HG_CORE_OP_ID_NULL   ((hg_core_op_id_t)0)
#define HG_CORE_OP_ID_IGNORE ((hg_core_op_id_t *)1)

/* Flags */
#define HG_CORE_MORE_DATA   (1 << 0) /* More data required */
#define HG_CORE_NO_RESPONSE (1 << 1) /* No response required */

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the core Mercury layer.
 * Must be finalized with HG_Core_finalize().
 *
 * \param na_info_string [IN]   host address with port number (e.g.,
 *                              "tcp://localhost:3344" or
 *                              "bmi+tcp://localhost:3344")
 * \param na_listen [IN]        listen for incoming connections
 *
 * \return Pointer to HG core class or NULL in case of failure
 */
HG_PUBLIC hg_core_class_t *HG_Core_init(const char *na_info_string, hg_bool_t na_listen);

/**
 * Initialize the Mercury layer with options provided by init_info.
 * Must be finalized with HG_Core_finalize().
 * \remark HG_Core_init_opt() may become HG_Core_init() in the future.
 *
 * \param na_info_string [IN]   host address with port number (e.g.,
 *                              "tcp://localhost:3344" or
 *                              "bmi+tcp://localhost:3344")
 * \param na_listen [IN]        listen for incoming connections
 * \param hg_init_info [IN]     (Optional) HG init info, NULL if no info
 *
 * \return Pointer to HG core class or NULL in case of failure
 */
HG_PUBLIC hg_core_class_t *HG_Core_init_opt(const char *na_info_string, hg_bool_t na_listen,
                                            const struct hg_init_info *hg_init_info);

/**
 * Finalize the Mercury layer.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_finalize(hg_core_class_t *hg_core_class);

/**
 * Clean up all temporary files that were created in previous HG instances.
 * While temporary resources (e.g., tmp files) are cleaned up on a call
 * to HG_Finalize(), this routine gives a chance to programs that terminate
 * abnormally to easily clean up those resources.
 */
HG_PUBLIC void HG_Core_cleanup(void);

/**
 * Set callback that will be triggered when additional data needs to be
 * transferred and HG_Core_set_more_data() has been called, usually when the
 * eager message size is exceeded. This allows upper layers to manually transfer
 * data using bulk transfers for example. The done_callback argument allows the
 * upper layer to notify back once the data has been successfully acquired.
 * The release callback allows the upper layer to release resources that were
 * allocated when acquiring the data.
 *
 * \param hg_core_class [IN]                pointer to HG core class
 * \param more_data_acquire_callback [IN]   pointer to acquire function callback
 * \param more_data_release_callback [IN]   pointer to release function callback
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_set_more_data_callback(
    struct hg_core_class *hg_core_class,
    hg_return_t (*more_data_acquire_callback)(hg_core_handle_t, hg_op_t,
                                              hg_return_t (*done_callback)(hg_core_handle_t)),
    void (*more_data_release_callback)(hg_core_handle_t));

/**
 * Obtain the name of the given class.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return the name of the class, or NULL if not a valid class
 */
static HG_INLINE const char *HG_Core_class_get_name(const hg_core_class_t *hg_core_class);

/**
 * Obtain the protocol of the given class.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return the protocol of the class, or NULL if not a valid class
 */
static HG_INLINE const char *HG_Core_class_get_protocol(const hg_core_class_t *hg_core_class);

/**
 * Test whether class is listening or not.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return HG_TRUE if listening or HG_FALSE if not, or not a valid class
 */
static HG_INLINE hg_bool_t HG_Core_class_is_listening(const hg_core_class_t *hg_core_class);

/**
 * Obtain the underlying NA class.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return Pointer to NA class or NULL if not a valid class
 */
static HG_INLINE na_class_t *HG_Core_class_get_na(const hg_core_class_t *hg_core_class);

#ifdef NA_HAS_SM
/**
 * Obtain the underlying NA SM class.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return Pointer to NA SM class or NULL if not a valid class
 */
static HG_INLINE na_class_t *HG_Core_class_get_na_sm(const hg_core_class_t *hg_core_class);
#endif

/**
 * Obtain the maximum eager size for sending RPC inputs.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return the maximum size, or 0 if hg_core_class is not a valid class or
 * XDR is being used
 */
static HG_INLINE hg_size_t HG_Core_class_get_input_eager_size(const hg_core_class_t *hg_core_class);

/**
 * Obtain the maximum eager size for sending RPC outputs.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return the maximum size, or 0 if hg_core_class is not a valid class or XDR
 * is being used
 */
static HG_INLINE hg_size_t HG_Core_class_get_output_eager_size(const hg_core_class_t *hg_core_class);

/**
 * Associate user data to class. When HG_Core_finalize() is called,
 * free_callback (if defined) is called to free the associated data.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param data [IN]             pointer to user data
 * \param free_callback [IN]    pointer to function
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t HG_Core_class_set_data(hg_core_class_t *hg_core_class, void *data,
                                                    void (*free_callback)(void *));

/**
 * Retrieve previously associated data from a given class.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return Pointer to user data or NULL if not set or any error has occurred
 */
static HG_INLINE void *HG_Core_class_get_data(const hg_core_class_t *hg_core_class);

/**
 * Create a new context. Must be destroyed by calling HG_Core_context_destroy().
 *
 * \param hg_core_class [IN]    pointer to HG core class
 *
 * \return Pointer to HG core context or NULL in case of failure
 */
HG_PUBLIC hg_core_context_t *HG_Core_context_create(hg_core_class_t *hg_core_class);

/**
 * Create a new context with a user-defined context identifier. The context
 * identifier can be used to route RPC requests to specific contexts by using
 * HG_Core_set_target_id().
 * Context must be destroyed by calling HG_Core_context_destroy().
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param id [IN]               context ID
 *
 * \return Pointer to HG core context or NULL in case of failure
 */
HG_PUBLIC hg_core_context_t *HG_Core_context_create_id(hg_core_class_t *hg_core_class, hg_uint8_t id);

/**
 * Destroy a context created by HG_Core_context_create().
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_context_destroy(hg_core_context_t *context);

/**
 * Retrieve the class used to create the given context.
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return the associated class
 */
static HG_INLINE hg_core_class_t *HG_Core_context_get_class(const hg_core_context_t *context);

/**
 * Retrieve the underlying NA context.
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return the associated context
 */
static HG_INLINE na_context_t *HG_Core_context_get_na(const hg_core_context_t *context);

#ifdef NA_HAS_SM
/**
 * Retrieve the underlying NA SM context.
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return the associated context
 */
static HG_INLINE na_context_t *HG_Core_context_get_na_sm(const hg_core_context_t *context);
#endif

/**
 * Retrieve context ID from context.
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return Non-negative integer (max value of 255) or 0 if no ID has been set
 */
static HG_INLINE hg_uint8_t HG_Core_context_get_id(const hg_core_context_t *context);

/**
 * Associate user data to context. When HG_Core_context_destroy() is called,
 * free_callback (if defined) is called to free the associated data.
 *
 * \param context [IN]          pointer to HG core context
 * \param data [IN]             pointer to user data
 * \param free_callback [IN]    pointer to function
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t HG_Core_context_set_data(hg_core_context_t *context, void *data,
                                                      void (*free_callback)(void *));

/**
 * Retrieve previously associated data from a given context.
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return Pointer to user data or NULL if not set or any error has occurred
 */
static HG_INLINE void *HG_Core_context_get_data(const hg_core_context_t *context);

/**
 * Set callback to be called on HG core handle creation. Handles are created
 * both on HG_Core_create() and HG_Core_context_post() calls. This allows
 * upper layers to create and attach data to a handle (using HG_Core_set_data())
 * and later retrieve it using HG_Core_get_data().
 *
 * \param context [IN]          pointer to HG core context
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_context_set_handle_create_callback(
    hg_core_context_t *context, hg_return_t (*callback)(hg_core_handle_t, void *), void *arg);

/**
 * Post requests associated to context in order to receive incoming RPCs.
 * Requests are automatically re-posted after completion until the context is
 * destroyed. Additionally a callback can be triggered on HG handle
 * creation. This allows upper layers to instantiate data that needs to be
 * attached to a handle. Number of requests that are posted can be controlled
 * through HG init info.
 *
 * \param context [IN]          pointer to HG core context
 *
 * \return the associated class
 */
HG_PUBLIC hg_return_t HG_Core_context_post(hg_core_context_t *context);

/**
 * Dynamically register an RPC ID as well as the RPC callback executed
 * when the RPC request ID is received.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param id [IN]               ID to use to register RPC
 * \param rpc_cb [IN]           RPC callback
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_register(hg_core_class_t *hg_core_class, hg_id_t id, hg_core_rpc_cb_t rpc_cb);

/**
 * Deregister RPC ID. Further requests with RPC ID will return an error, it
 * is therefore up to the user to make sure that all requests for that RPC ID
 * have been treated before it is unregistered.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param id [IN]               registered function ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_deregister(hg_core_class_t *hg_core_class, hg_id_t id);

/**
 * Indicate whether HG_Core_register() has been called.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param id [IN]               function ID
 * \param flag [OUT]            pointer to boolean
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_registered(hg_core_class_t *hg_core_class, hg_id_t id, hg_bool_t *flag);

/**
 * Register and associate user data to registered function. When
 * HG_Core_finalize() is called, free_callback (if defined) is called to free
 * the registered data.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param id [IN]               registered function ID
 * \param data [IN]             pointer to data
 * \param free_callback [IN]    pointer to function
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_register_data(hg_core_class_t *hg_core_class, hg_id_t id, void *data,
                                            void (*free_callback)(void *));

/**
 * Indicate whether HG_Core_register_data() has been called and return
 * associated data.
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param id [IN]               registered function ID
 *
 * \return Pointer to data or NULL
 */
HG_PUBLIC void *HG_Core_registered_data(hg_core_class_t *hg_core_class, hg_id_t id);

/**
 * Lookup an addr from a peer address/name. Addresses need to be
 * freed by calling HG_Core_addr_free(). After completion, user callback is
 * placed into a completion queue and can be triggered using HG_Core_trigger().
 *
 * \param context [IN]          pointer to context of execution
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 * \param name [IN]             lookup name
 * \param op_id [OUT]           pointer to returned operation ID (unused)
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_lookup1(hg_core_context_t *context, hg_core_cb_t callback, void *arg,
                                           const char *name, hg_core_op_id_t *op_id);

/**
 * Lookup an addr from a peer address/name. Addresses need to be
 * freed by calling HG_Core_addr_free().
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param name [IN]             lookup name
 * \param addr [OUT]            pointer to abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_lookup2(hg_core_class_t *hg_core_class, const char *name,
                                           hg_core_addr_t *addr);

/**
 * Free the addr from the list of peers.
 *
 * \param addr [IN]             abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_free(hg_core_addr_t addr);

/**
 * Hint that the address is no longer valid. This may happen if the peer is
 * no longer responding. This can be used to force removal of the
 * peer address from the list of the peers, before freeing it and reclaim
 * resources.
 *
 * \param addr [IN]             abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_set_remove(hg_core_addr_t addr);

/**
 * Obtain the underlying NA address from an HG address.
 *
 * \param addr [IN]             abstract address
 *
 * \return abstract NA addr or NA_ADDR_NULL if not a valid HG address
 */
static HG_INLINE na_addr_t HG_Core_addr_get_na(hg_core_addr_t addr);

#ifdef NA_HAS_SM
/**
 * Obtain the underlying NA SM address from an HG address.
 *
 * \param addr [IN]             abstract address
 *
 * \return abstract NA addr or NA_ADDR_NULL if not a valid HG address
 */
static HG_INLINE na_addr_t HG_Core_addr_get_na_sm(hg_core_addr_t addr);
#endif

/**
 * Access self address. Address must be freed with HG_Core_addr_free().
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param addr [OUT]            pointer to abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_self(hg_core_class_t *hg_core_class, hg_core_addr_t *addr);

/**
 * Duplicate an existing HG abstract address. The duplicated address can be
 * stored for later use and the origin address be freed safely. The duplicated
 * address must be freed with HG_Core_addr_free().
 *
 * \param addr [IN]             abstract address
 * \param new_addr [OUT]        pointer to abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_dup(hg_core_addr_t addr, hg_core_addr_t *new_addr);

/**
 * Compare two addresses.
 *
 * \param addr1 [IN]            abstract address
 * \param addr2 [IN]            abstract address
 *
 * \return HG_TRUE if addresses are determined to be equal, HG_FALSE otherwise
 */
HG_PUBLIC hg_bool_t HG_Core_addr_cmp(hg_core_addr_t addr1, hg_core_addr_t addr2);

/**
 * Test whether address is self or not.
 *
 * \param addr [IN]            pointer to abstract address
 *
 * \return HG_TRUE if address is self address, HG_FALSE otherwise
 */
static HG_INLINE hg_bool_t HG_Core_addr_is_self(hg_core_addr_t addr);

/**
 * Convert an addr to a string (returned string includes the terminating
 * null byte '\0'). If buf is NULL, the address is not converted and only
 * the required size of the buffer is returned. If the input value passed
 * through buf_size is too small, HG_SIZE_ERROR is returned and the buf_size
 * output is set to the minimum size required.
 *
 * \param buf [IN/OUT]          pointer to destination buffer
 * \param buf_size [IN/OUT]     pointer to buffer size
 * \param addr [IN]             abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_to_string(char *buf, hg_size_t *buf_size, hg_core_addr_t addr);

/**
 * Get size required to serialize address.
 *
 * \param addr [IN]             abstract address
 * \param flags [IN]            optional flags
 *
 * \return Non-negative value
 */
HG_PUBLIC hg_size_t HG_Core_addr_get_serialize_size(hg_core_addr_t addr, unsigned long flags);

/**
 * Serialize address into a buffer.
 *
 * \param buf [IN/OUT]          pointer to destination buffer
 * \param buf_size [IN]         pointer to buffer size
 * \param flags [IN]            optional flags
 * \param addr [IN]             abstract address
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_serialize(void *buf, hg_size_t buf_size, unsigned long flags,
                                             hg_core_addr_t addr);

/**
 * Deserialize address from a buffer. The returned address must be freed with
 * HG_Core_addr_free().
 *
 * \param hg_core_class [IN]    pointer to HG core class
 * \param addr [OUT]            pointer to abstract address
 * \param buf [IN]              pointer to buffer used for deserialization
 * \param buf_size [IN]         buffer size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_addr_deserialize(hg_core_class_t *hg_core_class, hg_core_addr_t *addr,
                                               const void *buf, hg_size_t buf_size);

/**
 * Initiate a new HG RPC using the specified function ID and the local/remote
 * target defined by addr. The HG handle created can be used to query input
 * and output buffers, as well as issuing the RPC by using HG_Core_forward().
 * After completion the handle must be freed using HG_Core_destroy().
 *
 * \param context [IN]          pointer to HG core context
 * \param addr [IN]             target address
 * \param id [IN]               registered function ID
 * \param handle [OUT]          pointer to HG handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_create(hg_core_context_t *context, hg_core_addr_t addr, hg_id_t id,
                                     hg_core_handle_t *handle);

/**
 * Destroy HG handle. Decrement reference count, resources associated to the
 * handle are freed when the reference count is null.
 *
 * \param handle [IN]           HG handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_destroy(hg_core_handle_t handle);

/**
 * Reset an existing HG handle to make it reusable for RPC forwarding.
 * Both target address and RPC ID can be modified at this time.
 * Operations on that handle must be completed in order to reset that handle
 * safely.
 *
 * \param handle [IN]           HG handle
 * \param addr [IN]             abstract network address of destination
 * \param id [IN]               registered function ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_reset(hg_core_handle_t handle, hg_core_addr_t addr, hg_id_t id);

/**
 * Increment ref count on handle.
 *
 * \param handle [IN]           HG handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_ref_incr(hg_core_handle_t handle);

/**
 * Retrieve ref count from handle.
 *
 * \param handle [IN]           HG handle
 *
 * \return Non-negative value or negative if the handle is not valid
 */
HG_PUBLIC hg_int32_t HG_Core_ref_get(hg_core_handle_t handle);

/**
 * Allows upper layers to attach data to an existing HG handle.
 * The free_callback argument allows allocated resources to be released when
 * the handle gets freed.
 *
 * \param handle [IN]           HG handle
 * \param data [IN]             pointer to user data
 * \param free_callback         pointer to free function callback
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t HG_Core_set_data(hg_core_handle_t handle, void *data,
                                              void (*free_callback)(void *));

/**
 * Allows upper layers to retrieve data from an existing HG handle.
 * Only valid if HG_Core_set_data() has been previously called.
 *
 * \param handle [IN]           HG handle
 *
 * \return Pointer to user data or NULL if not set or any error has occurred
 */
static HG_INLINE void *HG_Core_get_data(hg_core_handle_t handle);

/**
 * Get info from handle.
 *
 * \remark Users must call HG_Core_addr_dup() to safely re-use the addr field.
 *
 * \param handle [IN]           HG handle
 *
 * \return Pointer to info or NULL in case of failure
 */
static HG_INLINE const struct hg_core_info *HG_Core_get_info(hg_core_handle_t handle);

/**
 * Allows upper layers to retrieve cached RPC data from an existing HG handle.
 * Only valid if HG_Core_register_data() has been previously called.
 *
 * \param handle [IN]           HG handle
 *
 * \return Pointer to user data or NULL if not set or any error has occurred
 */
static HG_INLINE const void *HG_Core_get_rpc_data(hg_core_handle_t handle);

/**
 * Set target context ID that will receive and process the RPC request
 * (ID is defined on target context creation, see HG_Core_context_create_id()).
 *
 * \param handle [IN]           HG handle
 * \param id [IN]               user-defined target context ID
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t HG_Core_set_target_id(hg_core_handle_t handle, hg_uint8_t id);

/**
 * Get input buffer from handle that can be used for serializing/deserializing
 * parameters.
 *
 * \param handle [IN]           HG handle
 * \param in_buf [OUT]          pointer to input buffer
 * \param in_buf_size [OUT]     pointer to input buffer size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t HG_Core_get_input(hg_core_handle_t handle, void **in_buf,
                                               hg_size_t *in_buf_size);

/**
 * Get output buffer from handle that can be used for serializing/deserializing
 * parameters.
 *
 * \param handle [IN]           HG handle
 * \param out_buf [OUT]         pointer to output buffer
 * \param out_buf_size [OUT]    pointer to output buffer size
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
static HG_INLINE hg_return_t HG_Core_get_output(hg_core_handle_t handle, void **out_buf,
                                                hg_size_t *out_buf_size);

/**
 * Forward a call using an existing HG handle. Input and output buffers can be
 * queried from the handle to serialize/deserialize parameters.
 * Additionally, a bulk handle can be passed if the size of the input is larger
 * than the queried input buffer size.
 * After completion, the handle must be freed using HG_Core_destroy(), the user
 * callback is placed into a completion queue and can be triggered using
 * HG_Core_trigger().
 *
 * \param handle [IN]           HG handle
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 * \param payload_size [IN]     size of payload to send
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_forward(hg_core_handle_t handle, hg_core_cb_t callback, void *arg,
                                      hg_uint8_t flags, hg_size_t payload_size);

/**
 * Respond back to the origin. The output buffer, which can be used to encode
 * the response, must first be queried using HG_Core_get_output().
 * After completion, the user callback is placed into a completion queue and
 * can be triggered using HG_Core_trigger().
 *
 * \param handle [IN]           HG handle
 * \param callback [IN]         pointer to function callback
 * \param arg [IN]              pointer to data passed to callback
 * \param payload_size [IN]     size of payload to send
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_respond(hg_core_handle_t handle, hg_core_cb_t callback, void *arg,
                                      hg_uint8_t flags, hg_size_t payload_size);

/**
 * Try to progress RPC execution for at most timeout until timeout is reached or
 * any completion has occurred.
 * Progress should not be considered as wait, in the sense that it cannot be
 * assumed that completion of a specific operation will occur only when
 * progress is called.
 *
 * \param context [IN]          pointer to HG core context
 * \param timeout [IN]          timeout (in milliseconds)
 *
 * \return HG_SUCCESS if any completion has occurred / HG error code otherwise
 */
HG_PUBLIC hg_return_t HG_Core_progress(hg_core_context_t *context, unsigned int timeout);

/**
 * Execute at most max_count callbacks. If timeout is non-zero, wait up to
 * timeout before returning. Function can return when at least one or more
 * callbacks are triggered (at most max_count).
 *
 * \param context [IN]          pointer to HG core context
 * \param timeout [IN]          timeout (in milliseconds)
 * \param max_count [IN]        maximum number of callbacks triggered
 * \param actual_count [IN]     actual number of callbacks triggered
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_trigger(hg_core_context_t *context, unsigned int timeout,
                                      unsigned int max_count, unsigned int *actual_count);

/**
 * Cancel an ongoing operation.
 *
 * \param handle [IN]           HG handle
 *
 * \return HG_SUCCESS or corresponding HG error code
 */
HG_PUBLIC hg_return_t HG_Core_cancel(hg_core_handle_t handle);

/************************************/
/* Local Type and Struct Definition */
/************************************/

/* HG core class */
struct hg_core_class {
    na_class_t *na_class; /* NA class */
#ifdef NA_HAS_SM
    na_class_t *na_sm_class; /* NA SM class */
#endif
    void *data;                         /* User data */
    void (*data_free_callback)(void *); /* User data free callback */
};

/* HG core context */
struct hg_core_context {
    struct hg_core_class *core_class; /* HG core class */
    na_context_t *        na_context; /* NA context */
#ifdef NA_HAS_SM
    na_context_t *na_sm_context; /* NA SM context */
#endif
    void *data;                         /* User data */
    void (*data_free_callback)(void *); /* User data free callback */
    hg_uint8_t id;                      /* Context ID */
};

/* HG core addr */
struct hg_core_addr {
    struct hg_core_class *core_class; /* HG core class */
    na_addr_t             na_addr;    /* NA address */
#ifdef NA_HAS_SM
    na_addr_t na_sm_addr; /* NA SM address */
#endif
    hg_bool_t is_self; /* Self address */
};

/* HG core RPC registration info */
struct hg_core_rpc_info {
    hg_core_rpc_cb_t rpc_cb;       /* RPC callback */
    void *           data;         /* User data */
    void (*free_callback)(void *); /* User data free callback */
};

/* HG core handle */
struct hg_core_handle {
    struct hg_core_info      info;      /* HG info */
    struct hg_core_rpc_info *rpc_info;  /* Associated RPC registration info */
    void *                   data;      /* User data */
    void (*data_free_callback)(void *); /* User data free callback */
    void *    in_buf;                   /* Input buffer */
    void *    out_buf;                  /* Output buffer */
    na_size_t in_buf_size;              /* Input buffer size */
    na_size_t out_buf_size;             /* Output buffer size */
    na_size_t na_in_header_offset;      /* Input NA header offset */
    na_size_t na_out_header_offset;     /* Output NA header offset */
};

/*---------------------------------------------------------------------------*/
static HG_INLINE const char *
HG_Core_class_get_name(const hg_core_class_t *hg_core_class)
{
    return NA_Get_class_name(hg_core_class->na_class);
}

/*---------------------------------------------------------------------------*/
static HG_INLINE const char *
HG_Core_class_get_protocol(const hg_core_class_t *hg_core_class)
{
    return NA_Get_class_protocol(hg_core_class->na_class);
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_bool_t
HG_Core_class_is_listening(const hg_core_class_t *hg_core_class)
{
    return NA_Is_listening(hg_core_class->na_class);
}

/*---------------------------------------------------------------------------*/
static HG_INLINE na_class_t *
                 HG_Core_class_get_na(const hg_core_class_t *hg_core_class)
{
    return hg_core_class->na_class;
}

/*---------------------------------------------------------------------------*/
#ifdef NA_HAS_SM
static HG_INLINE na_class_t *
                 HG_Core_class_get_na_sm(const hg_core_class_t *hg_core_class)
{
    return hg_core_class->na_sm_class;
}
#endif

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
HG_Core_class_get_input_eager_size(const hg_core_class_t *hg_core_class)
{
    hg_size_t unexp  = NA_Msg_get_max_unexpected_size(hg_core_class->na_class),
              header = hg_core_header_request_get_size() +
                       NA_Msg_get_unexpected_header_size(hg_core_class->na_class);

    return (unexp > header) ? unexp - header : 0;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_size_t
HG_Core_class_get_output_eager_size(const hg_core_class_t *hg_core_class)
{
    hg_size_t exp    = NA_Msg_get_max_expected_size(hg_core_class->na_class),
              header = hg_core_header_response_get_size() +
                       NA_Msg_get_expected_header_size(hg_core_class->na_class);

    return (exp > header) ? exp - header : 0;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
HG_Core_class_set_data(hg_core_class_t *hg_core_class, void *data, void (*free_callback)(void *))
{
    hg_core_class->data               = data;
    hg_core_class->data_free_callback = free_callback;

    return HG_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE void *
HG_Core_class_get_data(const hg_core_class_t *hg_core_class)
{
    return hg_core_class->data;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_core_class_t *
                 HG_Core_context_get_class(const hg_core_context_t *context)
{
    return context->core_class;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE na_context_t *
                 HG_Core_context_get_na(const hg_core_context_t *context)
{
    return context->na_context;
}

/*---------------------------------------------------------------------------*/
#ifdef NA_HAS_SM
static HG_INLINE na_context_t *
                 HG_Core_context_get_na_sm(const hg_core_context_t *context)
{
    return context->na_sm_context;
}
#endif

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_uint8_t
HG_Core_context_get_id(const hg_core_context_t *context)
{
    return context->id;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
HG_Core_context_set_data(hg_core_context_t *context, void *data, void (*free_callback)(void *))
{
    context->data               = data;
    context->data_free_callback = free_callback;

    return HG_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE void *
HG_Core_context_get_data(const hg_core_context_t *context)
{
    return context->data;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE na_addr_t
HG_Core_addr_get_na(hg_core_addr_t addr)
{
    return addr->na_addr;
}

/*---------------------------------------------------------------------------*/
#ifdef NA_HAS_SM
static HG_INLINE na_addr_t
HG_Core_addr_get_na_sm(hg_core_addr_t addr)
{
    return addr->na_sm_addr;
}
#endif

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_bool_t
HG_Core_addr_is_self(hg_core_addr_t addr)
{
    return addr->is_self;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
HG_Core_set_data(hg_core_handle_t handle, void *data, void (*free_callback)(void *))
{
    handle->data               = data;
    handle->data_free_callback = free_callback;

    return HG_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE void *
HG_Core_get_data(hg_core_handle_t handle)
{
    return handle->data;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE const struct hg_core_info *
HG_Core_get_info(hg_core_handle_t handle)
{
    return &handle->info;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE const void *
HG_Core_get_rpc_data(hg_core_handle_t handle)
{
    return (handle->rpc_info) ? handle->rpc_info->data : NULL;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
HG_Core_set_target_id(hg_core_handle_t handle, hg_uint8_t id)
{
    handle->info.context_id = id;

    return HG_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
HG_Core_get_input(hg_core_handle_t handle, void **in_buf, hg_size_t *in_buf_size)
{
    hg_size_t header_offset = hg_core_header_request_get_size() + handle->na_in_header_offset;

    /* Space must be left for request header */
    *in_buf      = (char *)handle->in_buf + header_offset;
    *in_buf_size = handle->in_buf_size - header_offset;

    return HG_SUCCESS;
}

/*---------------------------------------------------------------------------*/
static HG_INLINE hg_return_t
HG_Core_get_output(hg_core_handle_t handle, void **out_buf, hg_size_t *out_buf_size)
{
    hg_size_t header_offset = hg_core_header_response_get_size() + handle->na_out_header_offset;

    /* Space must be left for response header */
    *out_buf      = (char *)handle->out_buf + header_offset;
    *out_buf_size = handle->out_buf_size - header_offset;

    return HG_SUCCESS;
}

#ifdef __cplusplus
}
#endif

#endif /* MERCURY_CORE_H */
