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

/*
 * This file contains public declarations for the H5Z (data filter) developer
 *      support routines.
 */

#ifndef H5Zdevelop_H
#define H5Zdevelop_H

/* Include package's public header */
#include "H5Zpublic.h"

/*****************/
/* Public Macros */
/*****************/

/**
 * Version of the filter class struct's \c version field used by
 * \c H5Z_class2_t. \c H5Z_class3_t plugins set \c version to the literal
 * value \c 2 instead (see \c H5Z_class3_t's documentation).
 */
#define H5Z_CLASS_T_VERS (1)

/**
 * Highest accepted version field value in H5Zregister(). \since 3.0.0
 */
#define H5Z_CLASS_T_VERS_MAX (2)

/**
 * Maximum byte length of the \c name field in H5Z_class3_t (not counting NUL).
 * H5Zregister() rejects names longer than this value. \since 3.0.0
 */
#define H5Z_CLASS3_NAME_MAX_LEN 255u

/*******************/
/* Public Typedefs */
/*******************/

/**
 * Structure for filter callback property
 */
typedef struct H5Z_cb_t {
    H5Z_filter_func_t func;
    void             *op_data;
} H5Z_cb_t;

/**
 * \brief This callback determines if a filter can be applied to the dataset
 *        with the characteristics provided
 *
 * \dcpl_id
 * \type_id
 * \space_id
 *
 * \return \htri_t
 *
 * \details Before a dataset gets created, the \ref H5Z_can_apply_func_t
 *          callbacks for any filters used in the dataset creation property list
 *          are called with the dataset's dataset creation property list, the
 *          dataset's datatype and a dataspace describing a chunk (for chunked
 *          dataset storage).
 *
 *          The \ref H5Z_can_apply_func_t callback must determine if the
 *          combination of the dataset creation property list setting, the
 *          datatype and the dataspace represent a valid combination to apply
 *          this filter to.  For example, some cases of invalid combinations may
 *          involve the filter not operating correctly on certain datatypes (or
 *          certain datatype sizes), or certain sizes of the chunk dataspace.
 *
 *          The \ref H5Z_can_apply_func_t callback can be the NULL pointer, in
 *          which case, the library will assume that it can apply to any
 *          combination of dataset creation property list values, datatypes and
 *          dataspaces.
 *
 *          The \ref H5Z_can_apply_func_t callback returns positive a valid
 *          combination, zero for an invalid combination and negative for an
 *          error.
 */
//! <!-- [H5Z_can_apply_func_t_snip] -->
typedef htri_t (*H5Z_can_apply_func_t)(hid_t dcpl_id, hid_t type_id, hid_t space_id);
//! <!-- [H5Z_can_apply_func_t_snip] -->

/**
 * \brief The filter operation callback function, defining a filter's operation
 *        on data
 *
 * \dcpl_id
 * \type_id
 * \space_id
 *
 * \return \herr_t
 *
 * \details After the \ref H5Z_can_apply_func_t callbacks are checked for new
 *          datasets, the \ref H5Z_set_local_func_t callbacks for any filters
 *          used in the dataset creation property list are called. These
 *          callbacks receive the dataset's private copy of the dataset creation
 *          property list passed in to H5Dcreate() (i.e. not the actual property
 *          list passed in to H5Dcreate()) and the datatype ID passed in to
 *          H5Dcreate() (which is not copied and should not be modified) and a
 *          dataspace describing the chunk (for chunked dataset storage) (which
 *          should also not be modified).
 *
 *          The \ref H5Z_set_local_func_t callback must set any parameters that
 *          are specific to this dataset, based on the combination of the
 *          dataset creation property list values, the datatype and the
 *          dataspace. For example, some filters perform different actions based
 *          on different datatypes (or datatype sizes) or different number of
 *          dimensions or dataspace sizes.
 *
 *          The \ref H5Z_set_local_func_t callback can be the NULL pointer, in
 *          which case, the library will assume that there are no
 *          dataset-specific settings for this filter.
 *
 *          The \ref H5Z_set_local_func_t callback must return non-negative on
 *          success and negative for an error.
 */
//! <!-- [H5Z_set_local_func_t_snip] -->
typedef herr_t (*H5Z_set_local_func_t)(hid_t dcpl_id, hid_t type_id, hid_t space_id);
//! <!-- [H5Z_set_local_func_t_snip] -->

/**
 * \brief The filter operation callback function, defining a filter's operation
 *        on data
 *
 * \param[in] flags Bit vector specifying certain general properties of the filter
 * \param[in] cd_nelmts Number of elements in \p cd_values
 * \param[in] cd_values Auxiliary data for the filter
 * \param[in] nbytes The number of valid bytes in \p buf to be filtered
 * \param[in,out] buf_size The size of \p buf
 * \param[in,out] buf The filter buffer
 *
 * \return Returns the number of valid bytes of data contained in \p buf. In the
 *         case of failure, the return value is 0 (zero) and all pointer
 *         arguments are left unchanged.
 *
 * \details A filter gets definition flags and invocation flags (defined
 *          above), the client data array and size defined when the filter was
 *          added to the pipeline, the size in bytes of the data on which to
 *          operate, and pointers to a buffer and its allocated size.
 *
 *          The filter should store the result in the supplied buffer if
 *          possible, otherwise it can allocate a new buffer, freeing the
 *          original. The allocated size of the new buffer should be returned
 *          through the \p buf_size pointer and the new buffer through the \p
 *          buf pointer.
 *
 *          The return value from the filter is the number of bytes in the
 *          output buffer. If an error occurs then the function should return
 *          zero and leave all pointer arguments unchanged.
 *
 * \since 1.0.0
 *
 */
//! <!-- [H5Z_func_t_snip] -->
typedef size_t (*H5Z_func_t)(unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[],
                             size_t nbytes, size_t *buf_size, void **buf);
//! <!-- [H5Z_func_t_snip] -->

/**
 * The filter table maps filter identification numbers to structs that
 * contain a pointers to the filter function and timing statistics.
 */
//! <!-- [H5Z_class2_t_snip] -->
typedef struct H5Z_class2_t {
    int                  version;         /**< Version number of the H5Z_class_t struct     */
    H5Z_filter_t         id;              /**< Filter ID number                             */
    unsigned             encoder_present; /**< Does this filter have an encoder?            */
    unsigned             decoder_present; /**< Does this filter have a decoder?             */
    const char          *name;            /**< Comment for debugging                        */
    H5Z_can_apply_func_t can_apply;       /**< The "can apply" callback for a filter        */
    H5Z_set_local_func_t set_local;       /**< The "set local" callback for a filter        */
    H5Z_func_t           filter;          /**< The actual filter function                   */
} H5Z_class2_t;
//! <!-- [H5Z_class2_t_snip] -->

/**
 * \brief Callback to configure a filter from a key=value parameter string.
 *
 * \param[in]     params         Comma-separated key=value parameter string, or NULL.
 * \param[in,out] flags          Caller's flags; callback may modify them.
 * \param[in,out] cd_nelmts      On input 0; on output, number of cd_values slots written
 *                               (or required when cd_values is NULL).
 *                               \b Must \b not \b be \b NULL; the HDF5 library
 *                               guarantees this precondition when invoking the
 *                               callback through #H5Pappend_filter.
 * \param[out]    cd_values      Array to populate, or NULL for a size query.
 * \param[in]     cd_values_size Capacity of cd_values in elements (0 when cd_values is NULL).
 *
 * \return Non-negative on success; negative on failure.
 *
 * \details Must return the same cd_nelmts on both the size-query pass
 *          (cd_values == NULL) and the populate pass (cd_values != NULL).
 *
 *          The public typed accessors #H5Zconfig_has_key, #H5Zconfig_get_int,
 *          #H5Zconfig_get_double, #H5Zconfig_get_bool, and #H5Zconfig_get_str
 *          are safe to call from inside this callback: they are pure parsers
 *          over the caller-provided \p params buffer and do not take the
 *          HDF5 API lock, so they will not deadlock in concurrency-mode
 *          (\c HDF5_ENABLE_CONCURRENCY) builds where this callback is invoked
 *          from within #H5Pappend_filter.
 *
 * \since 3.0.0
 */
typedef herr_t (*H5Z_set_config_func_t)(const char *params, unsigned *flags, size_t *cd_nelmts,
                                        unsigned cd_values[], size_t cd_values_size);

/**
 * \brief Callback to reconstruct a human-readable parameter string from cd_values.
 *
 * \param[in]  flags      Definition flags stored in the pipeline.
 * \param[in]  cd_nelmts  Number of elements in cd_values.
 * \param[in]  cd_values  Client data values.
 * \param[out] buf        Buffer to receive the parameter string, or NULL for size query.
 * \param[in,out] buf_size On entry, total capacity of \p buf in bytes, <b>including</b> the
 *                        NUL terminator slot (i.e., <tt>*buf_size >= strlen(output) + 1</tt>).
 *                        On return, set to the number of characters written, excluding the NUL
 *                        terminator. When \p buf is NULL the callback sets \p *buf_size to the
 *                        required character count (excluding NUL) and returns success, enabling
 *                        a size query. Because the capacity includes the NUL slot, implementations
 *                        may write with <tt>snprintf(buf, *buf_size, ...)</tt> directly.
 *
 * \return Non-negative on success; negative on failure.
 *
 * \note Implementations that format \c float or \c double values \b must use
 *       the C99 \c \%a format specifier (e.g., \c snprintf(buf,*buf_size,"\%a",val))
 *       rather than \c \%g, \c \%f, or \c \%e. \c \%a encodes the exact
 *       IEEE 754 bit pattern as a hexadecimal float literal, guaranteeing that
 *       \c strtod parses the output back to the identical value with no
 *       rounding. This makes exact round-trips possible for filters that store
 *       \c float or \c double parameters via the cd_values packing convention.
 *       Decimal float input (e.g., \c rate=3.5) remains valid for user
 *       convenience; the asymmetry (decimal in, hex-float out) is intentional.
 *
 * \since 3.0.0
 */
typedef herr_t (*H5Z_get_config_func_t)(unsigned flags, size_t cd_nelmts, const unsigned cd_values[],
                                        char *buf, size_t *buf_size);

/**
 * \brief Extended filter callback type for H5Z_class3_t.
 *
 * Extends \c H5Z_func_t with two additional parameters: the active data-transfer
 * property list (\p dxpl_id) and the chunk's scaled coordinates (\p scaled, \p ndims).
 * \c H5Z_class2_t continues to use \c H5Z_func_t; this type is used only by
 * \c H5Z_class3_t.
 *
 * \since 3.0.0
 */
typedef size_t (*H5Z_func2_t)(unsigned int flags, size_t cd_nelmts, const unsigned int cd_values[],
                              hid_t dxpl_id, const hsize_t *scaled, size_t ndims, size_t nbytes,
                              size_t *buf_size, void **buf);

/**
 * \brief Version 3 filter class structure with optional string-configuration callbacks.
 *
 * Plugin authors use H5Z_class3_t directly rather than relying on the H5Z_class_t alias.
 * This struct is NOT derived from H5Z_class2_t; it is an independent flat struct.
 *
 * \since 3.0.0
 */
//! <!-- [H5Z_class3_t_snip] -->
typedef struct H5Z_class3_t {
    int          version;            /**< Set to the literal value 2                */
    H5Z_filter_t id;                 /**< Filter ID number                           */
    unsigned     encoder_present;    /**< Does this filter have an encoder?          */
    unsigned     decoder_present;    /**< Does this filter have a decoder?           */
    const char  *name;               /**< Canonical string identifier (e.g., "zfp"); must not be NULL; used as
                                        display name */
    const char *description;         /**< Human-readable description of the filter (e.g., "Deflate (zlib)
                                        general-purpose compression"); may be NULL */
    H5Z_can_apply_func_t  can_apply; /**< The "can apply" callback for a filter      */
    H5Z_set_local_func_t  set_local; /**< The "set local" callback for a filter      */
    H5Z_func2_t           filter;    /**< Extended filter callback: dxpl_id + scaled */
    H5Z_set_config_func_t set_config; /**< String configuration callback; may be NULL */
    H5Z_get_config_func_t get_config; /**< Parameter string reconstruction; may be NULL */
} H5Z_class3_t;
//! <!-- [H5Z_class3_t_snip] -->

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup H5Z
 *
 * \brief Check whether a key is present in a TOML-subset filter parameter string.
 *
 * \param[in] params  TOML-subset key=value parameter string, or NULL.
 * \param[in] key     Key to search for (case-insensitive).
 *
 * \return Positive if the key is present, 0 if absent, negative on error.
 *
 * \details Bare keys (no '=' sign, boolean flags) return positive.
 *          This function validates the entire parameter string on every call;
 *          duplicate keys or malformed syntax return negative.
 *
 * \since 3.0.0
 */
H5_DLL htri_t H5Zconfig_has_key(const char *params, const char *key);

/**
 * \ingroup H5Z
 *
 * \brief Look up a TOML integer value in a filter parameter string.
 *
 * \param[in]  params  TOML-subset key=value parameter string.
 * \param[in]  key     Key to search for (case-insensitive).
 * \param[out] out     Receives the parsed int64_t value.
 *
 * \return Positive if found and converted, 0 if not found, negative on error.
 *
 * \details Accepts decimal, 0x (hex), 0o (octal), 0b (binary) integers with
 *          optional leading sign and TOML underscore digit separators.
 *          Returns negative (H5E_BADVALUE) if the key exists but its value
 *          is not a TOML integer (type mismatch).
 *
 * \since 3.0.0
 */
H5_DLL htri_t H5Zconfig_get_int(const char *params, const char *key, int64_t *out);

/**
 * \ingroup H5Z
 *
 * \brief Look up a TOML float value in a filter parameter string.
 *
 * \param[in]  params  TOML-subset key=value parameter string.
 * \param[in]  key     Key to search for (case-insensitive).
 * \param[out] out     Receives the parsed double value.
 *
 * \return Positive if found and converted, 0 if not found, negative on error.
 *
 * \details The decimal separator is always '.', regardless of locale.
 *          TOML special floats (inf, nan) are rejected with H5E_BADVALUE.
 *          Returns negative if the key exists but its value is not a TOML
 *          float (type mismatch).
 *
 * \since 3.0.0
 */
H5_DLL htri_t H5Zconfig_get_double(const char *params, const char *key, double *out);

/**
 * \ingroup H5Z
 *
 * \brief Look up a TOML boolean value in a filter parameter string.
 *
 * \param[in]  params  TOML-subset key=value parameter string.
 * \param[in]  key     Key to search for (case-insensitive).
 * \param[out] out     Receives TRUE or FALSE.
 *
 * \return Positive if found, 0 if not found, negative on error.
 *
 * \details Accepts "true" or "false" (lowercase only, per TOML).
 *          Bare keys (boolean flags with no '=' sign) are treated as TRUE.
 *          Returns negative if the key exists but its value is not a TOML
 *          boolean (type mismatch).
 *
 * \since 3.0.0
 */
H5_DLL htri_t H5Zconfig_get_bool(const char *params, const char *key, bool *out);

/**
 * \ingroup H5Z
 *
 * \brief Look up a TOML string value in a filter parameter string.
 *
 * \param[in]     params    TOML-subset key=value parameter string.
 * \param[in]     key       Key to search for (case-insensitive).
 * \param[out]    buf       Buffer to receive the decoded string (without quotes),
 *                          or NULL for a size query.
 * \param[in,out] buf_size  On entry, capacity of buf; on return, bytes required
 *                          (excluding NUL terminator).  May be NULL when buf is NULL.
 *
 * \return Positive if found, 0 if not found, negative on error.
 *
 * \details Only quoted values (double-quoted with backslash escapes, or
 *          single-quoted with no escape processing) are accepted.
 *          Unquoted integers, floats, booleans, and bare keys produce a
 *          type mismatch error (H5E_BADVALUE).
 *          If the buffer is too small, H5E_OVERFLOW is pushed.
 *
 * \since 3.0.0
 */
H5_DLL htri_t H5Zconfig_get_str(const char *params, const char *key, char *buf, size_t *buf_size);

/**
 * \ingroup H5Z
 *
 * \brief Registers a new filter with the HDF5 library
 *
 * \param[in] cls A pointer to a buffer for the struct containing the
 *                filter-definition
 *
 * \return \herr_t
 *
 * \details H5Zregister() registers a new filter with the HDF5 library.
 *
 * \details Making a new filter available to an application is a two-step
 *          process. The first step is to write the three filter callback
 *          functions described below: \c can_apply, \c set_local, and \c
 *          filter. This call to H5Zregister(), registering the filter with the
 *          library, is the second step. The can_apply and set_local fields can
 *          be set to NULL if they are not required for the filter being
 *          registered.
 *
 *          H5Zregister() accepts a single parameter, a pointer to a buffer for
 *          the \p cls data structure. That data structure must conform to one
 *          of the following definitions:
 *          \snippet this H5Z_class1_t_snip
 *          or
 *          \snippet this H5Z_class2_t_snip
 *
 *          \c version is a library-defined value reporting the version number
 *          of the #H5Z_class_t struct. This currently must be set to
 *          #H5Z_CLASS_T_VERS.
 *
 *          \c id is the identifier for the new filter. This is a user-defined
 *          value between #H5Z_FILTER_RESERVED and #H5Z_FILTER_MAX. These
 *          values are defined in the HDF5 source file H5Zpublic.h, but the
 *          symbols #H5Z_FILTER_RESERVED and #H5Z_FILTER_MAX should always be
 *          used instead of the literal values.
 *
 *          \c encoder_present is a library-defined value indicating whether
 *          the filter's encoding capability is available to the application.
 *
 *          \c decoder_present is a library-defined value indicating whether
 *          the filter's encoding capability is available to the application.
 *
 *          \c name is a descriptive comment used for debugging, may contain a
 *          descriptive name for the filter, and may be the null pointer.
 *
 *          \c can_apply, described in detail below, is a user-defined callback
 *          function that determines whether the combination of the dataset
 *          creation property list values, the datatype, and the dataspace
 *          represent a valid combination to apply this filter to.
 *
 *          \c set_local, described in detail below, is a user-defined callback
 *          function that sets any parameters that are specific to this
 *          dataset, based on the combination of the dataset creation property
 *          list values, the datatype, and the dataspace.
 *
 *          \c filter, described in detail below, is a user-defined callback
 *          function which performs the action of the filter.
 *
 *          The statistics associated with a filter are not reset by this
 *          function; they accumulate over the life of the library.
 *
 *          #H5Z_class_t is a macro that maps to either H5Z_class1_t or
 *          H5Z_class2_t, depending on the needs of the application. To affect
 *          only this macro, H5Z_class_t_vers may be defined as either 1 or 2.
 *          Otherwise, it will behave in the same manner as other API
 *          compatibility macros. See \ref api-compat-macros for more
 *          information. H5Z_class1_t matches the #H5Z_class_t structure that is
 *          used in the 1.6.x versions of the HDF5 library.
 *
 *          H5Zregister() will automatically detect which structure type has
 *          been passed in, regardless of the mapping of the #H5Z_class_t macro.
 *          However, the application must make sure that the fields are filled
 *          in according to the correct structure definition if the macro is
 *          used to declare the structure.
 *
 *          \Bold{The callback functions:}\n Before H5Zregister() can link a
 *          filter into an application, three callback functions must be
 *          defined as described in the HDF5 library header file H5Zpublic.h.
 *
 *          When a filter is applied to the fractal heap for a group (e.g.,
 *          when compressing group metadata) and if they can apply and set local
 *          callback functions that have been defined for that filter, HDF5 passes
 *          the value -1 for all parameters for those callback functions. This
 *          is done to ensure that the filter will not be applied to groups if
 *          it relies on these parameters, as they are not applicable to group
 *          fractal heaps; to operate on group fractal heaps, a filter must be
 *          capable of operating on an opaque block of binary data.
 *
 *          The \Emph{can-apply} callback function must return a positive value
 *          for a valid combination, zero for an invalid combination, and a
 *          negative value for an error.
 *          \snippet this H5Z_can_apply_func_t_snip
 *
 *          Before a dataset is created, the \Emph{can apply} callbacks for any
 *          filters used in the dataset creation property list are called with
 *          the dataset's dataset creation property list, \c dcpl_id, the
 *          dataset's datatype, \p type_id, and a dataspace describing a chunk,
 *          \p space_id, (for chunked dataset storage).
 *
 *          This callback must determine whether the combination of the dataset
 *          creation property list settings, the datatype, and the dataspace
 *          represent a valid combination to which to apply this filter. For
 *          example, an invalid combination may involve the filter not
 *          operating correctly on certain datatypes, on certain datatype
 *          sizes, or on certain sizes of the chunk dataspace. If this filter
 *          is enabled through H5Pset_filter() as optional and the can apply
 *          function returns 0, the library will skip the filter in the filter
 *          pipeline.
 *
 *          This callback can be the NULL pointer, in which case the library
 *          will assume that the filter can be applied to a dataset with any
 *          combination of dataset creation property list values, datatypes,
 *          and dataspaces.
 *
 *          The \Emph{set local} callback function is defined as follows:
 *          \snippet this H5Z_set_local_func_t_snip
 *
 *          After the can apply callbacks are checked for a new dataset, the
 *          \Emph{set local} callback functions for any filters used in the
 *          dataset creation property list are called. These callbacks receive
 *          \c dcpl_id, the dataset's private copy of the dataset creation
 *          property list passed into H5Dcreate() (i.e. not the actual
 *          property list passed into H5Dcreate()); \c type_id, the datatype
 *          identifier passed into H5Dcreate(), which is not copied and should
 *          not be modified; and \c space_id, a dataspace describing the chunk
 *          (for chunked dataset storage), which should also not be modified.
 *
 *          The set local callback must set any filter parameters that are
 *          specific to this dataset, based on the combination of the dataset
 *          creation property list values, the datatype, and the dataspace. For
 *          example, some filters perform different actions based on different
 *          datatypes, datatype sizes, numbers of dimensions, or dataspace
 *          sizes.
 *
 *          The \Emph{set local} callback may be the NULL pointer, in which
 *          case, the library will assume that there are no dataset-specific
 *          settings for this filter.
 *
 *          The \Emph{set local} callback function must return a non-negative
 *          value on success and a negative value for an error.
 *
 *          The \Emph{filter operation} callback function, defining the
 *          filter's operation on the data, is defined as follows:
 *          \snippet this H5Z_func_t_snip
 *
 *          The parameters \c flags, \c cd_nelmts, and \c cd_values are the
 *          same as for the function H5Pset_filter(). The one exception is that
 *          an additional flag, #H5Z_FLAG_REVERSE, is set when the filter is
 *          called as part of the input pipeline.
 *
 *          The parameter \c buf points to the input buffer which has a size of
 *          \c buf_size bytes, \c nbytes of which are valid data.
 *
 *          The filter should perform the transformation in place if possible.
 *          If the transformation cannot be done in place, then the filter
 *          should allocate a new buffer and assign it to \c buf, assigning
 *          the allocated size of that buffer to \c buf_size. The old
 *          buffer should be freed by the filter.
 *
 *          Some care must be taken with the functions that allocate and free
 *          memory. Standard C library functions like malloc(3) and free(3)
 *          will work in many cases, but if there is a mismatch between the
 *          memory allocators used in the library and any filter that
 *          reallocates a buffer, there could be problems. This is most often
 *          the case with Windows and/or when debugging memory allocators are being
 *          used. In both cases, the "state" of the memory allocator lies in
 *          different libraries and will get corrupted if you allocate in one
 *          library and free in another. Windows adds the C standard library
 *          via dlls that can vary with Visual Studio version and debug vs.
 *          release builds. Static links to the MSVC CRT can also introduce
 *          a new memory allocator state.
 *
 *          The library does provide H5allocate_memory() and H5free_memory()
 *          functions that will use the library's allocation and free functions,
 *          however using these functions will require linking your filter to
 *          a particular version of the library, which may be inconvenient.
 *
 *          If successful, the \Emph{filter operation} callback function
 *          returns the number of valid bytes of data contained in \c buf. The
 *          returned \c *buf_size must be large enough to hold the returned (via
 *          the return value) data size. In the case of failure, the return
 *          value is 0 (zero) and all pointer arguments are left unchanged.
 *
 *          When the filter is run in reverse mode, the \Emph{filter operation}
 *          callback function must return, if successful, a data size that is
 *          exactly equal to the original data size (\c nbytes) before the
 *          filter was run in forward mode.
 *
 * \version 1.8.6 Return type for the \Emph{can apply} callback function,
 *                \ref H5Z_can_apply_func_t, changed to \ref htri_t.
 * \version 1.8.5 Semantics of the \Emph{can apply} and \Emph{set local}
 *                callback functions changed to accommodate the use of filters
 *                with group fractal heaps.
 * \version 1.8.3 #H5Z_class_t renamed to H5Z_class2_t, H5Z_class1_t structure
 *                introduced for backwards compatibility with release 1.6.x,
 *                and #H5Z_class_t macro introduced in this release. Function
 *                modified to accept either structure type.
 * \version 1.8.0 The fields \c version, \c encoder_present, and
 *                \c decoder_present were added to the #H5Z_class_t \c struct
 *                in this release.
 * \version 1.6.0 This function was substantially revised in Release 1.6.0 with
 *                a new #H5Z_class_t struct and new set local and can apply
 *                callback functions.
 *
 * \since 1.0.0
 *
 */
H5_DLL herr_t H5Zregister(const void *cls);
/**
 * \ingroup H5Z
 *
 * \brief Unregisters a filter.
 *
 * \param[in] id Identifier of the filter to be unregistered.
 * \return \herr_t
 *
 * \details H5Zunregister() unregisters the filter specified in \p id.
 *
 * \details This function first iterates through all opened datasets and
 *          groups. If an open object that uses this filter is found, the
 *          function will fail with a message indicating that an object using
 *          the filter is still open. All open files are then flushed to make
 *          sure that all cached data that may use this filter are written out.
 *
 *          If the application is a parallel program, all processes that
 *          participate in collective data writing should call this function to
 *          ensure that all data is flushed.
 *
 *          After a call to H5Zunregister(), the filter specified in filter
 *          will no longer be available to the application.
 *
 * \version 1.8.12 Function modified to check for open objects using the
 *                 filter.
 * \since 1.6.0
 */
H5_DLL herr_t H5Zunregister(H5Z_filter_t id);

#ifdef __cplusplus
}
#endif

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/**
 * The filter table maps filter identification numbers to structs that
 * contain a pointers to the filter function and timing statistics.
 */
//! <!-- [H5Z_class1_t_snip] -->
typedef struct H5Z_class1_t {
    H5Z_filter_t         id;        /**< Filter ID number			     */
    const char          *name;      /**< Comment for debugging		     */
    H5Z_can_apply_func_t can_apply; /**< The "can apply" callback for a filter */
    H5Z_set_local_func_t set_local; /**< The "set local" callback for a filter */
    H5Z_func_t           filter;    /**< The actual filter function		     */
} H5Z_class1_t;
//! <!-- [H5Z_class1_t_snip] -->

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#endif /* H5Zdevelop_H */
