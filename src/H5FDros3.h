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
 * Purpose:	The public header file for the read-only S3 (ros3) virtual file driver (VFD)
 */
#ifndef H5FDros3_H
#define H5FDros3_H

/* Public header files */
#include "H5FDpublic.h" /* File drivers             */

#ifdef H5_HAVE_ROS3_VFD

/** Initializer for the ros3 VFD \since 1.8.22 */
#define H5FD_ROS3 (H5OPEN H5FD_ROS3_id_g)

/** Identifier for the ros3 VFD \since 1.14.0 */
#define H5FD_ROS3_VALUE H5_VFD_ROS3

#else
/** Initializer for the ros3 VFD (disabled) \since 1.8.22 */
#define H5FD_ROS3       (H5I_INVALID_HID)

/** Identifier for the ros3 VFD (disabled) \since 1.14.0 */
#define H5FD_ROS3_VALUE H5_VFD_INVALID
#endif

#ifdef H5_HAVE_ROS3_VFD

/**
 * \def H5FD_CURR_ROS3_FAPL_T_VERSION
 * The version number of the H5FD_ros3_fapl_t configuration
 * structure for the #H5FD_ROS3 driver.
 */
#define H5FD_CURR_ROS3_FAPL_T_VERSION 1

/**
 * \def H5FD_ROS3_MAX_REGION_LEN
 * Maximum string length for specifying the region of the S3 bucket.
 * \since 1.10.6
 */
#define H5FD_ROS3_MAX_REGION_LEN 32
/**
 * \def H5FD_ROS3_MAX_SECRET_ID_LEN
 * Maximum string length for specifying the security ID.
 * \since 1.10.6
 */
#define H5FD_ROS3_MAX_SECRET_ID_LEN 128
/**
 * \def H5FD_ROS3_MAX_SECRET_KEY_LEN
 * Maximum string length for specifying the security key.
 * \since 1.10.6
 */
#define H5FD_ROS3_MAX_SECRET_KEY_LEN 128
/**
 * \def H5FD_ROS3_MAX_SECRET_TOK_LEN
 * Maximum string length for specifying the session/security token.
 */
#define H5FD_ROS3_MAX_SECRET_TOK_LEN 4096

/**
 * \def H5FD_ROS3_VFD_DEFAULT_LOG_FILE
 * The default filename of the file that logging output is written
 * to when enabled. This filename can be overridden with the
 * HDF5_ROS3_VFD_LOG_FILE environment variable.
 * \since 2.0.0
 */
#define H5FD_ROS3_VFD_DEFAULT_LOG_FILE "hdf5_ros3_vfd.log"

/*
 * Environment variables interpreted by the HDF5 ROS3 VFD
 */

/**
 * \def HDF5_ROS3_VFD_DEBUG
 * Macro for name of the environment variable that specifies debugging
 * output should be enabled for the ROS3 VFD. This output includes
 * brief details about operations that the VFD is performing. Debugging
 * output will be enabled if this environment variable is defined to
 * anything other than one of (case-insensitive):
 *
 *  'false' <br />
 *  'off' <br />
 *  '0' <br />
 *
 * Debugging output will be printed to stderr.
 * \since 2.0.0
 */
#define HDF5_ROS3_VFD_DEBUG "HDF5_ROS3_VFD_DEBUG"
/**
 * \def HDF5_ROS3_VFD_LOG_LEVEL
 * Macro for name of the environment variable that specifies whether
 * logging output should be enabled for the ROS3 VFD. This environment
 * variable should be specified as one of (case-insensitive):
 *
 *  'error' <br />
 *  'info' <br />
 *  'debug' <br />
 *  'trace' <br />
 *
 * If specified as one of these values, logging output will be written
 * to the file specified by the #HDF5_ROS3_VFD_LOG_FILE environment
 * variable. If that environment variable is not specified, logging will
 * be written to the default file specified by #H5FD_ROS3_VFD_DEFAULT_LOG_FILE.
 *
 * Note that this logging output is separate from and much more detailed
 * than the debugging information enabled by the #HDF5_ROS3_VFD_DEBUG
 * environment variable.
 * \since 2.0.0
 */
#define HDF5_ROS3_VFD_LOG_LEVEL "HDF5_ROS3_VFD_LOG_LEVEL"
/**
 * \def HDF5_ROS3_VFD_LOG_FILE
 * Macro for name of the environment variable that specifies the
 * filename to write logging output to when it is enabled. This
 * environment variable may be specified as one of the values
 * 'stderr' or 'stdout' to write output to those standard streams.
 * Otherwise, the value is treated as a regular filename. Used in
 * conjunction with #HDF5_ROS3_VFD_LOG_LEVEL.
 * \since 2.0.0
 */
#define HDF5_ROS3_VFD_LOG_FILE "HDF5_ROS3_VFD_LOG_FILE"
/**
 * \def HDF5_ROS3_VFD_FORCE_PATH_STYLE
 * Macro for name of the environment variable that forces the VFD
 * to use path-style requests rather than virtual-hosted-style
 * requests. The VFD attempts to use virtual-hosted-style requests
 * by default when possible, but in some cases it may be necessary
 * to force it to use path-style requests for compatibility reasons.
 * The VFD will use path-style requests if this environment variable
 * is defined to anything other than one of (case-insensitive):
 *
 *  'false' <br />
 *  'off' <br />
 *  '0' <br />
 *
 * \since 2.0.0
 */
#define HDF5_ROS3_VFD_FORCE_PATH_STYLE "HDF5_ROS3_VFD_FORCE_PATH_STYLE"

/**
 * \def HDF5_ROS3_VFD_DEFAULT_PAGE_SIZE
 * The default size, in bytes, of an I/O "page". By default, the ROS3
 * VFD tries to reduce requests to S3 by performing I/O in fixed-size
 * blocks and caching these blocks in an "I/O page cache".
 * This value may be specified for the \p page_size parameter to
 * H5Pset_fapl_ros3_paging() in order to set the default I/O "page" size.
 *
 * \since 2.2.0
 */
#define HDF5_ROS3_VFD_DEFAULT_PAGE_SIZE 16777216

/**
 * \struct H5FD_ros3_fapl_t
 * \brief Configuration structure for H5Pset_fapl_ros3() / H5Pget_fapl_ros3().
 *
 * \details H5FD_ros3_fapl_t is a public structure that is used to pass
 *          configuration data to the #H5FD_ROS3 driver via a File Access
 *          Property List. A pointer to an instance of this structure is
 *          a parameter to H5Pset_fapl_ros3() and H5Pget_fapl_ros3().
 *
 * \var int32_t H5FD_ros3_fapl_t::version
 *      Version number of the H5FD_ros3_fapl_t structure. Any instance passed
 *      to H5Pset_fapl_ros3() / H5Pget_fapl_ros3() must have a recognized version
 *      number or an error will be raised. Currently, this field should be set
 *      to #H5FD_CURR_ROS3_FAPL_T_VERSION.
 *
 * \var bool H5FD_ros3_fapl_t::authenticate
 *      A Boolean which specifies if security credentials from this structure
 *      should be used for accessing a S3 bucket.
 *
 *      If true, ALL credentials must come from the FAPL and no attempt will
 *      be made to load credentials from other places. In this case, both
 *      `secret_id` and `secret_key` must be non-empty strings. If only one
 *      of `secret_id` or `secret_key` are non-empty strings while the other
 *      is an empty string, an error will be returned when opening a file.
 *      If a session token is to be used in this case, it must be specified
 *      with H5Pset_fapl_ros3_token().
 *
 *      If false, the ROS3 VFD will instead attempt to load credentials from
 *      several different places, in this order:
 *
 *        - From the environment, by checking AWS environment variables such
 *          as AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN
 *          and AWS_ACCOUNT_ID
 *        - From the AWS profile files, by reading from ~/.aws/config and
 *          ~/.aws/credentials, by default. The specific files read from can
 *          be overridden with the AWS_CONFIG_FILE and AWS_SHARED_CREDENTIALS_FILE
 *          environment variables.
 *        - From STS, by using AssumeRoleWithWebIdentity
 *        - From EC2 instance metadata
 *
 *      If the ROS3 VFD cannot source credentials from any of these locations,
 *      it will fallback to using anonymous credentials.
 *
 * \var char H5FD_ros3_fapl_t::aws_region[H5FD_ROS3_MAX_REGION_LEN + 1]
 *      A string which specifies the AWS region of the S3 bucket, e.g. "us-east-1".
 *      Specifying an AWS region is always required for the ROS3 VFD, though
 *      it does not need to be specified here in the FAPL. The ROS3 VFD looks
 *      for the AWS region in the following places, in order:
 *
 *        - The FAPL, if `aws_region` is not an empty string
 *        - The AWS_REGION environment variable
 *        - The AWS_DEFAULT_REGION environment variable
 *        - The AWS configuration file (~/.aws/config by default)
 *          - The 'default' profile from this file is used, unless a different
 *            profile is specified with the AWS_PROFILE environment variable
 *
 *      If the ROS3 VFD cannot determine an AWS region from one of these
 *      locations, an error will be returned when opening a file.
 *
 * \var char H5FD_ros3_fapl_t::secret_id[H5FD_ROS3_MAX_SECRET_ID_LEN + 1]
 *      A string which specifies the security ID.
 *
 * \var char H5FD_ros3_fapl_t::secret_key[H5FD_ROS3_MAX_SECRET_KEY_LEN + 1]
 *      A string which specifies the security key.
 */
typedef struct H5FD_ros3_fapl_t {
    int32_t version;
    bool    authenticate;
    char    aws_region[H5FD_ROS3_MAX_REGION_LEN + 1];
    char    secret_id[H5FD_ROS3_MAX_SECRET_ID_LEN + 1];
    char    secret_key[H5FD_ROS3_MAX_SECRET_KEY_LEN + 1];
} H5FD_ros3_fapl_t;

#ifdef __cplusplus
extern "C" {
#endif

/** @private
 *
 * \brief ID for the ros3 VFD
 */
H5_DLLVAR hid_t H5FD_ROS3_id_g;

/**
 * \ingroup FAPL
 *
 * \brief Queries a File Access Property List for #H5FD_ROS3 file driver properties.
 *
 * \fapl_id
 * \param[out] fa_out Pointer to #H5FD_ROS3 driver configuration structure.
 * \returns \herr_t
 */
H5_DLL herr_t H5Pget_fapl_ros3(hid_t fapl_id, H5FD_ros3_fapl_t *fa_out);

/**
 * \ingroup FAPL
 *
 * \brief Modifies the specified File Access Property List to use the #H5FD_ROS3 driver.
 *
 * \fapl_id
 * \param[in] fa Pointer to #H5FD_ROS3 driver configuration structure.
 * \returns \herr_t
 *
 * \note    As of HDF5 2.0.0, as a side effect of calling this function, if the
 *          page buffer size has not been set on fapl_id, it is set to 64 MiB.
 *          To set a different page buffer size, simply call
 *          H5Pset_page_buffer() with fapl_id and your desired page buffer size.
 *          To disable the page buffer, call H5Pset_page_buffer() with a size of
 *          0. Disabling the page buffer with the ROS3 driver may cause severe
 *          performance degradation.
 */
H5_DLL herr_t H5Pset_fapl_ros3(hid_t fapl_id, const H5FD_ros3_fapl_t *fa);

/**
 * \ingroup FAPL
 *
 * \brief Queries a File Access Property List for #H5FD_ROS3 file driver session/security
 *        token.
 *
 * \fapl_id
 * \param[in] size Size of the provided char array for storing the session/security token.
 * \param[out] token Session/security token.
 * \returns \herr_t
 *
 * \since 1.14.2
 */
H5_DLL herr_t H5Pget_fapl_ros3_token(hid_t fapl_id, size_t size, char *token);

/**
 * \ingroup FAPL
 *
 * \brief Modifies the specified File Access Property List to use the #H5FD_ROS3 driver
 *        by adding the specified session/security token.
 *
 * \fapl_id
 * \param[in] token Session/security token.
 * \returns \herr_t
 *
 * \details H5Pset_fapl_ros3_token() modifies an existing File Access Property List which
 *          is used by #H5FD_ROS3 driver by adding or updating the session/security token
 *          of the property list. Be aware, to set the token first you need to create
 *          a proper File Access Property List using H5Pset_fapl_ros() and use this list
 *          as input argument of the function H5Pset_fapl_ros3_token().
 *
 *          Note, the session token is only needed when you want to access a S3 bucket
 *          using temporary security credentials.
 *
 * \since 1.14.2
 */
H5_DLL herr_t H5Pset_fapl_ros3_token(hid_t fapl_id, const char *token);

/**
 * \ingroup FAPL
 *
 * \brief Queries a File Access Property List for #H5FD_ROS3 file driver endpoint url.
 *
 * \fapl_id
 * \param[in] size Size of the provided char array for storing the endpoint url.
 * \param[out] endpoint Alternate endpoint url.
 * \returns \herr_t
 *
 * \since 2.0.0
 */
H5_DLL herr_t H5Pget_fapl_ros3_endpoint(hid_t fapl_id, size_t size, char *endpoint);

/**
 * \ingroup FAPL
 *
 * \brief Modifies the specified File Access Property List to use an alternative endpoint
 *        URL when opening files with the #H5FD_ROS3 driver.
 *
 * \fapl_id
 * \param[in] endpoint Alternate endpoint url.
 * \returns \herr_t
 *
 * \details H5Pset_fapl_ros3_endpoint() modifies an existing File Access Property List which
 *          is used by #H5FD_ROS3 driver by adding or updating the endpoint url of the
 *          property list. When not specified, the #H5FD_ROS3 driver uses the standard
 *          s3.<region-code>.amazonaws.com, unless an alternative endpoint URL is specified
 *          in the AWS_ENDPOINT_URL_S3 or AWS_ENDPOINT_URL environment variable. Be aware,
 *          to set the endpoint first you need to create a proper File Access Property List
 *          using H5Pset_fapl_ros() and use this list as input argument of the function
 *          H5Pset_fapl_ros3_endpoint().
 *
 *          Note, the endpoint url is only needed when you want to access a S3 bucket
 *          using an alternate url. For example, this can be useful when using s3:// object
 *          URIs to access files which are located somewhere other than the standard
 *          s3.<region-code>.amazonaws.com.
 *
 * \since 2.0.0
 */
H5_DLL herr_t H5Pset_fapl_ros3_endpoint(hid_t fapl_id, const char *endpoint);

/**
 * \ingroup FAPL
 *
 * \brief Queries a File Access Property List for #H5FD_ROS3 I/O page caching parameters.
 *
 * \fapl_id
 * \param[out] page_size Pointer for returning the currently set size, in bytes, of an I/O page.
 *                       May be NULL, in which case no value is returned.
 * \param[out] page_cache_size Pointer for returning the currently set maximum size, in bytes,
 *                       of the #H5FD_ROS3 I/O page cache. This is an upper limit of the amount
 *                       of bytes which will be allocated for caching I/O pages, excluding a small
 *                       amount of additional metadata bytes allocated for each page. May be NULL,
 *                       in which case no value is returned.
 * \param[out] lock_super_page Pointer for returning the currently set boolean value for whether
 *                             the I/O page containing a file's superblock metadata should be
 *                             locked in the I/O page cache. This will prevent that page from
 *                             being evicted from the page cache when trying to make space for
 *                             other I/O pages. May be NULL, in which case no value is returned.
 * \returns \herr_t
 *
 * \since 2.2.0
 */
H5_DLL herr_t H5Pget_fapl_ros3_paging(hid_t fapl_id, size_t *page_size, size_t *page_cache_size,
                                      bool *lock_super_page);

/**
 * \ingroup FAPL
 *
 * \brief Modifies the specified File Access Property List to set I/O page caching
 *        parameters for the #H5FD_ROS3 driver.
 *
 * \fapl_id
 * \param[in] page_size Specifies the size, in bytes, of an I/O page. The default value
 *                      is #HDF5_ROS3_VFD_DEFAULT_PAGE_SIZE.
 * \param[in] page_cache_size Specifies the total size, in bytes, of the I/O page cache.
 *                            The default value is 64MiB.
 * \param[in] lock_super_page Specifies whether or not to keep the I/O page containing a
 *                            file's superblock metadata locked in the I/O cache and unable
 *                            to be evicted. The default value is true.
 *
 * \details H5Pset_fapl_ros3_paging() sets parameters that control how the #H5FD_ROS3
 *          driver caches I/O to reduce requests to S3. By default, the #H5FD_ROS3
 *          driver performs I/O in large, fixed-size (#HDF5_ROS3_VFD_DEFAULT_PAGE_SIZE)
 *          blocks which are cached in memory and used to serve I/O requests. This
 *          function can be used to modify the default parameters to better suit a
 *          particular I/O pattern.
 *
 *          The macro #HDF5_ROS3_VFD_DEFAULT_PAGE_SIZE may be used to specify that the
 *          default page size is desired. The macro #H5F_PAGE_BUFFER_SIZE_DEFAULT may
 *          be used to specify that the default page cache size (currently, 64MiB) is
 *          desired. Setting either \p page_size or \p page_cache_size to 0 will disable
 *          I/O page caching in the #H5FD_ROS3 driver. This can be useful for reading
 *          small amounts of data from a file when the default \p page_size is much
 *          larger than the amount of data being read.
 *
 *          \p page_cache_size is an upper limit of the amount of bytes which will be
 *          allocated for caching I/O pages, excluding a small amount of additional
 *          metadata bytes allocated for each page. The page cache will only keep whole
 *          pages, so \p page_cache_size should ideally be set to some multiple of
 *          \p page_size. If \p page_cache_size is specified as a value smaller than
 *          \p page_size, \p page_size will be adjusted down to be equal to
 *          \p page_cache_size and the page cache will only hold a single page.
 *
 *          \p lock_super_page determines whether the I/O page containing a file's
 *          superblock metadata should be locked in the page cache and prevented from
 *          being evicted when trying to make space for other I/O pages. Depending on
 *          the layout of a file, this can be useful for keeping specific often-used
 *          metadata in the page cache.
 *
 * \parblock
 * \note When \p lock_super_page is specified as true, the first I/O page cached when
 *       attempting to locate a file's superblock is assumed to contain the superblock
 *       metadata. If a file contains a user block that is as large as, or larger than,
 *       the page size, this will cause the I/O page containing the file's superblock
 *       metadata to <strong>NOT</strong> be locked in the I/O page cache. For this case,
 *       the page size should be adjusted accordingly to be at least as large as the
 *       user block plus some bytes for the superblock metadata.
 * \endparblock
 *
 * \since 2.2.0
 */
H5_DLL herr_t H5Pset_fapl_ros3_paging(hid_t fapl_id, size_t page_size, size_t page_cache_size,
                                      bool lock_super_page);

#ifdef __cplusplus
}
#endif

#endif /* H5_HAVE_ROS3_VFD */

#endif /* ifndef H5FDros3_H */
