/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/**\defgroup H5F H5F
 *
 * Use the functions in this module to manage HDF5 files.
 *
 * In the code snippets below, we show the skeletal life cycle of an HDF5 file,
 * when creating a new file (left) or when opening an existing file (right).
 * File creation is essentially controlled through \ref FCPL, and file access to
 * new and existing files is controlled through \ref FAPL. The file \c name and
 * creation or access \c mode control the interaction with the underlying
 * storage such as file systems.
 *
 * <table>
 * <tr><th>Create</th><th>Read</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5F_examples.c create
 *   </td>
 *   <td>
 *   \snippet{lineno} H5F_examples.c read
 *   </td>
 * </tr>
 * <tr><th>Update</th><th>Delete</th></tr>
 * <tr valign="top">
 *   <td>
 *   \snippet{lineno} H5F_examples.c update
 *   </td>
 *   <td>
 *   \snippet{lineno} H5F_examples.c delete
 *   </td>
 * </tr>
 * </table>
 *
 * In addition to general file management functions, there are three categories
 * of functions that deal with advanced file management tasks and use cases:
 * 1. The control of the HDF5 \ref MDC
 * 2. The use of (MPI-) \ref PH5F HDF5
 *
 * \defgroup MDC Metadata Cache
 * \ingroup H5F
 * \defgroup PH5F Parallel
 * \ingroup H5F
 */

/*
 * This file contains public declarations for the H5F module.
 */
#ifndef H5Fpublic_H
#define H5Fpublic_H

/* Public header files needed by this file */
#include "H5public.h"
#include "H5ACpublic.h"
#include "H5Ipublic.h"

/* When this header is included from a private header, don't make calls to H5check() */
#undef H5CHECK
#ifndef H5private_H
#define H5CHECK H5check(),
#else /* H5private_H */
#define H5CHECK
#endif /* H5private_H */

/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.  As a side effect, they call H5check_version() to make sure
 * that the application is compiled with a version of the hdf5 header files
 * which are compatible with the library to which the application is linked.
 * We're assuming that these constants are used rather early in the hdf5
 * session.
 *
 * H5F_ACC_DEBUG no longer prints any special debug info. The symbol is
 * being retained and will be listed as deprecated in HDF5 1.10.0.
 */
#define H5F_ACC_RDONLY (H5CHECK 0x0000u) /**< Absence of RDWR: read-only */
#define H5F_ACC_RDWR   (H5CHECK 0x0001u) /**< Open for read and write    */
#define H5F_ACC_TRUNC  (H5CHECK 0x0002u) /**< Overwrite existing files   */
#define H5F_ACC_EXCL   (H5CHECK 0x0004u) /**< Fail if file already exists*/
#define H5F_ACC_DEBUG  (H5CHECK 0x0000u) /**< print debug info (no longer used) */
#define H5F_ACC_CREAT  (H5CHECK 0x0010u) /**< create non-existing files         */

/**
 * Default property list identifier
 *
 * \internal Value passed to H5Pset_elink_acc_flags to cause flags to be taken from the parent file.
 * \internal ignore setting on lapl
 */
#define H5F_ACC_DEFAULT (H5CHECK 0xffffu)

/* Flags for H5Fget_obj_count() & H5Fget_obj_ids() calls */
#define H5F_OBJ_FILE     (0x0001u) /**< File objects */
#define H5F_OBJ_DATASET  (0x0002u) /**< Dataset objects */
#define H5F_OBJ_GROUP    (0x0004u) /**< Group objects */
#define H5F_OBJ_DATATYPE (0x0008u) /**< Named datatype objects */
#define H5F_OBJ_ATTR     (0x0010u) /**< Attribute objects */
#define H5F_OBJ_ALL      (H5F_OBJ_FILE | H5F_OBJ_DATASET | H5F_OBJ_GROUP | H5F_OBJ_DATATYPE | H5F_OBJ_ATTR)
#define H5F_OBJ_LOCAL                                                                                        \
    (0x0020u) /**< Restrict search to objects opened through current file ID                                 \
                   (as opposed to objects opened through any file ID accessing this file) */

#define H5F_FAMILY_DEFAULT (hsize_t)0

#ifdef H5_HAVE_PARALLEL
/**
 * Use this constant string as the MPI_Info key to set H5Fmpio debug flags.
 * To turn on H5Fmpio debug flags, set the MPI_Info value with this key to
 * have the value of a string consisting of the characters that turn on the
 * desired flags.
 */
#define H5F_MPIO_DEBUG_KEY "H5F_mpio_debug_key"
#endif /* H5_HAVE_PARALLEL */

/**
 * The scope of an operation such as H5Fflush(), e.g.,
 * a single file vs. a set of mounted files
 */
typedef enum H5F_scope_t {
    H5F_SCOPE_LOCAL  = 0, /**< The specified file handle only */
    H5F_SCOPE_GLOBAL = 1  /**< The entire virtual file        */
} H5F_scope_t;

/**
 * Unlimited file size for H5Pset_external()
 */
#define H5F_UNLIMITED ((hsize_t)(-1L))

/**
 * How does file close behave?
 */
typedef enum H5F_close_degree_t {
    H5F_CLOSE_DEFAULT = 0, /**< Use the degree pre-defined by underlying VFD */
    H5F_CLOSE_WEAK    = 1, /**< File closes only after all opened objects are closed */
    H5F_CLOSE_SEMI    = 2, /**< If no opened objects, file is closed; otherwise, file close fails */
    H5F_CLOSE_STRONG  = 3  /**< If there are opened objects, close them first, then close file */
} H5F_close_degree_t;

/**
 * Current "global" information about file
 */
//! <!-- [H5F_info_t_snip] -->
typedef struct H5F_info_t {
    hsize_t super_ext_size; /**< Superblock extension size */
    struct {
        hsize_t      hdr_size;  /**< Shared object header message header size */
        H5_ih_info_t msgs_info; /**< Shared object header message index & heap size */
    } sohm;
} H5F_info_t;
//! <!-- [H5F_info_t_snip] -->

/**
 * Types of allocation requests. The values larger than #H5FD_MEM_DEFAULT
 * should not change other than adding new types to the end. These numbers
 * might appear in files.
 *
 * \internal Please change the log VFD flavors array if you change this
 *           enumeration.
 */
typedef enum H5F_mem_t {
    H5FD_MEM_NOLIST = -1, /**< Data should not appear in the free list.
                           * Must be negative.
                           */
    H5FD_MEM_DEFAULT = 0, /**< Value not yet set.  Can also be the
                           * datatype set in a larger allocation
                           * that will be suballocated by the library.
                           * Must be zero.
                           */
    H5FD_MEM_SUPER = 1,   /**< Superblock data */
    H5FD_MEM_BTREE = 2,   /**< B-tree data */
    H5FD_MEM_DRAW  = 3,   /**< Raw data (content of datasets, etc.) */
    H5FD_MEM_GHEAP = 4,   /**< Global heap data */
    H5FD_MEM_LHEAP = 5,   /**< Local heap data */
    H5FD_MEM_OHDR  = 6,   /**< Object header data */

    H5FD_MEM_NTYPES /**< Sentinel value - must be last */
} H5F_mem_t;

/**
 * Library's format versions
 */
typedef enum H5F_libver_t {
    H5F_LIBVER_EARLIEST, /**< Use the earliest possible format for storing objects */
    H5F_LIBVER_LATEST    /**< Use the latest possible format available for storing objects*/
} H5F_libver_t;

/* Define file format version for 1.8 to prepare for 1.10 release.
 * (Not used anywhere now)*/
#define H5F_LIBVER_18 H5F_LIBVER_LATEST

#ifdef __cplusplus
extern "C" {
#endif

/**
 * \ingroup H5F
 *
 * \brief Checks if a file can be opened with a given file access property
 *        list
 *
 * \param[in] filename Name of a file
 *
 * \return \htri_t
 *
 * \details H5F__is_hdf5() checks if the file specified by \p
 *          filename can be opened.
 *
 * \note The H5Fis_hdf5(), only uses
 *       the default file driver when opening a file.
 *
 */
H5_DLL htri_t H5Fis_hdf5(const char *filename);
/**
 * \ingroup H5F
 *
 * \brief Creates an HDF5 file
 *
 * \param[in] filename Name of the file to create
 * \param[in] flags    File access flags. Allowable values are:
 *                     - #H5F_ACC_TRUNC: Truncate file, if it already exists,
 *                       erasing all data previously stored in the file
 *                     - #H5F_ACC_EXCL: Fail if file already exists
 * \fcpl_id
 * \fapl_id
 * \return \hid_t{file}
 *
 * \details H5Fcreate() is the primary function for creating HDF5 files; it
 *          creates a new HDF5 file with the specified name and property lists.
 *
 *          The \p filename parameter specifies the name of the new file.
 *
 *          The \p flags parameter specifies whether an existing file is to be
 *          overwritten. It should be set to either #H5F_ACC_TRUNC to overwrite
 *          an existing file or #H5F_ACC_EXCL, instructing the function to fail
 *          if the file already exists.
 *
 *          New files are always created in read-write mode, so the read-write
 *          and read-only flags, #H5F_ACC_RDWR and #H5F_ACC_RDONLY,
 *          respectively, are not relevant in this function. Further note that
 *          a specification of #H5F_ACC_RDONLY will be ignored; the file will
 *          be created in read-write mode, regardless.
 *
 *          More complex behaviors of file creation and access are controlled
 *          through the file creation and file access property lists,
 *          \p fcpl_id and \p fapl_id, respectively. The value of #H5P_DEFAULT
 *          for any property list value indicates that the library should use
 *          the default values for that appropriate property list.
 *
 *          The return value is a file identifier for the newly-created file;
 *          this file identifier should be closed by calling H5Fclose() when
 *          it is no longer needed.
 *
 * \par Example
 * \snippet H5F_examples.c minimal
 *
 * \note  #H5F_ACC_TRUNC and #H5F_ACC_EXCL are mutually exclusive; use
 *        exactly one.
 *
 * \note An additional flag, #H5F_ACC_DEBUG, prints debug information. This
 *       flag can be combined with one of the above values using the bit-wise
 *       OR operator (\c |), but it is used only by HDF5 library developers;
 *       \Emph{it is neither tested nor supported for use in applications}.
 *
 * \attention \Bold{Special case — File creation in the case of an already-open file:}
 *            If a file being created is already opened, by either a previous
 *            H5Fopen() or H5Fcreate() call, the HDF5 library may or may not
 *            detect that the open file and the new file are the same physical
 *            file. (See H5Fopen() regarding the limitations in detecting the
 *            re-opening of an already-open file.)\n
 *            If the library detects that the file is already opened,
 *            H5Fcreate() will return a failure, regardless of the use of
 *            #H5F_ACC_TRUNC.\n
 *            If the library does not detect that the file is already opened
 *            and #H5F_ACC_TRUNC is not used, H5Fcreate() will return a failure
 *            because the file already exists. Note that this is correct
 *            behavior.\n
 *            But if the library does not detect that the file is already
 *            opened and #H5F_ACC_TRUNC is used, H5Fcreate() will truncate the
 *            existing file and return a valid file identifier. Such a
 *            truncation of a currently-opened file will almost certainly
 *            result in errors. While unlikely, the HDF5 library may not be
 *            able to detect, and thus report, such errors.\n
 *            Applications should avoid calling H5Fcreate() with an already
 *            opened file.
 *
 * \since 1.0.0
 *
 * \see H5Fopen(), H5Fclose()
 *
 */
H5_DLL hid_t H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
/**
 * \ingroup H5F
 *
 * \brief Opens an existing HDF5 file
 *
 * \param[in] filename Name of the file to be opened
 * \param[in] flags    File access flags. Allowable values are:
 *                     - #H5F_ACC_RDWR: Allows read and write access to file
 *                     - #H5F_ACC_RDONLY: Allows read-only access to file
 *                     - An additional flag, #H5F_ACC_DEBUG, prints debug
 *                       information. This flag can be combined with one of the
 *                       above values using the bit-wise OR operator (\c |), but
 *                       it is used only by HDF5 library developers;
 *                       \Emph{it is neither tested nor supported} for use in
 *                       applications.
 * \fapl_id
 * \return \hid_t{file}
 *
 * \details H5Fopen() is the primary function for accessing existing HDF5 files.
 *          This function opens the named file in the specified access mode and
 *          with the specified access property list.
 *
 *          Note that H5Fopen() does not create a file if it does not already
 *          exist; see H5Fcreate().
 *
 *          The \p filename parameter specifies the name of the file to be
 *          opened.
 *
 *          The \p fapl_id parameter specifies the file access property list.
 *          Use of #H5P_DEFAULT specifies that default I/O access properties
 *          are to be used.
 *
 *          The \p flags parameter specifies whether the file will be opened in
 *          read-write or read-only mode, #H5F_ACC_RDWR or #H5F_ACC_RDONLY,
 *          respectively. More complex behaviors of file access are controlled
 *          through the file-access property list.
 *
 *          The return value is a file identifier for the open file; this file
 *          identifier should be closed by calling H5Fclose() when it is no
 *          longer needed.
 *
 * \par Example
 * \snippet H5F_examples.c open
 *
 * \note  #H5F_ACC_RDWR and #H5F_ACC_RDONLY are mutually exclusive; use
 *        exactly one.
 *
 * \attention \Bold{Special cases — Multiple opens:} A file can often be opened
 *            with a new H5Fopen() call without closing an already-open
 *            identifier established in a previous H5Fopen() or H5Fcreate()
 *            call. Each such H5Fopen() call will return a unique identifier
 *            and the file can be accessed through any of these identifiers as
 *            long as the identifier remains valid. In such multiply-opened
 *            cases, the open calls must use the same flags argument and the
 *            file access property lists must use the same file close degree
 *            property setting (see the external link discussion below and
 *            H5Pset_fclose_degree()).\n
 *            In some cases, such as files on a local Unix file system, the
 *            HDF5 library can detect that a file is multiply opened and will
 *            maintain coherent access among the file identifiers.\n
 *            But in many other cases, such as parallel file systems or
 *            networked file systems, it is not always possible to detect
 *            multiple opens of the same physical file. In such cases, HDF5
 *            will treat the file identifiers as though they are accessing
 *            different files and will be unable to maintain coherent access.
 *            Errors are likely to result in these cases. While unlikely, the
 *            HDF5 library may not be able to detect, and thus report,
 *            such errors.\n
 *            It is generally recommended that applications avoid multiple
 *            opens of the same file.
 *
 * \attention \Bold{Special restriction on multiple opens of a file first
 *            opened by means of an external link:} When an external link is
 *            followed, the external file is always opened with the weak file
 *            close degree property setting, #H5F_CLOSE_WEAK (see
 *            H5Lcreate_external() and H5Pset_fclose_degree()). If the file is
 *            reopened with H5Fopen while it remains held open from such an
 *            external link call, the file access property list used in the
 *            open call must include the file close degree setting
 *            #H5F_CLOSE_WEAK or the open will fail.
 *
 * \see H5Fclose()
 *
 */
H5_DLL hid_t H5Fopen(const char *filename, unsigned flags, hid_t fapl_id);
/**
 * \ingroup H5F
 *
 * \brief Returns a new identifier for a previously-opened HDF5 file
 *
 * \param[in] file_id Identifier of a file for which an additional identifier
 *                    is required
 *
 * \return \hid_t{file}
 *
 * \details H5Freopen() returns a new file identifier for an already-open HDF5
 *          file, as specified by \p file_id. Both identifiers share caches and
 *          other information. The only difference between the identifiers is
 *          that the new identifier is not mounted anywhere and no files are
 *          mounted on it.
 *
 *          The new file identifier should be closed by calling H5Fclose() when
 *          it is no longer needed.
 *
 * \note Note that there is no circumstance under which H5Freopen() can
 *       actually open a closed file; the file must already be open and have an
 *       active \p file_id. E.g., one cannot close a file with H5Fclose() on
 *       \p file_id then use H5Freopen() on \p file_id to reopen it.
 *
 */
H5_DLL hid_t H5Freopen(hid_t file_id);
/**
 * \ingroup H5F
 *
 * \brief Flushes all buffers associated with a file to storage
 *
 * \loc_id{object_id}
 * \param[in] scope The scope of the flush action
 *
 * \return \herr_t
 *
 * \details H5Fflush() causes all buffers associated with a file to be
 *          immediately flushed to storage without removing the data from the
 *          cache.
 *
 *          \p object_id can be any object associated with the file, including
 *          the file itself, a dataset, a group, an attribute, or a named
 *          datatype.
 *
 *          \p scope specifies whether the scope of the flush action is
 *          global or local. Valid values are as follows:
 *          \scopes
 *
 * \par Example
 * \snippet H5F_examples.c flush
 *
 * \attention HDF5 does not possess full control over buffering. H5Fflush()
 *            flushes the internal HDF5 buffers then asks the operating system
 *            (the OS) to flush the system buffers for the open files. After
 *            that, the OS is responsible for ensuring that the data is
 *            actually flushed to disk.
 *
 */
H5_DLL herr_t H5Fflush(hid_t object_id, H5F_scope_t scope);
/**
 * \ingroup H5F
 *
 * \brief Terminates access to an HDF5 file
 *
 * \file_id
 * \return \herr_t
 *
 * \details H5Fclose() terminates access to an HDF5 file (specified by
 *          \p file_id) by flushing all data to storage.
 *
 *          If this is the last file identifier open for the file and no other
 *          access identifier is open (e.g., a dataset identifier, group
 *          identifier, or shared datatype identifier), the file will be fully
 *          closed and access will end.
 *
 * \par Example
 * \snippet H5F_examples.c minimal
 *
 * \note \Bold{Delayed close:} Note the following deviation from the
 *       above-described behavior. If H5Fclose() is called for a file but one
 *       or more objects within the file remain open, those objects will remain
 *       accessible until they are individually closed. Thus, if the dataset
 *       \c data_sample is open when H5Fclose() is called for the file
 *       containing it, \c data_sample will remain open and accessible
 *       (including writable) until it is explicitly closed. The file will be
 *       automatically closed once all objects in the file have been closed.\n
 *       Be warned, however, that there are circumstances where it is not
 *       possible to delay closing a file. For example, an MPI-IO file close is
 *       a collective call; all of the processes that opened the file must
 *       close it collectively. The file cannot be closed at some time in the
 *       future by each process in an independent fashion. Another example is
 *       that an application using an AFS token-based file access privilege may
 *       destroy its AFS token after H5Fclose() has returned successfully. This
 *       would make any future access to the file, or any object within it,
 *       illegal.\n
 *       In such situations, applications must close all open objects in a file
 *       before calling H5Fclose. It is generally recommended to do so in all
 *       cases.
 *
 * \see H5Fopen()
 *
 */
H5_DLL herr_t H5Fclose(hid_t file_id);
/**
 * \ingroup H5F
 *
 * \brief Returns a file creation property list identifier
 *
 * \file_id
 * \return \hid_t{file creation property list}
 *
 * \details H5Fget_create_plist() returns the file creation property list
 *          identifier identifying the creation properties used to create this
 *          file. This function is useful for duplicating properties when
 *          creating another file.
 *
 *          The creation property list identifier should be released with
 *          H5Pclose().
 *
 */
H5_DLL hid_t H5Fget_create_plist(hid_t file_id);
/**
 * \ingroup H5F
 *
 * \brief Returns a file access property list identifier
 *
 * \file_id
 * \return \hid_t{file access property list}
 *
 * \details H5Fget_access_plist() returns the file access property list
 *          identifier of the specified file.
 *
 */
H5_DLL hid_t H5Fget_access_plist(hid_t file_id);
/**
 * \ingroup H5F
 *
 * \brief Determines the read/write or read-only status of a file
 *
 * \file_id
 * \param[out] intent Access mode flag as originally passed with H5Fopen()
 *
 * \return \herr_t
 *
 * \details Given the identifier of an open file, \p file_id, H5Fget_intent()
 *          retrieves the intended access mode" flag passed with H5Fopen() when
 *          the file was opened.
 *
 *          The value of the flag is returned in \p intent. Valid values are as
 *          follows:
 *          \file_access
 *
 * \note The function will not return an error if intent is NULL; it will
 *       simply do nothing.
 *
 * \version 1.10.0 Function enhanced to work with SWMR functionality.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Fget_intent(hid_t file_id, unsigned *intent);
/**
 * \ingroup H5F
 *
 * \brief Returns the number of open object identifiers for an open file
 *
 * \file_id or #H5F_OBJ_ALL for all currently-open HDF5 files
 * \param[in] types Type of object for which identifiers are to be returned
 *
 * \return Returns the number of open objects if successful; otherwise returns
 *         a negative value.
 *
 * \details Given the identifier of an open file, file_id, and the desired
 *          object types, types, H5Fget_obj_count() returns the number of open
 *          object identifiers for the file.
 *
 *          To retrieve a count of open identifiers for open objects in all
 *          HDF5 application files that are currently open, pass the value
 *          #H5F_OBJ_ALL in \p file_id.
 *
 *          The types of objects to be counted are specified in types as
 *          follows:
 *          \obj_types
 *
 *          Multiple object types can be combined with the
 *          logical \c OR operator (|). For example, the expression
 *          \c (#H5F_OBJ_DATASET|#H5F_OBJ_GROUP) would call for datasets and
 *          groups.
 *
 * \version 1.6.8, 1.8.2 Function return type changed to \c ssize_t.
 * \version 1.6.5 #H5F_OBJ_LOCAL has been added as a qualifier on the types
 *                of objects to be counted. #H5F_OBJ_LOCAL restricts the
 *                search to objects opened through current file identifier.
 *
 */
H5_DLL ssize_t H5Fget_obj_count(hid_t file_id, unsigned types);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Returns a list of open object identifiers
 *
 * \file_id or #H5F_OBJ_ALL for all currently-open HDF5 files
 * \param[in] types Type of object for which identifiers are to be returned
 * \param[in] max_objs Maximum number of object identifiers to place into
 *                     \p obj_id_list
 * \param[out] obj_id_list Pointer to the returned buffer of open object
 *                         identifiers
 *
 * \return Returns number of objects placed into \p obj_id_list if successful;
 *         otherwise returns a negative value.
 *
 * \details Given the file identifier \p file_id and the type of objects to be
 *          identified, types, H5Fget_obj_ids() returns the list of identifiers
 *          for all open HDF5 objects fitting the specified criteria.
 *
 *          To retrieve identifiers for open objects in all HDF5 application
 *          files that are currently open, pass the value #H5F_OBJ_ALL in
 *          \p file_id.
 *
 *          The types of object identifiers to be retrieved are specified in
 *          types using the codes listed for the same parameter in
 *          H5Fget_obj_count().
 *
 *          To retrieve a count of open objects, use the H5Fget_obj_count()
 *          function. This count can be used to set the \p max_objs parameter.
 *
 * \version 1.8.2 Function return type changed to \c ssize_t and \p
 *                max_objs parameter datatype changed to \c size_t.
 * \version 1.6.8 Function return type changed to \c ssize_t and \p
 *                max_objs parameter datatype changed to \c size_t.
 * \since 1.6.0
 *
 */
H5_DLL ssize_t H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *obj_id_list);
/**
 * \ingroup H5F
 *
 * \brief Returns pointer to the file handle from the virtual file driver
 *
 * \file_id
 * \fapl_id{fapl}
 * \param[out] file_handle Pointer to the file handle being used by the
 *                         low-level virtual file driver
 *
 * \return \herr_t
 *
 * \details Given the file identifier \p file_id and the file access property
 *          list \p fapl_id, H5Fget_vfd_handle() returns a pointer to the file
 *          handle from the low-level file driver currently being used by the
 *          HDF5 library for file I/O.
 *
 * \note For most drivers, the value of \p fapl_id will be #H5P_DEFAULT. For
 *       the \c FAMILY or \c MULTI drivers, this value should be defined
 *       through the property list functions: H5Pset_family_offset() for the
 *       \c FAMILY driver and H5Pset_multi_type() for the \c MULTI driver
 *
 * \since 1.6.0
 *
 */
H5_DLL herr_t H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle);
/**
 * \ingroup H5F
 *
 * \brief Mounts an HDF5 file
 *
 * \loc_id{loc}
 * \param[in] name Name of the group onto which the file specified by \p child
 *                 is to be mounted
 * \file_id{child}
 * \param[in] plist File mount property list identifier. Pass #H5P_DEFAULT!
 *
 * \return \herr_t
 *
 * \details H5Fmount() mounts the file specified by \p child onto the object
 *          specified by \p loc and \p name using the mount properties \p plist
 *          If the object specified by \p loc is a dataset, named datatype or
 *          attribute, then the file will be mounted at the location where the
 *          attribute, dataset, or named datatype is attached.
 *
 * \par Example
 * \snippet H5F_examples.c mount
 *
 * \note To date, no file mount properties have been defined in HDF5. The
 *       proper value to pass for \p plist is #H5P_DEFAULT, indicating the
 *       default file mount property list.
 *
 */
H5_DLL herr_t H5Fmount(hid_t loc, const char *name, hid_t child, hid_t plist);
/**
 * \ingroup H5F
 *
 * \brief Unounts an HDF5 file
 *
 * \loc_id{loc}
 * \param[in] name Name of the mount point
 *
 * \return \herr_t
 *
 * \details Given a mount point, H5Funmount() dissociates the mount point's
 *          file from the file mounted there. This function does not close
 *          either file.
 *
 *          The mount point can be either the group in the parent or the root
 *          group of the mounted file (both groups have the same name). If the
 *          mount point was opened before the mount then it is the group in the
 *          parent; if it was opened after the mount then it is the root group
 *          of the child.
 *
 */
H5_DLL herr_t H5Funmount(hid_t loc, const char *name);
/**
 * \ingroup H5F
 *
 * \brief Returns the amount of free space in a file (in bytes)
 *
 * \file_id
 *
 * \return Returns the amount of free space in the file if successful;
 *         otherwise returns a negative value.
 *
 * \details Given the identifier of an open file, \p file_id,
 *          H5Fget_freespace() returns the amount of space that is unused by
 *          any objects in the file.
 *
 *          The interpretation of this number depends on the configured free space
 *          management strategy. For example, if the HDF5 library only tracks free
 *          space in a file from a file open or create until that file is closed,
 *          then this routine will report the free space that has been created
 *          during that interval.
 *
 * \since 1.6.1
 *
 */
H5_DLL hssize_t H5Fget_freespace(hid_t file_id);
/**
 * \ingroup H5F
 *
 * \brief Returns the size of an HDF5 file (in bytes)
 *
 * \file_id
 * \param[out] size Size of the file, in bytes
 *
 * \return \herr_t
 *
 * \details H5Fget_filesize() returns the size of the HDF5 file specified by
 *          \p file_id.
 *
 *          The returned size is that of the entire file, as opposed to only
 *          the HDF5 portion of the file. I.e., size includes the user block,
 *          if any, the HDF5 portion of the file, and any data that may have
 *          been appended beyond the data written through the HDF5 library.
 *
 * \since 1.6.3
 *
 */
H5_DLL herr_t H5Fget_filesize(hid_t file_id, hsize_t *size);
/**
 * \ingroup H5F
 *
 * \brief Retrieves a copy of the image of an existing, open file
 *
 * \file_id
 * \param[out] buf_ptr Pointer to the buffer into which the image of the
 *                     HDF5 file is to be copied. If \p buf_ptr is NULL,
 *                     no data will be copied but the function’s return value
 *                     will still indicate the buffer size required (or a
 *                     negative value on error).
 * \param[out] buf_len Size of the supplied buffer
 *
 * \return ssize_t
 *
 * \details H5Fget_file_image() retrieves a copy of the image of an existing,
 *          open file. This routine can be used with files opened using the
 *          SEC2 (or POSIX), STDIO, and Core (or Memory) virtual file drivers
 *          (VFDs).
 *
 *          If the return value of H5Fget_file_image() is a positive value, it
 *          will be the length in bytes of the buffer required to store the
 *          file image. So if the file size is unknown, it can be safely
 *          determined with an initial H5Fget_file_image() call with buf_ptr
 *          set to NULL. The file image can then be retrieved with a second
 *          H5Fget_file_image() call with \p buf_len set to the initial call’s
 *          return value.
 *
 *          While the current file size can also be retrieved with
 *          H5Fget_filesize(), that call may produce a larger value than is
 *          needed. The value returned by H5Fget_filesize() includes the user
 *          block, if it exists, and any unallocated space at the end of the
 *          file. It is safe in all situations to get the file size with
 *          H5Fget_file_image() and it often produces a value that is more
 *          appropriate for the size of a file image buffer.
 *
 * \note \Bold{Recommended Reading:} This function is part of the file image
 *       operations feature set. It is highly recommended to study the guide
 *       \ref_file_image_ops before using this feature set.
 *
 * \attention H5Pget_file_image() will fail, returning a negative value, if the
 *            file is too large for the supplied buffer.
 *
 * \see H5LTopen_file_image(), H5Pset_file_image(), H5Pget_file_image(),
 *      H5Pset_file_image_callbacks(), H5Pget_file_image_callbacks()
 *
 * \since 1.8.0
 *
 */
H5_DLL ssize_t H5Fget_file_image(hid_t file_id, void *buf_ptr, size_t buf_len);
/**
 * \ingroup MDC
 *
 * \brief Obtains current metadata cache configuration for target file
 *
 * \file_id
 * \param[in,out] config_ptr Pointer to the H5AC_cache_config_t instance in which
 *                        the current metadata cache configuration is to be
 *                        reported. The fields of this structure are discussed
 *                        \ref H5AC-cache-config-t "here".
 * \return \herr_t
 *
 * \note The \c in direction applies only to the H5AC_cache_config_t::version
 *       field. All other fields are out parameters.
 *
 * \details H5Fget_mdc_config() loads the current metadata cache configuration
 *          into the instance of H5AC_cache_config_t pointed to by the \p config_ptr
 *          parameter.\n
 *          The fields of the H5AC_cache_config_t structure are shown below:
 *          \snippet H5ACpublic.h H5AC_cache_config_t_snip
 *          \click4more
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Fget_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr);
/**
 * \ingroup MDC
 *
 * \brief Attempts to configure metadata cache of target file
 *
 * \file_id
 * \param[in,out] config_ptr Pointer to the H5AC_cache_config_t instance
 *                           containing the desired configuration.
 *                           The fields of this structure are discussed
 *                           \ref H5AC-cache-config-t "here".
 * \return \herr_t
 *
 * \details H5Fset_mdc_config() attempts to configure the file's metadata cache
 *          according configuration supplied in \p config_ptr.
 *          \snippet H5ACpublic.h H5AC_cache_config_t_snip
 *          \click4more
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Fset_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr);
/**
 * \ingroup MDC
 *
 * \brief Obtains target file's metadata cache hit rate
 *
 * \file_id
 * \param[out] hit_rate_ptr Pointer to the double in which the hit rate is returned. Note that
 *                          \p hit_rate_ptr is undefined if the API call fails
 * \return \herr_t
 *
 * \details H5Fget_mdc_hit_rate() queries the metadata cache of the target file to obtain its hit rate
 *          \Code{(cache hits / (cache hits + cache misses))} since the last time hit rate statistics
 *          were reset. If the cache has not been accessed since the last time the hit rate stats were
 *          reset, the hit rate is defined to be 0.0.
 *
 *          The hit rate stats can be reset either manually (via H5Freset_mdc_hit_rate_stats()), or
 *          automatically. If the cache's adaptive resize code is enabled, the hit rate stats will be
 *          reset once per epoch. If they are reset manually as well, the cache may behave oddly.
 *
 *          See the overview of the metadata cache in the special topics section of the user manual for
 *          details on the metadata cache and its adaptive resize algorithms.
 *
 */
H5_DLL herr_t H5Fget_mdc_hit_rate(hid_t file_id, double *hit_rate_ptr);
/**
 * \ingroup MDC
 *
 * \brief Obtains current metadata cache size data for specified file
 *
 * \file_id
 * \param[out] max_size_ptr Pointer to the location in which the current cache maximum size is to be
 *                          returned, or NULL if this datum is not desired
 * \param[out] min_clean_size_ptr Pointer to the location in which the current cache minimum clean
 *                                size is to be returned, or NULL if that datum is not desired
 * \param[out] cur_size_ptr Pointer to the location in which the current cache size is to be returned,
 *                          or NULL if that datum is not desired
 * \param[out] cur_num_entries_ptr Pointer to the location in which the current number of entries in
 *                                 the cache is to be returned, or NULL if that datum is not desired
 * \returns \herr_t
 *
 * \details H5Fget_mdc_size()  queries the metadata cache of the target file for the desired size
 *          information, and returns this information in the locations indicated by the pointer
 *          parameters. If any pointer parameter is NULL, the associated data is not returned.
 *
 *          If the API call fails, the values returned via the pointer parameters are undefined.
 *
 *          If adaptive cache resizing is enabled, the cache maximum size and minimum clean size
 *          may change at the end of each epoch. Current size and current number of entries can
 *          change on each cache access.
 *
 *          Current size can exceed maximum size under certain conditions. See the overview of the
 *          metadata cache in the special topics section of the user manual for a discussion of this.
 *
 */
H5_DLL herr_t H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
                              size_t *cur_size_ptr, int *cur_num_entries_ptr);
/**
 * \ingroup MDC
 *
 * \brief Resets hit rate statistics counters for the target file
 *
 * \file_id
 * \returns \herr_t
 *
 * \details
 * \parblock
 * H5Freset_mdc_hit_rate_stats() resets the hit rate statistics counters in the metadata cache
 * associated with the specified file.
 *
 * If the adaptive cache resizing code is enabled, the hit rate statistics are reset at the beginning
 * of each epoch. This API call allows you to do the same thing from your program.
 *
 * The adaptive cache resizing code may behave oddly if you use this call when adaptive cache resizing
 * is enabled. However, the call should be useful if you choose to control metadata cache size from your
 * program.
 *
 * See \ref_mdc_in_hdf5 for details about the metadata cache and the adaptive cache resizing
 * algorithms. If you have not read, understood, and thought about the material covered in that
 * documentation,
 * you should not be using this API call.
 * \endparblock
 *
 */
H5_DLL herr_t H5Freset_mdc_hit_rate_stats(hid_t file_id);
/**
 * \ingroup H5F
 *
 * \brief Retrieves name of file to which object belongs
 *
 * \obj_id
 * \param[out] name Buffer for the file name
 * \param[in] size Size, in bytes, of the \p name buffer
 *
 * \return Returns the length of the file name if successful; otherwise returns
 *         a negative value.
 *
 * \details H5Fget_name() retrieves the name of the file to which the object \p
 *          obj_id belongs. The object can be a file, group, dataset,
 *          attribute, or named datatype.
 *
 *          Up to \p size characters of the file name are returned in \p name;
 *          additional characters, if any, are not returned to the user
 *          application.
 *
 *          If the length of the name, which determines the required value of
 *          size, is unknown, a preliminary H5Fget_name() call can be made by
 *          setting \p name to NULL. The return value of this call will be the
 *          size of the file name; that value plus one (1) can then be assigned
 *          to size for a second H5Fget_name() call, which will retrieve the
 *          actual name. (The value passed in with the parameter \p size must
 *          be one greater than size in bytes of the actual name in order to
 *          accommodate the null terminator; if \p size is set to the exact
 *          size of the name, the last byte passed back will contain the null
 *          terminator and the last character will be missing from the name
 *          passed back to the calling application.)
 *
 *          If an error occurs, the buffer pointed to by \p name is unchanged
 *          and the function returns a negative value.
 *
 * \since 1.6.3
 *
 */
H5_DLL ssize_t H5Fget_name(hid_t obj_id, char *name, size_t size);
/**
 * \ingroup H5F
 *
 * \brief Retrieves name of file to which object belongs
 *
 * \fgdta_obj_id
 * \param[out] file_info Buffer for global file information
 *
 * \return \herr_t
 *
 * \details H5Fget_info() returns global information for the file associated
 *          with the object identifier \p obj_id in the H5F_info_t \c struct
 *          named \p file_info.
 *
 *          \p obj_id is an identifier for any object in the file of interest.
 *
 *          H5F_info_t struct is defined in H5Fpublic.h as follows:
 *          \snippet this H5F_info_t_snip
 *
 *          \c super_ext_size is the size of the superblock extension.
 *
 *          The \c sohm sub-struct contains shared object header message
 *          information as follows:
 *          \li \c hdr_size is the size of the shared object header message.
 *          \li \c msgs_info is an H5_ih_info_t struct defined in H5public.h as
 *              follows: \snippet H5public.h H5_ih_info_t_snip
 *          \li \p index_size is the summed size of all the shared object
 *              header indexes. Each index might be either a B-tree or
 *              a list.
 *
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Fget_info(hid_t obj_id, H5F_info_t *file_info);
/**
 * \ingroup H5F
 *
 * \brief Clears the external link open file cache
 *
 * \file_id
 * \return \herr_t
 *
 * \details H5Fclear_elink_file_cache() evicts all the cached child files in
 *          the specified file’s external file cache, causing them to be closed
 *          if there is nothing else holding them open.
 *
 *          H5Fclear_elink_file_cache() does not close the cache itself;
 *          subsequent external link traversals from the parent file will again
 *          cache the target file. See H5Pset_elink_file_cache_size() for
 *          information on closing the file cache.
 *
 * \see H5Pset_elink_file_cache_size(), H5Pget_elink_file_cache_size()
 *
 * \since 1.8.7
 *
 */
H5_DLL herr_t H5Fclear_elink_file_cache(hid_t file_id);
#ifdef H5_HAVE_PARALLEL
/**
 * \ingroup PH5F
 *
 * \brief Sets the MPI atomicity mode
 *
 * \file_id
 * \param[in] flag Logical flag for atomicity setting. Valid values are:
 *                 \li \c 1 -- Sets MPI file access to atomic mode.
 *                 \li \c 0 -- Sets MPI file access to nonatomic mode.
 * \returns \herr_t
 *
 * \par Motivation
 * H5Fset_mpi_atomicity() is applicable only in parallel environments using MPI I/O.
 * The function is one of the tools used to ensure sequential consistency. This means
 * that a set of operations will behave as though they were performed in a serial
 * order consistent with the program order.
 *
 * \details
 * \parblock
 * H5Fset_mpi_atomicity() sets MPI consistency semantics for data access to the file,
 * \p file_id.
 *
 * If \p flag is set to \c 1, all file access operations will appear atomic, guaranteeing
 * sequential consistency. If \p flag is set to \c 0, enforcement of atomic file access
 * will be turned off.
 *
 * H5Fset_mpi_atomicity() is a collective function and all participating processes must
 * pass the same values for \p file_id and \p flag.
 *
 * This function is available only when the HDF5 library is configured with parallel support
 * (\Code{--enable-parallel}). It is useful only when used with the #H5FD_MPIO driver
 * (see H5Pset_fapl_mpio()).
 * \endparblock
 *
 * \attention
 * \parblock
 * H5Fset_mpi_atomicity() calls \Code{MPI_File_set_atomicity} underneath and is not supported
 * if the execution platform does not support \Code{MPI_File_set_atomicity}. When it is
 * supported and used, the performance of data access operations may drop significantly.
 *
 * In certain scenarios, even when \Code{MPI_File_set_atomicity} is supported, setting
 * atomicity with H5Fset_mpi_atomicity() and \p flag set to 1 does not always yield
 * strictly atomic updates. For example, some H5Dwrite() calls translate to multiple
 * \Code{MPI_File_write_at} calls. This happens in all cases where the high-level file
 * access routine translates to multiple lower level file access routines.
 * The following scenarios will raise this issue:
 * \li Non-contiguous file access using independent I/O
 * \li Partial collective I/O using chunked access
 * \li Collective I/O using filters or when data conversion is required
 *
 * This issue arises because MPI atomicity is a matter of MPI file access operations rather
 * than HDF5 access operations. But the user is normally seeking atomicity at the HDF5 level.
 * To accomplish this, the application must set a barrier after a write, H5Dwrite(), but before
 * the next read, H5Dread(), in addition to calling H5Fset_mpi_atomicity().The barrier will
 * guarantee that all underlying write operations execute atomically before the read
 * operations starts. This ensures additional ordering semantics and will normally produce
 * the desired behavior.
 * \endparblock
 *
 * \see \ref_cons_semantics
 *
 * \since 1.8.9
 *
 */
H5_DLL herr_t H5Fset_mpi_atomicity(hid_t file_id, hbool_t flag);
/**
 * \ingroup PH5F
 *
 * \brief Retrieves the atomicity mode in use
 *
 * \file_id
 * \param[out] flag Logical flag for atomicity setting. Valid values are:
 *                  \li 1 -- MPI file access is set to atomic mode.
 *                  \li 0 -- MPI file access is set to nonatomic mode.
 * \returns \herr_t
 *
 * \details H5Fget_mpi_atomicity() retrieves the current consistency semantics mode for
 *          data access for the file \p file_id.
 *
 *          Upon successful return, \p flag will be set to \c 1 if file access is set
 *          to atomic mode and \c 0 if file access is set to nonatomic mode.
 *
 * \see \ref_cons_semantics
 *
 * \since 1.8.9
 *
 */
H5_DLL herr_t H5Fget_mpi_atomicity(hid_t file_id, hbool_t *flag);
#endif /* H5_HAVE_PARALLEL */

#ifdef __cplusplus
}
#endif
#endif /* H5Fpublic_H */
