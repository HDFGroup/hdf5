/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5F module.
 */
#ifndef _H5Fpublic_H
#define _H5Fpublic_H

/* Public header files needed by this file */
#include "H5public.h"
#include "H5ACpublic.h"
#include "H5Ipublic.h"

/* When this header is included from a private header, don't make calls to H5check() */
#undef H5CHECK
#ifndef _H5private_H
#define H5CHECK          H5check(),
#else   /* _H5private_H */
#define H5CHECK
#endif  /* _H5private_H */

/* When this header is included from a private HDF5 header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5private_H
#define H5OPEN        H5open(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */

/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.  As a side effect, they call H5check_version() to make sure
 * that the application is compiled with a version of the hdf5 header files
 * which are compatible with the library to which the application is linked.
 * We're assuming that these constants are used rather early in the hdf5
 * session.
 */
#define H5F_ACC_RDONLY    (H5CHECK H5OPEN 0x0000u)    /*absence of rdwr => rd-only */
#define H5F_ACC_RDWR    (H5CHECK H5OPEN 0x0001u)    /*open for read and write    */
#define H5F_ACC_TRUNC    (H5CHECK H5OPEN 0x0002u)    /*overwrite existing files   */
#define H5F_ACC_EXCL    (H5CHECK H5OPEN 0x0004u)    /*fail if file already exists*/
/* NOTE: 0x0008u was H5F_ACC_DEBUG, now deprecated */
#define H5F_ACC_CREAT    (H5CHECK H5OPEN 0x0010u)    /*create non-existing files  */
#define H5F_ACC_SWMR_WRITE    (H5CHECK 0x0020u) /*indicate that this file is
                                                 * open for writing in a
                                                 * single-writer/multi-reader (SWMR)
                                                 * scenario.  Note that the
                                                 * process(es) opening the file
                                                 * for reading must open the file
                                                 * with RDONLY access, and use
                                                 * the special "SWMR_READ" access
                                                 * flag. */
#define H5F_ACC_SWMR_READ    (H5CHECK 0x0040u) /*indicate that this file is
                                                 * open for reading in a
                                                 * single-writer/multi-reader (SWMR)
                                                 * scenario.  Note that the
                                                 * process(es) opening the file
                                                 * for SWMR reading must also
                                                 * open the file with the RDONLY
                                                 * flag.  */

/* Value passed to H5Pset_elink_acc_flags to cause flags to be taken from the
 * parent file. */
#define H5F_ACC_DEFAULT (H5CHECK H5OPEN 0xffffu)    /*ignore setting on lapl     */

/* Flags for H5Fget_obj_count() & H5Fget_obj_ids() calls */
#define H5F_OBJ_FILE    (0x0001u)       /* File objects */
#define H5F_OBJ_DATASET    (0x0002u)       /* Dataset objects */
#define H5F_OBJ_GROUP    (0x0004u)       /* Group objects */
#define H5F_OBJ_DATATYPE (0x0008u)      /* Named datatype objects */
#define H5F_OBJ_ATTR    (0x0010u)       /* Attribute objects */
#define H5F_OBJ_ALL     (H5F_OBJ_FILE|H5F_OBJ_DATASET|H5F_OBJ_GROUP|H5F_OBJ_DATATYPE|H5F_OBJ_ATTR)
#define H5F_OBJ_LOCAL   (0x0020u)       /* Restrict search to objects opened through current file ID */
                                        /* (as opposed to objects opened through any file ID accessing this file) */

#define H5F_FAMILY_DEFAULT (hsize_t)0

#ifdef H5_HAVE_PARALLEL
/*
 * Use this constant string as the MPI_Info key to set H5Fmpio debug flags.
 * To turn on H5Fmpio debug flags, set the MPI_Info value with this key to
 * have the value of a string consisting of the characters that turn on the
 * desired flags.
 */
#define H5F_MPIO_DEBUG_KEY "H5F_mpio_debug_key"
#endif /* H5_HAVE_PARALLEL */

/**
 * The difference between a single file and a set of mounted files
 */
typedef enum H5F_scope_t {
    H5F_SCOPE_LOCAL    = 0,    /**< specified file handle only        */
    H5F_SCOPE_GLOBAL   = 1     /**< entire virtual file            */
} H5F_scope_t;

/** Unlimited file size for H5Pset_external() */
#define H5F_UNLIMITED    ((hsize_t)(-1L))

/**
 * How does file close behave?
 */
typedef enum H5F_close_degree_t {
    H5F_CLOSE_DEFAULT   = 0, /**< Use the degree pre-defined by underlining VFL */
    H5F_CLOSE_WEAK      = 1, /**< File closes only after all opened objects are closed */
    H5F_CLOSE_SEMI      = 2, /**< If no opened objects, file is close; otherwise, file close fails */
    H5F_CLOSE_STRONG    = 3  /**< If there are opened objects, close them first, then close file */
} H5F_close_degree_t;

/**
 * Current "global" information about file
 */
typedef struct H5F_info2_t {
    struct {
    unsigned    version;    /**< Superblock version # */
    hsize_t        super_size;    /**< Superblock size */
    hsize_t        super_ext_size;    /**< Superblock extension size */
    } super;
    struct {
    unsigned    version;    /**< Version # of file free space management */
    hsize_t        meta_size;    /**< Free space manager metadata size */
    hsize_t        tot_space;    /**< Amount of free space in the file */
    } free;
    struct {
    unsigned    version;    /**< Version # of shared object header info */
    hsize_t        hdr_size;       /**< Shared object header message header size */
    H5_ih_info_t    msgs_info;      /**< Shared object header message index & heap size */
    } sohm;
} H5F_info2_t;

/**
 * Types of allocation requests. The values larger than #H5FD_MEM_DEFAULT
 * should not change other than adding new types to the end. These numbers
 * might appear in files.
 *
 * \internal Please change the log VFD flavors array if you change this
 *           enumeration.
 */
typedef enum H5F_mem_t {
    H5FD_MEM_NOLIST     = -1,   /**< Data should not appear in the free list.
                                 * Must be negative.
                                 */
    H5FD_MEM_DEFAULT    = 0,    /**< Value not yet set.  Can also be the
                                 * datatype set in a larger allocation
                                 * that will be suballocated by the library.
                                 * Must be zero.
                                 */
    H5FD_MEM_SUPER      = 1,    /**< Superblock data */
    H5FD_MEM_BTREE      = 2,    /**< B-tree data */
    H5FD_MEM_DRAW       = 3,    /**< Raw data (content of datasets, etc.) */
    H5FD_MEM_GHEAP      = 4,    /**< Global heap data */
    H5FD_MEM_LHEAP      = 5,    /**< Local heap data */
    H5FD_MEM_OHDR       = 6,    /**< Object header data */

    H5FD_MEM_NTYPES             /**< Sentinel value - must be last */
} H5F_mem_t;

/**
 * Free space section information
 */
typedef struct H5F_sect_info_t {
    haddr_t             addr;   /**< Address of free space section */
    hsize_t             size;   /**< Size of free space section */
} H5F_sect_info_t;

/**
 * Library's format versions
 */
typedef enum H5F_libver_t {
    H5F_LIBVER_ERROR = -1,
    H5F_LIBVER_EARLIEST = 0,    /**< Use the earliest possible format for storing objects */
    H5F_LIBVER_V18 = 1,         /**< Use the latest v18 format for storing objects */
    H5F_LIBVER_V110 = 2,        /**< Use the latest v110 format for storing objects */
    H5F_LIBVER_V112 = 3,        /**< Use the latest v112 format for storing objects */
    H5F_LIBVER_V114 = 4,        /**< Use the latest v114 format for storing objects */
    H5F_LIBVER_NBOUNDS
} H5F_libver_t;

#define H5F_LIBVER_LATEST   H5F_LIBVER_V114

/**
 * File space handling strategy
 */
typedef enum H5F_fspace_strategy_t {
    H5F_FSPACE_STRATEGY_FSM_AGGR = 0,   /**< Mechanisms: free-space managers, aggregators, and virtual file drivers */
                                        /* This is the library default when not set */
    H5F_FSPACE_STRATEGY_PAGE = 1,   /**< Mechanisms: free-space managers with embedded paged aggregation and virtual file drivers */
    H5F_FSPACE_STRATEGY_AGGR = 2,   /**< Mechanisms: aggregators and virtual file drivers */
    H5F_FSPACE_STRATEGY_NONE = 3,   /**< Mechanisms: virtual file drivers */
    H5F_FSPACE_STRATEGY_NTYPES      /* must be last */
} H5F_fspace_strategy_t;

/* Deprecated: File space handling strategy for release 1.10.0 */
/* They are mapped to H5F_fspace_strategy_t as defined above from release 1.10.1 onwards */
typedef enum H5F_file_space_type_t {
    H5F_FILE_SPACE_DEFAULT = 0,     /* Default (or current) free space strategy setting */
    H5F_FILE_SPACE_ALL_PERSIST = 1, /* Persistent free space managers, aggregators, virtual file driver */
    H5F_FILE_SPACE_ALL = 2,        /* Non-persistent free space managers, aggregators, virtual file driver */
                    /* This is the library default */
    H5F_FILE_SPACE_AGGR_VFD = 3,    /* Aggregators, Virtual file driver */
    H5F_FILE_SPACE_VFD = 4,        /* Virtual file driver */
    H5F_FILE_SPACE_NTYPES        /* must be last */
} H5F_file_space_type_t;

/* Data structure to report the collection of read retries for metadata items with checksum */
/* Used by public routine H5Fget_metadata_read_retry_info() */
#define H5F_NUM_METADATA_READ_RETRY_TYPES    21
typedef struct H5F_retry_info_t {
    unsigned nbins;
    uint32_t *retries[H5F_NUM_METADATA_READ_RETRY_TYPES];
} H5F_retry_info_t;

/**
 * Callback for H5Pset_object_flush_cb() in a file access property list
 */
typedef herr_t (*H5F_flush_cb_t)(hid_t object_id, void *udata);

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

H5_DLL htri_t H5Fis_accessible(const char *container_name, hid_t fapl_id);
/**
 * \example H5Fcreate.c
 *          After creating an HDF5 file with H5Fcreate(), we close it with
 *          H5Fclose().
 */
/**
 *-------------------------------------------------------------------------
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
 * \include H5Fcreate.c
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
 * \version 1.8.10 Removed #H5F_ACC_RDWR_F and #H5F_ACC_RDONLY_F from comments
 *                 for access_flag field in Fortran subroutine, and changed
 *                 “Possible values” to “Valid values”.
 * \version 1.4.0 Fortran API introduced in this release.
 * \version 1.0.0 C function introduced in this release.
 *
 * \see H5Fopen(), H5Fclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t  H5Fcreate(const char *filename, unsigned flags,
                        hid_t create_plist, hid_t access_plist);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Opens an existing HDF5 file
 *
 * \param[in] filename Name of the file to be opened
 * \param[in] flags    File access flags. Allowable values are:
 *                     - #H5F_ACC_RDWR: Allows read and write access to file
 *                     - #H5F_ACC_RDONLY: Allows read-only access to file
 *                     - #H5F_ACC_RDWR \c | #H5F_ACC_SWMR_WRITE: Indicates that
 *                       the file is open for writing in a
 *                       single-writer/multi-writer (SWMR) scenario.
 *                     - #H5F_ACC_RDONLY \c | #H5F_ACC_SWMR_READ:  Indicates
 *                       that the file is open for reading in a
 *                       single-writer/multi-reader (SWMR) scenario.
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
 * \version 1.10.0 The #H5F_ACC_SWMR_WRITE and #H5F_ACC_SWMR_READ flags were added.
 *
 * \see H5Fclose()
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t  H5Fopen(const char *filename, unsigned flags, hid_t access_plist);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Returns a new identifier for a previously-opened HDF5 file
 *
 * \param[in] file_id Identifier of a file for which an additional identifier
 *                    is required
 *
 * \return \hid_t
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
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t  H5Freopen(hid_t file_id);
/**
 *-------------------------------------------------------------------------
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
 * \attention HDF5 does not possess full control over buffering. H5Fflush()
 *            flushes the internal HDF5 buffers then asks the operating system
 *            (the OS) to flush the system buffers for the open files. After
 *            that, the OS is responsible for ensuring that the data is
 *            actually flushed to disk.
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Fflush(hid_t object_id, H5F_scope_t scope);
/**
 * \example H5Fclose.c
 *          After creating an HDF5 file with H5Fcreate(), we close it with
 *          H5Fclose().
 */
/**
 *-------------------------------------------------------------------------
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
 *          Use H5Fclose() as shown in the following example:
 * \include H5Fclose.c
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
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Fclose(hid_t file_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Deletes an HDF5 file
 *
 * \param[in] filename Name of the file to delete
 * \fapl_id
 *
 * \return \herr_t
 *
 * \details H5Fdelete() deletes an HDF5 file \p filename with a file access
 *          property list \p fapl_id. The \p fapl_id should be configured with
 *          the same VOL connector or VFD that was used to open the file.
 *
 *          This API was introduced for use with the Virtual Object Layer
 *          (VOL). With the VOL, HDF5 "files" can map to arbitrary storage
 *          schemes such as object stores and relational database tables. The
 *          data created by these implementations may be inconvenient for a
 *          user to remove without a detailed knowledge of the storage scheme.
 *          H5Fdelete() gives VOL connector authors the ability to add
 *          connector-specific delete code to their connectors so that users
 *          can remove these "files" without detailed knowledge of the storage
 *          scheme.
 *
 *          For a VOL connector, H5Fdelete() deletes the file in a way that
 *          makes sense for the specified VOL connector.
 *
 *          For the native HDF5 connector, HDF5 files will be deleted via the
 *          VFDs, each of which will have to be modified to delete the files it
 *          creates.
 *
 *          For all implementations, H5Fdelete() will first check if the file
 *          is an HDF5 file via H5Fis_accessible(). This is done to ensure that
 *          H5Fdelete() cannot be used as an arbitrary file deletion call.
 *
 * \since 1.12.0
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Fdelete(const char *filename, hid_t fapl_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Returns a file creation property list identifier
 *
 * \file_id
 * \return \hid_t
 *
 * \details H5Fget_create_plist() returns the file creation property list
 *          identifier identifying the creation properties used to create this
 *          file. This function is useful for duplicating properties when
 *          creating another file.
 *
 *          See "File Access Properties" in H5P: Property List Interface in
 *          this reference manual and "File Access Properties" in The HDF5 File
 *          chapter in the HDF5 User’s Guide for more information.
 *
 *          The creation property list identifier should be released with
 *          H5Pclose().
 *
 * \todo Fix the references.
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t  H5Fget_create_plist(hid_t file_id);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Returns a file access property list identifier
 *
 * \file_id
 * \return \hid_t
 *
 * \details H5Fget_access_plist() returns the file access property list
 *          identifier of the specified file.
 *
 *          See "File Access Properties" in H5P: Property List Interface in
 *          this reference manual and "File Access Properties" in The HDF5 File
 *          chapter in the HDF5 User’s Guide for more information.
 *
 * \todo Fix the references.
 *
 *-------------------------------------------------------------------------
 */
H5_DLL hid_t  H5Fget_access_plist(hid_t file_id);
H5_DLL herr_t H5Fget_intent(hid_t file_id, unsigned *intent);
H5_DLL herr_t H5Fget_fileno(hid_t file_id, unsigned long *fileno);
H5_DLL ssize_t H5Fget_obj_count(hid_t file_id, unsigned types);
H5_DLL ssize_t H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *obj_id_list);
H5_DLL herr_t H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle);
H5_DLL herr_t H5Fmount(hid_t loc, const char *name, hid_t child, hid_t plist);
H5_DLL herr_t H5Funmount(hid_t loc, const char *name);
H5_DLL hssize_t H5Fget_freespace(hid_t file_id);
H5_DLL herr_t H5Fget_filesize(hid_t file_id, hsize_t *size);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Retrieves the file's end-of-allocation (EOA)
 *
 * \file_id
 * \param[out] eoa The file's EOA
 *
 * \return \herr_t
 *
 * \details H5Fget_eoa() retrieves the file's EOA and returns it in the
 *          parameter eoa.
 *
 * \since 1.10.2
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Fget_eoa(hid_t file_id, haddr_t *eoa);
H5_DLL herr_t H5Fincrement_filesize(hid_t file_id, hsize_t increment);
H5_DLL ssize_t H5Fget_file_image(hid_t file_id, void * buf_ptr, size_t buf_len);
H5_DLL herr_t H5Fget_mdc_config(hid_t file_id,
                H5AC_cache_config_t * config_ptr);
H5_DLL herr_t H5Fset_mdc_config(hid_t file_id,
                H5AC_cache_config_t * config_ptr);
H5_DLL herr_t H5Fget_mdc_hit_rate(hid_t file_id, double * hit_rate_ptr);
H5_DLL herr_t H5Fget_mdc_size(hid_t file_id,
                              size_t * max_size_ptr,
                              size_t * min_clean_size_ptr,
                              size_t * cur_size_ptr,
                              int * cur_num_entries_ptr);
H5_DLL herr_t H5Freset_mdc_hit_rate_stats(hid_t file_id);
H5_DLL ssize_t H5Fget_name(hid_t obj_id, char *name, size_t size);
H5_DLL herr_t H5Fget_info2(hid_t obj_id, H5F_info2_t *finfo);
H5_DLL herr_t H5Fget_metadata_read_retry_info(hid_t file_id, H5F_retry_info_t *info);
H5_DLL herr_t H5Fstart_swmr_write(hid_t file_id);
H5_DLL ssize_t H5Fget_free_sections(hid_t file_id, H5F_mem_t type,
    size_t nsects, H5F_sect_info_t *sect_info/*out*/);
/**
 *-------------------------------------------------------------------------
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
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Fclear_elink_file_cache(hid_t file_id);
H5_DLL herr_t H5Fset_libver_bounds(hid_t file_id, H5F_libver_t low, H5F_libver_t high);
H5_DLL herr_t H5Fstart_mdc_logging(hid_t file_id);
H5_DLL herr_t H5Fstop_mdc_logging(hid_t file_id);
H5_DLL herr_t H5Fget_mdc_logging_status(hid_t file_id,
                                        /*OUT*/ hbool_t *is_enabled,
                                        /*OUT*/ hbool_t *is_currently_logging);
H5_DLL herr_t H5Fformat_convert(hid_t fid);
H5_DLL herr_t H5Freset_page_buffering_stats(hid_t file_id);
H5_DLL herr_t H5Fget_page_buffering_stats(hid_t file_id, unsigned accesses[2],
    unsigned hits[2], unsigned misses[2], unsigned evictions[2], unsigned bypasses[2]);
H5_DLL herr_t H5Fget_mdc_image_info(hid_t file_id, haddr_t *image_addr, hsize_t *image_size);
/**
 *-------------------------------------------------------------------------
 * \ingroup H5F
 *
 * \brief Retrieves the setting for whether or not a file will create minimized
 *        dataset object headers
 *
 * \file_id
 * \param[out] minimize Flag indicating whether the library will or will not
 *                      create minimized dataset object headers
 *
 * \return \herr_t
 *
 * \details H5Fget_dset_no_attrs_hint() retrieves the no dataset attributes
 *          hint setting for the file specified by the file identifier \p
 *          file_id. This setting is used to inform the library to create
 *          minimized dataset object headers when #TRUE.
 *
 *          The setting's value is returned in the boolean pointer minimize.
 *
 * \since 1.10.5
 *
 *-------------------------------------------------------------------------
 */
H5_DLL herr_t H5Fget_dset_no_attrs_hint(hid_t file_id, hbool_t *minimize);
H5_DLL herr_t H5Fset_dset_no_attrs_hint(hid_t file_id, hbool_t minimize);

#ifdef H5_HAVE_PARALLEL
H5_DLL herr_t H5Fset_mpi_atomicity(hid_t file_id, hbool_t flag);
H5_DLL herr_t H5Fget_mpi_atomicity(hid_t file_id, hbool_t *flag);
#endif /* H5_HAVE_PARALLEL */

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */
#define H5F_ACC_DEBUG    (H5CHECK H5OPEN 0x0000u)    /*print debug info (deprecated)*/

/* Typedefs */

/* Current "global" information about file */
typedef struct H5F_info1_t {
    hsize_t        super_ext_size;    /* Superblock extension size */
    struct {
    hsize_t        hdr_size;       /* Shared object header message header size */
    H5_ih_info_t    msgs_info;      /* Shared object header message index & heap size */
    } sohm;
} H5F_info1_t;


/* Function prototypes */
H5_DLL herr_t H5Fget_info1(hid_t obj_id, H5F_info1_t *finfo);
H5_DLL herr_t H5Fset_latest_format(hid_t file_id, hbool_t latest_format);
H5_DLL htri_t H5Fis_hdf5(const char *filename);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Fpublic_H */
