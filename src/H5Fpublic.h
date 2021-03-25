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
#define H5CHECK H5check(),
#else /* _H5private_H */
#define H5CHECK
#endif /* _H5private_H */

/* When this header is included from a private HDF5 header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5private_H
#define H5OPEN H5open(),
#else /* _H5private_H */
#define H5OPEN
#endif /* _H5private_H */

/*
 * These are the bits that can be passed to the `flags' argument of
 * H5Fcreate() and H5Fopen(). Use the bit-wise OR operator (|) to combine
 * them as needed.  As a side effect, they call H5check_version() to make sure
 * that the application is compiled with a version of the hdf5 header files
 * which are compatible with the library to which the application is linked.
 * We're assuming that these constants are used rather early in the hdf5
 * session.
 */
#define H5F_ACC_RDONLY (H5CHECK H5OPEN 0x0000u) /*absence of rdwr => rd-only */
#define H5F_ACC_RDWR   (H5CHECK H5OPEN 0x0001u) /*open for read and write    */
#define H5F_ACC_TRUNC  (H5CHECK H5OPEN 0x0002u) /*overwrite existing files   */
#define H5F_ACC_EXCL   (H5CHECK H5OPEN 0x0004u) /*fail if file already exists*/
/* NOTE: 0x0008u was H5F_ACC_DEBUG, now deprecated */
#define H5F_ACC_CREAT (H5CHECK H5OPEN 0x0010u) /*create non-existing files  */
#define H5F_ACC_SWMR_WRITE                                                                                   \
    (H5CHECK 0x0020u) /*indicate that this file is                                                           \
                       * open for writing in a                                                               \
                       * single-writer/multi-reader (SWMR)                                                   \
                       * scenario.  Note that the                                                            \
                       * process(es) opening the file                                                        \
                       * for reading must open the file                                                      \
                       * with RDONLY access, and use                                                         \
                       * the special "SWMR_READ" access                                                      \
                       * flag. */
#define H5F_ACC_SWMR_READ                                                                                    \
    (H5CHECK 0x0040u) /*indicate that this file is                                                           \
                       * open for reading in a                                                               \
                       * single-writer/multi-reader (SWMR)                                                   \
                       * scenario.  Note that the                                                            \
                       * process(es) opening the file                                                        \
                       * for SWMR reading must also                                                          \
                       * open the file with the RDONLY                                                       \
                       * flag.  */

/* Value passed to H5Pset_elink_acc_flags to cause flags to be taken from the
 * parent file. */
#define H5F_ACC_DEFAULT (H5CHECK H5OPEN 0xffffu) /*ignore setting on lapl     */

/* Flags for H5Fget_obj_count() & H5Fget_obj_ids() calls */
#define H5F_OBJ_FILE     (0x0001u) /* File objects */
#define H5F_OBJ_DATASET  (0x0002u) /* Dataset objects */
#define H5F_OBJ_GROUP    (0x0004u) /* Group objects */
#define H5F_OBJ_DATATYPE (0x0008u) /* Named datatype objects */
#define H5F_OBJ_ATTR     (0x0010u) /* Attribute objects */
#define H5F_OBJ_ALL      (H5F_OBJ_FILE | H5F_OBJ_DATASET | H5F_OBJ_GROUP | H5F_OBJ_DATATYPE | H5F_OBJ_ATTR)
#define H5F_OBJ_LOCAL    (0x0020u) /* Restrict search to objects opened through current file ID */
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

/* The difference between a single file and a set of mounted files */
typedef enum H5F_scope_t {
    H5F_SCOPE_LOCAL  = 0, /*specified file handle only        */
    H5F_SCOPE_GLOBAL = 1  /*entire virtual file            */
} H5F_scope_t;

/* Unlimited file size for H5Pset_external() */
#define H5F_UNLIMITED ((hsize_t)(-1L))

/* How does file close behave?
 * H5F_CLOSE_DEFAULT - Use the degree pre-defined by underlining VFL
 * H5F_CLOSE_WEAK    - file closes only after all opened objects are closed
 * H5F_CLOSE_SEMI    - if no opened objects, file is close; otherwise, file
            close fails
 * H5F_CLOSE_STRONG  - if there are opened objects, close them first, then
            close file
 */
typedef enum H5F_close_degree_t {
    H5F_CLOSE_DEFAULT = 0,
    H5F_CLOSE_WEAK    = 1,
    H5F_CLOSE_SEMI    = 2,
    H5F_CLOSE_STRONG  = 3
} H5F_close_degree_t;

/* Current "global" information about file */
typedef struct H5F_info2_t {
    struct {
        unsigned version;        /* Superblock version # */
        hsize_t  super_size;     /* Superblock size */
        hsize_t  super_ext_size; /* Superblock extension size */
    } super;
    struct {
        unsigned version;   /* Version # of file free space management */
        hsize_t  meta_size; /* Free space manager metadata size */
        hsize_t  tot_space; /* Amount of free space in the file */
    } free;
    struct {
        unsigned     version;   /* Version # of shared object header info */
        hsize_t      hdr_size;  /* Shared object header message header size */
        H5_ih_info_t msgs_info; /* Shared object header message index & heap size */
    } sohm;
} H5F_info2_t;

/*
 * Types of allocation requests. The values larger than H5FD_MEM_DEFAULT
 * should not change other than adding new types to the end. These numbers
 * might appear in files.
 *
 * Note: please change the log VFD flavors array if you change this
 * enumeration.
 */
typedef enum H5F_mem_t {
    H5FD_MEM_NOLIST = -1, /* Data should not appear in the free list.
                           * Must be negative.
                           */
    H5FD_MEM_DEFAULT = 0, /* Value not yet set.  Can also be the
                           * datatype set in a larger allocation
                           * that will be suballocated by the library.
                           * Must be zero.
                           */
    H5FD_MEM_SUPER = 1,   /* Superblock data */
    H5FD_MEM_BTREE = 2,   /* B-tree data */
    H5FD_MEM_DRAW  = 3,   /* Raw data (content of datasets, etc.) */
    H5FD_MEM_GHEAP = 4,   /* Global heap data */
    H5FD_MEM_LHEAP = 5,   /* Local heap data */
    H5FD_MEM_OHDR  = 6,   /* Object header data */

    H5FD_MEM_NTYPES /* Sentinel value - must be last */
} H5F_mem_t;

/* Free space section information */
typedef struct H5F_sect_info_t {
    haddr_t addr; /* Address of free space section */
    hsize_t size; /* Size of free space section */
} H5F_sect_info_t;

/* Library's format versions */
typedef enum H5F_libver_t {
    H5F_LIBVER_ERROR    = -1,
    H5F_LIBVER_EARLIEST = 0, /* Use the earliest possible format for storing objects */
    H5F_LIBVER_V18      = 1, /* Use the latest v18 format for storing objects */
    H5F_LIBVER_V110     = 2, /* Use the latest v110 format for storing objects */
    H5F_LIBVER_V112     = 3, /* Use the latest v112 format for storing objects */
    H5F_LIBVER_V114     = 4, /* Use the latest v114 format for storing objects */
    H5F_LIBVER_NBOUNDS
} H5F_libver_t;

#define H5F_LIBVER_LATEST H5F_LIBVER_V114

/* File space handling strategy */
typedef enum H5F_fspace_strategy_t {
    H5F_FSPACE_STRATEGY_FSM_AGGR =
        0, /* Mechanisms: free-space managers, aggregators, and virtual file drivers */
           /* This is the library default when not set */
    H5F_FSPACE_STRATEGY_PAGE =
        1, /* Mechanisms: free-space managers with embedded paged aggregation and virtual file drivers */
    H5F_FSPACE_STRATEGY_AGGR = 2, /* Mechanisms: aggregators and virtual file drivers */
    H5F_FSPACE_STRATEGY_NONE = 3, /* Mechanisms: virtual file drivers */
    H5F_FSPACE_STRATEGY_NTYPES    /* must be last */
} H5F_fspace_strategy_t;

/* Deprecated: File space handling strategy for release 1.10.0 */
/* They are mapped to H5F_fspace_strategy_t as defined above from release 1.10.1 onwards */
typedef enum H5F_file_space_type_t {
    H5F_FILE_SPACE_DEFAULT     = 0, /* Default (or current) free space strategy setting */
    H5F_FILE_SPACE_ALL_PERSIST = 1, /* Persistent free space managers, aggregators, virtual file driver */
    H5F_FILE_SPACE_ALL         = 2, /* Non-persistent free space managers, aggregators, virtual file driver */
                                    /* This is the library default */
    H5F_FILE_SPACE_AGGR_VFD = 3,    /* Aggregators, Virtual file driver */
    H5F_FILE_SPACE_VFD      = 4,    /* Virtual file driver */
    H5F_FILE_SPACE_NTYPES           /* must be last */
} H5F_file_space_type_t;

/* Data structure to report the collection of read retries for metadata items with checksum */
/* Used by public routine H5Fget_metadata_read_retry_info() */
#define H5F_NUM_METADATA_READ_RETRY_TYPES 21
typedef struct H5F_retry_info_t {
    unsigned  nbins;
    uint32_t *retries[H5F_NUM_METADATA_READ_RETRY_TYPES];
} H5F_retry_info_t;

/* Callback for H5Pset_object_flush_cb() in a file access property list */
typedef herr_t (*H5F_flush_cb_t)(hid_t object_id, void *udata);

/* VFD SWMR configuration data used by H5Pset/get_vfd_swmr_config */
#define H5F__CURR_VFD_SWMR_CONFIG_VERSION 1
#define H5F__MAX_VFD_SWMR_FILE_NAME_LEN   1024
#define H5F__MAX_PB_EXPANSION_THRESHOLD   100
/*
 *  struct H5F_vfd_swmr_config_t
 *
 *  Instances of H5F_vfd_swmr_config_t are used by VFD SWMR writers and readers
 *  to pass necessary configuration data to the HDF5 library on file open (or
 *  creation, in the case of writers).
 *
 *  The fields of the structure are discussed below:
 *      version:
 *          An integer field indicating the version of the H5F_vfd_swmr_config
 *          structure used.  This field must always be set to a known version
 *          number.  The most recent version of the structure will always be
 *          H5F__CURR_VFD_SWMR_CONFIG_VERSION.
 *
 *      tick_len:
 *          An integer field containing the length of a tick in tenths of
 *          a second.  If tick_len is zero, end of tick processing may only be
 *          triggered manually via the H5Fvfd_swmr_end_tick() function.
 *
 *      max_lag:
 *          An integer field indicating the maximum expected lag (in ticks)
 *          between the writer and the readers.  This value must be at least 3,
 *          with 10 being the recommended minimum value.
 *
 *      writer:
 *          A boolean flag indicating whether the file opened with this FAPL entry
 *          will be opened R/W. (i.e. as a VFD SWMR writer)
 *
 *      flush_raw_data:
 *          A boolean flag indicating whether raw data should be flushed
 *          as part of the end of tick processing.  If set to TRUE, raw
 *          data will be flushed and thus be consistent with the metadata file.
 *          However, this will also greatly increase end of tick I/O, and will
 *          likely break any real time guarantees unless a very large tick_len
 *          is selected.
 *
 *      md_pages_reserved:
 *          The `md_pages_reserved` parameter tells how many pages to reserve
 *          at the beginning of the shadow file for the shadow-file header
 *          and the shadow index.  The header has an entire page to itself.
 *          The remaining `md_pages_reserved - 1` pages are reserved for the
 *          shadow index.  If the index grows larger than its initial
 *          allocation, then it will move to a new location in the shadow file,
 *          and the initial allocation will be reclaimed.  `md_pages_reserved`
 *          must be at least 2.
 *
 *      pb_expansion_threshold:
 *          An integer field indicating the threshold for the page buffer size.
 *          During a tick, the page buffer must expand as necessary to retain copies
 *          of all modified metadata pages and multi-page metadata entries.
 *          If the page buffer size exceeds this thresold, an early end of tick
 *          will be triggered.
 *          Note that this is not a limit on the maximum page buffer size, as the
 *          metadata cache is flushed as part of end of tick processing.
 *          This threshold must be in the range [0, 100].  If the threshold is 0,
 *          the feature is disabled.  For all other values, the page buffer size is
 *          multiplied by this threshold.  If this value is exceeded, an early end
 *          of tick is triggered.
 *
 *      md_file_path:
 *          POSIX: this field contains the path of the metadata file.
 *          NFS: it contains the path and base name of the metadata file
 *          updater files.
 *          Object store: it contains the base URL for the objects used
 *          to store metadata file updater objects.
 *
 *      log_file_path:
 *          This field contains the path to the log file.  If defined, this path should
 *          be unique to each process.  If this field contains the empty string, a log
 *          file will not be created.
 *
 */
typedef struct H5F_vfd_swmr_config_t {
    int32_t  version;
    uint32_t tick_len;
    uint32_t max_lag;
    hbool_t  writer;
    hbool_t  flush_raw_data;
    uint32_t md_pages_reserved;
    uint32_t pb_expansion_threshold;
    char     md_file_path[H5F__MAX_VFD_SWMR_FILE_NAME_LEN + 1];
    char     log_file_path[H5F__MAX_VFD_SWMR_FILE_NAME_LEN + 1];
} H5F_vfd_swmr_config_t;

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

H5_DLL htri_t   H5Fis_accessible(const char *container_name, hid_t fapl_id);
H5_DLL hid_t    H5Fcreate(const char *filename, unsigned flags, hid_t create_plist, hid_t access_plist);
H5_DLL hid_t    H5Fopen(const char *filename, unsigned flags, hid_t access_plist);
H5_DLL hid_t    H5Freopen(hid_t file_id);
H5_DLL herr_t   H5Fflush(hid_t object_id, H5F_scope_t scope);
H5_DLL herr_t   H5Fclose(hid_t file_id);
H5_DLL herr_t   H5Fdelete(const char *filename, hid_t fapl_id);
H5_DLL hid_t    H5Fget_create_plist(hid_t file_id);
H5_DLL hid_t    H5Fget_access_plist(hid_t file_id);
H5_DLL herr_t   H5Fget_intent(hid_t file_id, unsigned *intent);
H5_DLL herr_t   H5Fget_fileno(hid_t file_id, unsigned long *fileno);
H5_DLL ssize_t  H5Fget_obj_count(hid_t file_id, unsigned types);
H5_DLL ssize_t  H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *obj_id_list);
H5_DLL herr_t   H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle);
H5_DLL herr_t   H5Fmount(hid_t loc, const char *name, hid_t child, hid_t plist);
H5_DLL herr_t   H5Funmount(hid_t loc, const char *name);
H5_DLL hssize_t H5Fget_freespace(hid_t file_id);
H5_DLL herr_t   H5Fget_filesize(hid_t file_id, hsize_t *size);
H5_DLL herr_t   H5Fget_eoa(hid_t file_id, haddr_t *eoa);
H5_DLL herr_t   H5Fincrement_filesize(hid_t file_id, hsize_t increment);
H5_DLL ssize_t  H5Fget_file_image(hid_t file_id, void *buf_ptr, size_t buf_len);
H5_DLL herr_t   H5Fget_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr);
H5_DLL herr_t   H5Fset_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr);
H5_DLL herr_t   H5Fget_mdc_hit_rate(hid_t file_id, double *hit_rate_ptr);
H5_DLL herr_t   H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
                                size_t *cur_size_ptr, int *cur_num_entries_ptr);
H5_DLL herr_t   H5Freset_mdc_hit_rate_stats(hid_t file_id);
H5_DLL ssize_t  H5Fget_name(hid_t obj_id, char *name, size_t size);
H5_DLL herr_t   H5Fget_info2(hid_t obj_id, H5F_info2_t *finfo);
H5_DLL herr_t   H5Fget_metadata_read_retry_info(hid_t file_id, H5F_retry_info_t *info);
H5_DLL herr_t   H5Fstart_swmr_write(hid_t file_id);
H5_DLL ssize_t  H5Fget_free_sections(hid_t file_id, H5F_mem_t type, size_t nsects,
                                     H5F_sect_info_t *sect_info /*out*/);
H5_DLL herr_t   H5Fclear_elink_file_cache(hid_t file_id);
H5_DLL herr_t   H5Fset_libver_bounds(hid_t file_id, H5F_libver_t low, H5F_libver_t high);
H5_DLL herr_t   H5Fstart_mdc_logging(hid_t file_id);
H5_DLL herr_t   H5Fstop_mdc_logging(hid_t file_id);
H5_DLL herr_t   H5Fget_mdc_logging_status(hid_t            file_id,
                                          /*OUT*/ hbool_t *is_enabled,
                                          /*OUT*/ hbool_t *is_currently_logging);
H5_DLL herr_t   H5Fformat_convert(hid_t fid);
H5_DLL herr_t   H5Freset_page_buffering_stats(hid_t file_id);
H5_DLL herr_t   H5Fget_page_buffering_stats(hid_t file_id, unsigned accesses[2], unsigned hits[2],
                                            unsigned misses[2], unsigned evictions[2], unsigned bypasses[2]);
H5_DLL herr_t   H5Fget_mdc_image_info(hid_t file_id, haddr_t *image_addr, hsize_t *image_size);
H5_DLL herr_t   H5Fget_dset_no_attrs_hint(hid_t file_id, hbool_t *minimize);
H5_DLL herr_t   H5Fset_dset_no_attrs_hint(hid_t file_id, hbool_t minimize);

/* VFD SWMR */
H5_DLL herr_t H5Fvfd_swmr_end_tick(hid_t file_id);
H5_DLL herr_t H5Fvfd_swmr_disable_end_of_tick(hid_t file_id);
H5_DLL herr_t H5Fvfd_swmr_enable_end_of_tick(hid_t file_id);
H5_DLL bool   vfd_swmr_writer_may_increase_tick_to(uint64_t, bool);
H5_DLL void   vfd_swmr_reader_did_increase_tick_to(uint64_t);

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
#define H5F_ACC_DEBUG (H5CHECK H5OPEN 0x0000u) /*print debug info (deprecated)*/

/* Typedefs */

/* Current "global" information about file */
typedef struct H5F_info1_t {
    hsize_t super_ext_size; /* Superblock extension size */
    struct {
        hsize_t      hdr_size;  /* Shared object header message header size */
        H5_ih_info_t msgs_info; /* Shared object header message index & heap size */
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
