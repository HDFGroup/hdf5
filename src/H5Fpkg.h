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

/*
 * Programmer:	Quincey Koziol
 *		Thursday, September 28, 2000
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5F package.  Source files outside the H5F package should
 *		include H5Fprivate.h instead.
 */
#if !(defined H5F_FRIEND || defined H5F_MODULE)
#error "Do not include this file outside the H5F package!"
#endif

#ifndef H5Fpkg_H
#define H5Fpkg_H

/* Get package's private header */
#include "H5Fprivate.h"

/* Other private headers needed by this file */
#include "H5private.h"   /* Generic Functions                        */
#include "H5ACprivate.h" /* Metadata cache                           */
#include "H5Bprivate.h"  /* B-trees                                  */
#include "H5FDprivate.h" /* File drivers                             */
#include "H5FLprivate.h" /* Free Lists                               */
#include "H5FOprivate.h" /* File objects                             */
#include "H5FSprivate.h" /* File free space                          */
#include "H5Gprivate.h"  /* Groups                                   */
#include "H5Oprivate.h"  /* Object header messages                   */
#include "H5PBprivate.h" /* Page buffer                              */
#include "H5UCprivate.h" /* Reference counted object functions       */

/*
 * Feature: Define this constant on the compiler command-line if you want to
 *	    see some debugging messages on the debug stream.
 */
#ifdef NDEBUG
#undef H5F_DEBUG
#endif

/* Superblock status flags */
#define H5F_SUPER_WRITE_ACCESS      0x01
#define H5F_SUPER_FILE_OK           0x02
#define H5F_SUPER_SWMR_WRITE_ACCESS 0x04
#define H5F_SUPER_ALL_FLAGS         (H5F_SUPER_WRITE_ACCESS | H5F_SUPER_FILE_OK | H5F_SUPER_SWMR_WRITE_ACCESS)

/* Mask for removing private file access flags */
#define H5F_ACC_PUBLIC_FLAGS 0x007fu

/* Free space section+aggregator merge flags */
#define H5F_FS_MERGE_METADATA 0x01 /* Section can merge with metadata aggregator */
#define H5F_FS_MERGE_RAWDATA  0x02 /* Section can merge with small 'raw' data aggregator */

/* Macro to abstract checking whether file is using a free space manager */
#define H5F_HAVE_FREE_SPACE_MANAGER(F)                                                                       \
    ((F)->shared->fs_strategy == H5F_FSPACE_STRATEGY_FSM_AGGR ||                                             \
     (F)->shared->fs_strategy == H5F_FSPACE_STRATEGY_PAGE)

/* Macros for encoding/decoding superblock */
#define H5F_MAX_DRVINFOBLOCK_SIZE 1024 /* Maximum size of superblock driver info buffer */
#define H5F_DRVINFOBLOCK_HDR_SIZE 16   /* Size of superblock driver info header */

/* Superblock sizes for various versions */
#define H5F_SIZEOF_CHKSUM 4 /* Checksum size in the file */

/* Fixed-size portion at the beginning of all superblocks */
#define H5F_SUPERBLOCK_FIXED_SIZE (H5F_SIGNATURE_LEN + 1) /* superblock version */

/* The H5F_SUPERBLOCK_MINIMAL_VARLEN_SIZE is the minimal amount of super block
 * variable length data guarnateed to load the sizeof offsets and the sizeof
 * lengths fields in all versions of the superblock.
 *
 * This is necessary in the V3 cache, as on the initial load, we need to
 * get enough of the superblock to determine its version and size so that
 * the metadata cache can load the correct amount of data from file to
 * allow the second deserialization attempt to succeed.
 *
 * The value selected will have to be revisited for each new version
 * of the super block.  Note that the current value is one byte larger
 * than it needs to be.
 */
#define H5F_SUPERBLOCK_MINIMAL_VARLEN_SIZE 7

/* Macros for computing variable-size superblock size */
#define H5F_SUPERBLOCK_VARLEN_SIZE_COMMON                                                                    \
    (2    /* freespace, and root group versions */                                                           \
     + 1  /* reserved */                                                                                     \
     + 3  /* shared header vers, size of address, size of lengths */                                         \
     + 1  /* reserved */                                                                                     \
     + 4  /* group leaf k, group internal k */                                                               \
     + 4) /* consistency flags */
#define H5F_SUPERBLOCK_VARLEN_SIZE_V0(sizeof_addr, sizeof_size)                                              \
    (H5F_SUPERBLOCK_VARLEN_SIZE_COMMON             /* Common variable-length info */                         \
     + (sizeof_addr)                               /* base address */                                        \
     + (sizeof_addr)                               /* <unused> */                                            \
     + (sizeof_addr)                               /* EOF address */                                         \
     + (sizeof_addr)                               /* driver block address */                                \
     + H5G_SIZEOF_ENTRY(sizeof_addr, sizeof_size)) /* root group ptr */
#define H5F_SUPERBLOCK_VARLEN_SIZE_V1(sizeof_addr, sizeof_size)                                              \
    (H5F_SUPERBLOCK_VARLEN_SIZE_COMMON             /* Common variable-length info */                         \
     + 2                                           /* indexed B-tree internal k */                           \
     + 2                                           /* reserved */                                            \
     + (sizeof_addr)                               /* base address */                                        \
     + (sizeof_addr)                               /* <unused> */                                            \
     + (sizeof_addr)                               /* EOF address */                                         \
     + (sizeof_addr)                               /* driver block address */                                \
     + H5G_SIZEOF_ENTRY(sizeof_addr, sizeof_size)) /* root group ptr */
#define H5F_SUPERBLOCK_VARLEN_SIZE_V2(sizeof_addr)                                                           \
    (2                    /* size of address, size of lengths */                                             \
     + 1                  /* consistency flags */                                                            \
     + (sizeof_addr)      /* base address */                                                                 \
     + (sizeof_addr)      /* superblock extension address */                                                 \
     + (sizeof_addr)      /* EOF address */                                                                  \
     + (sizeof_addr)      /* root group object header address */                                             \
     + H5F_SIZEOF_CHKSUM) /* superblock checksum (keep this last) */
#define H5F_SUPERBLOCK_VARLEN_SIZE(v, sizeof_addr, sizeof_size)                                              \
    ((v == 0 ? H5F_SUPERBLOCK_VARLEN_SIZE_V0(sizeof_addr, sizeof_size) : 0) +                                \
     (v == 1 ? H5F_SUPERBLOCK_VARLEN_SIZE_V1(sizeof_addr, sizeof_size) : 0) +                                \
     (v >= 2 ? H5F_SUPERBLOCK_VARLEN_SIZE_V2(sizeof_addr) : 0))

/* Total size of superblock, depends on superblock version */
#define H5F_SUPERBLOCK_SIZE(s)                                                                               \
    (H5F_SUPERBLOCK_FIXED_SIZE +                                                                             \
     H5F_SUPERBLOCK_VARLEN_SIZE((s)->super_vers, (s)->sizeof_addr, (s)->sizeof_size))

/* For superblock version 0 & 1:
   Offset to the file consistency flags (status_flags) in the superblock (excluding H5F_SUPERBLOCK_FIXED_SIZE)
 */
#define H5F_SUPER_STATUS_OFF_V01                                                                             \
    (unsigned)(2    /* freespace, and root group versions */                                                 \
               + 1  /* reserved */                                                                           \
               + 3  /* shared header vers, size of address, size of lengths */                               \
               + 1  /* reserved */                                                                           \
               + 4) /* group leaf k, group internal k */

#define H5F_SUPER_STATUS_OFF(v) (v >= 2 ? (unsigned)2 : H5F_SUPER_STATUS_OFF_V01)

/* Offset to the file consistency flags (status_flags) in the superblock */
#define H5F_SUPER_STATUS_FLAGS_OFF(v) (H5F_SUPERBLOCK_FIXED_SIZE + H5F_SUPER_STATUS_OFF(v))

/* Size of file consistency flags (status_flags) in the superblock */
#define H5F_SUPER_STATUS_FLAGS_SIZE(v) (v >= 2 ? 1 : 4)

/* Forward declaration external file cache struct used below (defined in
 * H5Fefc.c) */
typedef struct H5F_efc_t H5F_efc_t;

/* Structure for passing 'user data' to superblock cache callbacks */
typedef struct H5F_superblock_cache_ud_t {
    /* IN: */
    H5F_t * f;                          /* Pointer to file */
    hbool_t ignore_drvrinfo;            /* Indicate if the driver info should be ignored */
                                        /* OUT: */
    unsigned sym_leaf_k;                /* Symbol table leaf node's 'K' value */
    unsigned btree_k[H5B_NUM_BTREE_ID]; /* B-tree key values for each type */
    haddr_t  stored_eof;                /* End-of-file in file */
    hbool_t  drvrinfo_removed;          /* Indicate if the driver info was removed */
    unsigned super_vers;                /* Superblock version obtained in get_load_size callback.
                                         * It will be used later in verify_chksum callback
                                         */
} H5F_superblock_cache_ud_t;

/* Structure for passing 'user data' to driver info block cache callbacks */
typedef struct H5F_drvrinfo_cache_ud_t {
    H5F_t * f;           /* Pointer to file */
    haddr_t driver_addr; /* address of driver info block */
} H5F_drvrinfo_cache_ud_t;

/* Structure for metadata & "small [raw] data" block aggregation fields */
struct H5F_blk_aggr_t {
    unsigned long feature_flag; /* Feature flag type */
    hsize_t       alloc_size;   /* Size for allocating new blocks */
    hsize_t       tot_size;     /* Total amount of bytes aggregated into block */
    hsize_t       size;         /* Current size of block left */
    haddr_t       addr;         /* Location of block left */
};

/* Structure for metadata accumulator fields */
typedef struct H5F_meta_accum_t {
    unsigned char *buf;        /* Buffer to hold the accumulated metadata */
    haddr_t        loc;        /* File location (offset) of the accumulated metadata */
    size_t         size;       /* Size of the accumulated metadata buffer used (in bytes) */
    size_t         alloc_size; /* Size of the accumulated metadata buffer allocated (in bytes) */
    size_t         dirty_off;  /* Offset of the dirty region in the accumulator buffer */
    size_t         dirty_len;  /* Length of the dirty region in the accumulator buffer */
    hbool_t        dirty;      /* Flag to indicate that the accumulated metadata is dirty */
} H5F_meta_accum_t;

/* A record of the mount table */
typedef struct H5F_mount_t {
    struct H5G_t *group; /* Mount point group held open		*/
    struct H5F_t *file;  /* File mounted at that point		*/
} H5F_mount_t;

/*
 * The mount table describes what files are attached to (mounted on) the file
 * to which this table belongs.
 */
typedef struct H5F_mtab_t {
    unsigned     nmounts; /* Number of children which are mounted	*/
    unsigned     nalloc;  /* Number of mount slots allocated	*/
    H5F_mount_t *child;   /* An array of mount records		*/
} H5F_mtab_t;

/* Deferred-free record for the shadow file: records a region of bytes in
 * the shadow file to release after max_lag ticks.
 */
typedef struct shadow_defree {
    uint64_t offset;                 // offset of the region in *bytes*
    uint32_t length;                 // length of the region in *bytes*
    uint64_t tick_num;               // tick number when the free was deferred
    TAILQ_ENTRY(shadow_defree) link; // deferred-free queue linkage
} shadow_defree_t;

/* Structure specifically to store superblock. This was originally
 * maintained entirely within H5F_shared_t, but is now extracted
 * here because the superblock is now handled by the cache */
typedef struct H5F_super_t {
    H5AC_info_t cache_info;                /* Cache entry information structure          */
    unsigned    super_vers;                /* Superblock version                         */
    uint8_t     sizeof_addr;               /* Size of addresses in file                  */
    uint8_t     sizeof_size;               /* Size of offsets in file                    */
    uint8_t     status_flags;              /* File status flags                          */
    unsigned    sym_leaf_k;                /* Size of leaves in symbol tables            */
    unsigned    btree_k[H5B_NUM_BTREE_ID]; /* B-tree key values for each type */
    haddr_t     base_addr;                 /* Absolute base address for rel.addrs.       */
                                           /* (superblock for file is at this offset)    */
    haddr_t      ext_addr;                 /* Relative address of superblock extension   */
    haddr_t      driver_addr;              /* File driver information block address      */
    haddr_t      root_addr;                /* Root group address                         */
    H5G_entry_t *root_ent;                 /* Root group symbol table entry              */
} H5F_super_t;

/* Deferred-free record for the lower file: records a region of bytes in
 * the file below the SWMR VFD to release after a delay.
 */
typedef struct lower_defree {
    SIMPLEQ_ENTRY(lower_defree) link; // deferred-free queue linkage
    H5FD_mem_t alloc_type;            // type with which the region was allocated
    haddr_t    addr;                  // start of the region *in bytes*
    hsize_t    size;                  // length of the region *in bytes*
    uint64_t   free_after_tick;       /* the region may be reused on tick
                                       * free_after_tick + 1 at the earliest
                                       */
} lower_defree_t;

/* Queue of deferred-free records (lower_defree_t) for the lower file, sorted
 * head-to-tail in increasing `free_after_tick` order.
 */
typedef SIMPLEQ_HEAD(lower_defree_queue, lower_defree) lower_defree_queue_t;

/* Queue of deferred-free records (shadow_defree_t) for the shadow file, sorted
 * head-to-tail in increasing `tick_num` order.
 */
typedef TAILQ_HEAD(shadow_defree_queue, shadow_defree) shadow_defree_queue_t;

/*
 * Define the structure to store the file information for HDF5 files. One of
 * these structures is allocated per file, not per H5Fopen(). That is, set of
 * H5F_t structs can all point to the same H5F_shared_t struct. The `nrefs'
 * count in this struct indicates the number of H5F_t structs which are
 * pointing to this struct.
 */
struct H5F_shared_t {
    H5FD_t *       lf;             /* Lower level file handle for I/O	*/
    H5F_super_t *  sblock;         /* Pointer to (pinned) superblock for file */
    H5O_drvinfo_t *drvinfo;        /* Pointer to the (pinned) driver info
                                    * cache entry.  This field is only defined
                                    * for older versions of the super block,
                                    * and then only when a driver information
                                    * block is present.  At all other times
                                    * it should be NULL.
                                    */
    hbool_t drvinfo_sb_msg_exists; /* Convenience field used to track
                                    * whether the driver info superblock
                                    * extension message has been created
                                    * yet. This field should be TRUE iff the
                                    * superblock extension exists and contains
                                    * a driver info message.  Under all other
                                    * circumstances, it must be set to FALSE.
                                    */
    unsigned   nrefs;              /* Ref count for times file is opened	*/
    unsigned   flags;              /* Access Permissions for file          */
    H5F_mtab_t mtab;               /* File mount table                     */
    H5F_efc_t *efc;                /* External file cache                  */

    /* Cached values from FCPL/superblock */
    uint8_t       sizeof_addr;   /* Size of addresses in file            */
    uint8_t       sizeof_size;   /* Size of offsets in file              */
    haddr_t       sohm_addr;     /* Relative address of shared object header message table */
    unsigned      sohm_vers;     /* Version of shared message table on disk */
    unsigned      sohm_nindexes; /* Number of shared messages indexes in the table */
    unsigned long feature_flags; /* VFL Driver feature Flags            */
    haddr_t       maxaddr;       /* Maximum address for file             */

    H5PB_t *            page_buf;                    /* The page buffer cache                */
    H5AC_t *            cache;                       /* The object cache	 		*/
    H5AC_cache_config_t mdc_initCacheCfg;            /* initial configuration for the      */
                                                     /* metadata cache.  This structure is   */
                                                     /* fixed at creation time and should    */
                                                     /* not change thereafter.               */
    H5AC_cache_image_config_t mdc_initCacheImageCfg; /* initial configuration for the */
                                                     /* generate metadata cache image on     */
                                                     /* close option.  This structure is     */
                                                     /* fixed at creation time and should    */
                                                     /* not change thereafter.               */
    hbool_t use_mdc_logging;                         /* Set when metadata logging is desired */
    hbool_t start_mdc_log_on_access;                 /* set when mdc logging should  */
                                                     /* begin on file access/create          */
    char *             mdc_log_location;             /* location of mdc log               */
    hid_t              fcpl_id;                      /* File creation property list ID 	*/
    H5F_close_degree_t fc_degree;                    /* File close behavior degree	*/
    hbool_t  evict_on_close; /* If the file's objects should be evicted from the metadata cache on close */
    size_t   rdcc_nslots;    /* Size of raw data chunk cache (slots)	*/
    size_t   rdcc_nbytes;    /* Size of raw data chunk cache	(bytes)	*/
    double   rdcc_w0;        /* Preempt read chunks first? [0.0..1.0]*/
    size_t   sieve_buf_size; /* Size of the data sieve buffer allocated (in bytes) */
    hsize_t  threshold;      /* Threshold for alignment		*/
    hsize_t  alignment;      /* Alignment				*/
    unsigned gc_ref;         /* Garbage-collect references?		*/
    H5F_libver_t         low_bound;         /* The 'low' bound of library format versions */
    H5F_libver_t         high_bound;        /* The 'high' bound of library format versions */
    hbool_t              store_msg_crt_idx; /* Store creation index for object header messages?	*/
    unsigned             ncwfs;             /* Num entries on cwfs list		*/
    struct H5HG_heap_t **cwfs;              /* Global heap cache			*/
    struct H5G_t *       root_grp;          /* Open root group			*/
    H5FO_t *             open_objs;         /* Open objects in file                 */
    H5UC_t *             grp_btree_shared;  /* Ref-counted group B-tree node info   */
    hbool_t              use_file_locking;  /* Whether or not to use file locking */
    hbool_t              closing;           /* File is in the process of being closed */

    /* Cached VOL connector ID & info */
    hid_t               vol_id;   /* ID of VOL connector for the container */
    const H5VL_class_t *vol_cls;  /* Pointer to VOL connector class for the container */
    void *              vol_info; /* Copy of VOL connector info for container */

    /* File space allocation information */
    H5F_fspace_strategy_t fs_strategy;  /* File space handling strategy	*/
    hsize_t               fs_threshold; /* Free space section threshold 	*/
    hbool_t               fs_persist;   /* Free-space persist or not */
    unsigned              fs_version;   /* Free-space version: */
                                        /* It is used to update fsinfo message in the superblock
                                           extension when closing down the free-space managers */
    hbool_t use_tmp_space;              /* Whether temp. file space allocation is allowed */
    haddr_t tmp_addr;                   /* Next address to use for temp. space in the file */
    hbool_t point_of_no_return; /* Flag to indicate that we can't go back and delete a freespace header when
                                   it's used up */

    H5F_fs_state_t fs_state[H5F_MEM_PAGE_NTYPES]; /* State of free space manager for each type */
    haddr_t        fs_addr[H5F_MEM_PAGE_NTYPES];  /* Address of free space manager info for each type */
    H5FS_t *       fs_man[H5F_MEM_PAGE_NTYPES];   /* Free space manager for each file space type */
    hbool_t        null_fsm_addr;                 /* Used by h5clear tool to tell the library  */
                                                  /* to drop free-space to the floor */
    haddr_t eoa_fsm_fsalloc;                      /* eoa after file space allocation */
                                                  /* for self referential FSMs      */
    haddr_t eoa_post_mdci_fsalloc;                /* eoa past file space allocation */
                                                  /* for metadata cache image, or   */
                                                  /* HADDR_UNDEF if no cache image. */

    /* Free-space aggregation info */
    unsigned   fs_aggr_merge[H5FD_MEM_NTYPES]; /* Flags for whether free space can merge with aggregator(s) */
    H5FD_mem_t fs_type_map[H5FD_MEM_NTYPES];   /* Mapping of "real" file space type into tracked type */
    H5F_blk_aggr_t meta_aggr;  /* Metadata aggregation info (if aggregating metadata allocations) */
    H5F_blk_aggr_t sdata_aggr; /* "Small data" aggregation info (if aggregating "small data" allocations) */

    /* Paged aggregation info */
    hsize_t fs_page_size;     /* File space page size */
    size_t  pgend_meta_thres; /* Do not track page end meta section <= this threshold */

    /* Metadata accumulator information */
    H5F_meta_accum_t accum; /* Metadata accumulator info */

    /* Metadata retry info */
    unsigned  read_attempts;        /* The # of reads to try when reading metadata with checksum */
    unsigned  retries_nbins;        /* # of bins for each retries[] */
    uint32_t *retries[H5AC_NTYPES]; /* Track # of read retries for metdata items with checksum */

    /* Object flush info */
    H5F_object_flush_t object_flush;           /* Information for object flush callback */
    hbool_t            crt_dset_min_ohdr_flag; /* flag to minimize created dataset object header */

    /* VFD SWMR */

    /* Configuration info */
    H5F_vfd_swmr_config_t vfd_swmr_config; /* Copy of the VFD SWMR
                                            * configuration from the
                                            * FAPL used to open the file
                                            */
    haddr_t writer_index_offset;           /* Current byte offset of the
                                            * shadow index in the shadow file.
                                            */
    hbool_t vfd_swmr;                      /* The file is opened with VFD
                                            * SWMR configured or not
                                            */
    hbool_t vfd_swmr_writer;               /* This is the VFD SWMR writer or
                                            * not
                                            */
    uint64_t        tick_num;              /* Number of the current tick */
    uint64_t        max_jump_ticks;        /* Max # of jumps in tick number */
    struct timespec end_of_tick;           /* End time of the current tick */

    lower_defree_queue_t lower_defrees; /* Records of lower-file space
                                         * awaiting reclamation.
                                         */
    /* VFD SWMR metadata file index */
    H5FD_vfd_swmr_idx_entry_t *mdf_idx; /* pointer to an array of instance
                                         * of H5FD_vfd_swmr_idx_entry_t of
                                         * length mdf_idx_len.  This array
                                         * is used by the vfd swmr writer
                                         * to assemble the metadata file
                                         * index at the end of each tick,
                                         * and by the vfd swmr readers to
                                         * track changes in the index.
                                         * With one brief exception during
                                         * writer end of tick processing,
                                         * this index will always be sorted
                                         * in increasing HDF5 file page
                                         * offset order.
                                         *
                                         * This field should be NULL unless
                                         * the index is defined.
                                         */
    uint32_t mdf_idx_len;               /* number of entries in the array
                                         * of instances of
                                         * H5FD_vfd_swmr_idx_entry_t pointed
                                         * to by mdf_idx above.  Note that
                                         * not all entries in the index
                                         * need be used.
                                         */
    uint32_t mdf_idx_entries_used;      /* Number of entries in *mdf_idx
                                         * that are in use -- these will
                                         * be contiguous at indicies 0
                                         * through mdf_idx_entries_used - 1.
                                         */

    /* Old VFD SWMMR metadata file index.  These fields are used only
     * by the VFD SWMR reader to store the previous version of the
     * metadata file index so that it can be compared with the current
     * version to identify page buffer and metadata cache entries that
     * must be evicted or refreshed to avoid message from the past bugs.
     */
    H5FD_vfd_swmr_idx_entry_t *old_mdf_idx;
    uint32_t                   old_mdf_idx_len;
    uint32_t                   old_mdf_idx_entries_used;

    /* Metadata file and updater file for VFD SWMR writer */
    int vfd_swmr_md_fd;      /* POSIX: file descriptor for the
                              * metadata file or -1 if the metadata file 
                              * is not currently open.
                              * The vfd_swmr_config.generate_updater_files
                              * is FALSE.
                              */
                             /* NFS: 
                              * The vfd_swmr_config.generate_updater_files
                              * is TRUE and:
                              * --if vfd_swmr_config.writer is FALSE,
                              * this field is the file descriptor of the local
                              * copy of the metadata file, or -1 if the local
                              * copy is not currently open.
                              * --if vfd_swmr_config.writer is TRUE, this field
                              * is not used and is set to -1.
                              */
    H5F_generate_md_ck_t generate_md_ck_cb;
                             /* For testing only:
                              * Invoke the user-defined callback if exists to 
                              * generate checksum for the metadata file
                              */
    
    haddr_t vfd_swmr_md_eoa; /* POSIX: eoa for the metadata
                              * file
                              */
    uint64_t updater_seq_num;/* Sequence number of the next updater file to be
                              * genereated.  This field must be initialized to zero,
                              * and incremented after each updater file is generated.
                              */

    /* Free space manager for the metadata file */
    H5FS_t *       fs_man_md;   /* Free-space manager */
    H5F_fs_state_t fs_state_md; /* State of the free space
                                 * manager
                                 */
    /* Log file for VFD SWMR */
    FILE *vfd_swmr_log_file_ptr;        /* File pointer for the
                                         * log file.
                                         */
    hbool_t vfd_swmr_log_on;            /* flag to indicate if
                                         * the log file is
                                         * created. */
    H5_timer_t vfd_swmr_log_start_time; /* The starting time for
                                         * calculating the time
                                         * stamp of a log message.
                                         */
    /* Delayed free space release doubly linked list */
    shadow_defree_queue_t shadow_defrees;

    char *extpath; /* Path for searching target external link file                 */

#ifdef H5_HAVE_PARALLEL
    H5P_coll_md_read_flag_t coll_md_read;  /* Do all metadata reads collectively */
    hbool_t                 coll_md_write; /* Do all metadata writes collectively */
#endif                                     /* H5_HAVE_PARALLEL */
};

/*
 * This is the top-level file descriptor.  One of these structures is
 * allocated every time H5Fopen() is called although they may contain pointers
 * to shared H5F_shared_t structs.
 */
struct H5F_t {
    char *         open_name;   /* Name used to open file                                       */
    char *         actual_name; /* Actual name of the file, after resolving symlinks, etc.      */
    H5F_shared_t * shared;      /* The shared file info                                         */
    H5VL_object_t *vol_obj;     /* VOL object                                                   */
    unsigned       nopen_objs;  /* Number of open object headers                                */
    H5FO_t *       obj_count;   /* # of time each object is opened through top file structure   */
    hbool_t        id_exists;   /* Whether an ID for this struct exists                         */
    hbool_t        closing;     /* File is in the process of being closed                       */
    struct H5F_t * parent;      /* Parent file that this file is mounted to                     */
    unsigned       nmounts;     /* Number of children mounted to this file                      */
};

/*****************************/
/* Package Private Variables */
/*****************************/

/* Declare a free list to manage the H5F_t struct */
H5FL_EXTERN(H5F_t);

/* Declare a free list to manage the H5F_shared_t struct */
H5FL_EXTERN(H5F_shared_t);

/* Whether or not to use file locking (based on the environment variable)
 * FAIL means ignore the environment variable.
 */
H5_DLLVAR htri_t use_locks_env_g;

/******************************/
/* Package Private Prototypes */
/******************************/

/* General routines */
H5_DLL herr_t H5F__post_open(H5F_t *f);
H5_DLL H5F_t *H5F__reopen(H5F_t *f);
H5_DLL herr_t H5F__flush(H5F_t *f);
H5_DLL htri_t H5F__is_hdf5(const char *name, hid_t fapl_id);
H5_DLL herr_t H5F__get_file_image(H5F_t *f, void *buf_ptr, size_t buf_len, size_t *image_len);
H5_DLL herr_t H5F__get_info(H5F_t *f, H5F_info2_t *finfo);
H5_DLL herr_t H5F__format_convert(H5F_t *f);
H5_DLL herr_t H5F__start_swmr_write(H5F_t *f);
H5_DLL herr_t H5F__close(H5F_t *f);
H5_DLL herr_t H5F__set_libver_bounds(H5F_t *f, H5F_libver_t low, H5F_libver_t high);
H5_DLL herr_t H5F__get_cont_info(const H5F_t *f, H5VL_file_cont_info_t *info);
H5_DLL herr_t H5F__parse_file_lock_env_var(htri_t *use_locks);
H5_DLL herr_t H5F__delete(const char *filename, hid_t fapl_id);
H5_DLL herr_t H5F__vfd_swmr_end_tick(H5F_t *f);
H5_DLL herr_t H5F__vfd_swmr_disable_end_of_tick(H5F_t *f);
H5_DLL herr_t H5F__vfd_swmr_enable_end_of_tick(H5F_t *f);

/* File mount related routines */
H5_DLL herr_t H5F__close_mounts(H5F_t *f);
H5_DLL herr_t H5F__mount_count_ids(H5F_t *f, unsigned *nopen_files, unsigned *nopen_objs);

/* Superblock related routines */
H5_DLL herr_t H5F__super_init(H5F_t *f);
H5_DLL herr_t H5F__super_read(H5F_t *f, H5P_genplist_t *fa_plist, hbool_t initial_read);
H5_DLL herr_t H5F__super_size(H5F_t *f, hsize_t *super_size, hsize_t *super_ext_size);
H5_DLL herr_t H5F__super_free(H5F_super_t *sblock);

/* Superblock extension related routines */
H5_DLL herr_t H5F__super_ext_open(H5F_t *f, haddr_t ext_addr, H5O_loc_t *ext_ptr);
H5_DLL herr_t H5F__super_ext_write_msg(H5F_t *f, unsigned id, void *mesg, hbool_t may_create,
                                       unsigned mesg_flags);
H5_DLL herr_t H5F__super_ext_remove_msg(H5F_t *f, unsigned id);
H5_DLL herr_t H5F__super_ext_close(H5F_t *f, H5O_loc_t *ext_ptr, hbool_t was_created);

/* Metadata accumulator routines */
H5_DLL herr_t H5F__accum_read(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t addr, size_t size, void *buf);
H5_DLL herr_t H5F__accum_write(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t addr, size_t size,
                               const void *buf);
H5_DLL herr_t H5F__accum_free(H5F_shared_t *f, H5FD_mem_t type, haddr_t addr, hsize_t size);
H5_DLL herr_t H5F__accum_flush(H5F_shared_t *f_sh);
H5_DLL herr_t H5F__accum_reset(H5F_shared_t *f_sh, hbool_t flush);

/* Shared file list related routines */
H5_DLL herr_t H5F__sfile_add(H5F_shared_t *shared);
H5_DLL H5F_shared_t *H5F__sfile_search(H5FD_t *lf);
H5_DLL herr_t        H5F__sfile_remove(H5F_shared_t *shared);

/* Parallel I/O (i.e. MPI) related routines */
#ifdef H5_HAVE_PARALLEL
H5_DLL herr_t H5F__get_mpi_atomicity(const H5F_t *file, hbool_t *flag);
H5_DLL herr_t H5F__set_mpi_atomicity(H5F_t *file, hbool_t flag);
#endif /* H5_HAVE_PARALLEL */

/* External file cache routines */
H5_DLL H5F_efc_t *H5F__efc_create(unsigned max_nfiles);
H5_DLL H5F_t *  H5F__efc_open(H5F_t *parent, const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
H5_DLL unsigned H5F__efc_max_nfiles(H5F_efc_t *efc);
H5_DLL herr_t   H5F__efc_release(H5F_efc_t *efc);
H5_DLL herr_t   H5F__efc_destroy(H5F_efc_t *efc);
H5_DLL herr_t   H5F__efc_try_close(H5F_t *f);

/* Space allocation routines */
H5_DLL haddr_t H5F__alloc(H5F_t *f, H5F_mem_t type, hsize_t size, haddr_t *frag_addr, hsize_t *frag_size);
H5_DLL herr_t  H5F__free(H5F_t *f, H5F_mem_t type, haddr_t addr, hsize_t size);
H5_DLL htri_t  H5F__try_extend(H5F_t *f, H5FD_mem_t type, haddr_t blk_end, hsize_t extra_requested);

/* Functions that get/retrieve values from VFD layer */
H5_DLL herr_t H5F__set_eoa(const H5F_t *f, H5F_mem_t type, haddr_t addr);
H5_DLL herr_t H5F__set_base_addr(const H5F_t *f, haddr_t addr);
H5_DLL herr_t H5F__set_paged_aggr(const H5F_t *f, hbool_t paged);
H5_DLL herr_t H5F__get_max_eof_eoa(const H5F_t *f, haddr_t *max_eof_eoa);

/* Functions that flush or evict */
H5_DLL herr_t H5F__evict_cache_entries(H5F_t *f);

/* Testing functions */
#ifdef H5F_TESTING
H5_DLL herr_t H5F__get_sohm_mesg_count_test(hid_t fid, unsigned type_id, size_t *mesg_count);
H5_DLL herr_t H5F__check_cached_stab_test(hid_t file_id);
H5_DLL herr_t H5F__get_maxaddr_test(hid_t file_id, haddr_t *maxaddr);
H5_DLL herr_t H5F__get_sbe_addr_test(hid_t file_id, haddr_t *sbe_addr);

/* VFD SWMR testing routines */
H5_DLL herr_t H5F__vfd_swmr_writer_create_open_flush_test(hid_t file_id, hbool_t create);
H5_DLL herr_t H5F__vfd_swmr_writer_md_test(hid_t, unsigned, struct H5FD_vfd_swmr_idx_entry_t *, unsigned);

H5_DLL htri_t H5F__same_file_test(hid_t file_id1, hid_t file_id2);
H5_DLL herr_t H5F__reparse_file_lock_variable_test(void);
#endif /* H5F_TESTING */

/* VFD SMWR LOG REPORTING MACROS */

/* H5F_POST_VFD_SWMR_LOG_ENTRY is the macro that can help the developers debug VFD SWMR features.
 * It calls an internal reporting function H5F_post_vfd_swmr_log_entry() that receives
 * a log entry_type_code,  which generates a log tag,  and the message log_info, which
 * the developers want to save into a log file.
 *
 * The macro H5F_POST_VFD_SWMR_LOG_ENTRY_PRODUCTION(f, c, number_entry_production, m) is
 * called by H5F_POST_VFD_SWMR_LOG_ENTRY when the HDF5 library is built with the
 * production mode. Number_entry_production will control the number of entry tags that
 * applications can receive. Currently this number is set to 1 and is subject to change
 * when more tags are useful to be present to applications.
 *
 * The first argument of the macro is the HDF5 file pointer(H5F_t *).
 * Its value needs to be checked to avoid a failure caused by "Low-Level File I/O "
 * in the testhdf5 program, which involves the test of a non-existing HDF5 file.
 */

H5_DLL void H5F_post_vfd_swmr_log_entry(H5F_t *f, int entry_type_code, char *log_info);

#define H5F_POST_VFD_SWMR_LOG_ENTRY_INTERNAL(fp, entry_type_code, log_info)                                  \
    do {                                                                                                     \
        if (fp != NULL) {                                                                                    \
            if (fp->shared != NULL) {                                                                        \
                if (fp->shared->vfd_swmr_log_on == TRUE) {                                                   \
                    H5F_post_vfd_swmr_log_entry(fp, entry_type_code, log_info);                              \
                }                                                                                            \
            }                                                                                                \
        }                                                                                                    \
    } while (0)

#define H5F_POST_VFD_SWMR_LOG_ENTRY_PRODUCTION(fp, entry_type_code, max_code, log_info)                      \
    do {                                                                                                     \
        if (entry_type_code < max_code) {                                                                    \
            H5F_POST_VFD_SWMR_LOG_ENTRY_INTERNAL(fp, entry_type_code, log_info);                             \
        }                                                                                                    \
    } while (0)

/* Note: change H5F_POST_VFD_SWMR_LOG_ENTRY_PRODUCTION(f, c, 1, m) on the following lines to
 *       H5F_POST_VFD_SWMR_LOG_ENTRY_PRODUCTION(f, c, your_number_entry_production, m)
 *       as necessary.
 */
#ifndef NDEBUG
#define H5F_POST_VFD_SWMR_LOG_ENTRY(f, c, m) H5F_POST_VFD_SWMR_LOG_ENTRY_INTERNAL(f, c, m)
#else
#define H5F_POST_VFD_SWMR_LOG_ENTRY(f, c, m) H5F_POST_VFD_SWMR_LOG_ENTRY_PRODUCTION(f, c, 1, m)
#endif

/* Macros for VFD SWMR log entry code
 * Note: this should be consistent with const char *H5Fvfd_swmr_log_tags[] declared at
 * H5Fvfd_swmr.c .
 */
#define EOT_PROCESSING_TIME 0
#define FILE_OPEN           1
#define FILE_CLOSE          2
#define EOT_TRIGGER_TIME    3
#define EOT_META_FILE_INDEX 4
#endif /* H5Fpkg_H */
