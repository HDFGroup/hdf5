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
 * Read-Only S3 Virtual File Driver (VFD)
 *
 * Provides read-only access to files hosted on Amazon's S3 service.
 * Relies on "s3comms" utility layer to implement the AWS REST API.
 */

#include "H5FDmodule.h" /* This source code file is part of the H5FD module */

#include "H5private.h" /* Generic Functions        */

#ifdef H5_HAVE_ROS3_VFD

#include "H5Eprivate.h"       /* Error handling           */
#include "H5FDpkg.h"          /* File drivers             */
#include "H5FDros3.h"         /* ros3 file driver         */
#include "H5FDros3_s3comms.h" /* S3 Communications        */
#include "H5FLprivate.h"      /* Free Lists               */
#include "H5Iprivate.h"       /* IDs                      */
#include "H5MMprivate.h"      /* Memory management        */

/* Define to turn on stats collection and reporting */
/* #define ROS3_STATS */

/* The driver identification number, initialized at runtime */
hid_t H5FD_ROS3_id_g = H5I_INVALID_HID;

/* Flag to indicate whether global driver resources & settings have been
 *      initialized.
 */
static bool H5FD_ros3_init_s = false;

/* Session/security token property name */
#define ROS3_TOKEN_PROP_NAME "ros3_token_prop"

/* Endpoint URL property name */
#define ROS3_ENDPOINT_PROP_NAME "ros3_endpoint_prop"

/* I/O block caching parameters property name */
#define ROS3_BLOCK_CACHING_PARAMS_PROP_NAME "ros3_block_caching_params"

/* Default page buffer size */
#define ROS3_DEF_PAGE_BUF_SIZE ((size_t)64 * (size_t)1024 * (size_t)1024)

/* Insert entry at head of LRU linked list */
#define ROS3_BLOCK_CACHE_LRU_INSERT(file_ptr, block)                                                         \
    do {                                                                                                     \
        if (!(file_ptr)->block_cache.LRU_head) {                                                             \
            (file_ptr)->block_cache.LRU_head = (block);                                                      \
            (file_ptr)->block_cache.LRU_tail = (block);                                                      \
        }                                                                                                    \
        else {                                                                                               \
            (file_ptr)->block_cache.LRU_head->prev = (block);                                                \
            (block)->next                          = (file_ptr)->block_cache.LRU_head;                       \
            (file_ptr)->block_cache.LRU_head       = (block);                                                \
        }                                                                                                    \
    } while (0)

#define ROS3_BLOCK_CACHE_LRU_REMOVE(file_ptr, block)                                                         \
    do {                                                                                                     \
        if ((file_ptr)->block_cache.LRU_head == (block)) {                                                   \
            (file_ptr)->block_cache.LRU_head = (block)->next;                                                \
            if ((file_ptr)->block_cache.LRU_head)                                                            \
                (file_ptr)->block_cache.LRU_head->prev = NULL;                                               \
        }                                                                                                    \
        else                                                                                                 \
            (block)->prev->next = (block)->next;                                                             \
                                                                                                             \
        if ((file_ptr)->block_cache.LRU_tail == (block)) {                                                   \
            (file_ptr)->block_cache.LRU_tail = (block)->prev;                                                \
            if ((file_ptr)->block_cache.LRU_tail)                                                            \
                (file_ptr)->block_cache.LRU_tail->next = NULL;                                               \
        }                                                                                                    \
        else                                                                                                 \
            (block)->next->prev = (block)->prev;                                                             \
                                                                                                             \
        (block)->next = (block)->prev = NULL;                                                                \
    } while (0)

#ifdef ROS3_STATS

/* The ros3 VFD can collect some simple I/O stats on a per-file basis. These
 * are stored in arrays of bins (one for data and one for metadata) in the the
 * VFD's file structure. Each bin contains stats for I/O operations of a given
 * I/O size range. The bin boundaries are kept in a global "bin boundaries"
 * array that is initialized at VFD startup and does not change.
 */

/* Number of bins */
#define ROS3_STATS_BIN_COUNT 16

/* Array to hold pre-computed boundaries for stats bins */
static uint64_t ros3_stats_boundaries_g[ROS3_STATS_BIN_COUNT];

/* Structure for storing per-file usage statistics */
typedef struct H5FD_ros3_stats_bin {
    uint64_t count; /* # of reads with size in this bin's range */
    uint64_t bytes; /* Total bytes read in this bin */
    uint64_t min;   /* Smallest read size in this bin */
    uint64_t max;   /* Largest read size in this bin */
} H5FD_ros3_stats_bin_t;

#endif /* ROS3_STATS */

typedef struct H5FD_ros_block_hash_t {
    UT_hash_handle hh; /* Hash table handle */

    haddr_t addr;       /* Block size-aligned file address for block */
    size_t  block_size; /* Size of block data buffer (buf) in bytes */

    /* Fields for LRU eviction linked list */
    struct H5FD_ros_block_hash_t *next;
    struct H5FD_ros_block_hash_t *prev;

    uint8_t buf[]; /* block data buffer; flexible array member */
} H5FD_ros_block_hash_t;

/* Structure for partitioning an I/O request into smaller requests
 * along block size boundaries
 */
typedef struct H5FD_ros3_block_io_req_t {
    haddr_t addr;
    size_t  io_size;
} H5FD_ros3_block_io_req_t;

/* Parameters for I/O block caching */
typedef struct H5FD_ros3_block_caching_params_t {
    size_t block_size;
    size_t block_cache_size;
    bool   lock_superblock;
} H5FD_ros3_block_caching_params_t;

/***************************************************************************
 * Stores all information needed to maintain access to a single HDF5 file
 * that has been stored as a S3 object.
 *
 * pub
 *
 *     Instance of H5FD_t which contains all fields common to all VFDs.
 *     It must be the first item in this structure, since at higher levels,
 *     this structure will be treated as an instance of H5FD_t.
 *
 * fa
 *
 *     Instance of `H5FD_ros3_fapl_t` containing the S3 configuration data
 *     needed to "open" the HDF5 file.
 *
 * eoa
 *
 *     End of addressed space in file. After open, it should always
 *     equal the file size.
 *
 * s3r_handle
 *
 *     Instance of S3 Request handle associated with the target resource.
 *     Responsible for communicating with remote host and presenting file
 *     contents as indistinguishable from a file on the local filesystem.
 *
 * block_cache
 *
 *     Holds fields for implementing a simple I/O block cache used for
 *     optimizing I/O.
 *
 *     hash_table - Pointer to the head node of a uthash hash table that
 *     is used for looking up cached I/O blocks by block size-aligned
 *     addresses.
 *
 *     block_size - The size of each I/O block that is cached. Fixed upon
 *     file open.
 *
 *     max_num_blocks - The maximum number of I/O blocks that can be cached
 *     before blocks need to be evicted to make space for others.
 *
 *     lock_superblock - Boolean indicating whether the block which (usually)
 *     contains the superblock and nearby metadata should be locked in the
 *     cache to prevent its eviction. Note that a large userblock size
 *     could prevent this approach from being effective.
 *
 * *** present only if ROS3_STATS is set to enable stats collection ***
 *
 * `meta` (H5FD_ros3_stats_bin_t[])
 * `raw` (H5FD_ros3_stats_bin_t[])
 *
 *     Arrays of `H5FD_ros3_stats_bin_t` structures to record raw- and
 *     metadata reads.
 *
 *     Records count and size of reads performed by the VFD, and is used to
 *     print formatted usage statistics to stdout upon VFD shutdown.
 *
 *     Reads of each raw- and metadata type are recorded in an individual bin
 *     determined by the size of the read.  The last bin of each type is
 *     reserved for "big" reads, with no defined upper bound.
 *
 * *** end ROS3_STATS ***
 ***************************************************************************/
typedef struct H5FD_ros3_t {
    H5FD_t           pub;
    haddr_t          eoa;
    H5FD_ros3_fapl_t fa;
    s3r_t           *s3r_handle;

    struct {
        H5FD_ros_block_hash_t *hash_table;
        size_t                 block_size;
        size_t                 block_cache_size;
        size_t                 max_num_blocks;
        bool                   lock_superblock;
        H5FD_ros_block_hash_t *LRU_head;
        H5FD_ros_block_hash_t *LRU_tail;
        bool                   disabled;
    } block_cache;

#ifdef ROS3_STATS
    H5FD_ros3_stats_bin_t meta[ROS3_STATS_BIN_COUNT + 1];
    H5FD_ros3_stats_bin_t raw[ROS3_STATS_BIN_COUNT + 1];
#endif
} H5FD_ros3_t;

/* Prototypes */
static herr_t  H5FD__ros3_term(void);
static void   *H5FD__ros3_fapl_get(H5FD_t *_file);
static void   *H5FD__ros3_fapl_copy(const void *_old_fa);
static herr_t  H5FD__ros3_fapl_free(void *_fa);
static H5FD_t *H5FD__ros3_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t  H5FD__ros3_close(H5FD_t *_file);
static int     H5FD__ros3_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t  H5FD__ros3_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD__ros3_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD__ros3_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD__ros3_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD__ros3_get_handle(H5FD_t *_file, hid_t fapl, void **file_handle);
static herr_t  H5FD__ros3_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                               void *buf);
static herr_t  H5FD__ros3_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                                const void *buf);
static herr_t  H5FD__ros3_truncate(H5FD_t *_file, hid_t dxpl_id, bool closing);

static herr_t H5FD__ros3_validate_config(const H5FD_ros3_fapl_t *fa);

static herr_t H5FD__ros3_str_token_copy(const char *name, size_t size, void *_value);
static int    H5FD__ros3_str_token_cmp(const void *_value1, const void *_value2, size_t size);
static herr_t H5FD__ros3_str_token_close(const char *name, size_t size, void *_value);
static herr_t H5FD__ros3_str_token_delete(hid_t prop_id, const char *name, size_t size, void *_value);

static herr_t H5FD__ros3_str_endpoint_copy(const char *name, size_t size, void *_value);
static int    H5FD__ros3_str_endpoint_cmp(const void *_value1, const void *_value2, size_t size);
static herr_t H5FD__ros3_str_endpoint_close(const char *name, size_t size, void *_value);
static herr_t H5FD__ros3_str_endpoint_delete(hid_t prop_id, const char *name, size_t size, void *_value);

static herr_t H5FD__ros3_init_block_cache(H5FD_ros3_t *file);
static herr_t H5FD__ros3_determine_io_reqs(H5FD_ros3_t *file, haddr_t addr, size_t io_size,
                                           H5FD_ros3_block_io_req_t **io_reqs_out, size_t *num_io_reqs_out);
static herr_t H5FD__ros3_block_cache_make_space(H5FD_ros3_t *file);

#ifdef ROS3_STATS
static herr_t H5FD__ros3_reset_stats(H5FD_ros3_t *file);
static herr_t H5FD__ros3_log_read_stats(H5FD_ros3_t *file, H5FD_mem_t type, uint64_t size);
static herr_t H5FD__ros3_print_stats(FILE *stream, const H5FD_ros3_t *file);
#endif

static const H5FD_class_t H5FD_ros3_g = {
    H5FD_CLASS_VERSION,       /* struct version       */
    H5FD_ROS3_VALUE,          /* value                */
    "ros3",                   /* name                 */
    H5FD_MAXADDR,             /* maxaddr              */
    H5F_CLOSE_WEAK,           /* fc_degree            */
    H5FD__ros3_term,          /* terminate            */
    NULL,                     /* sb_size              */
    NULL,                     /* sb_encode            */
    NULL,                     /* sb_decode            */
    sizeof(H5FD_ros3_fapl_t), /* fapl_size            */
    H5FD__ros3_fapl_get,      /* fapl_get             */
    H5FD__ros3_fapl_copy,     /* fapl_copy            */
    H5FD__ros3_fapl_free,     /* fapl_free            */
    0,                        /* dxpl_size            */
    NULL,                     /* dxpl_copy            */
    NULL,                     /* dxpl_free            */
    H5FD__ros3_open,          /* open                 */
    H5FD__ros3_close,         /* close                */
    H5FD__ros3_cmp,           /* cmp                  */
    H5FD__ros3_query,         /* query                */
    NULL,                     /* get_type_map         */
    NULL,                     /* alloc                */
    NULL,                     /* free                 */
    H5FD__ros3_get_eoa,       /* get_eoa              */
    H5FD__ros3_set_eoa,       /* set_eoa              */
    H5FD__ros3_get_eof,       /* get_eof              */
    H5FD__ros3_get_handle,    /* get_handle           */
    H5FD__ros3_read,          /* read                 */
    H5FD__ros3_write,         /* write                */
    NULL,                     /* read_vector          */
    NULL,                     /* write_vector         */
    NULL,                     /* read_selection       */
    NULL,                     /* write_selection      */
    NULL,                     /* flush                */
    H5FD__ros3_truncate,      /* truncate             */
    NULL,                     /* lock                 */
    NULL,                     /* unlock               */
    NULL,                     /* del                  */
    NULL,                     /* ctl                  */
    H5FD_FLMAP_DICHOTOMY      /* fl_map               */
};

/* Declare a free list to manage the H5FD_ros3_t struct */
H5FL_DEFINE_STATIC(H5FD_ros3_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_register
 *
 * Purpose:     Register the driver with the library.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD__ros3_register(void)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    if (H5I_VFL != H5I_get_type(H5FD_ROS3_id_g))
        if ((H5FD_ROS3_id_g = H5FD_register(&H5FD_ros3_g, sizeof(H5FD_class_t), false)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTREGISTER, FAIL, "unable to register ros3 driver");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_register() */

/*---------------------------------------------------------------------------
 * Function:    H5FD__ros3_unregister
 *
 * Purpose:     Reset library driver info.
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5FD__ros3_unregister(void)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Reset VFL ID */
    H5FD_ROS3_id_g = H5I_INVALID_HID;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__ros3_unregister() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_init
 *
 * Purpose:     Singleton to initialize global driver settings & resources.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_init(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (H5FD__s3comms_init() < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to initialize S3 communications interface");

#ifdef ROS3_STATS
    /* Pre-compute stats bin boundaries on powers of 2 >= 10 */
    for (int i = 0; i < ROS3_STATS_BIN_COUNT; i++)
        ros3_stats_boundaries_g[i] = 1 << (10 + i);
#endif

    /* Indicate that driver is set up */
    H5FD_ros3_init_s = true;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_init() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_term
 *
 * Purpose:     Terminate access with the ROS3 VFD.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_term(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (H5FD_ros3_init_s) {
        if (H5FD__s3comms_term() < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTRELEASE, FAIL, "unable to terminate S3 communications interface");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_term() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_ros3
 *
 * Purpose:     Modify the file access property list to use the H5FD_ROS3
 *              driver defined in this source file.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_ros3(hid_t fapl_id, const H5FD_ros3_fapl_t *fa)
{
    H5P_genplist_t *plist         = NULL; /* Property list pointer */
    size_t          page_buf_size = 0;
    herr_t          ret_value     = FAIL;

    FUNC_ENTER_API(FAIL)

    assert(fa != NULL);

    plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, false);
    if (plist == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (H5FD__ros3_validate_config(fa) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid ros3 config");

    /* Check for page buffer set - if not set, set it to the default size */
    if (H5P_get(plist, H5F_ACS_PAGE_BUFFER_SIZE_NAME, &page_buf_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get page buffer size");

    if (page_buf_size == H5F_PAGE_BUFFER_SIZE_DEFAULT) {
        page_buf_size = ROS3_DEF_PAGE_BUF_SIZE;

        /* Set size */
        if (H5P_set(plist, H5F_ACS_PAGE_BUFFER_SIZE_NAME, &page_buf_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set page buffer size");
    }

    ret_value = H5P_set_driver(plist, H5FD_ROS3, (const void *)fa, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_ros3() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_validate_config
 *
 * Purpose:     Test to see if the supplied instance of H5FD_ros3_fapl_t
 *              contains internally consistent data.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_validate_config(const H5FD_ros3_fapl_t *fa)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(fa != NULL);

    if (fa->version != H5FD_CURR_ROS3_FAPL_T_VERSION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown H5FD_ros3_fapl_t version");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_validate_config() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_ros3
 *
 * Purpose:     Queries properties set by the H5Pset_fapl_ros3() function.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_ros3(hid_t fapl_id, H5FD_ros3_fapl_t *fa_dst /*out*/)
{
    const H5FD_ros3_fapl_t *fa_src    = NULL;
    H5P_genplist_t         *plist     = NULL;
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (fa_dst == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "fa_dst is NULL");
    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, true)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "fapl not set to use the ros3 VFD");

    if (NULL == (fa_src = (const H5FD_ros3_fapl_t *)H5P_peek_driver_info(plist)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "bad VFL driver info");

    /* Copy the ros3 fapl data out */
    H5MM_memcpy(fa_dst, fa_src, sizeof(H5FD_ros3_fapl_t));

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_ros3() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_fapl_get
 *
 * Purpose:     Gets a file access property list which could be used to
 *              create an identical file.
 *
 * Return:      Success:    Pointer to new file access property list value
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__ros3_fapl_get(H5FD_t *_file)
{
    H5FD_ros3_t      *file      = (H5FD_ros3_t *)_file;
    H5FD_ros3_fapl_t *fa        = NULL;
    void             *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    fa = (H5FD_ros3_fapl_t *)H5MM_calloc(sizeof(H5FD_ros3_fapl_t));
    if (fa == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Copy the fields of the structure */
    H5MM_memcpy(fa, &(file->fa), sizeof(H5FD_ros3_fapl_t));

    /* Set return value */
    ret_value = fa;

done:
    if (ret_value == NULL)
        if (fa != NULL)
            H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_fapl_copy
 *
 * Purpose:     Copies the ros3-specific file access properties.
 *
 * Return:      Success:    Pointer to a new property list
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__ros3_fapl_copy(const void *_old_fa)
{
    const H5FD_ros3_fapl_t *old_fa    = (const H5FD_ros3_fapl_t *)_old_fa;
    H5FD_ros3_fapl_t       *new_fa    = NULL;
    void                   *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    new_fa = (H5FD_ros3_fapl_t *)H5MM_malloc(sizeof(H5FD_ros3_fapl_t));
    if (new_fa == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    H5MM_memcpy(new_fa, old_fa, sizeof(H5FD_ros3_fapl_t));
    ret_value = new_fa;

done:
    if (ret_value == NULL)
        if (new_fa != NULL)
            H5MM_xfree(new_fa);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_fapl_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_fapl_free
 *
 * Purpose:     Frees the ros3-specific file access properties.
 *
 * Return:      SUCCEED (cannot fail)
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_fapl_free(void *_fa)
{
    H5FD_ros3_fapl_t *fa = (H5FD_ros3_fapl_t *)_fa;

    FUNC_ENTER_PACKAGE_NOERR

    assert(fa != NULL);

    H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__ros3_fapl_free() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_ros3_token
 *
 * Purpose:     Returns session/security token of the ros3 file access
 *              property list though the function arguments.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_ros3_token(hid_t fapl_id, size_t size, char *token_dst /*out*/)
{
    H5P_genplist_t *plist = NULL;
    char           *token_src;
    htri_t          token_exists;
    size_t          tokenlen  = 0;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "size cannot be zero.");
    if (token_dst == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "token_dst is NULL");

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, true)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");
    if ((token_exists = H5P_exist_plist(plist, ROS3_TOKEN_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "failed to check if property token exists in plist");
    if (token_exists) {
        if (H5P_get(plist, ROS3_TOKEN_PROP_NAME, &token_src) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get token value");
    }

    /* Copy the token data out */
    tokenlen = strlen(token_src);
    if (size <= tokenlen) {
        tokenlen = size - 1;
    }
    H5MM_memcpy(token_dst, token_src, sizeof(char) * tokenlen);
    token_dst[tokenlen] = '\0';

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_ros3_token() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_token_copy
 *
 * Purpose:     Create a copy of the token string.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_str_token_copy(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *_value)
{
    char **value     = (char **)_value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (*value)
        if (NULL == (*value = strdup(*value)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't copy string property token");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_token_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_token_cmp
 *
 * Purpose:     Compares two token strings with each other.
 *
 * Return:      A value like strcmp()
 *-------------------------------------------------------------------------
 */
static int
H5FD__ros3_str_token_cmp(const void *_value1, const void *_value2, size_t H5_ATTR_UNUSED size)
{
    char *const *value1    = (char *const *)_value1;
    char *const *value2    = (char *const *)_value2;
    int          ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (*value1) {
        if (*value2)
            ret_value = strcmp(*value1, *value2);
        else
            ret_value = 1;
    }
    else {
        if (*value2)
            ret_value = -1;
        else
            ret_value = 0;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_token_cmp */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_token_close
 *
 * Purpose:     Closes/frees the memory associated to the token string.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_str_token_close(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *_value)
{
    char **value     = (char **)_value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (*value)
        free(*value);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_token_close */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_token_delete
 *
 * Purpose:     Deletes the property token from the property list and frees
 *              the memory associated to the token string.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_str_token_delete(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
                            size_t H5_ATTR_UNUSED size, void *_value)
{
    char **value     = (char **)_value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (*value)
        free(*value);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_token_delete */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_ros3_token
 *
 * Purpose:     Modify the file access property list to use the H5FD_ROS3
 *              driver defined in this source file by adding or
 *              modifying the session/security token property.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_ros3_token(hid_t fapl_id, const char *token)
{
    H5P_genplist_t *plist = NULL;
    char           *token_src;
    htri_t          token_exists;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list");
    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, false)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");
    if (strlen(token) > H5FD_ROS3_MAX_SECRET_TOK_LEN)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL,
                    "specified token exceeds the internally specified maximum string length");

    if ((token_exists = H5P_exist_plist(plist, ROS3_TOKEN_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "failed to check if property token exists in plist");

    if (token_exists) {
        if (H5P_get(plist, ROS3_TOKEN_PROP_NAME, &token_src) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get token value");

        H5MM_memcpy(token_src, token, strlen(token) + 1);
    }
    else {
        token_src = (char *)malloc(sizeof(char) * (H5FD_ROS3_MAX_SECRET_TOK_LEN + 1));
        if (token_src == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "cannot make space for token_src variable.");
        H5MM_memcpy(token_src, token, strlen(token) + 1);
        if (H5P_insert(plist, ROS3_TOKEN_PROP_NAME, sizeof(char *), &token_src, NULL, NULL, NULL, NULL,
                       H5FD__ros3_str_token_delete, H5FD__ros3_str_token_copy, H5FD__ros3_str_token_cmp,
                       H5FD__ros3_str_token_close) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to register property in plist");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_ros3_token() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_endpoint_copy
 *
 * Purpose:     Create a copy of the endpoint string.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_str_endpoint_copy(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *_value)
{
    char **value     = (char **)_value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (*value)
        if (NULL == (*value = strdup(*value)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't copy endpoint URL string");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_endpoint_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_endpoint_cmp
 *
 * Purpose:     Compares two endpoint strings with each other.
 *
 * Return:      A value like strcmp()
 *-------------------------------------------------------------------------
 */
static int
H5FD__ros3_str_endpoint_cmp(const void *_value1, const void *_value2, size_t H5_ATTR_UNUSED size)
{
    char *const *value1    = (char *const *)_value1;
    char *const *value2    = (char *const *)_value2;
    int          ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (*value1) {
        if (*value2)
            ret_value = strcmp(*value1, *value2);
        else
            ret_value = 1;
    }
    else {
        if (*value2)
            ret_value = -1;
        else
            ret_value = 0;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_endpoint_cmp */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_endpoint_close
 *
 * Purpose:     Closes/frees the memory associated to the endpoint string.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_str_endpoint_close(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *_value)
{
    char **value     = (char **)_value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (*value)
        free(*value);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_endpoint_close */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_str_endpoint_delete
 *
 * Purpose:     Deletes the property endpoint from the property list and frees
 *              the memory associated to the token string.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_str_endpoint_delete(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
                               size_t H5_ATTR_UNUSED size, void *_value)
{
    char **value     = (char **)_value;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    if (*value)
        free(*value);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_str_endpoint_delete */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_ros3_endpoint
 *
 * Purpose:     Modify the file access property list by adding or modifying
 *              the endpoint url property.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_ros3_endpoint(hid_t fapl_id, const char *endpoint_url)
{
    H5P_genplist_t *plist = NULL;
    char           *endpoint_src;
    htri_t          endpoint_exists;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list");
    if (!endpoint_url)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "endpoint URL string was NULL");
    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, false)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");

    if ((endpoint_exists = H5P_exist_plist(plist, ROS3_ENDPOINT_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "failed to check if endpoint URL property exists in plist");

    if (NULL == (endpoint_src = strdup(endpoint_url)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't copy endpoint URL string");

    if (endpoint_exists) {
        if (H5P_set(plist, ROS3_ENDPOINT_PROP_NAME, &endpoint_src) < 0) {
            free(endpoint_src);
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set endpoint URL value");
        }
    }
    else {
        if (H5P_insert(plist, ROS3_ENDPOINT_PROP_NAME, sizeof(char *), &endpoint_src, NULL, NULL, NULL, NULL,
                       H5FD__ros3_str_endpoint_delete, H5FD__ros3_str_endpoint_copy,
                       H5FD__ros3_str_endpoint_cmp, H5FD__ros3_str_endpoint_close) < 0) {
            free(endpoint_src);
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "unable to register property in plist");
        }
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_ros3_endpoint() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_ros3_endpoint
 *
 * Purpose:     Returns endpoint url of the ros3 file access
 *              property list though the function arguments.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_ros3_endpoint(hid_t fapl_id, size_t size, char *endpoint_dst /*out*/)
{
    H5P_genplist_t *plist = NULL;
    htri_t          endpoint_exists;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "size cannot be zero.");
    if (endpoint_dst == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "endpoint_dst is NULL");

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, true)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");
    if ((endpoint_exists = H5P_exist_plist(plist, ROS3_ENDPOINT_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "failed to check if endpoint URL property exists in plist");
    if (endpoint_exists) {
        char *endpoint_src;

        if (H5P_get(plist, ROS3_ENDPOINT_PROP_NAME, &endpoint_src) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get endpoint URL value");

        if (endpoint_src) {
            strncpy(endpoint_dst, endpoint_src, size);
            endpoint_dst[size - 1] = '\0';
        }
    }
    else
        memset(endpoint_dst, 0, size);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_ros3_endpoint() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_block_caching_params_cmp
 *
 * Purpose:     Compares two H5FD_ros3_block_caching_params_t structures
 *
 * Return:      -1/0/1
 *-------------------------------------------------------------------------
 */
static int
H5FD__ros3_block_caching_params_cmp(const void *value1, const void *value2, size_t H5_ATTR_UNUSED size)
{
    const H5FD_ros3_block_caching_params_t *params1 = (const H5FD_ros3_block_caching_params_t *)value1;
    const H5FD_ros3_block_caching_params_t *params2 = (const H5FD_ros3_block_caching_params_t *)value2;

    if (params1->block_size != params2->block_size)
        return (params1->block_size > params2->block_size) - (params1->block_size < params2->block_size);
    if (params1->block_cache_size != params2->block_cache_size)
        return (params1->block_cache_size > params2->block_cache_size) -
               (params1->block_cache_size < params2->block_cache_size);
    if (params1->lock_superblock != params2->lock_superblock)
        return (params1->lock_superblock > params2->lock_superblock) -
               (params1->lock_superblock < params2->lock_superblock);

    return 0;
} /* end H5FD__ros3_block_caching_params_cmp() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_ros3_block_caching
 *
 * Purpose:     Sets I/O block caching parameters for the ros3 VFD.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_ros3_block_caching(hid_t fapl_id, size_t block_size, size_t block_cache_size,
                               bool lock_superblock)
{
    H5FD_ros3_block_caching_params_t block_caching_params = {0};
    H5P_genplist_t                  *plist                = NULL;
    htri_t                           block_caching_params_exist;
    herr_t                           ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (fapl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list");
    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, false)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "ROS3 driver is not set on FAPL");

    if ((block_caching_params_exist = H5P_exist_plist(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,
                    "failed to check if I/O block caching parameters property exists in plist");

    block_caching_params.block_size       = block_size;
    block_caching_params.block_cache_size = block_cache_size;
    block_caching_params.lock_superblock  = lock_superblock;

    /* If block size is larger than block cache size, round block size down
     * to block cache size so that block cache size is an upper limit but
     * can still hold at least 1 block.
     */
    if (block_caching_params.block_size != 0 && block_caching_params.block_cache_size != 0) {
        if (block_caching_params.block_size > block_caching_params.block_cache_size)
            block_caching_params.block_size = block_caching_params.block_cache_size;
    }

    if (block_caching_params_exist) {
        if (H5P_set(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME, &block_caching_params) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set I/O block caching parameters");
    }
    else {
        if (H5P_insert(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME, sizeof(H5FD_ros3_block_caching_params_t),
                       &block_caching_params, NULL, NULL, NULL, NULL, NULL, NULL,
                       H5FD__ros3_block_caching_params_cmp, NULL) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL,
                        "unable to register I/O block caching parameters property in plist");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_ros3_block_caching() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_ros3_block_caching
 *
 * Purpose:     Retrieves any I/O block caching parameters set for the ros3
 *              VFD.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_ros3_block_caching(hid_t fapl_id, size_t *block_size, size_t *block_cache_size,
                               bool *lock_superblock)
{
    H5P_genplist_t *plist = NULL;
    htri_t          block_caching_params_exist;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, true)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_ROS3 != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "ROS3 driver is not set on FAPL");

    if ((block_caching_params_exist = H5P_exist_plist(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,
                    "failed to check if I/O block caching parameters property exists in plist");
    if (block_caching_params_exist) {
        H5FD_ros3_block_caching_params_t block_caching_params = {0};

        if (H5P_get(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME, &block_caching_params) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "unable to get I/O block caching parameters");

        if (block_size)
            *block_size = block_caching_params.block_size;
        if (block_cache_size)
            *block_cache_size = block_caching_params.block_cache_size;
        if (lock_superblock)
            *lock_superblock = block_caching_params.lock_superblock;
    }
    else {
        if (block_size)
            *block_size = HDF5_ROS3_VFD_DEFAULT_BLOCK_SIZE;
        if (block_cache_size)
            *block_cache_size = ROS3_DEF_PAGE_BUF_SIZE;
        if (lock_superblock)
            *lock_superblock = true;
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_ros3_block_caching() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_open
 *
 * Purpose:     Create and/or open a file as an HDF5 file
 *
 *     Any flag except H5F_ACC_RDONLY will cause an error
 *
 *     `url` param (as received from `H5FD_open()`) must conform to web url:
 *         NAME   :: HTTP "://" DOMAIN [PORT] ["/" [URI] [QUERY] ]
 *         HTTP   :: "http" [ "s" ]
 *         DOMAIN :: e.g., "mybucket.host.org"
 *         PORT   :: ":" <number> (e.g., ":9000" )
 *         URI    :: <string> (e.g., "path/to/resource.hd5" )
 *         QUERY  :: "?" <string> (e.g., "arg1=param1&arg2=param2")
 *
 * Return:      Success:    A pointer to a new file data structure
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__ros3_open(const char *url, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_ros3_t            *file                       = NULL;
    s3r_t                  *handle                     = NULL;
    const H5FD_ros3_fapl_t *fa                         = NULL;
    H5P_genplist_t         *plist                      = NULL;
    char                   *fapl_token                 = NULL;
    char                   *fapl_endpoint              = NULL;
    H5FD_t                 *ret_value                  = NULL;
    htri_t                  endpt_exists               = false;
    htri_t                  block_caching_params_exist = false;

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if (!url || !*url)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name");
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr");
    if (H5FD_ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr");
    if (flags != H5F_ACC_RDONLY)
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, NULL, "only Read-Only access allowed");
    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS, true)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");

    /* Initialize driver, if it's not yet */
    if (!H5FD_ros3_init_s)
        if (H5FD__ros3_init() < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, NULL, "can't initialize driver");

    /* Get ros3 driver info */
    if (NULL == (fa = (const H5FD_ros3_fapl_t *)H5P_peek_driver_info(plist)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "could not get ros3 VFL driver info");

    /* Get the token, if it exists */
    if (fa->authenticate) {
        htri_t token_exists;

        /* Does the token exist in the fapl? */
        if ((token_exists = H5P_exist_plist(plist, ROS3_TOKEN_PROP_NAME)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "failed check for property token in plist");

        /* If so, get it */
        if (token_exists) {
            if (H5P_get(plist, ROS3_TOKEN_PROP_NAME, &fapl_token) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "unable to get token value");
        }
    }

    /* Does the endpoint exist in the fapl? */
    if ((endpt_exists = H5P_exist_plist(plist, ROS3_ENDPOINT_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "failed check for property endpoint in plist");

    /* If so, get it */
    if (endpt_exists) {
        if (H5P_get(plist, ROS3_ENDPOINT_PROP_NAME, &fapl_endpoint) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "unable to get endpoint value");
    }

    /* Open file; procedure depends on whether or not the fapl instructs to
     * authenticate requests or not.
     */
    if (NULL == (handle = H5FD__s3comms_s3r_open(url, fa, fapl_token, fapl_endpoint)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "s3r_open failed");

    /* Create new file struct */
    if (NULL == (file = H5FL_CALLOC(H5FD_ros3_t)))
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, NULL, "unable to allocate file struct");

    file->s3r_handle = handle;
    H5MM_memcpy(&(file->fa), fa, sizeof(H5FD_ros3_fapl_t));

    if ((block_caching_params_exist = H5P_exist_plist(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL,
                    "failed to check if I/O block caching parameters property exists in plist");
    if (block_caching_params_exist) {
        H5FD_ros3_block_caching_params_t block_caching_params = {0};

        if (H5P_get(plist, ROS3_BLOCK_CACHING_PARAMS_PROP_NAME, &block_caching_params) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "unable to get I/O block caching parameters");

        file->block_cache.block_size       = block_caching_params.block_size;
        file->block_cache.block_cache_size = block_caching_params.block_cache_size;
        file->block_cache.lock_superblock  = block_caching_params.lock_superblock;
    }
    else {
        file->block_cache.block_size       = HDF5_ROS3_VFD_DEFAULT_BLOCK_SIZE;
        file->block_cache.block_cache_size = HDF5_ROS3_VFD_DEFAULT_BLOCK_CACHE_SIZE;
        file->block_cache.lock_superblock  = true;
    }

    if (file->block_cache.block_size == 0 || file->block_cache.block_cache_size == 0)
        file->block_cache.disabled = true;

    /* Determine the maximum number of blocks to keep around in the block cache */
    if (!file->block_cache.disabled) {
        /* final sanity check; the block size should never exceed the block cache size */
        if (file->block_cache.block_size > file->block_cache.block_cache_size)
            file->block_cache.block_size = file->block_cache.block_cache_size;

        file->block_cache.max_num_blocks = file->block_cache.block_cache_size / file->block_cache.block_size;
        assert(file->block_cache.max_num_blocks >= 1);
    }

#ifdef ROS3_STATS
    if (H5FD__ros3_reset_stats(file) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_UNINITIALIZED, NULL, "unable to reset file statistics");
#endif

    ret_value = (H5FD_t *)file;

done:
    if (ret_value == NULL) {
        if (handle != NULL)
            if (H5FD__s3comms_s3r_close(handle) < 0)
                HDONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, NULL, "unable to close s3 file handle");
        if (file != NULL) {
            file = H5FL_FREE(H5FD_ros3_t, file);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_open() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_close
 *
 * Purpose:     Close the file.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_close(H5FD_t H5_ATTR_UNUSED *_file)
{
    H5FD_ros3_t *file      = (H5FD_ros3_t *)_file;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(file != NULL);
    assert(file->s3r_handle != NULL);

#ifdef ROS3_STATS
    if (H5FD__ros3_print_stats(stdout, file) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_ERROR, FAIL, "problem while writing file statistics");
#endif

    /* Close the underlying request handle */
    if (H5FD__s3comms_s3r_close(file->s3r_handle) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close S3 request handle");

    /* Release the file info */
    if (file->block_cache.hash_table) {
        H5FD_ros_block_hash_t *p, *tmp;

        HASH_ITER(hh, file->block_cache.hash_table, p, tmp)
        {
            HASH_DEL(file->block_cache.hash_table, p);
            H5MM_free(p);
        }
    }

    file = H5FL_FREE(H5FD_ros3_t, file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_cmp
 *
 * Purpose:     Compares two files belonging to this driver using an
 *              arbitrary (but consistent) ordering:
 *
 *              + url scheme
 *              + url host
 *              + url port
 *              + url path
 *              + url query
 *              + fapl aws_region
 *              + fapl secret_id
 *              + fapl secret_key
 *
 * TODO:        This should return -1/0/1 like the other VFDs
 *
 * Return:      Equivalent:         0
 *              Not Equivalent:    -1
 *              (Can't fail)
 *-------------------------------------------------------------------------
 */
static int
H5FD__ros3_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_ros3_t  *f1        = (const H5FD_ros3_t *)_f1;
    const H5FD_ros3_t  *f2        = (const H5FD_ros3_t *)_f2;
    const parsed_url_t *purl1     = NULL;
    const parsed_url_t *purl2     = NULL;
    int                 ret_value = 0;

    FUNC_ENTER_PACKAGE_NOERR

    assert(f1->s3r_handle != NULL);
    assert(f2->s3r_handle != NULL);

    purl1 = (const parsed_url_t *)f1->s3r_handle->purl;
    purl2 = (const parsed_url_t *)f2->s3r_handle->purl;
    assert(purl1 != NULL);
    assert(purl2 != NULL);
    assert(purl1->scheme != NULL);
    assert(purl2->scheme != NULL);
    assert(purl1->host != NULL);
    assert(purl2->host != NULL);

    /* URL: SCHEME */
    if (strcmp(purl1->scheme, purl2->scheme))
        HGOTO_DONE(-1);

    /* URL: HOST */
    if (strcmp(purl1->host, purl2->host))
        HGOTO_DONE(-1);

    /* URL: PORT */
    if (purl1->port && purl2->port) {
        if (strcmp(purl1->port, purl2->port))
            HGOTO_DONE(-1);
    }
    else if (purl1->port)
        HGOTO_DONE(-1);
    else if (purl2->port)
        HGOTO_DONE(-1);

    /* URL: PATH */
    if (purl1->path && purl2->path) {
        if (strcmp(purl1->path, purl2->path))
            HGOTO_DONE(-1);
    }
    else if (purl1->path && !purl2->path)
        HGOTO_DONE(-1);
    else if (purl2->path && !purl1->path)
        HGOTO_DONE(-1);

    /* URL: QUERY */
    if (purl1->query && purl2->query) {
        if (strcmp(purl1->query, purl2->query))
            HGOTO_DONE(-1);
    }
    else if (purl1->query && !purl2->query)
        HGOTO_DONE(-1);
    else if (purl2->query && !purl1->query)
        HGOTO_DONE(-1);

    /* FAPL: AWS_REGION */
    if (f1->fa.aws_region[0] != '\0' && f2->fa.aws_region[0] != '\0') {
        if (strcmp(f1->fa.aws_region, f2->fa.aws_region))
            HGOTO_DONE(-1);
    }
    else if (f1->fa.aws_region[0] != '\0')
        HGOTO_DONE(-1);
    else if (f2->fa.aws_region[0] != '\0')
        HGOTO_DONE(-1);

    /* FAPL: SECRET_ID */
    if (f1->fa.secret_id[0] != '\0' && f2->fa.secret_id[0] != '\0') {
        if (strcmp(f1->fa.secret_id, f2->fa.secret_id))
            HGOTO_DONE(-1);
    }
    else if (f1->fa.secret_id[0] != '\0')
        HGOTO_DONE(-1);
    else if (f2->fa.secret_id[0] != '\0')
        HGOTO_DONE(-1);

    /* FAPL: SECRET_KEY */
    if (f1->fa.secret_key[0] != '\0' && f2->fa.secret_key[0] != '\0') {
        if (strcmp(f1->fa.secret_key, f2->fa.secret_key))
            HGOTO_DONE(-1);
    }
    else if (f1->fa.secret_key[0] != '\0')
        HGOTO_DONE(-1);
    else if (f2->fa.secret_key[0] != '\0')
        HGOTO_DONE(-1);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_cmp() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      SUCCEED (Can't fail)
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Set the VFL feature flags that this driver supports
     *
     * Since the ros3 VFD is read-only, many flags are irrelevant.
     */
    if (flags) {
        *flags = 0;
        /* OK to perform data sieving for faster raw data reads & writes */
        *flags |= H5FD_FEAT_DATA_SIEVE;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FD__ros3_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file. The EOA marker
 *              is the first address past the last byte allocated in the
 *              format address space.
 *
 * Return:      The end-of-address marker (Can't fail)
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ros3_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_ros3_t *file = (const H5FD_ros3_t *)_file;

    FUNC_ENTER_PACKAGE_NOERR

    FUNC_LEAVE_NOAPI(file->eoa)
} /* end H5FD__ros3_get_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file.
 *
 * Return:      SUCCEED  (can't fail)
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_ros3_t *file = (H5FD_ros3_t *)_file;

    FUNC_ENTER_PACKAGE_NOERR

    file->eoa = addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FD__ros3_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_get_eof
 *
 * Purpose:     Returns the end-of-file marker.
 *
 * Return:      End of file address, the first address past the end of the
 *              "file". (Can't fail)
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ros3_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_ros3_t *file = (const H5FD_ros3_t *)_file;

    FUNC_ENTER_PACKAGE_NOERR

    FUNC_LEAVE_NOAPI(H5FD__s3comms_s3r_get_filesize(file->s3r_handle))
} /* end H5FD__ros3_get_eof() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_get_handle
 *
 * Purpose:     Returns the S3 Request handle (s3r_t) of ros3 file driver.
 *
 * Returns:     SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void **file_handle)
{
    H5FD_ros3_t *file      = (H5FD_ros3_t *)_file;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid");

    *file_handle = file->s3r_handle;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_get_handle() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_read
 *
 * Purpose:     Reads SIZE bytes of data from FILE beginning at address ADDR
 *              into buffer BUF according to data transfer properties in
 *              DXPL_ID.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr,
                size_t size, void *buf)
{
    H5FD_ros3_block_io_req_t *io_reqs       = NULL;
    H5FD_ros_block_hash_t    *new_block     = NULL;
    H5FD_ros3_t              *file          = (H5FD_ros3_t *)_file;
    uint8_t                  *buf_ptr       = (uint8_t *)buf;
    size_t                    filesize      = 0;
    size_t                    num_io_blocks = 0;
    bool                      is_cached     = false;
    herr_t                    ret_value     = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(file);
    assert(file->s3r_handle);
    assert(buf);

    filesize = H5FD__s3comms_s3r_get_filesize(file->s3r_handle);

    if ((addr > filesize) || ((addr + size) > filesize))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "range exceeds file address");

    /* If this is the first read for the file and block caching is not disabled,
     * read an initial "block size" worth of bytes and add the block to the block
     * cache. This initial block is mostly used to optimize locating the file's
     * superblock, as well as general metadata reads following that process.
     * Note that while reading the superblock of a file, it can't be determined
     * at that time whether paged aggregation will be enabled or not, so the
     * first "block size" bytes (rather than just an estimated amount of superblock
     * bytes) are always cached for possible later use. Also note that it can't
     * be determined at that time whether a userblock exists or how large it is,
     * so a userblock size >= "block size" will effectively make this cache
     * useless.
     */
    if (!file->block_cache.disabled && !file->block_cache.hash_table) {
        if (H5FD__ros3_init_block_cache(file) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to initialize I/O block cache");
        assert(file->block_cache.hash_table);
    }

    /* If the I/O request falls within the already-cached superblock block and
     * either paged aggregation is enabled or the superblock block is locked
     * in the cache, just serve the request from the cache. When paged aggregation
     * is enabled, this can save a possible request to S3. When paged aggregation
     * is not enabled, this saves a tiny bit of overhead from below since it's
     * known that there will be only one I/O request block.
     *
     * Since no modifications are made to the block cache when paged aggregation
     * is enabled other than caching the superblock block, the superblock block
     * should always be available for use in this case. When paged aggregation
     * isn't enabled, this optimization is only used when the superblock block
     * is locked in the cache and thus should be found by a hash table lookup.
     */
    /* clang-format off */
    is_cached =
        !file->block_cache.disabled &&                               /* Block caching is enabled */
        (addr + size <= file->block_cache.block_size) &&             /* Addr + size falls within cached block */
        (file->pub.paged_aggr || file->block_cache.lock_superblock); /* Paged aggr. or locked cached block */
    /* clang-format on */

    if (is_cached) {
        H5FD_ros_block_hash_t *super_block      = NULL;
        haddr_t                super_block_addr = 0;

        HASH_FIND(hh, file->block_cache.hash_table, &super_block_addr, sizeof(haddr_t), super_block);
        if (!super_block)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to locate superblock block in block cache");

        /* No need to promote block in LRU - if paged aggregation is enabled,
         * block caching (and thus the LRU eviction list) is not enabled.
         * Otherwise, the superblock block is locked in the cache and not part
         * of the LRU eviction policy.
         */

        memcpy(buf, super_block->buf + addr, size);
        HGOTO_DONE(SUCCEED);
    }

    /* If block caching is disabled or if paged aggregation is enabled, just
     * issue reads to S3 directly. When paged aggregation is enabled, it's
     * assumed that higher layers are already performing caching and so
     * additional caching is not needed here.
     */
    if (file->block_cache.disabled || file->pub.paged_aggr) {
        /*
         * Note that the VFD interface doesn't specify the size of buf.
         * Assume that the caller knows what they're doing.
         */
        if (H5FD__s3comms_s3r_read(file->s3r_handle, addr, size, buf, size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "unable to execute read");

#ifdef ROS3_STATS
        if (H5FD__ros3_log_read_stats(file, type, (uint64_t)size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "unable to log read stats");
#endif

        HGOTO_DONE(SUCCEED);
    }

    /* If paged aggregation is not enabled, serve this I/O request from the block
     * cache as able and otherwise issue reads to S3.
     */

    /* Split I/O request among block boundaries as necessary */
    if (H5FD__ros3_determine_io_reqs(file, addr, size, &io_reqs, &num_io_blocks) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "unable to partition read request into I/O blocks");

    for (size_t i = 0; i < num_io_blocks; i++) {
        H5FD_ros_block_hash_t *io_block   = NULL;
        haddr_t                block_addr = HADDR_UNDEF;
        bool                   can_cache  = true;

        /* If block caching is enabled, check if the block is in the cache */
        if (!file->block_cache.disabled) {
            /* The first I/O request may not be "block size"-aligned; all others will be. */
            if (i == 0)
                block_addr = (io_reqs[i].addr / file->block_cache.block_size) * file->block_cache.block_size;
            else
                block_addr = io_reqs[i].addr;

            HASH_FIND(hh, file->block_cache.hash_table, &block_addr, sizeof(haddr_t), io_block);
            if (io_block) {
                /* Serve read from the block cache if the block was found */
                memcpy(buf_ptr, io_block->buf + (io_reqs[i].addr - io_block->addr), io_reqs[i].io_size);
                buf_ptr += io_reqs[i].io_size;

                /* If the block that was found is not the superblock block OR if it is
                 * the superblock block and the block isn't locked in the cache (can be
                 * evicted), promote the block to the head of the LRU eviction list
                 * (making it a less likely candidate for eviction) if it's not already
                 * at the head of the list.
                 */
                if (io_block->addr != 0 || !file->block_cache.lock_superblock) {
                    /* At this point, the block should already be in the LRU eviction list. */
                    assert(file->block_cache.LRU_head);
                    assert((file->block_cache.LRU_head == io_block) || io_block->prev);

                    if (file->block_cache.LRU_head != io_block) {
                        ROS3_BLOCK_CACHE_LRU_REMOVE(file, io_block);
                        ROS3_BLOCK_CACHE_LRU_INSERT(file, io_block);
                    }
                }

                continue;
            }
        }

        /* If the I/O request wasn't served from the block cache, check if the
         * request can be cached before issuing it. A lack of space in the
         * cache isn't considered here, as old blocks will be evicted as necessary.
         */
        if (file->block_cache.disabled)
            can_cache = false;
        else {
            /* If the cache can only hold one block and the superblock block is
             * locked in the cache, we cannot cache any more blocks.
             */
            if (file->block_cache.lock_superblock && file->block_cache.max_num_blocks == 1) {
                assert(HASH_COUNT(file->block_cache.hash_table) == 1); /* superblock block should be cached */
                can_cache = false;
            }
        }

        /* If the I/O request can be cached, allocate a new block for it and insert
         * it into the block cache after the request is finished.
         */
        if (can_cache) {
            size_t alloc_size = sizeof(*new_block) + file->block_cache.block_size;
            size_t read_size  = 0;

            /* Allocate a new block and read the entire block's bytes */
            if (NULL == (new_block = H5MM_malloc(alloc_size)))
                HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to allocate I/O block");
            new_block->addr       = block_addr;
            new_block->block_size = file->block_cache.block_size;
            new_block->next       = NULL;
            new_block->prev       = NULL;
            memset(&new_block->hh, 0, sizeof(UT_hash_handle));

            read_size = MIN(new_block->block_size, filesize - new_block->addr);
            if (H5FD__s3comms_s3r_read(file->s3r_handle, new_block->addr, read_size, new_block->buf,
                                       new_block->block_size) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "unable to execute read");

#ifdef ROS3_STATS
            if (H5FD__ros3_log_read_stats(file, type, (uint64_t)read_size) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "unable to log read stats");
#endif

            memcpy(buf_ptr, new_block->buf + (io_reqs[i].addr - new_block->addr), io_reqs[i].io_size);

            /* If block cache is full, evict oldest block before inserting a new block */
            if (H5FD__ros3_block_cache_make_space(file) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to make space in block cache");

            /* Add block to block cache */
            HASH_ADD(hh, file->block_cache.hash_table, addr, sizeof(haddr_t), new_block);
            if (!new_block->hh.tbl)
                HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to add I/O block to hash table");

            /* Add block to head of LRU eviction list */
            ROS3_BLOCK_CACHE_LRU_INSERT(file, new_block);

            new_block = NULL; /* Now owned by hash table */
        }
        else {
            /* Unable to cache this I/O request in the block cache. Just read
             * from S3 directly. Note that the VFD interface doesn't specify
             * the size of buf. Assume that the caller knows what they're doing.
             */
            if (H5FD__s3comms_s3r_read(file->s3r_handle, io_reqs[i].addr, io_reqs[i].io_size, buf_ptr,
                                       io_reqs[i].io_size) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "unable to execute read");

#ifdef ROS3_STATS
            if (H5FD__ros3_log_read_stats(file, type, (uint64_t)io_reqs[i].io_size) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "unable to log read stats");
#endif
        }

        buf_ptr += io_reqs[i].io_size;
    }

done:
    H5MM_free(new_block);
    H5MM_free(io_reqs);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_init_block_cache
 *
 * Purpose:     Initializes an I/O block cache for optimizing I/O. Reads
 *              the first "block size" bytes of the file (or the entire
 *              file if it's smaller than "block size") and stores the
 *              block in a hash table for quick lookups by
 *              "block size"-aligned addresses.
 *
 *              If the 'lock_superblock' field in the file struct is false,
 *              this initial block will be added to an LRU list of blocks
 *              which can be evicted when attempting to add another block
 *              to the block cache. Otherwise, this block will be omitted
 *              from that list, preventing it from being evicted until file
 *              close.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_init_block_cache(H5FD_ros3_t *file)
{
    H5FD_ros_block_hash_t *super_block = NULL;
    size_t                 file_size   = 0;
    size_t                 alloc_size  = 0;
    size_t                 read_size   = 0;
    herr_t                 ret_value   = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(file);
    assert(!file->block_cache.disabled);
    assert(file->block_cache.block_size > 0);

    /* Already initialized */
    if (file->block_cache.hash_table)
        HGOTO_DONE(SUCCEED);

    alloc_size = sizeof(*super_block) + file->block_cache.block_size;

    if (NULL == (super_block = H5MM_calloc(alloc_size)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to allocate I/O block");
    super_block->addr       = 0;
    super_block->block_size = file->block_cache.block_size;

    /* Read either the entire file or "block size" bytes, whichever is smaller.
     * If the file size is smaller than the "block size", the remaining bytes
     * in the block will be zeroes.
     */
    file_size = H5FD__s3comms_s3r_get_filesize(file->s3r_handle);
    read_size = (file_size < file->block_cache.block_size) ? file_size : file->block_cache.block_size;
    if (H5FD__s3comms_s3r_read(file->s3r_handle, 0, read_size, super_block->buf,
                               file->block_cache.block_size) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "unable to execute read");

    /* Add block to block cache */
    HASH_ADD(hh, file->block_cache.hash_table, addr, sizeof(haddr_t), super_block);
    if (!file->block_cache.hash_table)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to allocate I/O block hash table");
    if (!super_block->hh.tbl)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to add I/O block to hash table");

    /* Add the superblock block to the list of blocks that can be
     * evicted if it isn't setup to be locked in the cache.
     */
    if (!file->block_cache.lock_superblock)
        ROS3_BLOCK_CACHE_LRU_INSERT(file, super_block);

    super_block = NULL; /* Now owned by hash table */

done:
    if (ret_value < 0) {
        H5MM_free(super_block);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_init_block_cache() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_determine_io_reqs
 *
 * Purpose:     Given an offset and length for an I/O request, partitions
 *              the I/O request among "block size"-aligned boundaries and
 *              returns a new array of smaller I/O requests.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_determine_io_reqs(H5FD_ros3_t *file, haddr_t addr, size_t io_size,
                             H5FD_ros3_block_io_req_t **io_reqs_out, size_t *num_io_reqs_out)
{
    H5FD_ros3_block_io_req_t *io_reqs          = NULL;
    haddr_t                   cur_addr         = HADDR_UNDEF;
    haddr_t                   first_block_addr = HADDR_UNDEF;
    haddr_t                   last_block_addr  = HADDR_UNDEF;
    size_t                    block_size       = 0;
    size_t                    num_blocks       = 0;
    size_t                    num_blocks_left  = 0;
    herr_t                    ret_value        = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(file);
    assert(!file->block_cache.disabled);
    assert(file->block_cache.block_size > 0);
    assert(io_reqs_out);
    assert(num_io_reqs_out);

    if (io_size == 0) {
        *io_reqs_out     = NULL;
        *num_io_reqs_out = 0;
        HGOTO_DONE(SUCCEED);
    }

    block_size = file->block_cache.block_size;

    first_block_addr = ((addr / block_size) * block_size);
    last_block_addr  = ((addr + io_size - 1) / block_size) * block_size;
    num_blocks       = (last_block_addr / block_size + 1) - (first_block_addr / block_size);

    assert(num_blocks > 0);
    if (NULL == (io_reqs = H5MM_malloc(num_blocks * sizeof(*io_reqs))))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate array of I/O requests");

    /* Setup I/O request to first block */
    io_reqs[0].addr    = addr;
    io_reqs[0].io_size = MIN(io_size, block_size - (size_t)(addr - first_block_addr));
    assert(io_reqs[0].io_size <= io_size);
    io_size -= io_reqs[0].io_size;

    num_blocks_left = num_blocks - 1;
    cur_addr        = addr + io_reqs[0].io_size;

    /* Setup I/O requests for any blocks between first and last */
    if (num_blocks_left > 1) {
        for (size_t i = 1; i < num_blocks - 1; i++) {
            io_reqs[i].addr    = cur_addr;
            io_reqs[i].io_size = block_size;

            cur_addr += block_size;
            assert(io_reqs[i].io_size <= io_size);
            io_size -= io_reqs[i].io_size;

            num_blocks_left--;
        }
    }

    /* Setup I/O request for last block, if applicable */
    if (num_blocks_left) {
        assert(num_blocks_left == 1);
        io_reqs[num_blocks - 1].addr    = cur_addr;
        io_reqs[num_blocks - 1].io_size = io_size;
        io_size -= io_reqs[num_blocks - 1].io_size;
    }

    assert(io_size == 0);

    *io_reqs_out     = io_reqs;
    *num_io_reqs_out = num_blocks;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_determine_io_reqs() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_block_cache_make_space
 *
 * Purpose:     If necessary, evicts the oldest block in the block cache
 *              (by LRU policy) to make space for a new block.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_block_cache_make_space(H5FD_ros3_t *file)
{
    H5FD_ros_block_hash_t *oldest_block = NULL;
    herr_t                 ret_value    = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    assert(file);

    /* If there's space in the cache, no need to evict a block */
    if (HASH_COUNT(file->block_cache.hash_table) < file->block_cache.max_num_blocks)
        HGOTO_DONE(SUCCEED);

    oldest_block = file->block_cache.LRU_tail;
    ROS3_BLOCK_CACHE_LRU_REMOVE(file, oldest_block);
    HASH_DEL(file->block_cache.hash_table, oldest_block);

    H5MM_free(oldest_block);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_block_cache_make_space() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_write
 *
 * Purpose:     Write bytes to file
 *
 *              UNSUPPORTED IN READ-ONLY ROS3 VFD.
 *
 * Return:      FAIL (Not possible with Read-Only S3 file)
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_write(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id,
                 haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED size, const void H5_ATTR_UNUSED *buf)
{
    herr_t ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, FAIL, "cannot write to read-only file.");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__ros3_write() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ros3_truncate
 *
 * Purpose:     Makes sure that the true file size is the same (or larger)
 *              than the end-of-address.
 *
 *              NOT POSSIBLE ON READ-ONLY S3 FILES
 *
 * Return:      FAIL (Not possible on Read-Only S3 files)
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_truncate(H5FD_t H5_ATTR_UNUSED *_file, hid_t H5_ATTR_UNUSED dxpl_id, bool H5_ATTR_UNUSED closing)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, FAIL, "cannot truncate read-only file.");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_truncate() */

#ifdef ROS3_STATS
/*----------------------------------------------------------------------------
 * Function:    H5FD__ros3_reset_stats
 *
 * Purpose:     Reset the collected statistics
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_reset_stats(H5FD_ros3_t *file)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (file == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file was null");

    for (int i = 0; i <= ROS3_STATS_BIN_COUNT; i++) {
        file->raw[i].bytes = 0;
        file->raw[i].count = 0;
        file->raw[i].min   = 0;
        file->raw[i].max   = 0;

        file->meta[i].bytes = 0;
        file->meta[i].count = 0;
        file->meta[i].min   = 0;
        file->meta[i].max   = 0;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_reset_stats() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__ros3_log_read_stats
 *
 * Purpose:     Add data for a read to the ros3 stats
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_log_read_stats(H5FD_ros3_t *file, H5FD_mem_t type, uint64_t size)
{
    H5FD_ros3_stats_bin_t *bin       = NULL;
    int                    i         = 0;
    herr_t                 ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (file == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file was null");

    /* Find which "bin" this read fits in */
    for (i = 0; i < ROS3_STATS_BIN_COUNT; i++)
        if (size < ros3_stats_boundaries_g[i])
            break;
    bin = (type == H5FD_MEM_DRAW) ? &file->raw[i] : &file->meta[i];

    /* Store collected stats in appropriate bin */
    bin->count++;
    bin->bytes += size;
    if (size < bin->min)
        bin->min = size;
    if (size > bin->max)
        bin->max = size;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ros3_log_read_stats() */

/*----------------------------------------------------------------------------
 * Function:    H5FD__ros3_print_stats
 *
 * Purpose:     Tabulate and pretty-print statistics for this virtual file.
 *
 *     Should be called upon file close.
 *
 *     Shows number of reads and bytes read, broken down by
 *     "raw" (H5FD_MEM_DRAW) or "meta" (any other flag)
 *
 *     Prints filename and listing of total number of reads and bytes read,
 *     both as a grand total and separate  meta- and raw data reads.
 *
 *     If any reads were done, prints out two tables:
 *
 *     1. overview of raw- and metadata reads
 *         - min (smallest size read)
 *         - average of size read
 *             - k,M,G suffixes by powers of 1024 (2^10)
 *         - max (largest size read)
 *     2. tabulation of "bins", sepraring reads into exponentially-larger
 *        ranges of size.
 *         - columns for number of reads, total bytes, and average size, with
 *           separate sub-colums for raw- and metadata reads.
 *         - each row represents one bin, identified by the top of its range
 *
 *     Bins without any reads in their bounds are not printed.
 *
 *     An "overflow" bin is also present, to catch "big" reads.
 *
 *     Output for all bins (and range ceiling and average size report) is
 *     divied by powers of 1024. By corollary, four digits before the decimal
 *     is valid.
 *
 *     - 41080 bytes is represented by 40.177k, not 41.080k
 *     - 1004.831M represents approx. 1052642000 bytes
 *
 * Return:      SUCCEED/FAIL
 *----------------------------------------------------------------------------
 */
static herr_t
H5FD__ros3_print_stats(FILE *stream, const H5FD_ros3_t *file)
{
    herr_t        ret_value    = SUCCEED;
    parsed_url_t *purl         = NULL;
    unsigned      i            = 0;
    unsigned long count_meta   = 0;
    unsigned long count_raw    = 0;
    double        average_meta = 0.0;
    double        average_raw  = 0.0;
    uint64_t      min_meta     = 0;
    uint64_t      min_raw      = 0;
    uint64_t      max_meta     = 0;
    uint64_t      max_raw      = 0;
    uint64_t      bytes_raw    = 0;
    uint64_t      bytes_meta   = 0;
    double        re_dub       = 0.0; /* reusable double variable */
    unsigned      suffix_i     = 0;
    const char    suffixes[]   = {' ', 'K', 'M', 'G', 'T', 'P'};

    FUNC_ENTER_PACKAGE

    if (stream == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file stream cannot be null");
    if (file == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file cannot be null");
    if (file->s3r_handle == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "s3 request handle cannot be null");
    if (file->s3r_handle->purl == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parsed url structure cannot be null");
    purl = file->s3r_handle->purl;

    /******************
     * PRINT FILENAME *
     ******************/

    fprintf(stream, "stats for %s://%s", purl->scheme, purl->host);
    if (purl->port != NULL && purl->port[0] != '\0')
        fprintf(stream, ":%s", purl->port);
    if (purl->query != NULL && purl->query[0] != '\0') {
        if (purl->path != NULL && purl->path[0] != '\0')
            fprintf(stream, "/%s", purl->path);
        else
            fprintf(stream, "/");
        fprintf(stream, "?%s", purl->query);
    }
    else if (purl->path != NULL && purl->path[0] != '\0') {
        fprintf(stream, "/%s", purl->path);
    }
    fprintf(stream, "\n");

    /*******************
     * AGGREGATE STATS *
     *******************/

    for (i = 0; i <= ROS3_STATS_BIN_COUNT; i++) {
        const H5FD_ros3_stats_bin_t *r = &file->raw[i];
        const H5FD_ros3_stats_bin_t *m = &file->meta[i];

        if (m->min < min_meta)
            min_meta = m->min;
        if (r->min < min_raw)
            min_raw = r->min;
        if (m->max > max_meta)
            max_meta = m->max;
        if (r->max > max_raw)
            max_raw = r->max;

        count_raw += r->count;
        count_meta += m->count;
        bytes_raw += r->bytes;
        bytes_meta += m->bytes;
    }
    if (count_raw > 0)
        average_raw = (double)bytes_raw / (double)count_raw;
    if (count_meta > 0)
        average_meta = (double)bytes_meta / (double)count_meta;

    /******************
     * PRINT OVERVIEW *
     ******************/

    fprintf(stream, "TOTAL READS: %lu  (%lu meta, %lu raw)\n", count_raw + count_meta, count_meta, count_raw);
    fprintf(stream, "TOTAL BYTES: %" PRIu64 "  (%" PRIu64 " meta, %" PRIu64 " raw)\n", bytes_raw + bytes_meta,
            bytes_meta, bytes_raw);

    if (count_raw + count_meta == 0)
        goto done;

    /*************************
     * PRINT AGGREGATE STATS *
     *************************/

    fprintf(stream, "SIZES     meta      raw\n");
    fprintf(stream, "  min ");
    if (count_meta == 0)
        fprintf(stream, "   0.000  ");
    else {
        re_dub = (double)min_meta;
        for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
            re_dub /= 1024.0;
        assert(suffix_i < sizeof(suffixes));
        fprintf(stream, "%8.3lf%c ", re_dub, suffixes[suffix_i]);
    }

    if (count_raw == 0)
        fprintf(stream, "   0.000 \n");
    else {
        re_dub = (double)min_raw;
        for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
            re_dub /= 1024.0;
        assert(suffix_i < sizeof(suffixes));
        fprintf(stream, "%8.3lf%c\n", re_dub, suffixes[suffix_i]);
    }

    fprintf(stream, "  avg ");
    re_dub = (double)average_meta;
    for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
        re_dub /= 1024.0;
    assert(suffix_i < sizeof(suffixes));
    fprintf(stream, "%8.3lf%c ", re_dub, suffixes[suffix_i]);

    re_dub = (double)average_raw;
    for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
        re_dub /= 1024.0;
    assert(suffix_i < sizeof(suffixes));
    fprintf(stream, "%8.3lf%c\n", re_dub, suffixes[suffix_i]);

    fprintf(stream, "  max ");
    re_dub = (double)max_meta;
    for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
        re_dub /= 1024.0;
    assert(suffix_i < sizeof(suffixes));
    fprintf(stream, "%8.3lf%c ", re_dub, suffixes[suffix_i]);

    re_dub = (double)max_raw;
    for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
        re_dub /= 1024.0;
    assert(suffix_i < sizeof(suffixes));
    fprintf(stream, "%8.3lf%c\n", re_dub, suffixes[suffix_i]);

    /******************************
     * PRINT INDIVIDUAL BIN STATS *
     ******************************/

    fprintf(stream, "BINS             # of reads      total bytes         average size\n");
    fprintf(stream, "    up-to      meta     raw     meta      raw       meta      raw\n");

    for (i = 0; i <= ROS3_STATS_BIN_COUNT; i++) {
        const H5FD_ros3_stats_bin_t *m;
        const H5FD_ros3_stats_bin_t *r;
        uint64_t                     range_end = 0;
        char                         bm_suffix = ' '; /* bytes-meta */
        double                       bm_val    = 0.0;
        char                         br_suffix = ' '; /* bytes-raw */
        double                       br_val    = 0.0;
        char                         am_suffix = ' '; /* average-meta */
        double                       am_val    = 0.0;
        char                         ar_suffix = ' '; /* average-raw */
        double                       ar_val    = 0.0;

        m = &file->meta[i];
        r = &file->raw[i];
        if (r->count == 0 && m->count == 0)
            continue;

        range_end = ros3_stats_boundaries_g[i];

        if (i == ROS3_STATS_BIN_COUNT) {
            range_end = ros3_stats_boundaries_g[i - 1];
            fprintf(stream, ">");
        }
        else
            fprintf(stream, " ");

        bm_val = (double)m->bytes;
        for (suffix_i = 0; bm_val >= 1024.0; suffix_i++)
            bm_val /= 1024.0;
        assert(suffix_i < sizeof(suffixes));
        bm_suffix = suffixes[suffix_i];

        br_val = (double)r->bytes;
        for (suffix_i = 0; br_val >= 1024.0; suffix_i++)
            br_val /= 1024.0;
        assert(suffix_i < sizeof(suffixes));
        br_suffix = suffixes[suffix_i];

        if (m->count > 0)
            am_val = (double)(m->bytes) / (double)(m->count);
        for (suffix_i = 0; am_val >= 1024.0; suffix_i++)
            am_val /= 1024.0;
        assert(suffix_i < sizeof(suffixes));
        am_suffix = suffixes[suffix_i];

        if (r->count > 0)
            ar_val = (double)(r->bytes) / (double)(r->count);
        for (suffix_i = 0; ar_val >= 1024.0; suffix_i++)
            ar_val /= 1024.0;
        assert(suffix_i < sizeof(suffixes));
        ar_suffix = suffixes[suffix_i];

        re_dub = (double)range_end;
        for (suffix_i = 0; re_dub >= 1024.0; suffix_i++)
            re_dub /= 1024.0;
        assert(suffix_i < sizeof(suffixes));

        fprintf(stream, " %8.3f%c %7" PRIu64 " %7" PRIu64 " %8.3f%c %8.3f%c %8.3f%c %8.3f%c\n", re_dub,
                suffixes[suffix_i], /* Bin ceiling      */
                m->count,           /* Metadata reads   */
                r->count,           /* Raw data reads    */
                bm_val, bm_suffix,  /* Metadata bytes   */
                br_val, br_suffix,  /* Raw data bytes    */
                am_val, am_suffix,  /* Metadata average */
                ar_val, ar_suffix); /* Raw data average  */

        fflush(stream);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD__ros3_print_stats */
#endif /* ROS3_STATS */

#endif /* H5_HAVE_ROS3_VFD */
