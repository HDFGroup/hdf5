/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose: An initial implementation of a subfiling VFD which is
 *          derived from other "stacked" VFDs such as the splitter,
 *          mirror, and family VFDs.
 */

#include "H5FDdrvr_module.h" /* This source code file is part of the H5FD driver module */

#include "H5private.h"          /* Generic Functions        */
#include "H5CXprivate.h"        /* API contexts, etc.       */
#include "H5Dprivate.h"         /* Dataset stuff            */
#include "H5Eprivate.h"         /* Error handling           */
#include "H5FDprivate.h"        /* File drivers             */
#include "H5FDsubfiling.h"      /* Subfiling file driver    */
#include "H5FDsubfiling_priv.h" /* Subfiling file driver    */
#include "H5FDsec2.h"           /* Sec2 VFD                 */
#include "H5FLprivate.h"        /* Free Lists               */
#include "H5Fprivate.h"         /* File access              */
#include "H5Iprivate.h"         /* IDs                      */
#include "H5MMprivate.h"        /* Memory management        */
#include "H5Pprivate.h"         /* Property lists           */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_SUBFILING_g = H5I_INVALID_HID;

/* Whether the driver initialized MPI on its own */
static bool H5FD_mpi_self_initialized = false;

/* The description of a file belonging to this driver. The 'eoa' and 'eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying filesystem file). The
 * 'pos' value is used to eliminate file position updates when they would be a
 * no-op. Unfortunately we've found systems that use separate file position
 * indicators for reading and writing so the lseek can only be eliminated if
 * the current operation is the same as the previous operation.  When opening
 * a file the 'eof' will be set to the current file size, `eoa' will be set
 * to zero, 'pos' will be set to H5F_ADDR_UNDEF (as it is when an error
 * occurs), and 'op' will be set to H5F_OP_UNKNOWN.
 */
/***************************************************************************
 *
 * Structure: H5FD_subfiling_t
 *
 * Purpose:
 *
 *     H5FD_subfiling_t is a structure used to store all information needed
 *     to setup, manage, and take down subfiling for a HDF5 file.
 *
 *     This structure is created when such a file is "opened" and
 *     discarded when it is "closed".
 *
 *     Presents a system of subfiles as a single file to the HDF5 library.
 *
 *
 * `pub` (H5FD_t)
 *
 *     Instance of H5FD_t which contains all fields common to all VFDs.
 *     It must be the first item in this structure, since at higher levels,
 *     this structure will be treated as an instance of H5FD_t.
 *
 * `fa` (H5FD_subfiling_config_t)
 *
 *     Instance of `H5FD_subfiling_config_t` containing the subfiling
 *     configuration data needed to "open" the HDF5 file.
 *
 *
 *  Document additional subfiling fields here.
 *
 *  Recall that the existing fields are inherited from the sec2 driver
 *  and should be kept or not as appropriate for the sub-filing VFD.
 *
 *
 ***************************************************************************/

typedef struct H5FD_subfiling_t {
    H5FD_t                  pub; /* public stuff, must be first      */
    H5FD_subfiling_config_t fa;  /* driver-specific file access properties */

    /* MPI Info */
    MPI_Comm comm;
    MPI_Comm ext_comm;
    MPI_Info info;
    int      mpi_rank;
    int      mpi_size;

    H5FD_t *sf_file;
    H5FD_t *stub_file;

    uint64_t file_id;
    int64_t  context_id; /* The value used to lookup a subfiling context for the file */

    bool fail_to_encode; /* Used to check for failures from sb_get_size routine */

    char *file_dir;  /* Directory where we find files */
    char *file_path; /* The user defined filename */

    /*
     * The element layouts above this point are identical with the
     * H5FD_ioc_t structure. As a result,
     *
     * Everything which follows is unique to the H5FD_subfiling_t
     */
    haddr_t        eoa;                             /* end of allocated region                    */
    haddr_t        eof;                             /* end of file; current file size             */
    haddr_t        last_eoa;                        /* Last known end-of-address marker           */
    haddr_t        local_eof;                       /* Local end-of-file address for each process */
    haddr_t        pos;                             /* current file I/O position                  */
    H5FD_file_op_t op;                              /* last operation                             */
    char           filename[H5FD_MAX_FILENAME_LEN]; /* Copy of file name from open operation */
} H5FD_subfiling_t;

typedef enum H5FD_subfiling_io_type_t {
    IO_TYPE_WRITE,
    IO_TYPE_READ,
} H5FD_subfiling_io_type_t;

/*
 * These macros check for overflow of various quantities.  These macros
 * assume that HDoff_t is signed and haddr_t and size_t are unsigned.
 *
 * ADDR_OVERFLOW:   Checks whether a file address of type `haddr_t'
 *                  is too large to be represented by the second argument
 *                  of the file seek function.
 *
 * SIZE_OVERFLOW:   Checks whether a buffer size of type `hsize_t' is too
 *                  large to be represented by the `size_t' type.
 *
 * REGION_OVERFLOW: Checks whether an address and size pair describe data
 *                  which can be addressed entirely by the second
 *                  argument of the file seek function.
 */
#define MAXADDR          (((haddr_t)1 << (8 * sizeof(HDoff_t) - 1)) - 1)
#define ADDR_OVERFLOW(A) (HADDR_UNDEF == (A) || ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z) ((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A, Z)                                                                                \
    (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) || HADDR_UNDEF == (A) + (Z) || (HDoff_t)((A) + (Z)) < (HDoff_t)(A))

/*
 * NOTE: Must be kept in sync with the private
 * H5F_MAX_DRVINFOBLOCK_SIZE macro value for now
 */
#define H5FD_SUBFILING_MAX_DRV_INFO_SIZE 1024

/* Prototypes */
static herr_t  H5FD__subfiling_term(void);
static hsize_t H5FD__subfiling_sb_size(H5FD_t *_file);
static herr_t  H5FD__subfiling_sb_encode(H5FD_t *_file, char *name, unsigned char *buf);
static herr_t  H5FD__subfiling_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf);
static void   *H5FD__subfiling_fapl_get(H5FD_t *_file);
static void   *H5FD__subfiling_fapl_copy(const void *_old_fa);
static herr_t  H5FD__subfiling_fapl_free(void *_fa);
static H5FD_t *H5FD__subfiling_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t  H5FD__subfiling_close(H5FD_t *_file);
static int     H5FD__subfiling_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t  H5FD__subfiling_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD__subfiling_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD__subfiling_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD__subfiling_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD__subfiling_get_handle(H5FD_t *_file, hid_t fapl, void **file_handle);
static herr_t  H5FD__subfiling_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                                    void *buf);
static herr_t  H5FD__subfiling_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                                     const void *buf);
static herr_t  H5FD__subfiling_read_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                           haddr_t addrs[], size_t sizes[], void *bufs[] /* out */);
static herr_t  H5FD__subfiling_write_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                            haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */);
static herr_t  H5FD__subfiling_truncate(H5FD_t *_file, hid_t dxpl_id, bool closing);
#if 0
static herr_t  H5FD__subfiling_lock(H5FD_t *_file, bool rw);
static herr_t  H5FD__subfiling_unlock(H5FD_t *_file);
#endif
static herr_t H5FD__subfiling_del(const char *name, hid_t fapl);
static herr_t H5FD__subfiling_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags, const void *input,
                                  void **output);

static herr_t H5FD__subfiling_get_default_config(hid_t fapl_id, H5FD_subfiling_config_t *config_out);
static herr_t H5FD__subfiling_validate_config(const H5FD_subfiling_config_t *fa);
static int    H5FD__copy_plist(hid_t fapl_id, hid_t *id_out_ptr);

static herr_t H5FD__subfiling_close_int(H5FD_subfiling_t *file_ptr);

static herr_t H5FD__subfiling_io_helper(H5FD_subfiling_t *file_ptr, size_t io_count, H5FD_mem_t types[],
                                        haddr_t addrs[], size_t sizes[], H5_flexible_const_ptr_t bufs[],
                                        H5FD_subfiling_io_type_t io_type);
static herr_t H5FD__subfiling_mirror_writes_to_stub(H5FD_subfiling_t *file_ptr, uint32_t count,
                                                    H5FD_mem_t types[], haddr_t addrs[], size_t sizes[],
                                                    const void *bufs[]);
static herr_t generate_io_vectors(subfiling_context_t *sf_context, size_t in_count, H5FD_mem_t types[],
                                  haddr_t file_offsets[], size_t nelemts[], H5_flexible_const_ptr_t bufs[],
                                  size_t dtype_extent, H5FD_subfiling_io_type_t io_type, size_t *ioreq_count,
                                  uint32_t *iovec_len, H5FD_mem_t **io_types, haddr_t **io_addrs,
                                  size_t **io_sizes, H5_flexible_const_ptr_t **io_bufs);
static void   get_iovec_sizes(subfiling_context_t *sf_context, size_t in_count, haddr_t file_offsets[],
                              size_t nelemts[], size_t dtype_extent, size_t *max_iovec_depth,
                              size_t *max_num_subfiles);
static herr_t translate_io_req_to_iovec(subfiling_context_t *sf_context, size_t iovec_idx, size_t iovec_len,
                                        size_t iovec_count, H5FD_mem_t type, haddr_t addr, size_t io_size,
                                        H5_flexible_const_ptr_t io_buf, H5FD_subfiling_io_type_t io_type,
                                        H5FD_mem_t *io_types, haddr_t *io_addrs, size_t *io_sizes,
                                        H5_flexible_const_ptr_t *io_bufs);
static herr_t iovec_fill_first(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                               int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                               int64_t first_io_len, H5_flexible_const_ptr_t buf,
                               H5FD_subfiling_io_type_t io_type, haddr_t *io_addrs_ptr, size_t *io_sizes_ptr,
                               H5_flexible_const_ptr_t *io_bufs_ptr);
static herr_t iovec_fill_last(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                              int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                              int64_t last_io_len, H5_flexible_const_ptr_t buf,
                              H5FD_subfiling_io_type_t io_type, haddr_t *io_addrs_ptr, size_t *io_sizes_ptr,
                              H5_flexible_const_ptr_t *io_bufs_ptr);
static herr_t iovec_fill_first_last(subfiling_context_t *sf_context, size_t iovec_len,
                                    int64_t cur_iovec_depth, int64_t target_datasize,
                                    int64_t start_mem_offset, int64_t start_file_offset, int64_t first_io_len,
                                    int64_t last_io_len, H5_flexible_const_ptr_t buf,
                                    H5FD_subfiling_io_type_t io_type, haddr_t *io_addrs_ptr,
                                    size_t *io_sizes_ptr, H5_flexible_const_ptr_t *io_bufs_ptr);
static herr_t iovec_fill_uniform(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                                 int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                                 H5_flexible_const_ptr_t buf, H5FD_subfiling_io_type_t io_type,
                                 haddr_t *io_addrs_ptr, size_t *io_sizes_ptr,
                                 H5_flexible_const_ptr_t *io_bufs_ptr);

#ifdef H5_SUBFILING_DEBUG
void H5_subfiling_dump_iovecs(subfiling_context_t *sf_context, size_t ioreq_count, size_t iovec_len,
                              H5FD_subfiling_io_type_t io_type, H5FD_mem_t *io_types, haddr_t *io_addrs,
                              size_t *io_sizes, H5_flexible_const_ptr_t *io_bufs);
#endif

void H5FD__subfiling_mpi_finalize(void);

static const H5FD_class_t H5FD_subfiling_g = {
    H5FD_CLASS_VERSION,                /* VFD interface version */
    H5_VFD_SUBFILING,                  /* value                 */
    H5FD_SUBFILING_NAME,               /* name                  */
    MAXADDR,                           /* maxaddr               */
    H5F_CLOSE_WEAK,                    /* fc_degree             */
    H5FD__subfiling_term,              /* terminate             */
    H5FD__subfiling_sb_size,           /* sb_size               */
    H5FD__subfiling_sb_encode,         /* sb_encode             */
    H5FD__subfiling_sb_decode,         /* sb_decode             */
    sizeof(H5FD_subfiling_config_t),   /* fapl_size             */
    H5FD__subfiling_fapl_get,          /* fapl_get              */
    H5FD__subfiling_fapl_copy,         /* fapl_copy             */
    H5FD__subfiling_fapl_free,         /* fapl_free             */
    0,                                 /* dxpl_size             */
    NULL,                              /* dxpl_copy             */
    NULL,                              /* dxpl_free             */
    H5FD__subfiling_open,              /* open                  */
    H5FD__subfiling_close,             /* close                 */
    H5FD__subfiling_cmp,               /* cmp                   */
    H5FD__subfiling_query,             /* query                 */
    NULL,                              /* get_type_map          */
    NULL,                              /* alloc                 */
    NULL,                              /* free                  */
    H5FD__subfiling_get_eoa,           /* get_eoa               */
    H5FD__subfiling_set_eoa,           /* set_eoa               */
    H5FD__subfiling_get_eof,           /* get_eof               */
    H5FD__subfiling_get_handle,        /* get_handle            */
    H5FD__subfiling_read,              /* read                  */
    H5FD__subfiling_write,             /* write                 */
    H5FD__subfiling_read_vector,       /* read_vector           */
    H5FD__subfiling_write_vector,      /* write_vector          */
    NULL,                              /* read_selection        */
    NULL,                              /* write_selection       */
    NULL,                              /* flush                 */
    H5FD__subfiling_truncate,          /* truncate              */
    NULL /* H5FD__subfiling_lock */,   /* lock                  */
    NULL /* H5FD__subfiling_unlock */, /* unlock                */
    H5FD__subfiling_del,               /* del                   */
    H5FD__subfiling_ctl,               /* ctl                   */
    H5FD_FLMAP_DICHOTOMY               /* fl_map                */
};

/* Declare a free list to manage the H5FD_subfiling_t struct */
H5FL_DEFINE_STATIC(H5FD_subfiling_t);

/*
 * If this VFD initialized MPI, this routine will be registered
 * as an atexit handler in order to finalize MPI before the
 * application exits.
 */
void
H5FD__subfiling_mpi_finalize(void)
{
    H5close();
    MPI_Finalize();
}

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:    The driver ID for the subfiling driver
 *              Failure:    H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_subfiling_init(void)
{
    hid_t ret_value = H5I_INVALID_HID; /* Return value */

    /* Register the Subfiling VFD, if it isn't already registered */
    if (H5I_VFL != H5I_get_type(H5FD_SUBFILING_g)) {
        int mpi_initialized = 0;
        int provided        = 0;
        int mpi_code;

        if ((H5FD_SUBFILING_g = H5FD_register(&H5FD_subfiling_g, sizeof(H5FD_class_t), false)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_ID, H5E_CANTREGISTER, H5I_INVALID_HID,
                                    "can't register subfiling VFD");

        /* Initialize error reporting */
        if ((H5subfiling_err_stack_g = H5Ecreate_stack()) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID, "can't create HDF5 error stack");
        if ((H5subfiling_err_class_g = H5Eregister_class(H5SUBFILING_ERR_CLS_NAME, H5SUBFILING_ERR_LIB_NAME,
                                                         H5SUBFILING_ERR_VER)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                    "can't register error class with HDF5 error API");

        /* Initialize MPI if not already initialized */
        if (MPI_SUCCESS != (mpi_code = MPI_Initialized(&mpi_initialized)))
            H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Initialized failed", mpi_code);
        if (mpi_initialized) {
            /* If MPI is initialized, validate that it was initialized with MPI_THREAD_MULTIPLE */
            if (MPI_SUCCESS != (mpi_code = MPI_Query_thread(&provided)))
                H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Query_thread failed", mpi_code);
            if (provided != MPI_THREAD_MULTIPLE)
                H5_SUBFILING_GOTO_ERROR(
                    H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                    "Subfiling VFD requires the use of MPI_Init_thread with MPI_THREAD_MULTIPLE");
        }
        else {
            int required = MPI_THREAD_MULTIPLE;

            if (MPI_SUCCESS != (mpi_code = MPI_Init_thread(NULL, NULL, required, &provided)))
                H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Init_thread failed", mpi_code);

            H5FD_mpi_self_initialized = true;

            if (provided != required)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                        "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE");

            if (atexit(H5FD__subfiling_mpi_finalize) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                        "can't register atexit handler for MPI_Finalize");
        }

        /*
         * Create the MPI Datatype that will be used
         * for sending/receiving RPC messages
         */
        HDcompile_assert(sizeof(((sf_work_request_t *)NULL)->header) == 3 * sizeof(int64_t));
        if (H5_subfiling_rpc_msg_type == MPI_DATATYPE_NULL) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous(3, MPI_INT64_T, &H5_subfiling_rpc_msg_type)))
                H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Type_contiguous failed", mpi_code);
            if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(&H5_subfiling_rpc_msg_type)))
                H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Type_commit failed", mpi_code);
        }
    }

    /* Set return value */
    ret_value = H5FD_SUBFILING_g;

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD_subfiling_init() */

/*---------------------------------------------------------------------------
 * Function:    H5FD__subfiling_term
 *
 * Purpose:     Shut down the VFD
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_term(void)
{
    herr_t ret_value = SUCCEED;

    if (H5FD_SUBFILING_g >= 0) {
        int mpi_finalized;
        int mpi_code;

        /*
         * Retrieve status of whether MPI has already been terminated.
         * This can happen if an HDF5 ID is left unclosed and HDF5
         * shuts down after MPI_Finalize() is called in an application.
         */
        if (MPI_SUCCESS != (mpi_code = MPI_Finalized(&mpi_finalized)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalized failed", mpi_code);

        /* Free RPC message MPI Datatype */
        if (H5_subfiling_rpc_msg_type != MPI_DATATYPE_NULL) {
            if (!mpi_finalized) {
                if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&H5_subfiling_rpc_msg_type)))
                    H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Type_free failed", mpi_code);
            }
#ifdef H5_SUBFILING_DEBUG
            else
                printf("** WARNING **: HDF5 is terminating the Subfiling VFD after MPI_Finalize() was "
                       "called - an HDF5 ID was probably left unclosed\n");
#endif
        }

        /* Clean up resources */
        if (H5_subfiling_terminate() < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL,
                                    "can't cleanup internal subfiling resources");

        /* Unregister from HDF5 error API */
        if (H5subfiling_err_class_g >= 0) {
            if (H5Eunregister_class(H5subfiling_err_class_g) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CLOSEERROR, FAIL,
                                        "can't unregister error class from HDF5 error API");
        }
        if (H5subfiling_err_stack_g >= 0) {
            /* Print the current error stack before destroying it */
            PRINT_ERROR_STACK;

            /* Destroy the error stack */
            if (H5Eclose_stack(H5subfiling_err_stack_g) < 0) {
                H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CLOSEERROR, FAIL, "can't close HDF5 error stack");
                PRINT_ERROR_STACK;
            } /* end if */

            H5subfiling_err_stack_g = H5I_INVALID_HID;
            H5subfiling_err_class_g = H5I_INVALID_HID;
        }
    }

done:
    /* Reset VFL ID */
    H5FD_SUBFILING_g = H5I_INVALID_HID;

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_term() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_subfiling
 *
 * Purpose:     Modify the file access property list to use the
 *              H5FD_SUBFILING driver defined in this source file.  All
 *              driver specific properties are passed in as a pointer to
 *              a suitably initialized instance of H5FD_subfiling_config_t.
 *              If NULL is passed for the H5FD_subfiling_config_t
 *              structure, a default structure will be used instead.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_subfiling(hid_t fapl_id, const H5FD_subfiling_config_t *vfd_config)
{
    H5FD_subfiling_config_t *subfiling_conf = NULL;
    H5P_genplist_t          *plist          = NULL;
    H5P_genplist_t          *ioc_plist      = NULL;
    MPI_Comm                 comm           = MPI_COMM_NULL;
    MPI_Info                 info           = MPI_INFO_NULL;
    herr_t                   ret_value      = SUCCEED;

    /*NO TRACE*/

    /* Ensure Subfiling (and therefore MPI) is initialized before doing anything */
    if (H5FD_subfiling_init() < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize subfiling VFD");

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (vfd_config == NULL) {
        if (NULL == (subfiling_conf = calloc(1, sizeof(*subfiling_conf))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate subfiling VFD configuration");
        subfiling_conf->ioc_fapl_id = H5I_INVALID_HID;

        /* Get subfiling VFD defaults */
        if (H5FD__subfiling_get_default_config(fapl_id, subfiling_conf) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL,
                                    "can't get default subfiling VFD configuration");

        vfd_config = subfiling_conf;
    }

    /* Check if any MPI parameters were set on the FAPL */
    if (H5P_get(plist, H5F_ACS_MPI_PARAMS_COMM_NAME, &comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI communicator from plist");
    if (H5P_get(plist, H5F_ACS_MPI_PARAMS_INFO_NAME, &info) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI info from plist");
    if (comm == MPI_COMM_NULL)
        comm = MPI_COMM_WORLD;

    /* Set MPI parameters on IOC FAPL */
    if (NULL == (ioc_plist = H5P_object_verify(vfd_config->ioc_fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5P_set(ioc_plist, H5F_ACS_MPI_PARAMS_COMM_NAME, &comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI communicator on plist");
    if (H5P_set(ioc_plist, H5F_ACS_MPI_PARAMS_INFO_NAME, &info) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI info on plist");

    if (H5FD__subfiling_validate_config(vfd_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid subfiling VFD configuration");

    /* Set Subfiling configuration on IOC FAPL */
    if (H5_subfiling_set_config_prop(ioc_plist, &vfd_config->shared_cfg) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL,
                                "can't set subfiling configuration on IOC FAPL");

    ret_value = H5P_set_driver(plist, H5FD_SUBFILING, vfd_config, NULL);

done:
    if (H5_mpi_comm_free(&comm) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI Communicator");
    if (H5_mpi_info_free(&info) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI Info object");

    if (subfiling_conf) {
        if (subfiling_conf->ioc_fapl_id >= 0 && H5I_dec_ref(subfiling_conf->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTDEC, FAIL, "can't close IOC FAPL");
        free(subfiling_conf);
    }

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5Pset_fapl_subfiling() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_subfiling
 *
 * Purpose:     Returns information about the subfiling file access
 *              property list though the function arguments.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *config_out)
{
    const H5FD_subfiling_config_t *config_ptr         = NULL;
    H5P_genplist_t                *plist              = NULL;
    bool                           use_default_config = false;
    herr_t                         ret_value          = SUCCEED;

    /*NO TRACE*/

    if (config_out == NULL)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL");

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (H5FD_SUBFILING != H5P_peek_driver(plist))
        use_default_config = true;
    else {
        config_ptr = H5P_peek_driver_info(plist);
        if (NULL == config_ptr)
            use_default_config = true;
    }

    if (use_default_config) {
        if (H5FD__subfiling_get_default_config(fapl_id, config_out) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,
                                    "can't get default Subfiling VFD configuration");
    }
    else {
        /* Copy the subfiling fapl data out */
        H5MM_memcpy(config_out, config_ptr, sizeof(H5FD_subfiling_config_t));

        /* Copy the driver info value */
        if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(config_out->ioc_fapl_id)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't copy IOC FAPL");
    }

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5Pget_fapl_subfiling() */

static herr_t
H5FD__subfiling_get_default_config(hid_t fapl_id, H5FD_subfiling_config_t *config_out)
{
    MPI_Comm comm = MPI_COMM_NULL;
    MPI_Info info = MPI_INFO_NULL;
    char    *h5_require_ioc;
    herr_t   ret_value = SUCCEED;

    assert(config_out);

    memset(config_out, 0, sizeof(*config_out));

    config_out->magic       = H5FD_SUBFILING_FAPL_MAGIC;
    config_out->version     = H5FD_SUBFILING_CURR_FAPL_VERSION;
    config_out->ioc_fapl_id = H5I_INVALID_HID;
    config_out->require_ioc = true;

    config_out->shared_cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    config_out->shared_cfg.stripe_size   = H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;
    config_out->shared_cfg.stripe_count  = H5FD_SUBFILING_DEFAULT_STRIPE_COUNT;

    if ((h5_require_ioc = getenv("H5_REQUIRE_IOC")) != NULL) {
        int value_check = atoi(h5_require_ioc);
        if (value_check == 0)
            config_out->require_ioc = false;
    }

    /* Check if any MPI parameters were set on the FAPL */
    if (H5Pget_mpi_params(fapl_id, &comm, &info) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI Comm/Info");
    if (comm == MPI_COMM_NULL) {
        comm = MPI_COMM_WORLD;

        /* Set MPI_COMM_WORLD on FAPL if no MPI parameters were set */
        if (H5Pset_mpi_params(fapl_id, comm, info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set MPI Comm/Info");
    }

    /* Create a default FAPL and choose an appropriate underlying driver */
    if ((config_out->ioc_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "can't create default FAPL");

    if (config_out->require_ioc) {
        if (H5Pset_mpi_params(config_out->ioc_fapl_id, comm, info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get MPI Comm/Info on IOC FAPL");

        if (H5Pset_fapl_ioc(config_out->ioc_fapl_id, NULL) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set IOC VFD on IOC FAPL");
    }
    else {
        if (H5Pset_fapl_sec2(config_out->ioc_fapl_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set Sec2 VFD on IOC FAPL");
    }

done:
    if (H5_mpi_comm_free(&comm) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI Communicator");
    if (H5_mpi_info_free(&info) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI Info object");

    if (ret_value < 0) {
        if (config_out->ioc_fapl_id >= 0 && H5Pclose(config_out->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, FAIL, "can't close FAPL");
        config_out->ioc_fapl_id = H5I_INVALID_HID;
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_validate_config()
 *
 * Purpose:     Test to see if the supplied instance of
 *              H5FD_subfiling_config_t contains internally consistent data.
 *              Return SUCCEED if so, and FAIL otherwise.
 *
 *              Note the difference between internally consistent and
 *              correct.  As we will have to try to setup subfiling to
 *              determine whether the supplied data is correct,
 *              we will settle for internal consistency at this point
 *
 * Return:      SUCCEED if instance of H5FD_subfiling_config_t contains
 *              internally consistent data, FAIL otherwise.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_validate_config(const H5FD_subfiling_config_t *fa)
{
    herr_t ret_value = SUCCEED;

    assert(fa != NULL);

    if (fa->version != H5FD_SUBFILING_CURR_FAPL_VERSION)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown H5FD_subfiling_config_t version");

    if (fa->magic != H5FD_SUBFILING_FAPL_MAGIC)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid H5FD_subfiling_config_t magic value");

    if (fa->ioc_fapl_id < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid IOC FAPL ID");

    if (!fa->require_ioc)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "Subfiling VFD currently always requires IOC VFD to be used");

    if (H5_subfiling_validate_config(&fa->shared_cfg) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid subfiling configuration parameters");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__subfiling_validate_config() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_sb_size
 *
 * Purpose:     Returns the size of the subfiling configuration information
 *              to be stored in the superblock.
 *
 * Return:      Size of subfiling configuration information (never fails)
 *-------------------------------------------------------------------------
 */
static hsize_t
H5FD__subfiling_sb_size(H5FD_t *_file)
{
    subfiling_context_t *sf_context = NULL;
    H5FD_subfiling_t    *file       = (H5FD_subfiling_t *)_file;
    hsize_t              ret_value  = 0;

    assert(file);

    /* Configuration structure magic number */
    ret_value += sizeof(uint32_t);

    /* Configuration structure version number */
    ret_value += sizeof(uint32_t);

    /* "Require IOC" field */
    ret_value += sizeof(int32_t);

    /* Subfiling stripe size */
    ret_value += sizeof(int64_t);

    /* Subfiling stripe count (encoded as int64_t for future) */
    ret_value += sizeof(int64_t);

    /* Subfiling config file prefix string length */
    ret_value += sizeof(uint64_t);

    /*
     * Since this callback currently can't return any errors, we
     * will set the "fail to encode" flag on the file if we fail
     * to retrieve the context object here so we can check for
     * errors later.
     */
    if (NULL == (sf_context = H5_get_subfiling_object(file->context_id))) {
        file->fail_to_encode = true;
    }
    else {
        if (sf_context->config_file_prefix) {
            ret_value += strlen(sf_context->config_file_prefix) + 1;
        }
    }

    /* Add superblock information from IOC file if necessary */
    if (file->sf_file) {
        /* Encode the IOC's name into the subfiling information */
        ret_value += 9;

        ret_value += H5FD_sb_size(file->sf_file);
    }

    /*
     * Since the library doesn't currently properly check this,
     * set the "fail to encode" flag if the message size is
     * larger than the library's currently accepted max message
     * size so that we don't try to encode the message and overrun
     * a buffer.
     */
    if (ret_value > H5FD_SUBFILING_MAX_DRV_INFO_SIZE)
        file->fail_to_encode = true;

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__subfiling_sb_size() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_sb_encode
 *
 * Purpose:     Encodes the subfiling configuration information into the
 *              specified buffer.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_sb_encode(H5FD_t *_file, char *name, unsigned char *buf)
{
    subfiling_context_t *sf_context = NULL;
    H5FD_subfiling_t    *file       = (H5FD_subfiling_t *)_file;
    uint8_t             *p          = (uint8_t *)buf;
    uint64_t             tmpu64;
    int64_t              tmp64;
    int32_t              tmp32;
    size_t               prefix_len = 0;
    herr_t               ret_value  = SUCCEED;

    /* Check if the "fail to encode flag" is set */
    if (file->fail_to_encode)
        H5_SUBFILING_GOTO_ERROR(
            H5E_VFL, H5E_CANTENCODE, FAIL,
            "can't encode subfiling driver info message - message was too large or internal error occurred");

    if (NULL == (sf_context = H5_get_subfiling_object(file->context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get subfiling context object");

    /* Encode driver name */
    strncpy(name, "Subfilin", 9);
    name[8] = '\0';

    /* Encode configuration structure magic number */
    UINT32ENCODE(p, file->fa.magic);

    /* Encode configuration structure version number */
    UINT32ENCODE(p, file->fa.version);

    /* Encode "require IOC" field */
    tmp32 = (int32_t)file->fa.require_ioc;
    INT32ENCODE(p, tmp32);

    /* Encode subfiling stripe size */
    INT64ENCODE(p, sf_context->sf_stripe_size);

    /* Encode subfiling stripe count (number of subfiles) */
    tmp64 = sf_context->sf_num_subfiles;
    INT64ENCODE(p, tmp64);

    /* Encode config file prefix string length */
    if (sf_context->config_file_prefix) {
        prefix_len = strlen(sf_context->config_file_prefix) + 1;
        H5_CHECKED_ASSIGN(tmpu64, uint64_t, prefix_len, size_t);
    }
    else
        tmpu64 = 0;
    UINT64ENCODE(p, tmpu64);

    /* Encode config file prefix string */
    if (sf_context->config_file_prefix) {
        H5MM_memcpy(p, sf_context->config_file_prefix, prefix_len);
        p += prefix_len;
    }

    /* Encode IOC VFD configuration information if necessary */
    if (file->sf_file) {
        char ioc_name[9];

        memset(ioc_name, 0, sizeof(ioc_name));

        if (H5FD_sb_encode(file->sf_file, ioc_name, p + 9) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTENCODE, FAIL,
                                    "unable to encode IOC VFD's superblock information");

        /* Copy the IOC VFD's name into our buffer */
        H5MM_memcpy(p, ioc_name, 9);
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__subfiling_sb_encode() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_sb_decode
 *
 * Purpose:     Decodes the subfiling configuration information from the
 *              specified buffer.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    subfiling_context_t *sf_context = NULL;
    H5FD_subfiling_t    *file       = (H5FD_subfiling_t *)_file;
    const uint8_t       *p          = (const uint8_t *)buf;
    uint64_t             tmpu64;
    int64_t              tmp64;
    int32_t              tmp32;
    herr_t               ret_value = SUCCEED;

    /* Check if we previously failed to encode the info */
    if (file->fail_to_encode)
        H5_SUBFILING_GOTO_ERROR(
            H5E_VFL, H5E_CANTDECODE, FAIL,
            "can't decode subfiling driver info message - message wasn't encoded (or encoded improperly)");

    if (NULL == (sf_context = H5_get_subfiling_object(file->context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get subfiling context object");

    if (strncmp(name, "Subfilin", 9))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid driver name in superblock");

    /* Decode configuration structure magic number */
    UINT32DECODE(p, file->fa.magic);

    /* Decode configuration structure version number */
    UINT32DECODE(p, file->fa.version);

    /* Decode "require IOC" field */
    INT32DECODE(p, tmp32);
    file->fa.require_ioc = (bool)tmp32;

    /* Decode subfiling stripe size */
    INT64DECODE(p, file->fa.shared_cfg.stripe_size);

    /* Decode subfiling stripe count */
    INT64DECODE(p, tmp64);
    H5_CHECK_OVERFLOW(tmp64, int64_t, int32_t);
    file->fa.shared_cfg.stripe_count = (int32_t)tmp64;

    /* Decode config file prefix string length */
    UINT64DECODE(p, tmpu64);

    /* Decode config file prefix string */
    if (tmpu64 > 0) {
        if (!sf_context->config_file_prefix) {
            if (NULL == (sf_context->config_file_prefix = malloc(tmpu64)))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate space for config file prefix string");

            H5MM_memcpy(sf_context->config_file_prefix, p, tmpu64);

            /* Just in case... */
            sf_context->config_file_prefix[tmpu64 - 1] = '\0';
        }

        p += tmpu64;
    }

    if (file->sf_file) {
        char ioc_name[9];

        H5MM_memcpy(ioc_name, p, 9);
        p += 9;

        if (H5FD_sb_load(file->sf_file, ioc_name, p) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL,
                                    "unable to decode IOC VFD's superblock information");
    }

    /* Validate the decoded configuration */
    if (H5FD__subfiling_validate_config(&file->fa) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                                "decoded subfiling configuration info is invalid");

    if (file->fa.shared_cfg.stripe_size != sf_context->sf_stripe_size)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                                "specified subfiling stripe size (%" PRId64
                                ") doesn't match value stored in file (%" PRId64 ")",
                                sf_context->sf_stripe_size, file->fa.shared_cfg.stripe_size);

    if (file->fa.shared_cfg.stripe_count != sf_context->sf_num_subfiles)
        H5_SUBFILING_GOTO_ERROR(
            H5E_VFL, H5E_BADVALUE, FAIL,
            "specified subfiling stripe count (%d) doesn't match value stored in file (%" PRId32 ")",
            sf_context->sf_num_subfiles, file->fa.shared_cfg.stripe_count);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__subfiling_sb_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_fapl_get
 *
 * Purpose:     Gets a file access property list which could be used to
 *              create an identical file.
 *
 * Return:      Success:        Ptr to new file access property list value.
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD__subfiling_fapl_get(H5FD_t *_file)
{
    H5FD_subfiling_t        *file      = (H5FD_subfiling_t *)_file;
    H5FD_subfiling_config_t *fa        = NULL;
    void                    *ret_value = NULL;

    fa = (H5FD_subfiling_config_t *)H5MM_calloc(sizeof(H5FD_subfiling_config_t));

    if (fa == NULL) {
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    }

    /* Copy the fields of the structure */
    H5MM_memcpy(fa, &(file->fa), sizeof(H5FD_subfiling_config_t));

    /* Copy the driver info value */
    if (H5FD__copy_plist(file->fa.ioc_fapl_id, &(fa->ioc_fapl_id)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy IOC FAPL");

    /* Set return value */
    ret_value = fa;

done:
    if (ret_value == NULL) {

        if (fa != NULL) {
            H5MM_xfree(fa);
        }
    }

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__copy_plist
 *
 * Purpose:     Sanity-wrapped H5P_copy_plist() for each channel.
 *              Utility function for operation in multiple locations.
 *
 * Return:      0 on success, -1 on error.
 *-------------------------------------------------------------------------
 */
/* TODO: no need for this function */
static int
H5FD__copy_plist(hid_t fapl_id, hid_t *id_out_ptr)
{
    int             ret_value = 0;
    H5P_genplist_t *plist_ptr = NULL;

    assert(id_out_ptr != NULL);

    if (false == H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, -1, "not a file access property list");

    plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id);
    if (NULL == plist_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, -1, "unable to get property list");

    *id_out_ptr = H5P_copy_plist(plist_ptr, false);
    if (H5I_INVALID_HID == *id_out_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADTYPE, -1, "unable to copy file access property list");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__copy_plist() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_fapl_copy
 *
 * Purpose:     Copies the subfiling-specific file access properties.
 *
 * Return:      Success:        Ptr to a new property list
 *
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD__subfiling_fapl_copy(const void *_old_fa)
{
    const H5FD_subfiling_config_t *old_fa    = (const H5FD_subfiling_config_t *)_old_fa;
    H5FD_subfiling_config_t       *new_fa    = NULL;
    void                          *ret_value = NULL;

    new_fa = (H5FD_subfiling_config_t *)H5MM_malloc(sizeof(H5FD_subfiling_config_t));
    if (new_fa == NULL) {
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    }

    H5MM_memcpy(new_fa, old_fa, sizeof(H5FD_subfiling_config_t));

    if (H5FD__copy_plist(old_fa->ioc_fapl_id, &(new_fa->ioc_fapl_id)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy the IOC FAPL");

    ret_value = new_fa;

done:
    if (ret_value == NULL) {

        if (new_fa != NULL) {
            H5MM_xfree(new_fa);
        }
    }

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_fapl_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_fapl_free
 *
 * Purpose:     Frees the subfiling-specific file access properties.
 *
 * Return:      SUCCEED (cannot fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_fapl_free(void *_fa)
{
    H5FD_subfiling_config_t *fa        = (H5FD_subfiling_config_t *)_fa;
    herr_t                   ret_value = SUCCEED;

    assert(fa != NULL); /* sanity check */

    if (fa->ioc_fapl_id >= 0 && H5I_dec_ref(fa->ioc_fapl_id) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTDEC, FAIL, "can't close IOC FAPL");
    fa->ioc_fapl_id = H5I_INVALID_HID;

    H5MM_xfree(fa);

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_fapl_free() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_open
 *
 * Purpose:     Create and/or opens a file as an HDF5 file.
 *
 * Return:      Success:    A pointer to a new file data structure. The
 *                          public fields will be initialized by the
 *                          caller, which is always H5FD_open().
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__subfiling_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_subfiling_t              *file_ptr   = NULL; /* Subfiling VFD info */
    const H5FD_subfiling_config_t *config_ptr = NULL; /* Driver-specific property list */
    H5FD_subfiling_config_t        default_config;
    H5FD_class_t                  *driver    = NULL; /* VFD for file */
    H5P_genplist_t                *plist_ptr = NULL;
    H5FD_driver_prop_t             driver_prop; /* Property for driver ID & info */
    bool                           bcasted_eof = false;
    int64_t                        sf_eof      = -1;
    int                            mpi_code; /* MPI return code */
    H5FD_t                        *ret_value = NULL;

    /* Check arguments */
    if (!name || !*name)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name");
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr");
    if (ADDR_OVERFLOW(maxaddr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr");

    if (NULL == (file_ptr = (H5FD_subfiling_t *)H5FL_CALLOC(H5FD_subfiling_t)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate file struct");
    file_ptr->comm           = MPI_COMM_NULL;
    file_ptr->info           = MPI_INFO_NULL;
    file_ptr->file_id        = UINT64_MAX;
    file_ptr->context_id     = -1;
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;
    file_ptr->ext_comm       = MPI_COMM_NULL;
    file_ptr->fail_to_encode = false;

    /* Get the driver-specific file access properties */
    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");

    if (H5FD_mpi_self_initialized) {
        file_ptr->comm = MPI_COMM_WORLD;
        file_ptr->info = MPI_INFO_NULL;
    }
    else {
        /* Get the MPI communicator and info object from the property list */
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_COMM_NAME, &file_ptr->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI communicator");
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_INFO_NAME, &file_ptr->info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI info object");

        if (file_ptr->comm == MPI_COMM_NULL)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid or unset MPI communicator in FAPL");
    }

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_ptr->comm, &file_ptr->mpi_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code);
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(file_ptr->comm, &file_ptr->mpi_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code);

    /* Work around an HDF5 metadata cache bug with distributed metadata writes when MPI size == 1 */
    if (file_ptr->mpi_size == 1) {
        H5AC_cache_config_t mdc_config;

        /* Get the current initial metadata cache resize configuration */
        if (H5P_get(plist_ptr, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, &mdc_config) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get metadata cache initial config");
        mdc_config.metadata_write_strategy = H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY;
        if (H5P_set(plist_ptr, H5F_ACS_META_CACHE_INIT_CONFIG_NAME, &mdc_config) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "can't set metadata cache initial config");
    }

    config_ptr = H5P_peek_driver_info(plist_ptr);
    if (!config_ptr || (H5P_FILE_ACCESS_DEFAULT == fapl_id)) {
        if (H5FD__subfiling_get_default_config(fapl_id, &default_config) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL,
                                    "can't get default subfiling VFD configuration");
        config_ptr = &default_config;
    }

    H5MM_memcpy(&file_ptr->fa, config_ptr, sizeof(H5FD_subfiling_config_t));
    if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(file_ptr->fa.ioc_fapl_id)) < 0) {
        file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy FAPL");
    }

    /* Check the "native" driver (IOC/sec2/etc.) */
    if (NULL == (plist_ptr = H5I_object(file_ptr->fa.ioc_fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "invalid IOC FAPL");

    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver ID & info");
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL,
                                "invalid driver ID in file access property list");

    if (driver->value != H5_VFD_IOC)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                                "unable to open file '%s' - only IOC VFD is currently supported for subfiles",
                                name);

    /* Fully resolve the given filepath and get its dirname */
    if (H5_resolve_pathname(name, file_ptr->comm, &file_ptr->file_path) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't resolve filepath");
    if (H5_dirname(file_ptr->file_path, &file_ptr->file_dir) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get filepath dirname");

    /*
     * Create/open the HDF5 stub file and get its inode value for
     * the internal mapping from file inode to subfiling context.
     */
    if (H5_open_subfiling_stub_file(file_ptr->file_path, flags, file_ptr->comm, &file_ptr->stub_file,
                                    &file_ptr->file_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "can't open HDF5 stub file");

    /* Set stub file ID on IOC fapl so it can reuse on open */
    if (H5_subfiling_set_file_id_prop(plist_ptr, file_ptr->file_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, NULL, "can't set stub file ID on FAPL");

    /* Open the HDF5 file's subfiles */
    if (NULL == (file_ptr->sf_file = H5FD_open(name, flags, file_ptr->fa.ioc_fapl_id, HADDR_UNDEF)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "unable to open IOC file");

    if (driver->value == H5_VFD_IOC) {
        /* Get a copy of the context ID for later use */
        if (H5_subfile_fid_to_context(file_ptr->file_id, &file_ptr->context_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL,
                                    "unable to retrieve subfiling context ID for this file");
        file_ptr->fa.require_ioc = true;
    }
    else if (driver->value == H5_VFD_SEC2) {
        int ioc_flags;

        /* Translate the HDF5 file open flags into standard POSIX open flags */
        ioc_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;
        if (H5F_ACC_TRUNC & flags)
            ioc_flags |= O_TRUNC;
        if (H5F_ACC_CREAT & flags)
            ioc_flags |= O_CREAT;
        if (H5F_ACC_EXCL & flags)
            ioc_flags |= O_EXCL;

        /*
         * Open the subfiles for this HDF5 file. A subfiling
         * context ID will be returned, which is used for
         * further interactions with this file's subfiles.
         */
        if (H5_open_subfiles(file_ptr->file_path, file_ptr->file_id, &file_ptr->fa.shared_cfg, ioc_flags,
                             file_ptr->comm, &file_ptr->context_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiling files = %s\n",
                                    name);
    }

    if (file_ptr->mpi_rank == 0) {
        if (H5FD__subfiling__get_real_eof(file_ptr->context_id, &sf_eof) < 0)
            sf_eof = -1;
    }

    if (file_ptr->mpi_size > 1) {
        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_eof, 1, MPI_INT64_T, 0, file_ptr->comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Bcast", mpi_code);
    }

    bcasted_eof = true;

    if (sf_eof < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "lead MPI process failed to get file EOF");

    file_ptr->eof       = (haddr_t)sf_eof;
    file_ptr->local_eof = file_ptr->eof;

    ret_value = (H5FD_t *)file_ptr;

done:
    if (config_ptr == &default_config)
        if (H5I_dec_ref(config_ptr->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, NULL, "can't close IOC FAPL");

    if (NULL == ret_value) {
        if (file_ptr) {
            /* Participate in possible MPI collectives on failure */
            if (file_ptr->comm != MPI_COMM_NULL) {
                if (!bcasted_eof) {
                    sf_eof = -1;

                    if (file_ptr->mpi_size > 1) {
                        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_eof, 1, MPI_INT64_T, 0, file_ptr->comm)))
                            H5_SUBFILING_MPI_DONE_ERROR(NULL, "MPI_Bcast failed", mpi_code);
                    }
                }
            }

            if (H5FD__subfiling_close_int(file_ptr) < 0)
                H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "couldn't close file");
        }
    }

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_open() */

static herr_t
H5FD__subfiling_close_int(H5FD_subfiling_t *file_ptr)
{
    int    mpi_finalized;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    assert(file_ptr);

    if (MPI_SUCCESS != (mpi_code = MPI_Finalized(&mpi_finalized)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalized failed", mpi_code);

    if (file_ptr->sf_file && H5FD_close(file_ptr->sf_file) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close subfile");
    if (file_ptr->stub_file && H5FD_close(file_ptr->stub_file) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close HDF5 stub file");

    /* if set, close the copy of the plist for the underlying VFD. */
    if ((file_ptr->fa.ioc_fapl_id >= 0) && (H5I_dec_ref(file_ptr->fa.ioc_fapl_id) < 0))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_ARGS, FAIL, "can't close IOC FAPL");
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;

    if (!mpi_finalized) {
        if (H5_mpi_comm_free(&file_ptr->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Communicator");
        if (H5_mpi_info_free(&file_ptr->info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Info object");

        if (H5_mpi_comm_free(&file_ptr->ext_comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI communicator");
    }

    file_ptr->fail_to_encode = false;

done:
    free(file_ptr->file_path);
    file_ptr->file_path = NULL;

    H5MM_free(file_ptr->file_dir);
    file_ptr->file_dir = NULL;

    if (file_ptr->context_id >= 0 && H5_free_subfiling_object(file_ptr->context_id) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "can't free subfiling context object");

    /* Release the file info */
    file_ptr = H5FL_FREE(H5FD_subfiling_t, file_ptr);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_close
 *
 * Purpose:     Closes an HDF5 file.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL, file not closed.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_close(H5FD_t *_file)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    if (H5FD__subfiling_close_int(file_ptr) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close file");

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_cmp
 *
 * Purpose:     Compares two files belonging to this driver using an
 *              arbitrary (but consistent) ordering.
 *
 * Return:      Success:    A value like strcmp()
 *              Failure:    never fails (arguments were checked by the
 *                          caller).
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD__subfiling_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_subfiling_t *f1        = (const H5FD_subfiling_t *)_f1;
    const H5FD_subfiling_t *f2        = (const H5FD_subfiling_t *)_f2;
    int                     ret_value = 0;

    assert(f1);
    assert(f2);

    ret_value = H5FD_cmp(f1->sf_file, f2->sf_file);

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_cmp() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 *              For now, duplicate the flags used for the MPIO VFD.
 *              Revisit this when we have a version of the subfiling VFD
 *              that is usable in serial builds.
 *
 * Return:      SUCCEED (Can't fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags /* out */)
{
    herr_t ret_value = SUCCEED;

    /* Set the VFL feature flags that this driver supports */
    if (flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations  */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
        *flags |= H5FD_FEAT_HAS_MPI;             /* This driver uses MPI */
    }

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file. The EOA marker
 *              is the first address past the last byte allocated in the
 *              format address space.
 *
 * Return:      The end-of-address marker.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__subfiling_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_subfiling_t *file      = (const H5FD_subfiling_t *)_file;
    haddr_t                 ret_value = HADDR_UNDEF;

    ret_value = file->eoa;

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_get_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file. This function is
 *              called shortly after an existing HDF5 file is opened in order
 *              to tell the driver where the end of the HDF5 data is located.
 *
 * Return:      SUCCEED (Can't fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    file_ptr->eoa = addr;

    /* Set EOA for HDF5 stub file */
    if (file_ptr->mpi_rank == 0) {
        if (H5FD_set_eoa(file_ptr->stub_file, type, addr) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set HDF5 stub file EOA");
    }

    ret_value = H5FD_set_eoa(file_ptr->sf_file, type, addr);

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_get_eof
 *
 * Purpose:     Returns the end-of-file marker from the filesystem
 *              perspective.
 *
 * Return:      End of file address, the first address past the end of the
 *              "file", either the filesystem file or the HDF5 file.
 *
 *              NOTE: This VFD mimics the MPI I/O VFD and so does not try
 *              to keep the EOF updated. The EOF is mostly just needed
 *              right after the file is opened so the library can determine
 *              if the file is empty, truncated or okay.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__subfiling_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_subfiling_t *file      = (const H5FD_subfiling_t *)_file;
    haddr_t                 ret_value = HADDR_UNDEF;

    ret_value = file->eof;

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_get_eof() */

/*-------------------------------------------------------------------------
 * Function:       H5FD__subfiling_get_handle
 *
 * Purpose:        Returns the file handle of subfiling file driver.
 *
 * Returns:        SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void **file_handle)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    if (!file_handle)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid");

    H5FD_get_vfd_handle(file->sf_file, file->fa.ioc_fapl_id, file_handle);

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_get_handle() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_read
 *
 * Purpose:     Reads SIZE bytes of data from FILE beginning at address ADDR
 *              into buffer BUF according to data transfer properties in
 *              DXPL_ID.
 *
 * Return:      Success:    SUCCEED. Result is stored in caller-supplied
 *                          buffer BUF.
 *              Failure:    FAIL, Contents of buffer BUF are undefined.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_read(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr, size_t size,
                     void *buf /*out*/)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    assert(file_ptr);
    assert(file_ptr->pub.driver_id == H5FD_SUBFILING);
    assert(buf);

    if (H5FD__subfiling_io_helper(file_ptr, 1, &type, &addr, &size, (H5_flexible_const_ptr_t *)&buf,
                                  IO_TYPE_READ) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "read from subfiles failed");

    /* Point to the end of the current I/O */
    addr += (haddr_t)size;

    /* Update current file position and EOF */
    file_ptr->pos = addr;
    file_ptr->op  = OP_READ;

done:
    if (ret_value < 0) {
        /* Reset last file I/O information */
        file_ptr->pos = HADDR_UNDEF;
        file_ptr->op  = OP_UNKNOWN;
    } /* end if */

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_write
 *
 * Purpose:     Writes SIZE bytes of data to FILE beginning at address ADDR
 *              from buffer BUF according to data transfer properties in
 *              DXPL_ID.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_write(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr, size_t size,
                      const void *buf /*in*/)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    assert(file_ptr);
    assert(file_ptr->pub.driver_id == H5FD_SUBFILING);
    assert(buf);

    if (H5FD__subfiling_io_helper(file_ptr, 1, &type, &addr, &size, (H5_flexible_const_ptr_t *)&buf,
                                  IO_TYPE_WRITE) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "write to subfiles failed");

    /* Point to the end of the current I/O */
    addr += (haddr_t)size;

    /* Update current file position and EOF */
    file_ptr->pos = addr;
    file_ptr->op  = OP_WRITE;

    /* Mimic the MPI I/O VFD */
    file_ptr->eof = HADDR_UNDEF;

    if (file_ptr->pos > file_ptr->local_eof)
        file_ptr->local_eof = file_ptr->pos;

done:
    if (ret_value < 0) {
        /* Reset last file I/O information */
        file_ptr->pos = HADDR_UNDEF;
        file_ptr->op  = OP_UNKNOWN;
    } /* end if */

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_write() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfile_read_vector  (internal function)
 *
 * Purpose:     Vector Read function for the sub-filing VFD.
 *
 *              Perform count reads from the specified file at the offsets
 *              provided in the addrs array, with the lengths and memory
 *              types provided in the sizes and types arrays.  Data read
 *              is returned in the buffers provided in the bufs array.
 *
 *              All reads are done according to the data transfer property
 *              list dxpl_id (which may be the constant H5P_DEFAULT).
 *
 * Return:      Success:    SUCCEED
 *                          All reads have completed successfully, and
 *                          the results havce been into the supplied
 *                          buffers.
 *
 *              Failure:    FAIL
 *                          The contents of supplied buffers are undefined.
 *
 * Notes:       Thus function doesn't actually implement vector read.
 *              Instead, it converts the vector read call into a series
 *              of scalar read calls.  Fix this when time permits.
 *
 *              Also, it didn't support the sizes and types optimization.
 *              I implemented a version of this which is more generous
 *              than that currently defined in the RFC.  This is good
 *              enough for now, but the final version should follow
 *              the RFC.
 *                                                    JRM -- 10/5/21
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_read_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                            size_t sizes[], void *bufs[] /* out */)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    assert(file_ptr);
    assert(file_ptr->pub.driver_id == H5FD_SUBFILING);
    assert((types) || (count == 0));
    assert((addrs) || (count == 0));
    assert((sizes) || (count == 0));
    assert((bufs) || (count == 0));

    /*
     * Verify that the first elements of the sizes and
     * types arrays are valid.
     */
    assert((count == 0) || (sizes[0] != 0));
    assert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (true != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    /* Set DXPL for operation */
    H5CX_set_dxpl(dxpl_id);

    if (file_ptr->fa.require_ioc) {
        if (H5FD__subfiling_io_helper(file_ptr, (size_t)count, types, addrs, sizes,
                                      (H5_flexible_const_ptr_t *)bufs, IO_TYPE_READ) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't read data");
    }
    else {
        if (H5FD_read_vector(_file, count, types, addrs, sizes, bufs) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't read data");
    }

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_read_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_write_vector
 *
 * Purpose:     Perform count writes to the specified file at the offsets
 *              provided in the addrs array. Lengths and memory types
 *              types are provided in the sizes and types arrays. Data to
 *              be written is referenced by the bufs array.
 *
 *              All writes are done according to the data transfer property
 *              list dxpl_id (which may be the constant H5P_DEFAULT).
 *
 * Return:      Success:    SUCCEED
 *                          All writes have completed successfully.
 *
 *              Failure:    FAIL
 *                          An internal error was encountered, e.g the
 *                          input arguments are not valid, or the actual
 *                          subfiling writes have failed for some reason.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_write_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                             haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    assert(file_ptr);
    assert(file_ptr->pub.driver_id == H5FD_SUBFILING);
    assert((types) || (count == 0));
    assert((addrs) || (count == 0));
    assert((sizes) || (count == 0));
    assert((bufs) || (count == 0));

    /*
     * Verify that the first elements of the sizes and
     * types arrays are valid.
     */
    assert((count == 0) || (sizes[0] != 0));
    assert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (true != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    /* Set DXPL for operation */
    H5CX_set_dxpl(dxpl_id);

    if (file_ptr->fa.require_ioc) {
        if (H5FD__subfiling_io_helper(file_ptr, (size_t)count, types, addrs, sizes,
                                      (H5_flexible_const_ptr_t *)bufs, IO_TYPE_WRITE) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't write data");
    }
    else {
        if (H5FD_write_vector(_file, count, types, addrs, sizes, bufs) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't write data");
    }

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FDsubfile__write_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_truncate
 *
 * Purpose:     Makes sure that the true file size is the same as
 *              the end-of-allocation.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, bool H5_ATTR_UNUSED closing)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED; /* Return value */

    assert(file);

    /* Extend the file to make sure it's large enough */
    if (!H5_addr_eq(file->eoa, file->last_eoa)) {
        int64_t sf_eof;
        int64_t eoa;
        int     mpi_code;

        if (!H5CX_get_mpi_file_flushing()) {
            if (file->mpi_size > 1)
                if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file->comm)))
                    H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);
        }

        if (0 == file->mpi_rank) {
            if (H5FD__subfiling__get_real_eof(file->context_id, &sf_eof) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get EOF");
        }

        if (file->mpi_size > 1) {
            if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_eof, 1, MPI_INT64_T, 0, file->comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
        }

        if (sf_eof < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid EOF");

        H5_CHECKED_ASSIGN(eoa, int64_t, file->eoa, haddr_t);

        /* truncate subfiles */
        /* This is a hack.  We should be doing the truncate of the subfiles via calls to
         * H5FD_truncate() with the IOC.  However, that system is messed up at present.
         * thus the following hack.
         *                                                 JRM -- 12/18/21
         */
        if (H5FD__subfiling__truncate_sub_files(file->context_id, eoa, file->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "subfile truncate request failed");

#if 0 /* TODO: Should be truncated only to size of superblock metadata */
        /* Truncate the HDF5 stub file */
        if (file->mpi_rank == 0) {
            if (H5FD_truncate(file->stub_file, closing) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "stub file truncate request failed");
        }
#endif

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op  = OP_UNKNOWN;

        /* Update the 'last' eoa value */
        file->last_eoa = file->eoa;
    }

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_truncate() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_lock
 *
 * Purpose:     To place an advisory lock on a file.
 *      The lock type to apply depends on the parameter "rw":
 *          true--opens for write: an exclusive lock
 *          false--opens for read: a shared lock
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
#if 0
static herr_t
H5FD__subfiling_lock(H5FD_t *_file, bool rw)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file; /* VFD file struct  */
    herr_t            ret_value = SUCCEED;                   /* Return value       */

    assert(file);

    if (file->fa.require_ioc) {
#ifdef VERBOSE
        puts("Subfiling driver doesn't support file locking");
#endif
    }
    else {
        if (H5FD_lock(file->sf_file, rw) < 0)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to lock file");
    } /* end if */

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_lock() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_unlock
 *
 * Purpose:     To remove the existing lock on the file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_unlock(H5FD_t *_file)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file; /* VFD file struct */
    herr_t            ret_value = SUCCEED;                   /* Return value             */

    assert(file);

    if (H5FD_unlock(file->sf_file) < 0)
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to lock file");

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_unlock() */
#endif

static herr_t
H5FD__subfiling_del(const char *name, hid_t fapl)
{
    const H5FD_subfiling_config_t *subfiling_config = NULL;
    H5FD_subfiling_config_t        default_config;
    H5P_genplist_t                *plist     = NULL;
    herr_t                         ret_value = SUCCEED;

    if (NULL == (plist = H5P_object_verify(fapl, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (H5FD_SUBFILING != H5P_peek_driver(plist))
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect driver set on FAPL");

    if (NULL == (subfiling_config = H5P_peek_driver_info(plist))) {
        if (H5FD__subfiling_get_default_config(fapl, &default_config) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,
                                    "can't get default Subfiling VFD configuration");
        subfiling_config = &default_config;
    }

    if (H5FD_delete(name, subfiling_config->ioc_fapl_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTDELETE, FAIL, "unable to delete file");

done:
    if (subfiling_config == &default_config)
        if (H5I_dec_ref(subfiling_config->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, FAIL, "unable to close IOC FAPL");

    H5_SUBFILING_FUNC_LEAVE_API;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_ctl
 *
 * Purpose:     Subfiling version of the ctl callback.
 *
 *              The desired operation is specified by the op_code
 *              parameter.
 *
 *              The flags parameter controls management of op_codes that
 *              are unknown to the callback
 *
 *              The input and output parameters allow op_code specific
 *              input and output
 *
 *              At present, the supported op codes are:
 *
 *                  H5FD_CTL_GET_MPI_COMMUNICATOR_OPCODE
 *                  H5FD_CTL_GET_MPI_RANK_OPCODE
 *                  H5FD_CTL_GET_MPI_SIZE_OPCODE
 *
 *              Note that these opcodes must be supported by all VFDs that
 *              support MPI.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags, const void H5_ATTR_UNUSED *input,
                    void **output)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED; /* Return value */

    /* Sanity checks */
    assert(file);
    assert(H5FD_SUBFILING == file->pub.driver_id);

    switch (op_code) {

        case H5FD_CTL_GET_MPI_COMMUNICATOR_OPCODE:
            assert(output);
            assert(*output);

            /*
             * Return a separate MPI communicator to the caller so
             * that our own MPI calls won't have a chance to conflict
             */
            if (file->ext_comm == MPI_COMM_NULL) {
                if (H5_mpi_comm_dup(file->comm, &file->ext_comm) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't duplicate MPI communicator");
            }

            **((MPI_Comm **)output) = file->ext_comm;
            break;

        case H5FD_CTL_GET_MPI_INFO_OPCODE:
            assert(output);
            assert(*output);
            **((MPI_Info **)output) = file->info;
            break;

        case H5FD_CTL_GET_MPI_RANK_OPCODE:
            assert(output);
            assert(*output);
            **((int **)output) = file->mpi_rank;
            break;

        case H5FD_CTL_GET_MPI_SIZE_OPCODE:
            assert(output);
            assert(*output);
            **((int **)output) = file->mpi_size;
            break;

        default: /* unknown op code */
            if (flags & H5FD_CTL_FAIL_IF_UNKNOWN_FLAG) {
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_FCNTL, FAIL, "unknown op_code and fail if unknown");
            }
            break;
    }

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_ctl() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_io_helper
 *
 * Purpose:     Helper routine to manage the common portions of I/O between
 *              normal and vector I/O calls.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_io_helper(H5FD_subfiling_t *file_ptr, size_t io_count, H5FD_mem_t types[], haddr_t addrs[],
                          size_t sizes[], H5_flexible_const_ptr_t bufs[], H5FD_subfiling_io_type_t io_type)
{
    H5_flexible_const_ptr_t *io_bufs      = NULL;
    subfiling_context_t     *sf_context   = NULL;
    H5FD_mpio_xfer_t         xfer_mode    = H5FD_MPIO_INDEPENDENT;
    H5FD_mem_t              *io_types     = NULL;
    haddr_t                 *io_addrs     = NULL;
    size_t                  *io_sizes     = NULL;
    haddr_t                  file_eoa     = HADDR_UNDEF;
    size_t                   io_size      = 0;
    bool                     rank0_bcast  = false;
    bool                     extend_sizes = false;
    int                      num_subfiles;
    herr_t                   ret_value = SUCCEED;

    assert(file_ptr);

    if (HADDR_UNDEF == (file_eoa = H5FD__subfiling_get_eoa((const H5FD_t *)file_ptr, H5FD_MEM_DEFAULT)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get file EOA");

    /* Perform some sanity checking on the given (address, size) pairs */
    extend_sizes = false;
    for (size_t i = 0; i < io_count; i++) {
        if (!extend_sizes) {
            if ((i > 0) && (sizes[i] == 0)) {
                extend_sizes = true;
                io_size      = sizes[i - 1];
            }
            else {
                io_size = sizes[i];
            }
        }

        if (!H5_addr_defined(addrs[i]))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr[%zu] undefined, addr = %" PRIuHADDR,
                                    i, addrs[i]);
        if (REGION_OVERFLOW(addrs[i], io_size))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                                    "addr[%zu] overflow, addr = %" PRIuHADDR ", size = %zu", i, addrs[i],
                                    io_size);
        if ((addrs[i] + io_size) > file_eoa)
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                                    "addr overflow, addrs[%zu] = %" PRIuHADDR
                                    ", sizes[%zu] = %zu, eoa = %" PRIuHADDR,
                                    i, addrs[i], i, io_size, file_eoa);
    }

    /*
     * Temporarily reject collective I/O until support is
     * implemented (unless types are simple MPI_BYTE), which
     * can be properly handled here.
     */
    if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL, "can't determine I/O collectivity setting");

    if (xfer_mode == H5FD_MPIO_COLLECTIVE) {
        MPI_Datatype btype, ftype;

        if (H5CX_get_mpi_coll_datatypes(&btype, &ftype) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL, "can't get MPI-I/O datatypes");
        if (MPI_BYTE != btype || MPI_BYTE != ftype)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "collective I/O is currently unsupported");
    }

    /*
     * If we reached here, we're still doing independent I/O regardless
     * of collectivity setting, so set that.
     */
    H5CX_set_io_xfer_mode(H5FD_MPIO_INDEPENDENT);

    /* Determine whether a rank 0 bcast approach has been requested */
    if (io_type == IO_TYPE_READ)
        rank0_bcast = H5CX_get_mpio_rank0_bcast();

    /*
     * Retrieve the subfiling context object and the number
     * of subfiles.
     *
     * Given the current I/O and the I/O concentrator info,
     * we can determine some I/O transaction parameters.
     * In particular, for large I/O operations, each IOC
     * may require multiple I/Os to fulfill the user I/O
     * request. The block size and number of IOCs are used
     * to size the vectors that will be used to invoke the
     * underlying I/O operations.
     */
    if (NULL == (sf_context = (subfiling_context_t *)H5_get_subfiling_object(file_ptr->context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL,
                                "invalid or missing subfiling context object");
    assert(sf_context->topology);

    if ((num_subfiles = sf_context->sf_num_subfiles) <= 0)
        H5_SUBFILING_GOTO_ERROR(H5E_INTERNAL, H5E_BADVALUE, FAIL, "invalid number of subfiles (%d)",
                                num_subfiles);

    if (num_subfiles == 1) {
        uint32_t u32_io_count;

        /***************************************
         * No striping - just a single subfile *
         ***************************************/

        /*
         * Convert the I/O count back to a uint32_t for the vector I/O
         * call until the interface can possibly be changed to use size_t
         * in the future
         */
        H5_CHECKED_ASSIGN(u32_io_count, uint32_t, io_count, size_t);

        if (io_type == IO_TYPE_WRITE) {
            /* Make vector write call to VFD controlling subfiles */
            if (H5FD_write_vector(file_ptr->sf_file, u32_io_count, types, addrs, sizes, (const void **)bufs) <
                0)
                H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "write to subfile failed");

            /*
             * Mirror superblock writes to the stub file so that
             * legacy HDF5 applications can check what type of
             * file they are reading
             */
            if (H5FD__subfiling_mirror_writes_to_stub(file_ptr, u32_io_count, types, addrs, sizes,
                                                      (const void **)bufs) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "mirrored write to stub file failed");
        }
        else {
            /* Make vector read call to VFD controlling subfiles */
            if (H5FD_read_vector(file_ptr->sf_file, u32_io_count, types, addrs, sizes, (void **)bufs) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "read from subfile failed");
        }
    }
    else {
        uint32_t iovec_len;
        size_t   ioreq_count = 0;
        herr_t   status;

        /*************************************
         * Striping across multiple subfiles *
         *************************************/

        /*
         * Generate the types, addrs, sizes and bufs I/O vectors for
         * this I/O request.
         */
        status = generate_io_vectors(
            sf_context,   /* IN:  Subfiling context used to look up config info */
            io_count,     /* IN:  Number of entries in `types`, `addrs`, `sizes` and `bufs` */
            types,        /* IN:  Array of memory types */
            addrs,        /* IN:  Array of starting file offsets */
            sizes,        /* IN:  Array of I/O sizes (in terms of elements) */
            bufs,         /* IN:  Array of I/O buffers */
            1,            /* IN:  Data extent of the 'type'; byte is assumed currently */
            io_type,      /* IN:  Type of I/O being performed (IO_TYPE_WRITE or IO_TYPE_READ) */
            &ioreq_count, /* OUT: Number of I/O requests to be made */
            &iovec_len,   /* OUT: Number of elements in I/O vector for a single I/O request */
            &io_types,    /* OUT: I/O vector of memory types for each I/O entry */
            &io_addrs,    /* OUT: I/O vector of file addresses for each I/O entry */
            &io_sizes,    /* OUT: I/O vector of I/O sizes for each I/O entry */
            &io_bufs);    /* OUT: I/O vector of buffers for each I/O entry */

        if (status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't initialize I/O vectors");

        /* Nothing to do
         *
         * TODO: Note that this does not let the sub-filing VFD participate in
         * collective calls when there is no data to write.  This is not an issue
         * now, as we don't do anything special with collective operations.
         * However, this needs to be fixed.
         */
        if (ioreq_count == 0)
            H5_SUBFILING_GOTO_DONE(SUCCEED);

#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_dump_iovecs(sf_context, ioreq_count, iovec_len, io_type, io_types, io_addrs, io_sizes,
                                 io_bufs);
#endif

        /* clang-format off */
        /*
         * Having now populated the I/O vectors for this I/O request and
         * having determined how many I/O calls need to be made to satisfy
         * the entire I/O request, loop that many times, making an I/O call
         * with each set of I/O vectors. Each I/O call uses a set of I/O
         * vectors with a length of up to 'number of subfiles' elements and
         * each I/O call's I/O vectors are setup to ensure that the I/O is
         * spread across as many subfiles as possible for each iteration. In
         * the simple case of N evenly-distributed and well-aligned I/O
         * requests being performed on 4 subfiles, this can be visualized as
         * the following:
         *
         *  I/O REQ. 0    I/O REQ. 1   ...              I/O REQ. N-1
         *      ||            ||                             ||
         *      VV            VV                             VV
         *  {IOVEC[0]}    {IOVEC[4]}   ...     {IOVEC[(N-1 * iovec_len)]}        -> SUBFILE 0
         *  {IOVEC[1]}    {IOVEC[5]}   ...     {IOVEC[(N-1 * iovec_len) + 1]}    -> SUBFILE 1
         *  {IOVEC[2]}    {IOVEC[6]}   ...     {IOVEC[(N-1 * iovec_len) + 2]}    -> SUBFILE 2
         *  {IOVEC[3]}    {IOVEC[7]}   ...     {IOVEC[(N-1 * iovec_len) + 3]}    -> SUBFILE 3
         *
         * where {IOVEC[X]} represents an I/O vector composed of the entries
         * at index X of io_types, io_addrs, io_sizes and io_bufs. Note that
         * the entire set of I/O vectors, e.g. [ {IOVEC[0]}, {IOVEC[1]}, {IOVEC[2]}, {IOVEC[3]} ]
         * from the above visualization will be sent to the underlying I/O
         * concentrator VFD in a single I/O call on each iteration. That VFD is
         * ultimately responsible for mapping each I/O vector to its corresponding
         * subfile (here, pointed to by '->' to the right of each I/O vector).
         */
        /* clang-format on */
        for (size_t ioreq_idx = 0; ioreq_idx < ioreq_count; ioreq_idx++) {
            H5_flexible_const_ptr_t *io_bufs_ptr   = NULL;
            H5FD_mem_t              *io_types_ptr  = NULL;
            uint32_t                 final_vec_len = iovec_len;
            haddr_t                 *io_addrs_ptr  = NULL;
            size_t                  *io_sizes_ptr  = NULL;

            /* Setup index into I/O vectors for this I/O operation */
            io_types_ptr = &io_types[ioreq_idx * iovec_len];
            io_addrs_ptr = &io_addrs[ioreq_idx * iovec_len];
            io_sizes_ptr = &io_sizes[ioreq_idx * iovec_len];
            io_bufs_ptr  = &io_bufs[ioreq_idx * iovec_len];

            /* Skip 0-sized I/Os */
            for (size_t vec_idx = 0; vec_idx < iovec_len; vec_idx++)
                if (io_sizes_ptr[vec_idx] == 0)
                    final_vec_len--;

            if (io_type == IO_TYPE_WRITE) {
                /* Make vector write call to VFD controlling subfiles */
                if (H5FD_write_vector(file_ptr->sf_file, final_vec_len, io_types_ptr, io_addrs_ptr,
                                      io_sizes_ptr, (const void **)io_bufs_ptr) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "write to subfile failed");

                /*
                 * Mirror superblock writes to the stub file so that
                 * legacy HDF5 applications can check what type of
                 * file they are reading
                 */
                if (H5FD__subfiling_mirror_writes_to_stub(file_ptr, final_vec_len, io_types_ptr, io_addrs_ptr,
                                                          io_sizes_ptr, (const void **)io_bufs_ptr) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                            "mirrored write to stub file failed");
            }
            else {
                if (!rank0_bcast || (file_ptr->mpi_rank == 0)) {
                    /* Make vector read call to VFD controlling subfiles */
                    if (H5FD_read_vector(file_ptr->sf_file, final_vec_len, io_types_ptr, io_addrs_ptr,
                                         io_sizes_ptr, (void **)io_bufs_ptr) < 0)
                        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "read from subfile failed");
                }
            }
        }

        if (rank0_bcast && (file_ptr->mpi_size > 1)) {
            size_t size;

            assert(io_type == IO_TYPE_READ);

            extend_sizes = false;
            for (size_t i = 0; i < io_count; i++) {
                if (!extend_sizes) {
                    if ((i > 0) && (sizes[i] == 0)) {
                        extend_sizes = true;
                        size         = sizes[i - 1];
                    }
                    else {
                        size = sizes[i];
                    }
                }

                H5_CHECK_OVERFLOW(size, size_t, int);
                if (MPI_SUCCESS != MPI_Bcast(bufs[i].vp, (int)size, MPI_BYTE, 0, file_ptr->comm))
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't broadcast data from rank 0");
            }
        }
    }

done:
    /* Restore original transfer mode if we changed it */
    if (xfer_mode != H5FD_MPIO_INDEPENDENT)
        if (H5CX_set_io_xfer_mode(xfer_mode) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_CONTEXT, H5E_CANTSET, FAIL, "can't set I/O collectivity setting");

    free(io_bufs);
    free(io_sizes);
    free(io_addrs);
    free(io_types);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_mirror_writes_to_stub
 *
 * Purpose:     Mirrors write calls to the Subfiling stub file so that
 *              legacy HDF5 applications can check what type of file they
 *              are reading. Only superblock I/O is mirrored to the stub
 *              file and only if that I/O comes from MPI rank 0. This
 *              means that file metadata could be missed if it comes from
 *              other MPI ranks (such as when using a distributed metadata
 *              write strategy), but, at least currently, we generally only
 *              care about the first few bytes of the file being properly
 *              written to the stub file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_mirror_writes_to_stub(H5FD_subfiling_t *file_ptr, uint32_t count, H5FD_mem_t types[],
                                      haddr_t addrs[], size_t sizes[], const void *bufs[])
{
    const void **copied_bufs       = NULL;
    H5FD_mem_t  *copied_types      = NULL;
    haddr_t     *copied_addrs      = NULL;
    size_t      *copied_sizes      = NULL;
    H5FD_mem_t   type              = H5FD_MEM_DEFAULT;
    size_t       io_size           = 0;
    bool         all_super_writes  = true;
    bool         some_super_writes = false;
    bool         extend_types      = false;
    bool         extend_sizes      = false;
    herr_t       ret_value         = SUCCEED;

    assert(file_ptr);

    /* Only mirror I/O from MPI rank 0 */
    if (file_ptr->mpi_rank != 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    if (count == 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    for (size_t i = 0; i < count; i++) {
        if (!extend_types) {
            if ((i > 0) && (types[i] == H5FD_MEM_NOLIST)) {
                extend_types = true;
                type         = types[i - 1];
            }
            else {
                type = types[i];
            }
        }

        if (type == H5FD_MEM_SUPER)
            some_super_writes = true;
        else
            all_super_writes = false;

        /*
         * If we find H5FD_MEM_NOLIST, we don't need to
         * keep looking through the array entries
         */
        if (extend_types)
            break;
    }

    if (all_super_writes) {
        if (H5FD_write_vector(file_ptr->stub_file, count, types, addrs, sizes, bufs) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                    "couldn't write superblock information to stub file");
    }
    else if (some_super_writes) {
        uint32_t vec_len = 0;

        /* Copy I/O vectors and strip out non-superblock I/O */

        if (NULL == (copied_types = malloc(count * sizeof(*copied_types))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate copy of I/O types array");
        if (NULL == (copied_addrs = malloc(count * sizeof(*copied_addrs))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate copy of I/O addresses array");
        if (NULL == (copied_sizes = malloc(count * sizeof(*copied_sizes))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate copy of I/O sizes array");
        if (NULL == (copied_bufs = malloc(count * sizeof(*copied_bufs))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate copy of I/O buffers array");

        extend_types = false;
        extend_sizes = false;
        for (size_t i = 0; i < count; i++) {
            if (!extend_types) {
                if ((i > 0) && (types[i] == H5FD_MEM_NOLIST)) {
                    extend_types = true;
                    type         = types[i - 1];

                    /* End early if none of the remaining memory types are H5FD_MEM_SUPER */
                    if (type != H5FD_MEM_SUPER)
                        break;
                }
                else {
                    type = types[i];
                }
            }

            if (!extend_sizes) {
                if ((i > 0) && (sizes[i] == 0)) {
                    extend_sizes = true;
                    io_size      = sizes[i - 1];
                }
                else {
                    io_size = sizes[i];
                }
            }

            if (type != H5FD_MEM_SUPER)
                continue;

            copied_types[vec_len] = type;
            copied_addrs[vec_len] = addrs[i];
            copied_sizes[vec_len] = io_size;
            copied_bufs[vec_len]  = bufs[i];

            vec_len++;
        }

        if ((vec_len > 0) && (H5FD_write_vector(file_ptr->stub_file, vec_len, copied_types, copied_addrs,
                                                copied_sizes, copied_bufs) < 0))
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                    "couldn't write superblock information to stub file");
    }

done:
    free(copied_bufs);
    free(copied_sizes);
    free(copied_addrs);
    free(copied_types);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    generate_io_vectors
 *
 * Purpose:     Given an array of memory types, an array of file offsets,
 *              an array of the number of I/O elements for each file
 *              offset and an array of I/O buffers, translates each (type,
 *              offset, number of elements, I/O buffer) tuple into a set of
 *              I/O vectors according to the subfiling configuration
 *              specified in `sf_context`. These I/O vectors are generated
 *              such that a set of `iovec_len` elements from each of
 *              `io_types`, `io_addrs`, `io_sizes` and `io_bufs` can be
 *              passed to H5FD_write_vector/H5FD_read_vector and that I/O
 *              call will span as many subfiles as possible, parallelizing
 *              the I/O. Then, the next set of `iovec_len` elements can be
 *              passed and so on, until the whole I/O request has been
 *              parallelized across the subfiles. Once this function
 *              returns, `io_types`, `io_addrs`, `io_sizes` and `io_bufs`
 *              will each contain `ioreq_count` sets of I/O vectors, with
 *              each set containing `iovec_len` elements.
 *
 *              sf_context (IN)
 *                - the subfiling context for the file
 *
 *              in_count (IN)
 *                - the number of entries in the `types`, `file_offsets`,
 *                  `nelemts` and `bufs` arrays
 *
 *              types (IN)
 *                - the memory types for each I/O entry
 *
 *              file_offsets (IN)
 *                - array of starting file offsets for I/O
 *
 *              nelemts (IN)
 *                - array of the number of data elements for the I/O
 *                  operation
 *
 *              bufs (IN)
 *                - array of the I/O buffers to use for each I/O entry
 *
 *              dtype_extent (IN)
 *                - the extent of the datatype of each data element for
 *                  the I/O operation (currently assumed to be 1, meaning
 *                  entries in `nelemts` are expressed in terms of
 *                  bytes)
 *
 *              io_type (IN)
 *                - the type of I/O being performed (IO_TYPE_WRITE or
 *                  IO_TYPE_READ)
 *
 *              ioreq_count (OUT)
 *                - the number of I/O requests needed to fully satisfy the
 *                  I/O operation
 *
 *              iovec_len (OUT)
 *                - the size of each I/O vector (in terms of array elements)
 *                  for each I/O request to be made
 *
 *              io_types (OUT)
 *                - I/O vector of memory types for the I/O operation.
 *                  Allocated by this function and must be freed by the
 *                  caller.
 *
 *              io_addrs (OUT)
 *                - I/O vector of file addresses for the I/O operation.
 *                  Allocated by this function and must be freed by the
 *                  caller.
 *
 *              io_sizes (OUT)
 *                - I/O vector of the I/O sizes for the I/O operation.
 *                  Allocated by this function and must be freed by the
 *                  caller.
 *
 *              io_bufs (OUT)
 *                - I/O vector of the I/O buffers for the I/O operation.
 *                  Allocated by this function and must be freed by the
 *                  caller.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 */
static herr_t
generate_io_vectors(subfiling_context_t *sf_context, size_t in_count, H5FD_mem_t types[],
                    haddr_t file_offsets[], size_t nelemts[], H5_flexible_const_ptr_t bufs[],
                    size_t dtype_extent, H5FD_subfiling_io_type_t io_type, size_t *ioreq_count,
                    uint32_t *iovec_len, H5FD_mem_t **io_types, haddr_t **io_addrs, size_t **io_sizes,
                    H5_flexible_const_ptr_t **io_bufs)
{
    H5_flexible_const_ptr_t *loc_io_bufs              = NULL;
    H5FD_mem_t              *loc_io_types             = NULL;
    H5FD_mem_t               mem_type                 = H5FD_MEM_DEFAULT;
    haddr_t                 *loc_io_addrs             = NULL;
    size_t                  *loc_io_sizes             = NULL;
    size_t                   max_iovec_depth          = 0;
    size_t                   max_num_subfiles_touched = 0;
    size_t                   tot_iovec_len            = 0;
    size_t                   io_size                  = 0;
    bool                     extend_sizes             = false;
    bool                     extend_types             = false;
    herr_t                   ret_value                = SUCCEED;

    assert(sf_context);
    assert(sf_context->sf_stripe_size > 0);
    assert(sf_context->sf_blocksize_per_stripe > 0);
    assert(sf_context->sf_num_subfiles > 0);
    assert(sf_context->topology);
    assert((types) || (in_count == 0));
    assert((file_offsets) || (in_count == 0));
    assert((nelemts) || (in_count == 0));
    assert((bufs) || (in_count == 0));
    assert(dtype_extent == 1); /* For now, assume 'byte'-sized elements */
    assert(ioreq_count);
    assert(iovec_len);
    assert(io_types);
    assert(io_addrs);
    assert(io_sizes);
    assert(io_bufs);

    /* Set some returned values early */
    *ioreq_count = 0;
    *iovec_len   = 0;

    /* Nothing to do */
    if (in_count == 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    /*
     * Do some initial pre-processing to determine how large of
     * I/O vectors we will need to allocate to satisfy the
     * entire I/O request
     */
    get_iovec_sizes(sf_context, in_count, file_offsets, nelemts, dtype_extent, &max_iovec_depth,
                    &max_num_subfiles_touched);

    tot_iovec_len = in_count * max_iovec_depth * max_num_subfiles_touched;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(
        sf_context->sf_context_id,
        "%s: I/O count: %zu, max_iovec_depth = %zu, max_num_subfiles_touched = %zu, iovec_len = %zu",
        __func__, in_count, max_iovec_depth, max_num_subfiles_touched, tot_iovec_len);
#endif

    /* Allocate I/O vectors that will be returned to the caller */
    if (NULL == (loc_io_types = calloc(1, tot_iovec_len * sizeof(*loc_io_types))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O types vector");
    if (NULL == (loc_io_addrs = calloc(1, tot_iovec_len * sizeof(*loc_io_addrs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "can't allocate subfile I/O addresses vector");
    if (NULL == (loc_io_sizes = calloc(1, tot_iovec_len * sizeof(*loc_io_sizes))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O sizes vector");
    if (NULL == (loc_io_bufs = calloc(1, tot_iovec_len * sizeof(*loc_io_bufs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "can't allocate subfile I/O buffers vector");

    /*
     * Populate the I/O vectors by looping through each
     * of the (type, addrs, I/O size, buf) tuples
     */
    for (size_t io_idx = 0; io_idx < in_count; io_idx++) {
        size_t iovec_idx;

        iovec_idx = (io_idx * max_iovec_depth * max_num_subfiles_touched);
        assert(iovec_idx < tot_iovec_len);

        if (!extend_types) {
            if ((io_idx > 0) && (types[io_idx] == H5FD_MEM_NOLIST)) {
                extend_types = true;
                mem_type     = types[io_idx - 1];
            }
            else {
                mem_type = types[io_idx];
            }
        }

        if (!extend_sizes) {
            if ((io_idx > 0) && (nelemts[io_idx] == 0)) {
                extend_sizes = true;
                io_size      = nelemts[io_idx - 1] * dtype_extent;
            }
            else {
                io_size = nelemts[io_idx] * dtype_extent;
            }
        }

        if (translate_io_req_to_iovec(sf_context, iovec_idx, max_num_subfiles_touched, max_iovec_depth,
                                      mem_type, file_offsets[io_idx], io_size, bufs[io_idx], io_type,
                                      loc_io_types, loc_io_addrs, loc_io_sizes, loc_io_bufs) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't translate I/O request to I/O vectors");
    }

    *ioreq_count = in_count * max_iovec_depth;
    H5_CHECK_OVERFLOW(max_num_subfiles_touched, size_t, uint32_t);
    *iovec_len = (uint32_t)max_num_subfiles_touched;
    *io_types  = loc_io_types;
    *io_addrs  = loc_io_addrs;
    *io_sizes  = loc_io_sizes;
    *io_bufs   = loc_io_bufs;

done:
    if (ret_value < 0) {
        free(loc_io_bufs);
        free(loc_io_sizes);
        free(loc_io_addrs);
        free(loc_io_types);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    get_iovec_sizes
 *
 * Purpose:     Helper routine to determine the maximum I/O vector depth
 *              (in terms of array elements) and maximum number of subfiles
 *              touched for any particular piece of an I/O request. This
 *              info is used to calculate the total size of I/O vectors we
 *              need to allocate to satisfy an entire I/O request.
 *
 * Return:      Maximum I/O vector depth and maximum number of subfiles
 *              touched (can't fail)
 *
 *-------------------------------------------------------------------------
 */
static void
get_iovec_sizes(subfiling_context_t *sf_context, size_t in_count, haddr_t file_offsets[], size_t nelemts[],
                size_t dtype_extent, size_t *max_iovec_depth, size_t *max_num_subfiles)
{
    int64_t stripe_size          = 0;
    int64_t block_size           = 0;
    size_t  loc_max_iovec_depth  = 0;
    size_t  loc_max_num_subfiles = 0;
    int     num_subfiles         = 0;

    assert(sf_context);
    assert(file_offsets);
    assert(nelemts);
    assert(max_iovec_depth);
    assert(max_num_subfiles);

    stripe_size  = sf_context->sf_stripe_size;
    block_size   = sf_context->sf_blocksize_per_stripe;
    num_subfiles = sf_context->sf_num_subfiles;

    for (size_t io_idx = 0; io_idx < in_count; io_idx++) {
        int64_t stripe_idx;
        int64_t final_stripe_idx;
        int64_t cur_file_offset;
        int64_t final_offset;
        int64_t data_size;
        int64_t first_subfile;
        int64_t last_subfile;
        int64_t row_stripe_idx_start;
        int64_t row_stripe_idx_final;
        int64_t cur_max_num_subfiles;
        size_t  cur_iovec_depth;

        H5_CHECKED_ASSIGN(cur_file_offset, int64_t, file_offsets[io_idx], haddr_t);
        H5_CHECKED_ASSIGN(data_size, int64_t, (nelemts[io_idx] * dtype_extent), size_t);

        /*
         * Calculate the following from the starting file offset:
         *
         *  stripe_idx
         *    - a stripe "index" given by the file offset divided by the
         *      stripe size. Note that when the file offset equals or exceeds
         *      the block size, we simply wrap around. So, for example, if 4
         *      subfiles are being used with a stripe size of 1KiB, the block
         *      size would be 4KiB and file offset 4096 would have a stripe
         *      index of 4 and reside in the same subfile as stripe index 0
         *      (offsets 0-1023)
         *  final_offset
         *    - the last offset in the virtual file covered by this I/O
         *      operation. Simply the I/O size added to the starting file
         *      offset.
         */
        stripe_idx   = cur_file_offset / stripe_size;
        final_offset = cur_file_offset + data_size;

        /* Determine which subfile the I/O request begins in */
        first_subfile = stripe_idx % num_subfiles;

        /*
         * Determine the stripe "index" of the last offset in the
         * virtual file and, from that, determine the subfile that
         * the I/O request ends in.
         */
        final_stripe_idx = final_offset / stripe_size;
        last_subfile     = final_stripe_idx % num_subfiles;

        /*
         * Determine how "deep" the resulting I/O vectors are at
         * most by calculating the maximum number of "rows" spanned
         * for any particular subfile; e.g. the maximum number of
         * I/O requests for any particular subfile
         */
        row_stripe_idx_start = stripe_idx - first_subfile;
        row_stripe_idx_final = final_stripe_idx - last_subfile;
        cur_iovec_depth      = (size_t)((row_stripe_idx_final - row_stripe_idx_start) / num_subfiles) + 1;

        /*
         * If the I/O request "wrapped around" and ends in a subfile
         * less than the subfile we started in, subtract one from the
         * I/O vector length to account for "empty space". This can be
         * visualized as follows:
         *
         *   SUBFILE 0   SUBFILE 1   SUBFILE 2   SUBFILE 3
         *  _______________________________________________
         * |           |           |   XXXXX   |   XXXXX   | ROW 0
         * |   XXXXX   |   XXXXX   |   XXXXX   |   XXXXX   | ROW 1
         * |   XXXXX   |   XXXXX   |           |           | ROW 2
         * |           |           |           |           | ROW ...
         * |           |           |           |           |
         * |           |           |           |           |
         * |           |           |           |           |
         * |___________|___________|___________|___________|
         *
         * Here, `stripe_idx` would be calculated as 2 (I/O begins in
         * the 3rd stripe, or subfile index 2), `first_subfile` would be
         * calculated as 2 and the starting "row" (row_stripe_idx_start)
         * would be calculated as "row" index 0. `final_stripe_idx` would
         * be calculated as 9, `last_subfile` would be calculated as
         * (9 % 4) = 1 and the ending "row" (row_stripe_idx_final) would
         * be calculated as (9 - 1) = 8. Thus, the calculated I/O vector
         * length would be ((8 - 0) / 4) + 1 = 3. However, since there is
         * no I/O to stripe indices 0 and 1 (residing in "row" 0 of subfile
         * index 0 and 1, respectively), it can be seen that the real I/O
         * vector length is 2.
         */
        if (last_subfile < first_subfile)
            cur_iovec_depth--;

        loc_max_iovec_depth = MAX(cur_iovec_depth, loc_max_iovec_depth);

        /*
         * Determine the maximum number of subfiles this piece of the
         * I/O request could touch
         */
        if (data_size >= block_size) {
            /*
             * I/O of a size greater than the block size definitionally
             * touches all subfiles at least once.
             */
            cur_max_num_subfiles = (int64_t)num_subfiles;
        }
        else if (data_size < stripe_size) {
            /*
             * I/O of a size smaller than the stripe size could
             * touch one or two subfiles at most, depending on
             * the file offset.
             */
            cur_max_num_subfiles = 2;
        }
        else {
            /*
             * I/O of a size smaller than the block size, but larger
             * than or equal to the stripe size must touch at least
             * (data_size / stripe_size) subfiles, but could touch
             * an additional subfile, depending on the file offset.
             */
            cur_max_num_subfiles = (((cur_file_offset % stripe_size) + data_size - 1) / stripe_size) + 1;
        }

        loc_max_num_subfiles = MAX((size_t)cur_max_num_subfiles, loc_max_num_subfiles);
    }

    *max_iovec_depth  = loc_max_iovec_depth;
    *max_num_subfiles = loc_max_num_subfiles;
}

/*-------------------------------------------------------------------------
 * Function:    translate_io_req_to_iovec
 *
 * Purpose:     Helper routine to perform the translation between an I/O
 *              request (type, addr, size, buf tuple) and a set of I/O
 *              vectors that spans all the subfiles touched by that I/O
 *              request. Once finished, this function will have generated
 *              at most `iovec_count` sets of I/O vectors, each containing
 *              `iovec_len` elements, but a smaller number of I/O vector
 *              sets could be generated, depending on the I/O request.
 *
 *              sf_context (IN)
 *                - the subfiling context for the file
 *
 *              iovec_idx (IN)
 *                - the index into `io_types`, `io_addrs`, `io_sizes` and
 *                  `io_bufs` where this function should begin filling in
 *                  the I/O vectors
 *
 *              iovec_len (IN)
 *                - the number of elements in each I/O vector generated
 *
 *              iovec_count (IN)
 *                - the maximum number of I/O vectors to be generated, as
 *                  calculated in generate_io_vectors()
 *
 *              type (IN)
 *                - the memory type to use for each component of the I/O
 *                  vectors generated
 *
 *              addr (IN)
 *                - the starting file offset used to generate the I/O
 *                  vectors
 *
 *              io_size (IN)
 *                - the size of the I/O to the given file offset, which is
 *                  used when generating the I/O vectors
 *
 *              io_buf (IN)
 *                - the I/O buffer to be partitioned up while generating
 *                  the I/O vectors
 *
 *              io_type (IN)
 *                - the type of I/O being performed (IO_TYPE_WRITE or
 *                  IO_TYPE_READ)
 *
 *              io_types (OUT)
 *                - pointer to the memory types I/O vector to populate
 *
 *              io_addrs (OUT)
 *                - pointer to the file offsets I/O vector to populate
 *
 *              io_sizes (OUT)
 *                - pointer to the I/O sizes I/O vector to populate
 *
 *              io_bufs (OUT)
 *                - pointer to the I/O buffers I/O vector to populate
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
translate_io_req_to_iovec(subfiling_context_t *sf_context, size_t iovec_idx, size_t iovec_len,
                          size_t iovec_count, H5FD_mem_t type, haddr_t addr, size_t io_size,
                          H5_flexible_const_ptr_t io_buf, H5FD_subfiling_io_type_t io_type,
                          H5FD_mem_t *io_types, haddr_t *io_addrs, size_t *io_sizes,
                          H5_flexible_const_ptr_t *io_bufs)
{
    int64_t stripe_idx           = 0;
    int64_t final_stripe_idx     = 0;
    int64_t stripe_size          = 0;
    int64_t block_size           = 0;
    int64_t file_offset          = 0;
    int64_t offset_in_stripe     = 0;
    int64_t offset_in_block      = 0;
    int64_t final_offset         = 0;
    int64_t start_length         = 0;
    int64_t final_length         = 0;
    int64_t first_subfile_idx    = 0;
    int64_t last_subfile_idx     = 0;
    int64_t start_row            = 0;
    int64_t row_offset           = 0;
    int64_t row_stripe_idx_start = 0;
    int64_t row_stripe_idx_final = 0;
    int64_t max_iovec_depth      = 0;
    int64_t mem_offset           = 0;
    size_t  total_bytes          = 0;
    int     num_subfiles         = 0;
    herr_t  ret_value            = SUCCEED;

    assert(sf_context);
    assert(io_types);
    assert(io_addrs);
    assert(io_sizes);
    assert(io_bufs);

    /*
     * Retrieve some needed fields from the subfiling context.
     *
     *  stripe_size
     *    - the size of the data striping across the file's subfiles
     *  block_size
     *    - the size of a "block" across the IOCs, as calculated
     *      by the stripe size multiplied by the number of
     *      subfiles
     *  num_subfiles
     *    - the total number of subfiles for the logical
     *      HDF5 file
     */
    stripe_size  = sf_context->sf_stripe_size;
    block_size   = sf_context->sf_blocksize_per_stripe;
    num_subfiles = sf_context->sf_num_subfiles;

    H5_CHECKED_ASSIGN(file_offset, int64_t, addr, haddr_t);
    H5_CHECK_OVERFLOW(io_size, size_t, int64_t);

    /*
     * Calculate the following from the starting file offset:
     *
     *  stripe_idx
     *    - a stripe "index" given by the file offset divided by the
     *      stripe size. Note that when the file offset equals or exceeds
     *      the block size, we simply wrap around. So, for example, if 4
     *      subfiles are being used with a stripe size of 1KiB, the block
     *      size would be 4KiB and file offset 4096 would have a stripe
     *      index of 4 and reside in the same subfile as stripe index 0
     *      (offsets 0-1023)
     *  offset_in_stripe
     *    - the relative offset in the stripe that the starting file
     *      offset resides in
     *  offset_in_block
     *    - the relative offset in the "block" of stripes across the
     *      subfiles
     *  final_offset
     *    - the last offset in the virtual file covered by this I/O
     *      request. Simply the I/O size minus one byte added to the
     *      starting file offset.
     */
    stripe_idx       = file_offset / stripe_size;
    offset_in_stripe = file_offset % stripe_size;
    offset_in_block  = file_offset % block_size;
    final_offset     = file_offset + (int64_t)(io_size > 0 ? io_size - 1 : 0);

    /* Determine the size of data written to the first and last stripes */
    start_length = MIN((int64_t)io_size, (stripe_size - offset_in_stripe));
    if (start_length == (int64_t)io_size)
        final_length = 0;
    else if (((final_offset + 1) % stripe_size) == 0)
        final_length = stripe_size;
    else
        final_length = (final_offset + 1) % stripe_size;
    assert(start_length <= stripe_size);
    assert(final_length <= stripe_size);

    /*
     * Determine which subfile the I/O request begins in and which
     * "row" the I/O request begins in within the "block" of stripes
     * across the subfiles. Note that "row" here is just a conceptual
     * way to think of how a block of data stripes is laid out across
     * the subfiles. A block's "column" size in bytes is equal to the
     * stripe size multiplied by the number of subfiles. Therefore,
     * file offsets that are multiples of the block size begin a new
     * "row".
     */
    start_row         = stripe_idx / num_subfiles;
    first_subfile_idx = stripe_idx % num_subfiles;
    H5_CHECK_OVERFLOW(first_subfile_idx, int64_t, int);

    /*
     * Set initial file offset for starting "row"
     * based on the start row index
     */
    row_offset = start_row * block_size;

    /*
     * Determine the stripe "index" of the last offset in the
     * virtual file and, from that, determine the subfile that
     * the I/O request ends in.
     */
    final_stripe_idx = final_offset / stripe_size;
    last_subfile_idx = final_stripe_idx % num_subfiles;

    /*
     * Determine how "deep" the current I/O vector is at most
     * by calculating the maximum number of "rows" spanned for
     * any particular subfile; e.g. the maximum number of I/O
     * requests for any particular subfile
     */
    row_stripe_idx_start = stripe_idx - first_subfile_idx;
    row_stripe_idx_final = final_stripe_idx - last_subfile_idx;
    max_iovec_depth      = ((row_stripe_idx_final - row_stripe_idx_start) / num_subfiles) + 1;

    /*
     * If the I/O request "wrapped around" and ends in a subfile
     * less than the subfile we started in, subtract one from the
     * I/O vector length to account for "empty space". This can be
     * visualized as follows:
     *
     *   SUBFILE 0   SUBFILE 1   SUBFILE 2   SUBFILE 3
     *  _______________________________________________
     * |           |           |   XXXXX   |   XXXXX   | ROW 0
     * |   XXXXX   |   XXXXX   |   XXXXX   |   XXXXX   | ROW 1
     * |   XXXXX   |   XXXXX   |           |           | ROW 2
     * |           |           |           |           | ROW ...
     * |           |           |           |           |
     * |           |           |           |           |
     * |           |           |           |           |
     * |___________|___________|___________|___________|
     *
     * Here, `stripe_idx` would be calculated as 2 (I/O begins in
     * the 3rd stripe, or subfile index 2), `first_subfile` would be
     * calculated as 2 and the starting "row" (row_stripe_idx_start)
     * would be calculated as "row" index 0. `final_stripe_idx` would
     * be calculated as 9, `last_subfile` would be calculated as
     * (9 % 4) = 1 and the ending "row" (row_stripe_idx_final) would
     * be calculated as (9 - 1) = 8. Thus, the calculated I/O vector
     * length would be ((8 - 0) / 4) + 1 = 3. However, since there is
     * no I/O to stripe indices 0 and 1 (residing in "row" 0 of subfile
     * index 0 and 1, respectively), it can be seen that the real I/O
     * vector length is 2.
     */
    if (last_subfile_idx < first_subfile_idx)
        max_iovec_depth--;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(
        sf_context->sf_context_id,
        "%s: TRANSLATING I/O REQUEST (MEMORY TYPE: %d, ADDR: %" PRIuHADDR ", I/O SIZE: %zu, BUF: %p)\n"
        "STRIPE SIZE: %" PRId64 ", BLOCK SIZE: %" PRId64 ", NUM SUBFILES: %d\n"
        "STRIPE IDX: %" PRId64 ", LAST STRIPE IDX: %" PRId64 ", FIRST SUBFILE IDX: %" PRId64
        ", LAST SUBFILE IDX: %" PRId64 "\n"
        "START SEGMENT LENGTH: %" PRId64 ", LAST SEGMENT LENGTH: %" PRId64 ", MAX IOVEC DEPTH: %" PRId64,
        __func__, type, addr, io_size,
        (io_type == IO_TYPE_WRITE) ? (const void *)io_buf.cvp : (void *)io_buf.vp, stripe_size, block_size,
        num_subfiles, stripe_idx, final_stripe_idx, first_subfile_idx, last_subfile_idx, start_length,
        final_length, max_iovec_depth);
#endif

    /*
     * Loop through the set of subfiles to determine the various
     * vector components for each. Subfiles whose data size is
     * zero will not have I/O requests passed to them.
     */
    for (int i = 0, subfile_idx = (int)first_subfile_idx; i < num_subfiles; i++) {
        H5_flexible_const_ptr_t *_io_bufs_ptr;
        H5FD_mem_t              *_io_types_ptr;
        haddr_t                 *_io_addrs_ptr;
        size_t                  *_io_sizes_ptr;
        int64_t                  iovec_depth;
        int64_t                  num_full_stripes;
        int64_t                  subfile_bytes = 0;
        bool                     is_first      = false;
        bool                     is_last       = false;

        if (total_bytes >= io_size)
            break;

        iovec_depth      = max_iovec_depth;
        num_full_stripes = iovec_depth;

        if (subfile_idx == first_subfile_idx) {
            is_first = true;

            /*
             * Add partial segment length if not
             * starting on a stripe boundary
             */
            if (start_length < stripe_size) {
                subfile_bytes += start_length;
                num_full_stripes--;
            }
        }

        if (subfile_idx == last_subfile_idx) {
            is_last = true;

            /*
             * Add partial segment length if not
             * ending on a stripe boundary
             */
            if (final_length < stripe_size) {
                subfile_bytes += final_length;
                if (num_full_stripes)
                    num_full_stripes--;
            }
        }

        /* Account for subfiles with uniform segments */
        if (!is_first && !is_last) {
            bool thin_uniform_section = false;

            if (last_subfile_idx >= first_subfile_idx) {
                /*
                 * In the case where the subfile with the final data
                 * segment has an index value greater than or equal
                 * to the subfile with the first data segment, I/O
                 * vectors directed to a subfile with an index value
                 * that is greater than the last subfile or less than
                 * the first subfile will be "thin", or rather will
                 * have a vector depth of 1 less than normal, which
                 * will be accounted for below. This can be visualized
                 * with the following I/O pattern:
                 *
                 *   SUBFILE 0   SUBFILE 1   SUBFILE 2   SUBFILE 3
                 *  _______________________________________________
                 * |           |   XXXXX   |   XXXXX   |   XXXXX   | ROW 0
                 * |   XXXXX   |   XXXXX   |   XXXXX   |           | ROW 1
                 * |           |           |           |           | ROW 2
                 * |           |           |           |           | ROW ...
                 * |           |           |           |           |
                 * |           |           |           |           |
                 * |           |           |           |           |
                 * |___________|___________|___________|___________|
                 *    (thin)                               (thin)
                 */
                thin_uniform_section = (subfile_idx > last_subfile_idx) || (subfile_idx < first_subfile_idx);
            }
            else { /* last_subfile_idx < first_subfile_idx */
                /*
                 * This can also happen when the subfile with the final
                 * data segment has a smaller subfile index than the
                 * subfile with the first data segment and the current
                 * subfile index falls between the two.
                 */
                thin_uniform_section =
                    ((last_subfile_idx < subfile_idx) && (subfile_idx < first_subfile_idx));
            }

            if (thin_uniform_section) {
                assert(iovec_depth > 1);
                assert(num_full_stripes > 1);

                iovec_depth--;
                num_full_stripes--;
            }
        }

        /*
         * After accounting for the length of the initial
         * and/or final data segments, add the combined
         * size of the fully selected I/O stripes to the
         * running bytes total
         */
        subfile_bytes += num_full_stripes * stripe_size;
        total_bytes += (size_t)subfile_bytes;

        /*
         * Setup the pointers to the next set of I/O vectors
         * in the output arrays
         */
        _io_types_ptr = &io_types[iovec_idx + (size_t)i];
        _io_addrs_ptr = &io_addrs[iovec_idx + (size_t)i];
        _io_sizes_ptr = &io_sizes[iovec_idx + (size_t)i];
        _io_bufs_ptr  = &io_bufs[iovec_idx + (size_t)i];

        /*
         * Fill in I/O vector with initial values. If more than 1
         * subfile is involved, these values will be adjusted below.
         */
        for (size_t vec_idx = 0; vec_idx < iovec_count; vec_idx++)
            *(_io_types_ptr + (vec_idx * iovec_len)) = type;
        *_io_addrs_ptr = (haddr_t)(row_offset + offset_in_block);
        *_io_sizes_ptr = (size_t)subfile_bytes;

        if (io_type == IO_TYPE_WRITE)
            _io_bufs_ptr->cvp = (const char *)(io_buf.cvp) + mem_offset;
        else
            _io_bufs_ptr->vp = (char *)(io_buf.vp) + mem_offset;

        if (num_subfiles > 1) {
            int64_t cur_file_offset = row_offset + offset_in_block;

            assert(iovec_depth <= max_iovec_depth);

            /* Fill the I/O vectors for the current subfile */
            if (is_first) {
                if (is_last) {
                    /*
                     * The current subfile being processed is both the first
                     * subfile touched by I/O and the last subfile touched by
                     * I/O. In this case, we may have to deal with partial
                     * stripe I/O in the first and last I/O segments.
                     */
                    if (iovec_fill_first_last(sf_context, iovec_len, iovec_depth, subfile_bytes, mem_offset,
                                              cur_file_offset, start_length, final_length, io_buf, io_type,
                                              _io_addrs_ptr, _io_sizes_ptr, _io_bufs_ptr) < 0)
                        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");
                }
                else {
                    /*
                     * The current subfile being processed is the first
                     * subfile touched by I/O. In this case, we may have
                     * to deal with partial stripe I/O in the first I/O
                     * segment.
                     */
                    if (iovec_fill_first(sf_context, iovec_len, iovec_depth, subfile_bytes, mem_offset,
                                         cur_file_offset, start_length, io_buf, io_type, _io_addrs_ptr,
                                         _io_sizes_ptr, _io_bufs_ptr) < 0)
                        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");
                }
                /* Move the memory pointer to the starting location
                 * for next subfile I/O request.
                 */
                mem_offset += start_length;
            }
            else if (is_last) {
                /*
                 * The current subfile being processed is the last subfile
                 * touched by I/O. In this case, we may have to deal with
                 * partial stripe I/O in the last I/O segment.
                 */
                if (iovec_fill_last(sf_context, iovec_len, iovec_depth, subfile_bytes, mem_offset,
                                    cur_file_offset, final_length, io_buf, io_type, _io_addrs_ptr,
                                    _io_sizes_ptr, _io_bufs_ptr) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");

                mem_offset += stripe_size;
            }
            else {
                /*
                 * The current subfile being processed is neither the first
                 * nor the last subfile touched by I/O. In this case, no
                 * partial stripe I/O will need to be dealt with; all I/O
                 * segments will cover a full I/O stripe.
                 */
                if (iovec_fill_uniform(sf_context, iovec_len, iovec_depth, subfile_bytes, mem_offset,
                                       cur_file_offset, io_buf, io_type, _io_addrs_ptr, _io_sizes_ptr,
                                       _io_bufs_ptr) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");

                mem_offset += stripe_size;
            }
        }

        offset_in_block += (int64_t)*_io_sizes_ptr;

        subfile_idx++;

        if (subfile_idx == num_subfiles) {
            subfile_idx     = 0;
            offset_in_block = 0;

            row_offset += block_size;
        }

        assert(offset_in_block <= block_size);
    }

    if (total_bytes != io_size)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "total bytes (%zu) didn't match data size (%zu)!",
                                total_bytes, io_size);

done:
    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    iovec_fill_first
 *
 * Purpose:     Fills I/O vectors for the case where the IOC has the first
 *              data segment of the I/O operation.
 *
 *              If the 'first_io_len' is sufficient to complete the I/O to
 *              the IOC, then the first entry in the I/O vectors is simply
 *              filled out with the given starting memory/file offsets and
 *              the first I/O size. Otherwise, the remaining entries in the
 *              I/O vectors are filled out as data segments with size equal
 *              to the stripe size. Each data segment is separated from a
 *              previous or following segment by 'sf_blocksize_per_stripe'
 *              bytes of data.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_first(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                 int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                 int64_t first_io_len, H5_flexible_const_ptr_t buf, H5FD_subfiling_io_type_t io_type,
                 haddr_t *io_addrs_ptr, size_t *io_sizes_ptr, H5_flexible_const_ptr_t *io_bufs_ptr)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    assert(sf_context);
    assert(cur_iovec_depth > 0);
    assert(io_addrs_ptr);
    assert(io_sizes_ptr);
    assert(io_bufs_ptr);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", first_io_len = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, first_io_len);
#endif

    *io_addrs_ptr = (haddr_t)start_file_offset;
    *io_sizes_ptr = (size_t)first_io_len;

    if (io_type == IO_TYPE_WRITE)
        io_bufs_ptr->cvp = (const char *)(buf.cvp) + start_mem_offset;
    else
        io_bufs_ptr->vp = (char *)(buf.vp) + start_mem_offset;

    if (first_io_len == target_datasize)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    if (first_io_len > 0) {
        int64_t offset_in_stripe = start_file_offset % stripe_size;
        int64_t next_mem_offset  = block_size - offset_in_stripe;
        int64_t next_file_offset = start_file_offset + (block_size - offset_in_stripe);

        total_bytes = first_io_len;

        for (size_t i = 1; i < (size_t)cur_iovec_depth; i++) {
            *(io_addrs_ptr + (i * iovec_len)) = (haddr_t)next_file_offset;
            *(io_sizes_ptr + (i * iovec_len)) = (size_t)stripe_size;

            if (io_type == IO_TYPE_WRITE)
                (io_bufs_ptr + (i * iovec_len))->cvp = (const char *)(buf.cvp) + next_mem_offset;
            else
                (io_bufs_ptr + (i * iovec_len))->vp = (char *)(buf.vp) + next_mem_offset;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%zu] = %" PRId64 ", file_offset[%zu] = %" PRId64
                             ", io_block_len[%zu] = %" PRId64,
                             __func__, i, next_mem_offset, i, next_file_offset, i, stripe_size);
#endif

            next_mem_offset += block_size;
            next_file_offset += block_size;
            total_bytes += stripe_size;
        }

        if (total_bytes != target_datasize)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                                    "total bytes (%" PRId64 ") didn't match target data size (%" PRId64 ")!",
                                    total_bytes, target_datasize);
    }

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    iovec_fill_last
 *
 * Purpose:     Fills I/O vectors for the case where the IOC has the last
 *              data segment of the I/O operation.
 *
 *              If the 'last_io_len' is sufficient to complete the I/O to
 *              the IOC, then the first entry in the I/O vectors is simply
 *              filled out with the given starting memory/file offsets and
 *              the last I/O size. Otherwise, all entries in the I/O
 *              vectors except the last entry are filled out as data
 *              segments with size equal to the stripe size. Each data
 *              segment is separated from a previous or following segment
 *              by 'sf_blocksize_per_stripe' bytes of data. Then, the last
 *              entry in the I/O vectors is filled out with the final
 *              memory/file offsets and the last I/O size.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_last(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                int64_t last_io_len, H5_flexible_const_ptr_t buf, H5FD_subfiling_io_type_t io_type,
                haddr_t *io_addrs_ptr, size_t *io_sizes_ptr, H5_flexible_const_ptr_t *io_bufs_ptr)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    assert(sf_context);
    assert(cur_iovec_depth > 0);
    assert(io_addrs_ptr);
    assert(io_sizes_ptr);
    assert(io_bufs_ptr);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", last_io_len = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, last_io_len);
#endif

    *io_addrs_ptr = (haddr_t)start_file_offset;
    *io_sizes_ptr = (size_t)last_io_len;

    if (io_type == IO_TYPE_WRITE)
        io_bufs_ptr->cvp = (const char *)(buf.cvp) + start_mem_offset;
    else
        io_bufs_ptr->vp = (char *)(buf.vp) + start_mem_offset;

    if (last_io_len == target_datasize)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    {
        int64_t next_mem_offset  = start_mem_offset + block_size;
        int64_t next_file_offset = start_file_offset + block_size;
        size_t  i;

        /*
         * If the last I/O size doesn't cover the target data
         * size, there is at least one full stripe preceding
         * the last I/O block
         */
        *io_sizes_ptr = (size_t)stripe_size;

        total_bytes = stripe_size;

        for (i = 1; i < (size_t)cur_iovec_depth - 1;) {
            *(io_addrs_ptr + (i * iovec_len)) = (haddr_t)next_file_offset;
            *(io_sizes_ptr + (i * iovec_len)) = (size_t)stripe_size;

            if (io_type == IO_TYPE_WRITE)
                (io_bufs_ptr + (i * iovec_len))->cvp = (const char *)(buf.cvp) + next_mem_offset;
            else
                (io_bufs_ptr + (i * iovec_len))->vp = (char *)(buf.vp) + next_mem_offset;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%zu] = %" PRId64 ", file_offset[%zu] = %" PRId64
                             ", io_block_len[%zu] = %" PRId64,
                             __func__, i, next_mem_offset, i, next_file_offset, i, stripe_size);
#endif

            next_mem_offset += block_size;
            next_file_offset += block_size;
            total_bytes += stripe_size;

            i++;
        }

        *(io_addrs_ptr + (i * iovec_len)) = (haddr_t)next_file_offset;
        *(io_sizes_ptr + (i * iovec_len)) = (size_t)last_io_len;

        if (io_type == IO_TYPE_WRITE)
            (io_bufs_ptr + (i * iovec_len))->cvp = (const char *)(buf.cvp) + next_mem_offset;
        else
            (io_bufs_ptr + (i * iovec_len))->vp = (char *)(buf.vp) + next_mem_offset;

#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id,
                         "%s: mem_offset[%zu] = %" PRId64 ", file_offset[%zu] = %" PRId64
                         ", io_block_len[%zu] = %" PRId64,
                         __func__, i, next_mem_offset, i, next_file_offset, i, last_io_len);
#endif

        total_bytes += last_io_len;

        if (total_bytes != target_datasize)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                                    "total bytes (%" PRId64 ") didn't match target data size (%" PRId64 ")!",
                                    total_bytes, target_datasize);
    }

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    iovec_fill_first_last
 *
 * Purpose:     Fills I/O vectors for the case where the IOC has the first
 *              and last data segments of the I/O operation. This function
 *              is essentially a merge of the iovec_fill_first and
 *              iovec_fill_last functions.
 *
 *              If the 'first_io_len' is sufficient to complete the I/O to
 *              the IOC, then the first entry in the I/O vectors is simply
 *              filled out with the given starting memory/file offsets and
 *              the first I/O size. Otherwise, the remaining entries in the
 *              I/O vectors except the last are filled out as data segments
 *              with size equal to the stripe size. Each data segment is
 *              separated from a previous or following segment by
 *              'sf_blocksize_per_stripe' bytes of data. Then, the last
 *              entry in the I/O vectors is filled out with the final
 *              memory/file offsets and the last I/O size.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_first_last(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                      int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                      int64_t first_io_len, int64_t last_io_len, H5_flexible_const_ptr_t buf,
                      H5FD_subfiling_io_type_t io_type, haddr_t *io_addrs_ptr, size_t *io_sizes_ptr,
                      H5_flexible_const_ptr_t *io_bufs_ptr)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    assert(sf_context);
    assert(cur_iovec_depth > 0);
    assert(io_addrs_ptr);
    assert(io_sizes_ptr);
    assert(io_bufs_ptr);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", first_io_len = %" PRId64 ", last_io_len = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, first_io_len, last_io_len);
#endif

    *io_addrs_ptr = (haddr_t)start_file_offset;
    *io_sizes_ptr = (size_t)first_io_len;

    if (io_type == IO_TYPE_WRITE)
        io_bufs_ptr->cvp = (const char *)(buf.cvp) + start_mem_offset;
    else
        io_bufs_ptr->vp = (char *)(buf.vp) + start_mem_offset;

    if (first_io_len == target_datasize)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    if (first_io_len > 0) {
        int64_t offset_in_stripe = start_file_offset % stripe_size;
        int64_t next_mem_offset  = block_size - offset_in_stripe;
        int64_t next_file_offset = start_file_offset + (block_size - offset_in_stripe);
        size_t  i;

        total_bytes = first_io_len;

        for (i = 1; i < (size_t)cur_iovec_depth - 1;) {
            *(io_addrs_ptr + (i * iovec_len)) = (haddr_t)next_file_offset;
            *(io_sizes_ptr + (i * iovec_len)) = (size_t)stripe_size;

            if (io_type == IO_TYPE_WRITE)
                (io_bufs_ptr + (i * iovec_len))->cvp = (const char *)(buf.cvp) + next_mem_offset;
            else
                (io_bufs_ptr + (i * iovec_len))->vp = (char *)(buf.vp) + next_mem_offset;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%zu] = %" PRId64 ", file_offset[%zu] = %" PRId64
                             ", io_block_len[%zu] = %" PRId64,
                             __func__, i, next_mem_offset, i, next_file_offset, i, stripe_size);
#endif

            next_mem_offset += block_size;
            next_file_offset += block_size;
            total_bytes += stripe_size;

            i++;
        }

        *(io_addrs_ptr + (i * iovec_len)) = (haddr_t)next_file_offset;
        *(io_sizes_ptr + (i * iovec_len)) = (size_t)last_io_len;

        if (io_type == IO_TYPE_WRITE)
            (io_bufs_ptr + (i * iovec_len))->cvp = (const char *)(buf.cvp) + next_mem_offset;
        else
            (io_bufs_ptr + (i * iovec_len))->vp = (char *)(buf.vp) + next_mem_offset;

#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id,
                         "%s: mem_offset[%zu] = %" PRId64 ", file_offset[%zu] = %" PRId64
                         ", io_block_len[%zu] = %" PRId64,
                         __func__, i, next_mem_offset, i, next_file_offset, i, last_io_len);
#endif

        total_bytes += last_io_len;

        if (total_bytes != target_datasize)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                                    "total bytes (%" PRId64 ") didn't match target data size (%" PRId64 ")!",
                                    total_bytes, target_datasize);
    }

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    iovec_fill_uniform
 *
 * Purpose:     Fills I/O vectors for the typical I/O operation when
 *              reading data from or writing data to an I/O Concentrator
 *              (IOC).
 *
 *              Each data segment is of 'stripe_size' length and will be
 *              separated from a previous or following segment by
 *              'sf_blocksize_per_stripe' bytes of data.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_uniform(subfiling_context_t *sf_context, size_t iovec_len, int64_t cur_iovec_depth,
                   int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                   H5_flexible_const_ptr_t buf, H5FD_subfiling_io_type_t io_type, haddr_t *io_addrs_ptr,
                   size_t *io_sizes_ptr, H5_flexible_const_ptr_t *io_bufs_ptr)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    assert(sf_context);
    assert((cur_iovec_depth > 0) || (target_datasize == 0));
    assert(io_addrs_ptr);
    assert(io_sizes_ptr);
    assert(io_bufs_ptr);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", segment size = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, stripe_size);
#endif

    *io_addrs_ptr = (haddr_t)start_file_offset;
    *io_sizes_ptr = (size_t)stripe_size;

    if (io_type == IO_TYPE_WRITE)
        io_bufs_ptr->cvp = (const char *)(buf.cvp) + start_mem_offset;
    else
        io_bufs_ptr->vp = (char *)(buf.vp) + start_mem_offset;

    if (target_datasize == 0) {
#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id, "%s: target_datasize = 0", __func__);
#endif

        *io_sizes_ptr = (size_t)0;
        H5_SUBFILING_GOTO_DONE(SUCCEED);
    }

    if (target_datasize > stripe_size) {
        int64_t next_mem_offset  = start_mem_offset + block_size;
        int64_t next_file_offset = start_file_offset + block_size;

        total_bytes = stripe_size;

        for (size_t i = 1; i < (size_t)cur_iovec_depth; i++) {
            *(io_addrs_ptr + (i * iovec_len)) = (haddr_t)next_file_offset;
            *(io_sizes_ptr + (i * iovec_len)) = (size_t)stripe_size;

            if (io_type == IO_TYPE_WRITE)
                (io_bufs_ptr + (i * iovec_len))->cvp = (const char *)(buf.cvp) + next_mem_offset;
            else
                (io_bufs_ptr + (i * iovec_len))->vp = (char *)(buf.vp) + next_mem_offset;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%zu] = %" PRId64 ", file_offset[%zu] = %" PRId64
                             ", io_block_len[%zu] = %" PRId64,
                             __func__, i, next_mem_offset, i, next_file_offset, i, stripe_size);
#endif

            next_mem_offset += block_size;
            next_file_offset += block_size;
            total_bytes += stripe_size;
        }

        if (total_bytes != target_datasize)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                                    "total bytes (%" PRId64 ") didn't match target data size (%" PRId64 ")!",
                                    total_bytes, target_datasize);
    }

done:
    return ret_value;
}

#ifdef H5_SUBFILING_DEBUG
void
H5_subfiling_dump_iovecs(subfiling_context_t *sf_context, size_t ioreq_count, size_t iovec_len,
                         H5FD_subfiling_io_type_t io_type, H5FD_mem_t *io_types, haddr_t *io_addrs,
                         size_t *io_sizes, H5_flexible_const_ptr_t *io_bufs)
{
    assert(sf_context);
    assert(io_types);
    assert(io_addrs);
    assert(io_sizes);
    assert(io_bufs);

    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: I/O REQUEST VECTORS (mem type, addr, size, buf):", __func__);

    for (size_t ioreq_idx = 0; ioreq_idx < ioreq_count; ioreq_idx++) {
        H5_subfiling_log_nonewline(sf_context->sf_context_id, "  -> I/O REQUEST %zu: ", ioreq_idx);

        H5_subfiling_log_nonewline(sf_context->sf_context_id, "[");
        for (size_t i = 0; i < iovec_len; i++) {
            if (i > 0)
                H5_subfiling_log_nonewline(sf_context->sf_context_id, ", ");

            H5_subfiling_log_nonewline(
                sf_context->sf_context_id, "(%d, %" PRIuHADDR ", %zu, %p)",
                *(io_types + (ioreq_idx * iovec_len) + i), *(io_addrs + (ioreq_idx * iovec_len) + i),
                *(io_sizes + (ioreq_idx * iovec_len) + i),
                (io_type == IO_TYPE_WRITE) ? (const void *)(io_bufs + (ioreq_idx * iovec_len) + i)->cvp
                                           : (void *)(io_bufs + (ioreq_idx * iovec_len) + i)->vp);
        }
        H5_subfiling_log_nonewline(sf_context->sf_context_id, "]\n");
    }
}
#endif
