/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Programmer:  Richard Warren
 *
 *
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
static hbool_t H5FD_mpi_self_initialized = FALSE;

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
 * Programmer: Richard Warren
 *
 ***************************************************************************/

typedef struct H5FD_subfiling_t {
    H5FD_t                  pub; /* public stuff, must be first      */
    int                     fd;  /* the filesystem file descriptor   */
    H5FD_subfiling_config_t fa;  /* driver-specific file access properties */

    /* MPI Info */
    MPI_Comm comm;
    MPI_Comm ext_comm;
    MPI_Info info;
    int      mpi_rank;
    int      mpi_size;

    H5FD_t *sf_file;

    int64_t context_id; /* The value used to lookup a subfiling context for the file */

    char *file_dir;  /* Directory where we find files */
    char *file_path; /* The user defined filename */

#ifndef H5_HAVE_WIN32_API
    /* On most systems the combination of device and i-node number uniquely
     * identify a file.  Note that Cygwin, MinGW and other Windows POSIX
     * environments have the stat function (which fakes inodes)
     * and will use the 'device + inodes' scheme as opposed to the
     * Windows code further below.
     */
    dev_t device; /* file device number   */
    ino_t inode;  /* file i-node number   */
#else
    /* Files in windows are uniquely identified by the volume serial
     * number and the file index (both low and high parts).
     *
     * There are caveats where these numbers can change, especially
     * on FAT file systems.  On NTFS, however, a file should keep
     * those numbers the same until renamed or deleted (though you
     * can use ReplaceFile() on NTFS to keep the numbers the same
     * while renaming).
     *
     * See the MSDN "BY_HANDLE_FILE_INFORMATION Structure" entry for
     * more information.
     *
     * http://msdn.microsoft.com/en-us/library/aa363788(v=VS.85).aspx
     */
    DWORD nFileIndexLow;
    DWORD nFileIndexHigh;
    DWORD dwVolumeSerialNumber;

    HANDLE hFile; /* Native windows file handle */
#endif /* H5_HAVE_WIN32_API */

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

#define H5FD_SUBFILING_DEBUG_OP_CALLS 0 /* debugging print toggle; 0 disables */

#if H5FD_SUBFILING_DEBUG_OP_CALLS
#define H5FD_SUBFILING_LOG_CALL(name)                                                                        \
    do {                                                                                                     \
        HDprintf("called %s()\n", (name));                                                                   \
        HDfflush(stdout);                                                                                    \
    } while (0)
#else
#define H5FD_SUBFILING_LOG_CALL(name) /* no-op */
#endif                                /* H5FD_SUBFILING_DEBUG_OP_CALLS */

/* Prototypes */
static herr_t  H5FD__subfiling_term(void);
static void *  H5FD__subfiling_fapl_get(H5FD_t *_file);
static void *  H5FD__subfiling_fapl_copy(const void *_old_fa);
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
static herr_t  H5FD__subfiling_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
#if 0
static herr_t  H5FD__subfiling_lock(H5FD_t *_file, hbool_t rw);
static herr_t  H5FD__subfiling_unlock(H5FD_t *_file);
#endif
static herr_t H5FD__subfiling_del(const char *name, hid_t fapl);
static herr_t H5FD__subfiling_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags, const void *input,
                                  void **output);

static herr_t H5FD__subfiling_get_default_config(hid_t fapl_id, H5FD_subfiling_config_t *config_out);
static herr_t H5FD__subfiling_validate_config(const H5FD_subfiling_config_t *fa);
static int    H5FD__copy_plist(hid_t fapl_id, hid_t *id_out_ptr);

static herr_t H5FD__subfiling_close_int(H5FD_subfiling_t *file_ptr);

static herr_t init_indep_io(subfiling_context_t *sf_context, int64_t file_offset, size_t io_nelemts,
                            size_t dtype_extent, size_t max_iovec_len, int64_t *mem_buf_offset,
                            int64_t *target_file_offset, int64_t *io_block_len, int *first_ioc_index,
                            int *n_iocs_used, int64_t *max_io_req_per_ioc);
static herr_t iovec_fill_first(subfiling_context_t *sf_context, int64_t iovec_depth, int64_t target_datasize,
                               int64_t start_mem_offset, int64_t start_file_offset, int64_t first_io_len,
                               int64_t *mem_offset_out, int64_t *target_file_offset_out,
                               int64_t *io_block_len_out);
static herr_t iovec_fill_last(subfiling_context_t *sf_context, int64_t iovec_depth, int64_t target_datasize,
                              int64_t start_mem_offset, int64_t start_file_offset, int64_t last_io_len,
                              int64_t *mem_offset_out, int64_t *target_file_offset_out,
                              int64_t *io_block_len_out);
static herr_t iovec_fill_first_last(subfiling_context_t *sf_context, int64_t iovec_depth,
                                    int64_t target_datasize, int64_t start_mem_offset,
                                    int64_t start_file_offset, int64_t first_io_len, int64_t last_io_len,
                                    int64_t *mem_offset_out, int64_t *target_file_offset_out,
                                    int64_t *io_block_len_out);
static herr_t iovec_fill_uniform(subfiling_context_t *sf_context, int64_t iovec_depth,
                                 int64_t target_datasize, int64_t start_mem_offset, int64_t start_file_offset,
                                 int64_t *mem_offset_out, int64_t *target_file_offset_out,
                                 int64_t *io_block_len_out);

void H5FD__subfiling_mpi_finalize(void);

static const H5FD_class_t H5FD_subfiling_g = {
    H5FD_CLASS_VERSION,                /* VFD interface version */
    H5_VFD_SUBFILING,                  /* value                 */
    H5FD_SUBFILING_NAME,               /* name                  */
    MAXADDR,                           /* maxaddr               */
    H5F_CLOSE_WEAK,                    /* fc_degree             */
    H5FD__subfiling_term,              /* terminate             */
    NULL,                              /* sb_size               */
    NULL,                              /* sb_encode             */
    NULL,                              /* sb_decode             */
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
 * Programmer:  Richard Warren
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

        if ((H5FD_SUBFILING_g = H5FD_register(&H5FD_subfiling_g, sizeof(H5FD_class_t), FALSE)) < 0)
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
            char *env_var;
            int   required = MPI_THREAD_MULTIPLE;

            /* Ensure that Subfiling VFD has been loaded dynamically */
            env_var = HDgetenv(HDF5_DRIVER);
            if (!env_var || HDstrcmp(env_var, H5FD_SUBFILING_NAME))
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID, "MPI isn't initialized");

            if (MPI_SUCCESS != (mpi_code = MPI_Init_thread(NULL, NULL, required, &provided)))
                H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Init_thread failed", mpi_code);

            H5FD_mpi_self_initialized = TRUE;

            if (provided != required)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                        "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE");

            if (HDatexit(H5FD__subfiling_mpi_finalize) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                        "can't register atexit handler for MPI_Finalize");
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
        /* Free the subfiling application layout information */
        if (sf_app_layout) {
            HDfree(sf_app_layout->layout);
            sf_app_layout->layout = NULL;

            HDfree(sf_app_layout->node_ranks);
            sf_app_layout->node_ranks = NULL;

            HDfree(sf_app_layout);
            sf_app_layout = NULL;
        }

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
 * Programmer:  John Mainzer
 *              9/10/17
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_subfiling(hid_t fapl_id, const H5FD_subfiling_config_t *vfd_config)
{
    H5FD_subfiling_config_t *subfiling_conf = NULL;
    H5P_genplist_t *         plist          = NULL;
    herr_t                   ret_value      = SUCCEED;

    /*NO TRACE*/

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (vfd_config == NULL) {
        if (NULL == (subfiling_conf = HDcalloc(1, sizeof(*subfiling_conf))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate subfiling VFD configuration");
        subfiling_conf->ioc_fapl_id = H5I_INVALID_HID;

        /* Get subfiling VFD defaults */
        if (H5FD__subfiling_get_default_config(fapl_id, subfiling_conf) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL,
                                    "can't get default subfiling VFD configuration");

        vfd_config = subfiling_conf;
    }

    if (H5FD__subfiling_validate_config(vfd_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid subfiling VFD configuration");

    ret_value = H5P_set_driver(plist, H5FD_SUBFILING, vfd_config, NULL);

done:
    if (subfiling_conf) {
        if (subfiling_conf->ioc_fapl_id >= 0 && H5I_dec_ref(subfiling_conf->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTDEC, FAIL, "can't close IOC FAPL");
        HDfree(subfiling_conf);
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
 * Programmer:  John Mainzer
 *              9/10/17
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *config_out)
{
    const H5FD_subfiling_config_t *config_ptr         = NULL;
    H5P_genplist_t *               plist              = NULL;
    hbool_t                        use_default_config = FALSE;
    herr_t                         ret_value          = SUCCEED;

    /*NO TRACE*/

    if (config_out == NULL)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL");

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (H5FD_SUBFILING != H5P_peek_driver(plist))
        use_default_config = TRUE;
    else {
        config_ptr = H5P_peek_driver_info(plist);
        if (NULL == config_ptr)
            use_default_config = TRUE;
    }

    if (use_default_config) {
        if (H5FD__subfiling_get_default_config(fapl_id, config_out) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL,
                                    "can't get default Subfiling VFD configuration");
    }
    else {
        /* Copy the subfiling fapl data out */
        HDmemcpy(config_out, config_ptr, sizeof(H5FD_subfiling_config_t));

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
    char *   h5_require_ioc;
    herr_t   ret_value = SUCCEED;

    HDassert(config_out);

    HDmemset(config_out, 0, sizeof(*config_out));

    config_out->magic       = H5FD_SUBFILING_FAPL_MAGIC;
    config_out->version     = H5FD_CURR_SUBFILING_FAPL_VERSION;
    config_out->ioc_fapl_id = H5I_INVALID_HID;
    config_out->require_ioc = TRUE;

    config_out->shared_cfg.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    config_out->shared_cfg.stripe_size   = H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;
    config_out->shared_cfg.stripe_count  = 0;

    if ((h5_require_ioc = HDgetenv("H5_REQUIRE_IOC")) != NULL) {
        int value_check = HDatoi(h5_require_ioc);
        if (value_check == 0)
            config_out->require_ioc = FALSE;
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
        H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't free MPI Communicator");
    if (H5_mpi_info_free(&info) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTFREE, FAIL, "can't free MPI Info object");

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

    HDassert(fa != NULL);

    if (fa->version != H5FD_CURR_SUBFILING_FAPL_VERSION)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown H5FD_subfiling_config_t version");

    if (fa->magic != H5FD_SUBFILING_FAPL_MAGIC)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid H5FD_subfiling_config_t magic value");

    /* TODO: add extra subfiling configuration validation code */

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__subfiling_validate_config() */

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
 * Programmer:  John Mainzer
 *              9/8/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD__subfiling_fapl_get(H5FD_t *_file)
{
    H5FD_subfiling_t *       file      = (H5FD_subfiling_t *)_file;
    H5FD_subfiling_config_t *fa        = NULL;
    void *                   ret_value = NULL;

    fa = (H5FD_subfiling_config_t *)H5MM_calloc(sizeof(H5FD_subfiling_config_t));

    if (fa == NULL) {
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    }

    /* Copy the fields of the structure */
    HDmemcpy(fa, &(file->fa), sizeof(H5FD_subfiling_config_t));

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

    H5FD_SUBFILING_LOG_CALL(__func__);

    HDassert(id_out_ptr != NULL);

    if (FALSE == H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, -1, "not a file access property list");

    plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id);
    if (NULL == plist_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, -1, "unable to get property list");

    *id_out_ptr = H5P_copy_plist(plist_ptr, FALSE);
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
 * Programmer:  John Mainzer
 *              9/8/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD__subfiling_fapl_copy(const void *_old_fa)
{
    const H5FD_subfiling_config_t *old_fa    = (const H5FD_subfiling_config_t *)_old_fa;
    H5FD_subfiling_config_t *      new_fa    = NULL;
    void *                         ret_value = NULL;

    new_fa = (H5FD_subfiling_config_t *)H5MM_malloc(sizeof(H5FD_subfiling_config_t));
    if (new_fa == NULL) {
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    }

    HDmemcpy(new_fa, old_fa, sizeof(H5FD_subfiling_config_t));

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
 * Programmer:  John Mainzer
 *              9/8/17
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_fapl_free(void *_fa)
{
    H5FD_subfiling_config_t *fa        = (H5FD_subfiling_config_t *)_fa;
    herr_t                   ret_value = SUCCEED;

    HDassert(fa != NULL); /* sanity check */

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
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__subfiling_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_subfiling_t *             file_ptr   = NULL; /* Subfiling VFD info */
    const H5FD_subfiling_config_t *config_ptr = NULL; /* Driver-specific property list */
    H5FD_subfiling_config_t        default_config;
    H5FD_class_t *                 driver    = NULL; /* VFD for file */
    H5P_genplist_t *               plist_ptr = NULL;
    H5FD_driver_prop_t             driver_prop; /* Property for driver ID & info */
    hbool_t                        bcasted_eof = FALSE;
    int64_t                        sf_eof      = -1;
    void *                         file_handle = NULL;
    int                            mpi_code; /* MPI return code */
    H5FD_t *                       ret_value = NULL;

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
    file_ptr->context_id     = -1;
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;
    file_ptr->ext_comm       = MPI_COMM_NULL;

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

    HDmemcpy(&file_ptr->fa, config_ptr, sizeof(H5FD_subfiling_config_t));

    if (NULL != (file_ptr->file_path = HDrealpath(name, NULL))) {
        char *path      = NULL;
        char *directory = dirname(path);

        if (NULL == (path = HDstrdup(file_ptr->file_path)))
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTCOPY, NULL, "can't copy subfiling subfile path");
        if (NULL == (file_ptr->file_dir = HDstrdup(directory))) {
            HDfree(path);
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTCOPY, NULL,
                                    "can't copy subfiling subfile directory path");
        }

        HDfree(path);
    }
    else {
        if (ENOENT == errno) {
            if (NULL == (file_ptr->file_path = HDstrdup(name)))
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTCOPY, NULL, "can't copy file name");
            if (NULL == (file_ptr->file_dir = HDstrdup(".")))
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "can't set subfile directory path");
        }
        else
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't resolve subfile path");
    }

    if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(file_ptr->fa.ioc_fapl_id)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy FAPL");

    file_ptr->sf_file = H5FD_open(name, flags, file_ptr->fa.ioc_fapl_id, HADDR_UNDEF);
    if (!file_ptr->sf_file)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "unable to open IOC file");

    /* Check the "native" driver (IOC/sec2/etc.) */
    if (NULL == (plist_ptr = H5I_object(file_ptr->fa.ioc_fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "invalid IOC FAPL");

    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver ID & info");
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL,
                                "invalid driver ID in file access property list");

    if (driver->value != H5_VFD_IOC && driver->value != H5_VFD_SEC2)
        H5_SUBFILING_GOTO_ERROR(
            H5E_FILE, H5E_CANTOPENFILE, NULL,
            "unable to open file '%s' - only IOC and Sec2 VFDs are currently supported for subfiles", name);

    if (H5FDget_vfd_handle(file_ptr->sf_file, file_ptr->fa.ioc_fapl_id, &file_handle) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "can't get file handle");

    if (driver->value == H5_VFD_IOC) {
        /* Get a copy of the context ID for later use */
        file_ptr->context_id     = H5_subfile_fhandle_to_context(file_handle);
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
        if (H5_open_subfiles(file_ptr->file_path, file_handle, file_ptr->fa.shared_cfg.ioc_selection,
                             ioc_flags, file_ptr->comm, &file_ptr->context_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiling files = %s\n",
                                    name);
    }

    if (file_ptr->mpi_rank == 0) {
        if (H5FD__subfiling__get_real_eof(file_ptr->context_id, &sf_eof) < 0)
            sf_eof = -1;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_eof, 1, MPI_INT64_T, 0, file_ptr->comm)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Bcast", mpi_code);

    bcasted_eof = TRUE;

    if (sf_eof < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "lead MPI process failed to get file EOF");

    file_ptr->eof       = (haddr_t)sf_eof;
    file_ptr->local_eof = file_ptr->eof;

    ret_value = (H5FD_t *)file_ptr;

done:
    if (NULL == ret_value) {
        if (file_ptr) {
            /* Participate in possible MPI collectives on failure */
            if (file_ptr->comm != MPI_COMM_NULL) {
                if (!bcasted_eof) {
                    sf_eof = -1;

                    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_eof, 1, MPI_INT64_T, 0, file_ptr->comm)))
                        H5_SUBFILING_MPI_DONE_ERROR(NULL, "MPI_Bcast failed", mpi_code);
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
    herr_t ret_value = SUCCEED;

    HDassert(file_ptr);

#if H5FD_SUBFILING_DEBUG_OP_CALLS
    {
        subfiling_context_t *sf_context = H5_get_subfiling_object(file_ptr->context_id);

        HDassert(sf_context);
        HDassert(sf_context->topology);

        if (sf_context->topology->rank_is_ioc)
            HDprintf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
        else
            HDprintf("[%s %d] fd=*\n", __func__, file_ptr->mpi_rank);
        HDfflush(stdout);
    }
#endif

    if (file_ptr->sf_file && H5FD_close(file_ptr->sf_file) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close subfile");

    if (!file_ptr->fa.require_ioc) {
        if (file_ptr->context_id >= 0 && H5_free_subfiling_object(file_ptr->context_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free subfiling context object");
    }

    /* if set, close the copy of the plist for the underlying VFD. */
    if ((file_ptr->fa.ioc_fapl_id >= 0) && (H5I_dec_ref(file_ptr->fa.ioc_fapl_id) < 0))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_ARGS, FAIL, "can't close IOC FAPL");
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;

    if (H5_mpi_comm_free(&file_ptr->comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Communicator");
    if (H5_mpi_info_free(&file_ptr->info) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Info object");

    if (H5_mpi_comm_free(&file_ptr->ext_comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI communicator");

done:
    HDfree(file_ptr->file_path);
    file_ptr->file_path = NULL;

    HDfree(file_ptr->file_dir);
    file_ptr->file_dir = NULL;

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
 * Programmer:  Richard Warren
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
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD__subfiling_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_subfiling_t *f1        = (const H5FD_subfiling_t *)_f1;
    const H5FD_subfiling_t *f2        = (const H5FD_subfiling_t *)_f2;
    int                     ret_value = 0;

    HDassert(f1);
    HDassert(f2);

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
 * Programmer:  John Mainzer
 *              11/15/21
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
        *flags |= H5FD_FEAT_ALLOCATE_EARLY;      /* Allocate space early instead of late  */
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
 * Programmer:  Richard Warren
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
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    file_ptr->eoa = addr;

    ret_value = H5FD_set_eoa(file_ptr->sf_file, type, addr);

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
 *              SUBFILING NOTE:
 *              The EOF calculation for subfiling is somewhat different
 *              than for the more traditional HDF5 file implementations.
 *              This statement derives from the fact that unlike "normal"
 *              HDF5 files, subfiling introduces a multi-file representation
 *              of a single HDF5 file.  The plurality of sub-files represents
 *              a software RAID-0 based HDF5 file.  As such, each sub-file
 *              contains a designated portion of the address space of the
 *              virtual HDF5 storage.  We have no notion of HDF5 datatypes,
 *              datasets, metadata, or other HDF5 structures; only BYTES.
 *
 *              The organization of the bytes within sub-files is consistent
 *              with the RAID-0 striping, i.e. there are IO Concentrators
 *              (IOCs) which correspond to a stripe-count (in Lustre) as
 *              well as a stripe_size.  The combination of these two
 *              variables determines the "address" (a combination of IOC
 *              and a file offset) of any storage operation.
 *
 *              Having a defined storage layout, the virtual file EOF
 *              calculation should be the MAXIMUM value returned by the
 *              collection of IOCs.  Every MPI rank which hosts an IOC
 *              maintains its own EOF by updating that value for each
 *              WRITE operation that completes, i.e. if a new local EOF
 *              is greater than the existing local EOF, the new EOF
 *              will replace the old.  The local EOF calculation is as
 *              follows.
 *              1. At file creation, each IOC is assigned a rank value
 *                 (0 to N-1, where N is the total number of IOCs) and
 *                 a 'sf_base_addr' = 'subfile_rank' * 'sf_stripe_size')
 *                 we also determine the 'sf_blocksize_per_stripe' which
 *                 is simply the 'sf_stripe_size' * 'n_ioc_concentrators'
 *
 *              2. For every write operation, the IOC receives a message
 *                 containing a file_offset and the data_size.
 *
 *              3. The file_offset + data_size are in turn used to
 *                 create a stripe_id:
 *                   IOC-(ioc_rank)       IOC-(ioc_rank+1)
 *                   |<- sf_base_address  |<- sf_base_address  |
 *                ID +--------------------+--------------------+
 *                 0:|<- sf_stripe_size ->|<- sf_stripe_size ->|
 *                 1:|<- sf_stripe_size ->|<- sf_stripe_size ->|
 *                   ~                    ~                    ~
 *                 N:|<- sf_stripe_size ->|<- sf_stripe_size ->|
 *                   +--------------------+--------------------+
 *
 *                The new 'stripe_id' is then used to calculate a
 *                potential new EOF:
 *                sf_eof = (stripe_id * sf_blocksize_per_stripe) + sf_base_addr
 *                         + ((file_offset + data_size) % sf_stripe_size)
 *
 *              4. If (sf_eof > current_sf_eof), then current_sf_eof = sf_eof.
 *
 *
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__subfiling_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_subfiling_t *file = (const H5FD_subfiling_t *)_file;
#if 0
    int64_t                 logical_eof = -1;
#endif
    haddr_t ret_value = HADDR_UNDEF;

#if 0
    /*
     * TODO: this is a heavy weight implementation.  We need something like this
     * for file open, and probably for file close.  However, in between, something
     * similar to the current solution in the MPIIO VFD might be more appropriate.
     */
    if (H5FD__subfiling__get_real_eof(file->fa.context_id, &logical_eof) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, HADDR_UNDEF, "can't get EOF")

    /* Return the global max of all the subfile EOF values */
    ret_value = (haddr_t)(logical_eof);

done:
#endif

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

    *file_handle = &(file->fd);

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
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                     void *buf /*out*/)
{
    subfiling_context_t *sf_context         = NULL;
    H5FD_subfiling_t *   file_ptr           = (H5FD_subfiling_t *)_file;
    H5FD_mem_t *         io_types           = NULL;
    haddr_t *            io_addrs           = NULL;
    size_t *             io_sizes           = NULL;
    void **              io_bufs            = NULL;
    int64_t *            source_data_offset = NULL;
    int64_t *            sf_data_size       = NULL;
    int64_t *            sf_offset          = NULL;
    hbool_t              rank0_bcast        = FALSE;
    int                  ioc_total;
    herr_t               ret_value = SUCCEED;

    HDassert(file_ptr && file_ptr->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if (!H5F_addr_defined(addr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %" PRIuHADDR, addr);
    if (REGION_OVERFLOW(addr, size))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                                "addr overflow, addr = %" PRIuHADDR ", size = %" PRIuHADDR, addr, size);

    /* TODO: Temporarily reject collective I/O until support is implemented (unless types are simple MPI_BYTE)
     */
    {
        H5FD_mpio_xfer_t xfer_mode;

        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL,
                                    "can't determine I/O collectivity setting");

        if (xfer_mode == H5FD_MPIO_COLLECTIVE) {
            MPI_Datatype btype, ftype;

            if (H5CX_get_mpi_coll_datatypes(&btype, &ftype) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O datatypes");
            if (MPI_BYTE != btype || MPI_BYTE != ftype)
                H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL,
                                        "collective I/O is currently unsupported");
        }

        /* Determine whether a rank 0 bcast approach has been requested */
        rank0_bcast = H5CX_get_mpio_rank0_bcast();

        /*
         * If we reached here, we're still doing independent I/O regardless
         * of collectivity setting, so set that.
         */
        H5CX_set_io_xfer_mode(H5FD_MPIO_INDEPENDENT);
    }

#if H5FD_SUBFILING_DEBUG_OP_CALLS
    HDprintf("[%s %d] addr=%ld, size=%ld\n", __func__, file_ptr->mpi_rank, addr, size);
    HDfflush(stdout);
#endif

    /*
     * Retrieve the subfiling context object and the number
     * of I/O concentrators.
     *
     * Given the current I/O and the I/O concentrator info,
     * we can determine some I/O transaction parameters.
     * In particular, for large I/O operations, each IOC
     * may require multiple I/Os to fulfill the user I/O
     * request. The block size and number of IOCs are used
     * to size the vectors that will be used to invoke the
     * underlying I/O operations.
     */
    sf_context = (subfiling_context_t *)H5_get_subfiling_object(file_ptr->context_id);
    HDassert(sf_context);
    HDassert(sf_context->topology);

    ioc_total = sf_context->topology->n_io_concentrators;

#if H5FD_SUBFILING_DEBUG_OP_CALLS
    if (sf_context->topology->rank_is_ioc)
        HDprintf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
    else
        HDprintf("[%s %d] fd=*\n", __func__, file_ptr->mpi_rank);
    HDfflush(stdout);
#endif

    if (ioc_total == 0) {
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid number of I/O concentrators (%d)",
                                ioc_total);
    }
    else if (ioc_total == 1) {
        /***********************************
         * No striping - just a single IOC *
         ***********************************/

        /* Make vector read call to subfile */
        if (H5FDread_vector(file_ptr->sf_file, dxpl_id, 1, &type, &addr, &size, &buf) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "read from subfile failed");
    }
    else {
        int64_t max_io_req_per_ioc;
        int64_t file_offset;
        int64_t block_size;
        size_t  max_depth;
        herr_t  status;
        int     ioc_count = 0;
        int     ioc_start = -1;

        /*********************************
         * Striping across multiple IOCs *
         *********************************/

        block_size = sf_context->sf_blocksize_per_stripe;
        max_depth  = (size / (size_t)block_size) + 2;

        /*
         * Given the number of I/O concentrators, allocate vectors (one per IOC)
         * to contain the translation of the I/O request into a collection of I/O
         * requests.
         */
        if (NULL ==
            (source_data_offset = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*source_data_offset))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate source data offset I/O vector");
        if (NULL == (sf_data_size = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_data_size))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate subfile data size I/O vector");
        if (NULL == (sf_offset = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_offset))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate subfile offset I/O vector");

        H5_CHECKED_ASSIGN(file_offset, int64_t, addr, haddr_t);

        /*
         * Get the potential set of IOC transactions; e.g., data sizes,
         * offsets and datatypes. These can all be used by either the
         * underlying IOC or by the sec2 driver.
         *
         * For now, assume we're dealing with contiguous datasets. Vector
         * I/O will probably handle the non-contiguous case.
         */
        status = init_indep_io(sf_context,           /* IN: Context used to look up config info */
                               file_offset,          /* IN: Starting file offset */
                               size,                 /* IN: I/O size */
                               1,                    /* IN: Data extent of the 'type' assumes byte */
                               max_depth,            /* IN: Maximum stripe depth */
                               source_data_offset,   /* OUT: Memory offset */
                               sf_offset,            /* OUT: File offset */
                               sf_data_size,         /* OUT: Length of this contiguous block */
                               &ioc_start,           /* OUT: IOC index corresponding to starting offset */
                               &ioc_count,           /* OUT: Number of actual IOCs used */
                               &max_io_req_per_ioc); /* OUT: Maximum number of requests to any IOC */

        if (status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't initialize IOC transactions");

        if (max_io_req_per_ioc > 0) {
            uint32_t vector_len;

            H5_CHECKED_ASSIGN(vector_len, uint32_t, ioc_count, int);

            /* Allocate I/O vectors */
            if (NULL == (io_types = HDmalloc(vector_len * sizeof(*io_types))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O types vector");
            if (NULL == (io_addrs = HDmalloc(vector_len * sizeof(*io_addrs))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O addresses vector");
            if (NULL == (io_sizes = HDmalloc(vector_len * sizeof(*io_sizes))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O sizes vector");
            if (NULL == (io_bufs = HDmalloc(vector_len * sizeof(*io_bufs))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O buffers vector");

            /* TODO: The following is left for future work */
            /*
             * Set ASYNC MODE
             * H5FD_class_aio_t *async_file_ptr = (H5FD_class_aio_t *)file_ptr->sf_file;
             * uint64_t op_code_begin = OPC_BEGIN;
             * uint64_t op_code_complete = OPC_COMPLETE;
             * const void *input = NULL;
             * void *output = NULL;
             * H5FDctl(file_ptr->sf_file, op_code_begin, flags, input, &output);
             * (*async_file_ptr->h5fdctl)(file_ptr->sf_file, op_code_begin, flags, input, &output);
             */

            for (int64_t i = 0; i < max_io_req_per_ioc; i++) {
                uint32_t final_vec_len = vector_len;
                int      next_ioc      = ioc_start;

                /* Fill in I/O types, offsets, sizes and buffers vectors */
                for (uint32_t k = 0, vec_idx = 0; k < vector_len; k++) {
                    size_t idx = (size_t)next_ioc * max_depth + (size_t)i;

                    io_types[vec_idx] = type;
                    H5_CHECKED_ASSIGN(io_addrs[vec_idx], haddr_t, sf_offset[idx], int64_t);
                    H5_CHECKED_ASSIGN(io_sizes[vec_idx], size_t, sf_data_size[idx], int64_t);
                    io_bufs[vec_idx] = ((char *)buf + source_data_offset[idx]);

                    next_ioc = (next_ioc + 1) % ioc_total;

                    /* Skip 0-sized I/Os */
                    if (io_sizes[vec_idx] == 0) {
                        final_vec_len--;
                        continue;
                    }

                    vec_idx++;
                }

                if (!rank0_bcast || (file_ptr->mpi_rank == 0)) {
                    /* Make vector read call to subfile */
                    if (H5FDread_vector(file_ptr->sf_file, dxpl_id, final_vec_len, io_types, io_addrs,
                                        io_sizes, io_bufs) < 0)
                        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "read from subfile failed");
                }
            }

            if (rank0_bcast) {
                H5_CHECK_OVERFLOW(size, size_t, int);
                if (MPI_SUCCESS != MPI_Bcast(buf, (int)size, MPI_BYTE, 0, file_ptr->comm))
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't broadcast data from rank 0");
            }

            /* TODO: The following is left for future work */
            /* H5FDctl(file_ptr->sf_file, op_code_complete, flags, input, &output); */
        }
    }

    /* Point to the end of the current I/O */
    addr += (haddr_t)size;

    /* Update current file position and EOF */
    file_ptr->pos = addr;
    file_ptr->op  = OP_READ;

done:
    HDfree(io_bufs);
    HDfree(io_sizes);
    HDfree(io_addrs);
    HDfree(io_types);
    HDfree(sf_offset);
    HDfree(sf_data_size);
    HDfree(source_data_offset);

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
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                      const void *buf /*in*/)
{
    subfiling_context_t *sf_context         = NULL;
    H5FD_subfiling_t *   file_ptr           = (H5FD_subfiling_t *)_file;
    const void **        io_bufs            = NULL;
    H5FD_mem_t *         io_types           = NULL;
    haddr_t *            io_addrs           = NULL;
    size_t *             io_sizes           = NULL;
    int64_t *            source_data_offset = NULL;
    int64_t *            sf_data_size       = NULL;
    int64_t *            sf_offset          = NULL;
    int                  ioc_total;
    herr_t               ret_value = SUCCEED;

    HDassert(file_ptr && file_ptr->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if (!H5F_addr_defined(addr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %" PRIuHADDR, addr);
    if (REGION_OVERFLOW(addr, size))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                                "addr overflow, addr = %" PRIuHADDR ", size = %" PRIuHADDR, addr, size);

    /* TODO: Temporarily reject collective I/O until support is implemented (unless types are simple MPI_BYTE)
     */
    {
        H5FD_mpio_xfer_t xfer_mode;

        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL,
                                    "can't determine I/O collectivity setting");

        if (xfer_mode == H5FD_MPIO_COLLECTIVE) {
            MPI_Datatype btype, ftype;

            if (H5CX_get_mpi_coll_datatypes(&btype, &ftype) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get MPI-I/O datatypes");
            if (MPI_BYTE != btype || MPI_BYTE != ftype)
                H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL,
                                        "collective I/O is currently unsupported");
        }

        /*
         * If we reached here, we're still doing independent I/O regardless
         * of collectivity setting, so set that.
         */
        H5CX_set_io_xfer_mode(H5FD_MPIO_INDEPENDENT);
    }

#if H5FD_SUBFILING_DEBUG_OP_CALLS
    HDprintf("[%s %d] addr=%ld, size=%ld\n", __func__, file_ptr->mpi_rank, addr, size);
    HDfflush(stdout);
#endif

    /*
     * Retrieve the subfiling context object and the number
     * of I/O concentrators.
     *
     * Given the current I/O and the I/O concentrator info,
     * we can determine some I/O transaction parameters.
     * In particular, for large I/O operations, each IOC
     * may require multiple I/Os to fulfill the user I/O
     * request. The block size and number of IOCs are used
     * to size the vectors that will be used to invoke the
     * underlying I/O operations.
     */
    sf_context = (subfiling_context_t *)H5_get_subfiling_object(file_ptr->context_id);
    HDassert(sf_context);
    HDassert(sf_context->topology);

    ioc_total = sf_context->topology->n_io_concentrators;

#if H5FD_SUBFILING_DEBUG_OP_CALLS
    if (sf_context->topology->rank_is_ioc)
        HDprintf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
    else
        HDprintf("[%s %d] fd=*\n", __func__, file_ptr->mpi_rank);
    HDfflush(stdout);
#endif

    if (ioc_total == 0) {
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid number of I/O concentrators (%d)",
                                ioc_total);
    }
    else if (ioc_total == 1) {
        /***********************************
         * No striping - just a single IOC *
         ***********************************/

        /* Make vector write call to subfile */
        if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, 1, &type, &addr, &size, &buf) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "write to subfile failed");
    }
    else {
        int64_t max_io_req_per_ioc;
        int64_t file_offset;
        int64_t block_size;
        size_t  max_depth;
        herr_t  status;
        int     ioc_count = 0;
        int     ioc_start = -1;

        /*********************************
         * Striping across multiple IOCs *
         *********************************/

        block_size = sf_context->sf_blocksize_per_stripe;
        max_depth  = (size / (size_t)block_size) + 2;

        /*
         * Given the number of I/O concentrators, allocate vectors (one per IOC)
         * to contain the translation of the I/O request into a collection of I/O
         * requests.
         */
        if (NULL ==
            (source_data_offset = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*source_data_offset))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate source data offset I/O vector");
        if (NULL == (sf_data_size = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_data_size))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate subfile data size I/O vector");
        if (NULL == (sf_offset = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_offset))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate subfile offset I/O vector");

        H5_CHECKED_ASSIGN(file_offset, int64_t, addr, haddr_t);

        /*
         * Get the potential set of IOC transactions; e.g., data sizes,
         * offsets and datatypes. These can all be used by either the
         * underlying IOC or by the sec2 driver.
         *
         * For now, assume we're dealing with contiguous datasets. Vector
         * I/O will probably handle the non-contiguous case.
         */
        status = init_indep_io(sf_context,           /* IN: Context used to look up config info */
                               file_offset,          /* IN: Starting file offset */
                               size,                 /* IN: I/O size */
                               1,                    /* IN: Data extent of the 'type' assumes byte */
                               max_depth,            /* IN: Maximum stripe depth */
                               source_data_offset,   /* OUT: Memory offset */
                               sf_offset,            /* OUT: File offset */
                               sf_data_size,         /* OUT: Length of this contiguous block */
                               &ioc_start,           /* OUT: IOC index corresponding to starting offset */
                               &ioc_count,           /* OUT: Number of actual IOCs used */
                               &max_io_req_per_ioc); /* OUT: Maximum number of requests to any IOC */

        if (status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't initialize IOC transactions");

        if (max_io_req_per_ioc > 0) {
            uint32_t vector_len;

            H5_CHECKED_ASSIGN(vector_len, uint32_t, ioc_count, int);

            /* Allocate I/O vectors */
            if (NULL == (io_types = HDmalloc(vector_len * sizeof(*io_types))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O types vector");
            if (NULL == (io_addrs = HDmalloc(vector_len * sizeof(*io_addrs))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O addresses vector");
            if (NULL == (io_sizes = HDmalloc(vector_len * sizeof(*io_sizes))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O sizes vector");
            if (NULL == (io_bufs = HDmalloc(vector_len * sizeof(*io_bufs))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate subfile I/O buffers vector");

            /* TODO: The following is left for future work */
            /*
             * Set ASYNC MODE
             * H5FD_class_aio_t *async_file_ptr = (H5FD_class_aio_t *)file_ptr->sf_file;
             * uint64_t op_code_begin = OPC_BEGIN;
             * uint64_t op_code_complete = OPC_COMPLETE;
             * const void *input = NULL;
             * void *output = NULL;
             * H5FDctl(file_ptr->sf_file, op_code_begin, flags, input, &output);
             * (*async_file_ptr->h5fdctl)(file_ptr->sf_file, op_code_begin, flags, input, &output);
             */

            for (int64_t i = 0; i < max_io_req_per_ioc; i++) {
                uint32_t final_vec_len = vector_len;
                int      next_ioc      = ioc_start;

                /* Fill in I/O types, offsets, sizes and buffers vectors */
                for (uint32_t k = 0, vec_idx = 0; k < vector_len; k++) {
                    size_t idx = (size_t)next_ioc * max_depth + (size_t)i;

                    io_types[vec_idx] = type;
                    H5_CHECKED_ASSIGN(io_addrs[vec_idx], haddr_t, sf_offset[idx], int64_t);
                    H5_CHECKED_ASSIGN(io_sizes[vec_idx], size_t, sf_data_size[idx], int64_t);
                    io_bufs[vec_idx] = ((const char *)buf + source_data_offset[idx]);

                    next_ioc = (next_ioc + 1) % ioc_total;

                    /* Skip 0-sized I/Os */
                    if (io_sizes[vec_idx] == 0) {
                        final_vec_len--;
                        continue;
                    }

                    vec_idx++;
                }

                /* Make vector write call to subfile */
                if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, final_vec_len, io_types, io_addrs, io_sizes,
                                     io_bufs) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "write to subfile failed");
            }

            /* TODO: The following is left for future work */
            /* H5FDctl(file_ptr->sf_file, op_code_complete, flags, input, &output); */
        }
    }

    /* Point to the end of the current I/O */
    addr += (haddr_t)size;

    /* Update current file position and EOF */
    file_ptr->pos = addr;
    file_ptr->op  = OP_WRITE;

#if 1 /* Mimic the MPI I/O VFD */
    file_ptr->eof = HADDR_UNDEF;

    if (file_ptr->pos > file_ptr->local_eof)
        file_ptr->local_eof = file_ptr->pos;
#else
    if (file_ptr->pos > file_ptr->eof)
        file_ptr->eof = file_ptr->pos;
#endif

done:
    HDfree(io_bufs);
    HDfree(io_sizes);
    HDfree(io_addrs);
    HDfree(io_types);
    HDfree(sf_offset);
    HDfree(sf_data_size);
    HDfree(source_data_offset);

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
 * Programmer:  RAW -- ??/??/21
 *
 * Changes:     None.
 *
 * Notes:       Thus function doesn't actually implement vector read.
 *              Instead, it comverts the vector read call into a series
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
    H5FD_mpio_xfer_t  xfer_mode = H5FD_MPIO_INDEPENDENT;
    herr_t            ret_value = SUCCEED; /* Return value             */

    /* Check arguments
     * RAW - Do we really need to check arguments once again?
     * These have already been checked in H5FD__subfiling_read_vector (see below)!
     */
    if (!file_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL");

    if ((!types) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "types parameter can't be NULL if count is positive");

    if ((!addrs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "addrs parameter can't be NULL if count is positive");

    if ((!sizes) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "sizes parameter can't be NULL if count is positive");

    if ((!bufs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "bufs parameter can't be NULL if count is positive");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    /* Set DXPL for operation */
    H5CX_set_dxpl(dxpl_id);

    /* TODO: setup real support for vector I/O */
    if (file_ptr->fa.require_ioc) {

        hbool_t    extend_sizes = FALSE;
        hbool_t    extend_types = FALSE;
        int        k;
        size_t     size;
        H5FD_mem_t type;
        haddr_t    eoa;

        HDassert((count == 0) || (sizes[0] != 0));
        HDassert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL,
                                    "can't determine I/O collectivity setting");

        /* Currently, treat collective calls as independent */
        if (xfer_mode != H5FD_MPIO_INDEPENDENT)
            if (H5CX_set_io_xfer_mode(H5FD_MPIO_INDEPENDENT) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTSET, FAIL, "can't set I/O collectivity setting");

        /* Note that the following code does not let the sub-filing VFD participate
         * in collective calls when there is no data to write.  This is not an issue
         * now, as we don't do anything special with collective operations.  However
         * this needs to be fixed.
         */
        for (k = 0; k < (int)count; k++) {

            if (!extend_sizes) {

                if (sizes[k] == 0) {

                    extend_sizes = TRUE;
                    size         = sizes[k - 1];
                }
                else {

                    size = sizes[k];
                }
            }

            if (!extend_types) {

                if (types[k] == H5FD_MEM_NOLIST) {

                    extend_types = TRUE;
                    type         = types[k - 1];
                }
                else {

                    type = types[k];
                }
            }

            if (HADDR_UNDEF == (eoa = H5FD__subfiling_get_eoa(_file, type)))
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed");

            if ((addrs[k] + size) > eoa)
                H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                                        "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, eoa = %llu",
                                        (int)k, (unsigned long long)(addrs[k]), (int)k,
                                        (unsigned long long)size, (unsigned long long)eoa);

            if (H5FD__subfiling_read(_file, type, dxpl_id, addrs[k], size, bufs[k]) != SUCCEED)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "file vector read request failed");
        }
    }
    else {
        /* sec2 driver..
         * Call the subfiling 'direct write' version
         * of subfiling.
         */
        if (H5FD_read_vector(_file, count, types, addrs, sizes, bufs) != SUCCEED)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "file vector read request failed");
    }

done:
    if (xfer_mode != H5FD_MPIO_INDEPENDENT)
        if (H5CX_set_io_xfer_mode(xfer_mode) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_CONTEXT, H5E_CANTSET, FAIL, "can't set I/O collectivity setting");

    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_read_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfile_write_vector  (internal function)
 *
 * Purpose:     Perform count writes to the specified file at the offsets
 *              provided in the addrs array. Lengths and memory
 *              types provided in the sizes and types arrays.  Data to be
 *              written is referenced by the bufs array.
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
 * Programmer:  RAW -- ??/??/21
 *
 * Changes:     None.
 *
 * Notes:       Thus function doesn't actually implement vector write.
 *              Instead, it comverts the vector write call into a series
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
H5FD__subfiling_write_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                             haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */)
{
    H5FD_subfiling_t *file_ptr  = (H5FD_subfiling_t *)_file;
    H5FD_mpio_xfer_t  xfer_mode = H5FD_MPIO_INDEPENDENT;
    herr_t            ret_value = SUCCEED; /* Return value             */

    HDassert(file_ptr != NULL); /* sanity check */

    /* Check arguments
     * RAW - Do we really need to check arguments once again?
     * These have already been checked in H5FD__subfiling_write_vector (see below)!
     */
    if (!file_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL");

    if ((!types) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "types parameter can't be NULL if count is positive");

    if ((!addrs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "addrs parameter can't be NULL if count is positive");

    if ((!sizes) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "sizes parameter can't be NULL if count is positive");

    if ((!bufs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "bufs parameter can't be NULL if count is positive");

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }
    /* Call the subfiling IOC write*/
    if (file_ptr->fa.require_ioc) {

        hbool_t    extend_sizes = FALSE;
        hbool_t    extend_types = FALSE;
        int        k;
        size_t     size;
        H5FD_mem_t type;
        haddr_t    eoa;

        HDassert((count == 0) || (sizes[0] != 0));
        HDassert((count == 0) || (types[0] != H5FD_MEM_NOLIST));

        if (H5CX_get_io_xfer_mode(&xfer_mode) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL,
                                    "can't determine I/O collectivity setting");

        /* Currently, treat collective calls as independent */
        if (xfer_mode != H5FD_MPIO_INDEPENDENT)
            if (H5CX_set_io_xfer_mode(H5FD_MPIO_INDEPENDENT) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_CONTEXT, H5E_CANTSET, FAIL, "can't set I/O collectivity setting");

        /* Note that the following code does not let the sub-filing VFD participate
         * in collective calls when there is no data to write.  This is not an issue
         * now, as we don't do anything special with collective operations.  However
         * this needs to be fixed.
         */
        for (k = 0; k < (int)count; k++) {

            if (!extend_sizes) {

                if (sizes[k] == 0) {

                    extend_sizes = TRUE;
                    size         = sizes[k - 1];
                }
                else {

                    size = sizes[k];
                }
            }

            if (!extend_types) {

                if (types[k] == H5FD_MEM_NOLIST) {

                    extend_types = TRUE;
                    type         = types[k - 1];
                }
                else {

                    type = types[k];
                }
            }

            if (HADDR_UNDEF == (eoa = H5FD__subfiling_get_eoa(_file, type)))
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed");

            if ((addrs[k] + size) > eoa)
                H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                                        "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, eoa = %llu",
                                        (int)k, (unsigned long long)(addrs[k]), (int)k,
                                        (unsigned long long)size, (unsigned long long)eoa);

            if (H5FD__subfiling_write(_file, type, dxpl_id, addrs[k], size, bufs[k]) != SUCCEED)
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file vector write request failed");
        }
    }
    else {
        /* sec2 driver..
         * Call the subfiling 'direct write' version
         * of subfiling.
         */
        if (H5FD_write_vector(_file, count, types, addrs, sizes, bufs) != SUCCEED)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file vector write request failed");
    }

done:
    if (xfer_mode != H5FD_MPIO_INDEPENDENT)
        if (H5CX_set_io_xfer_mode(xfer_mode) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_CONTEXT, H5E_CANTSET, FAIL, "can't set I/O collectivity setting");

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
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t H5_ATTR_UNUSED closing)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED; /* Return value */

    HDassert(file);

    /* Extend the file to make sure it's large enough */
#if 1 /* Mimic the MPI I/O VFD */
    if (!H5F_addr_eq(file->eoa, file->last_eoa)) {
        int64_t sf_eof;
        int64_t eoa;
        int     mpi_code;

        if (!H5CX_get_mpi_file_flushing())
            if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file->comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);

        if (0 == file->mpi_rank) {
            if (H5FD__subfiling__get_real_eof(file->context_id, &sf_eof) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get EOF");
        }

        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_eof, 1, MPI_INT64_T, 0, file->comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);

        if (sf_eof < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid EOF");

        H5_CHECKED_ASSIGN(eoa, int64_t, file->eoa, haddr_t);

        /* truncate sub-files */
        /* This is a hack.  We should be doing the truncate of the sub-files via calls to
         * H5FD_truncate() with the IOC.  However, that system is messed up at present.
         * thus the following hack.
         *                                                 JRM -- 12/18/21
         */
        if (H5FD__subfiling__truncate_sub_files(file->context_id, eoa, file->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "sub-file truncate request failed");

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op  = OP_UNKNOWN;

        /* Update the 'last' eoa value */
        file->last_eoa = file->eoa;
    }
#else
    if (!H5F_addr_eq(file->eoa, file->eof)) {

        /* Update the eof value */
        file->eof = file->eoa;

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op  = OP_UNKNOWN;

        /* Update the 'last' eoa value */
        file->last_eoa = file->eoa;
    } /* end if */

    /* truncate sub-files */
    /* This is a hack.  We should be doing the truncate of the sub-files via calls to
     * H5FD_truncate() with the IOC.  However, that system is messed up at present.
     * thus the following hack.
     *                                                 JRM -- 12/18/21
     */
    if (H5FD__subfiling__truncate_sub_files(file->context_id, file->eof, file->comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "sub-file truncate request failed");
#endif

done:
    H5_SUBFILING_FUNC_LEAVE_API;
} /* end H5FD__subfiling_truncate() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__subfiling_lock
 *
 * Purpose:     To place an advisory lock on a file.
 *      The lock type to apply depends on the parameter "rw":
 *          TRUE--opens for write: an exclusive lock
 *          FALSE--opens for read: a shared lock
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi; May 2013
 *
 *-------------------------------------------------------------------------
 */
#if 0
static herr_t
H5FD__subfiling_lock(H5FD_t *_file, hbool_t rw)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file; /* VFD file struct  */
    herr_t            ret_value = SUCCEED;                   /* Return value       */

    HDassert(file);

    /* TODO: Consider lock only on IOC ranks for one IOC per subfile case */
    if (file->fa.require_ioc) {
#ifdef VERBOSE
        HDputs("Subfiling driver doesn't support file locking");
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
 * Programmer:  Vailin Choi; May 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_unlock(H5FD_t *_file)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file; /* VFD file struct */
    herr_t            ret_value = SUCCEED;                   /* Return value             */

    HDassert(file);

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
    H5P_genplist_t *               plist     = NULL;
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
 * Programmer:  JRM -- 8/3/21
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
    HDassert(file);
    HDassert(H5FD_SUBFILING == file->pub.driver_id);

    switch (op_code) {

        case H5FD_CTL_GET_MPI_COMMUNICATOR_OPCODE:
            HDassert(output);
            HDassert(*output);

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

        case H5FD_CTL_GET_MPI_RANK_OPCODE:
            HDassert(output);
            HDassert(*output);
            **((int **)output) = file->mpi_rank;
            break;

        case H5FD_CTL_GET_MPI_SIZE_OPCODE:
            HDassert(output);
            HDassert(*output);
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
 * Function:    init_indep_io
 *
 * Purpose:     Utility function to initialize the set of I/O transactions
 *              used to communicate with I/O concentrators for read and
 *              write I/O operations.
 *
 *              Fills the I/O vectors contained in the output arrays
 *              `mem_buf_offset`, `target_file_offset` and `io_block_len`.
 *              As a consequence of not allowing use of MPI derived
 *              datatypes in the VFD layer, we need to accommodate the
 *              possibility that large I/O transactions will be required to
 *              use multiple I/Os per IOC.
 *
 *              Example: Using 4 IOCs, each with 1M stripe-depth; when
 *              presented an I/O request for 8MB then at a minimum each IOC
 *              will require 2 I/Os of 1MB each.  Depending on the starting
 *              file offset, the 2 I/Os can instead be 3...
 *
 *              To fully describe the I/O transactions for reads and writes
 *              the output arrays are therefore arrays of I/O vectors,
 *              where each vector has a length of which corresponds to the
 *              max number of I/O transactions per IOC. In the example
 *              above, these vector lengths can be 2 or 3. The actual
 *              length is determined by the 'container_depth' variable.
 *
 *              For I/O operations which involve a subset of I/O
 *              concentrators, the vector entries for the unused I/O
 *              concentrators IOCs will have lengths of zero and be empty.
 *              The 'container_depth' in this case will always be 1.
 *
 *              sf_context (IN)
 *                - the subfiling context for the file
 *
 *              file_offset (IN)
 *                - the starting file offset for I/O
 *
 *              io_nelemts (IN)
 *                - the number of data elements for the I/O operation
 *
 *              dtype_extent (IN)
 *                - the extent of the datatype of each data element for
 *                  the I/O operation
 *
 *              max_iovec_len (IN)
 *                - the maximum size for a single I/O vector in each of
 *                  the output arrays `mem_buf_offset`, `io_block_len`
 *                  and `sf_offset`. NOTE that this routine expects each
 *                  of these output arrays to have enough space allocated
 *                  for one I/O vector PER I/O concentrator. Therefore,
 *                  the total size of each output array should be at least
 *                  `max_iovec_len * n_io_concentrators`.
 *
 *              mem_buf_offset (OUT)
 *                - output array of vectors (one vector for each IOC)
 *                  containing the set of offsets into the memory buffer
 *                  for I/O
 *
 *              target_file_offset (OUT)
 *                - output array of vectors (one vector for each IOC)
 *                  containing the set of offsets into the target file
 *
 *              io_block_len (OUT)
 *                - output array of vectors (one vector for each IOC)
 *                  containing the set of block lengths for each source
 *                  buffer/target file offset.
 *
 *              first_ioc_index (OUT)
 *                - the index of the first I/O concentrator that this I/O
 *                  operation begins at
 *
 *              n_iocs_used (OUT)
 *                - the number of I/O concentrators actually used for this
 *                  I/O operation, which may be different from the total
 *                  number of I/O concentrators for the file
 *
 *              max_io_req_per_ioc (OUT)
 *                - the maximum number of I/O requests to any particular
 *                  I/O concentrator, or the maximum "depth" of each I/O
 *                  vector in the output arrays.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
init_indep_io(subfiling_context_t *sf_context, int64_t file_offset, size_t io_nelemts, size_t dtype_extent,
              size_t max_iovec_len, int64_t *mem_buf_offset, int64_t *target_file_offset,
              int64_t *io_block_len, int *first_ioc_index, int *n_iocs_used, int64_t *max_io_req_per_ioc)
{
    int64_t stripe_size          = 0;
    int64_t block_size           = 0;
    int64_t data_size            = 0;
    int64_t stripe_idx           = 0;
    int64_t final_stripe_idx     = 0;
    int64_t curr_stripe_idx      = 0;
    int64_t offset_in_stripe     = 0;
    int64_t offset_in_block      = 0;
    int64_t final_offset         = 0;
    int64_t start_length         = 0;
    int64_t final_length         = 0;
    int64_t ioc_start            = 0;
    int64_t ioc_final            = 0;
    int64_t start_row            = 0;
    int64_t row_offset           = 0;
    int64_t row_stripe_idx_start = 0;
    int64_t row_stripe_idx_final = 0;
    int64_t max_iovec_depth      = 0;
    int64_t curr_max_iovec_depth = 0;
    int64_t total_bytes          = 0;
    int64_t mem_offset           = 0;
    int     ioc_count            = 0;
    herr_t  ret_value            = SUCCEED;

    HDassert(sf_context);
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->n_io_concentrators > 0);
    HDassert(sf_context->sf_stripe_size > 0);
    HDassert(sf_context->sf_blocksize_per_stripe > 0);
    HDassert(mem_buf_offset);
    HDassert(target_file_offset);
    HDassert(io_block_len);
    HDassert(first_ioc_index);
    HDassert(n_iocs_used);
    HDassert(max_io_req_per_ioc);

    *first_ioc_index    = 0;
    *n_iocs_used        = 0;
    *max_io_req_per_ioc = 0;

    /*
     * Retrieve the needed fields from the subfiling context.
     *
     *  ioc_count
     *    - the total number of I/O concentrators in the
     *      application topology
     *  stripe_size
     *    - the size of the data striping across the file's subfiles
     *  block_size
     *    - the size of a "block" across the IOCs, as calculated
     *      by the stripe size multiplied by the number of I/O
     *      concentrators
     */
    ioc_count   = sf_context->topology->n_io_concentrators;
    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

    H5_CHECKED_ASSIGN(data_size, int64_t, (io_nelemts * dtype_extent), size_t);

    /*
     * Calculate the following from the starting file offset:
     *
     *  stripe_idx
     *    - a stripe "index" given by the file offset divided by the
     *      stripe size. Note that when the file offset equals or exceeds
     *      the block size, we simply wrap around. So, for example, if 4
     *      I/O concentrators are being used with a stripe size of 1MiB,
     *      the block size would be 4MiB and file offset 4096 would have
     *      a stripe index of 4 and reside in the same subfile as stripe
     *      index 0 (offsets 0-1023)
     *  offset_in_stripe
     *    - the relative offset in the stripe that the starting file
     *      offset resides in
     *  offset_in_block
     *    - the relative offset in the "block" of stripes across the I/O
     *      concentrators
     *  final_offset
     *    - the last offset in the virtual file covered by this I/O
     *      operation. Simply the I/O size added to the starting file
     *      offset.
     */
    stripe_idx       = file_offset / stripe_size;
    offset_in_stripe = file_offset % stripe_size;
    offset_in_block  = file_offset % block_size;
    final_offset     = file_offset + data_size;

    /* Determine the size of data written to the first and last stripes */
    start_length = MIN(data_size, (stripe_size - offset_in_stripe));
    final_length = (start_length == data_size ? 0 : final_offset % stripe_size);
    HDassert(start_length <= stripe_size);
    HDassert(final_length <= stripe_size);

    /*
     * Determine which I/O concentrator the I/O request begins
     * in and which "row" the I/O request begins in within the
     * "block" of stripes across the I/O concentrators. Note that
     * "row" here is just a conceptual way to think of how a block
     * of data stripes is laid out across the I/O concentrator
     * subfiles. A block's "column" size in bytes is equal to the
     * stripe size multiplied the number of I/O concentrators.
     * Therefore, file offsets that are multiples of the block size
     * begin a new "row".
     */
    start_row = stripe_idx / ioc_count;
    ioc_start = stripe_idx % ioc_count;
    H5_CHECK_OVERFLOW(ioc_start, int64_t, int);

    /*
     * Set initial file offset for starting "row"
     * based on the start row index
     */
    row_offset = start_row * block_size;

    /*
     * Determine the stripe "index" of the last offset in the
     * virtual file and, from that, determine the I/O concentrator
     * that the I/O request ends in.
     */
    final_stripe_idx = final_offset / stripe_size;
    ioc_final        = final_stripe_idx % ioc_count;

    /*
     * Determine how "deep" the resulting I/O vectors are at
     * most by calculating the maximum number of "rows" spanned
     * for any particular subfile; e.g. the maximum number of
     * I/O requests for any particular I/O concentrator
     */
    row_stripe_idx_start = stripe_idx - ioc_start;
    row_stripe_idx_final = final_stripe_idx - ioc_final;
    max_iovec_depth      = ((row_stripe_idx_final - row_stripe_idx_start) / ioc_count) + 1;

    if (ioc_final < ioc_start)
        max_iovec_depth--;

    /* Set returned parameters early */
    *first_ioc_index    = (int)ioc_start;
    *n_iocs_used        = ioc_count;
    *max_io_req_per_ioc = max_iovec_depth;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: FILE OFFSET = %" PRId64 ", DATA SIZE = %zu, STRIPE SIZE = %" PRId64, __func__,
                     file_offset, io_nelemts, stripe_size);
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: IOC START = %" PRId64 ", IOC FINAL = %" PRId64 ", "
                     "MAX IOVEC DEPTH = %" PRId64 ", START LENGTH = %" PRId64 ", FINAL LENGTH = %" PRId64,
                     __func__, ioc_start, ioc_final, max_iovec_depth, start_length, final_length);
#endif

    /*
     * Loop through the set of I/O concentrators to determine
     * the various vector components for each. I/O concentrators
     * whose data size is zero will not have I/O requests passed
     * to them.
     */
    curr_stripe_idx      = stripe_idx;
    curr_max_iovec_depth = max_iovec_depth;
    for (int i = 0, k = (int)ioc_start; i < ioc_count; i++) {
        int64_t *_mem_buf_offset;
        int64_t *_target_file_offset;
        int64_t *_io_block_len;
        int64_t  ioc_bytes = 0;
        int64_t  iovec_depth;
        hbool_t  is_first = FALSE;
        hbool_t  is_last  = FALSE;
        size_t   output_offset;

        iovec_depth = curr_max_iovec_depth;

        /*
         * Setup the pointers to the next set of I/O vectors in
         * the output arrays and clear those vectors
         */
        output_offset       = (size_t)(k)*max_iovec_len;
        _mem_buf_offset     = mem_buf_offset + output_offset;
        _target_file_offset = target_file_offset + output_offset;
        _io_block_len       = io_block_len + output_offset;

        HDmemset(_mem_buf_offset, 0, (max_iovec_len * sizeof(*_mem_buf_offset)));
        HDmemset(_target_file_offset, 0, (max_iovec_len * sizeof(*_target_file_offset)));
        HDmemset(_io_block_len, 0, (max_iovec_len * sizeof(*_io_block_len)));

        if (total_bytes == data_size) {
            *n_iocs_used = i;
            goto done;
        }

        if (total_bytes < data_size) {
            int64_t num_full_stripes = iovec_depth;

            if (k == ioc_start) {
                is_first = TRUE;

                /*
                 * Add partial segment length if not
                 * starting on a stripe boundary
                 */
                if (start_length < stripe_size) {
                    ioc_bytes += start_length;
                    num_full_stripes--;
                }
            }

            if (k == ioc_final) {
                is_last = TRUE;

                /*
                 * Add partial segment length if not
                 * ending on a stripe boundary
                 */
                if (final_length < stripe_size) {
                    ioc_bytes += final_length;
                    if (num_full_stripes)
                        num_full_stripes--;
                }
            }

            /* Account for IOCs with uniform segments */
            if (!is_first && !is_last) {
                hbool_t thin_uniform_section = FALSE;

                if (ioc_final >= ioc_start) {
                    /*
                     * When an IOC has an index value that is greater
                     * than both the starting IOC and ending IOC indices,
                     * it is a "thinner" section with a smaller I/O vector
                     * depth.
                     */
                    thin_uniform_section = (k > ioc_start) && (k > ioc_final);
                }

                if (ioc_final < ioc_start) {
                    /*
                     * This can also happen when the IOC with the final
                     * data segment has a smaller IOC index than the IOC
                     * with the first data segment and the current IOC
                     * index falls between the two.
                     */
                    thin_uniform_section = thin_uniform_section || ((ioc_final < k) && (k < ioc_start));
                }

                if (thin_uniform_section) {
                    HDassert(iovec_depth > 1);
                    HDassert(num_full_stripes > 1);

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
            ioc_bytes += num_full_stripes * stripe_size;
            total_bytes += ioc_bytes;
        }

        _mem_buf_offset[0]     = mem_offset;
        _target_file_offset[0] = row_offset + offset_in_block;
        _io_block_len[0]       = ioc_bytes;

        if (ioc_count > 1) {
            int64_t curr_file_offset = row_offset + offset_in_block;

            /* Fill the I/O vectors */
            if (is_first) {
                if (is_last) { /* First + Last */
                    if (iovec_fill_first_last(sf_context, iovec_depth, ioc_bytes, mem_offset,
                                              curr_file_offset, start_length, final_length, _mem_buf_offset,
                                              _target_file_offset, _io_block_len) < 0)
                        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");
                }
                else { /* First ONLY */
                    if (iovec_fill_first(sf_context, iovec_depth, ioc_bytes, mem_offset, curr_file_offset,
                                         start_length, _mem_buf_offset, _target_file_offset,
                                         _io_block_len) < 0)
                        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");
                }
                /* Move the memory pointer to the starting location
                 * for next IOC request.
                 */
                mem_offset += start_length;
            }
            else if (is_last) { /* Last ONLY */
                if (iovec_fill_last(sf_context, iovec_depth, ioc_bytes, mem_offset, curr_file_offset,
                                    final_length, _mem_buf_offset, _target_file_offset, _io_block_len) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");

                mem_offset += stripe_size;
            }
            else { /* Everything else (uniform) */
                if (iovec_fill_uniform(sf_context, iovec_depth, ioc_bytes, mem_offset, curr_file_offset,
                                       _mem_buf_offset, _target_file_offset, _io_block_len) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL, "can't fill I/O vectors");

                mem_offset += stripe_size;
            }
        }

        offset_in_block += _io_block_len[0];

        k++;
        curr_stripe_idx++;

        if (k == ioc_count) {
            k                    = 0;
            offset_in_block      = 0;
            curr_max_iovec_depth = ((final_stripe_idx - curr_stripe_idx) / ioc_count) + 1;

            row_offset += block_size;
        }

        HDassert(offset_in_block <= block_size);
    }

    if (total_bytes != data_size)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                                "total bytes (%" PRId64 ") didn't match data size (%" PRId64 ")!",
                                total_bytes, data_size);

done:
    return ret_value;
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
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_first(subfiling_context_t *sf_context, int64_t iovec_depth, int64_t target_datasize,
                 int64_t start_mem_offset, int64_t start_file_offset, int64_t first_io_len,
                 int64_t *mem_offset_out, int64_t *target_file_offset_out, int64_t *io_block_len_out)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    HDassert(sf_context);
    HDassert(mem_offset_out);
    HDassert(target_file_offset_out);
    HDassert(io_block_len_out);
    HDassert(iovec_depth > 0);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", first_io_len = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, first_io_len);
#endif

    mem_offset_out[0]         = start_mem_offset;
    target_file_offset_out[0] = start_file_offset;
    io_block_len_out[0]       = first_io_len;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: mem_offset[0] = %" PRId64 ", file_offset[0] = %" PRId64
                     ", io_block_len[0] = %" PRId64,
                     __func__, mem_offset_out[0], target_file_offset_out[0], io_block_len_out[0]);
#endif

    if (first_io_len == target_datasize)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    if (first_io_len > 0) {
        int64_t offset_in_stripe = start_file_offset % stripe_size;
        int64_t next_mem_offset  = block_size - offset_in_stripe;
        int64_t next_file_offset = start_file_offset + (block_size - offset_in_stripe);

        total_bytes = first_io_len;

        for (int64_t i = 1; i < iovec_depth; i++) {
            mem_offset_out[i]         = next_mem_offset;
            target_file_offset_out[i] = next_file_offset;
            io_block_len_out[i]       = stripe_size;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%" PRId64 "] = %" PRId64 ", file_offset[%" PRId64 "] = %" PRId64
                             ", io_block_len[%" PRId64 "] = %" PRId64,
                             __func__, i, mem_offset_out[i], i, target_file_offset_out[i], i,
                             io_block_len_out[i]);
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
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_last(subfiling_context_t *sf_context, int64_t iovec_depth, int64_t target_datasize,
                int64_t start_mem_offset, int64_t start_file_offset, int64_t last_io_len,
                int64_t *mem_offset_out, int64_t *target_file_offset_out, int64_t *io_block_len_out)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    HDassert(sf_context);
    HDassert(mem_offset_out);
    HDassert(target_file_offset_out);
    HDassert(io_block_len_out);
    HDassert(iovec_depth > 0);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", last_io_len = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, last_io_len);
#endif

    mem_offset_out[0]         = start_mem_offset;
    target_file_offset_out[0] = start_file_offset;
    io_block_len_out[0]       = last_io_len;

    if (last_io_len == target_datasize) {
#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id,
                         "%s: mem_offset[0] = %" PRId64 ", file_offset[0] = %" PRId64
                         ", io_block_len[0] = %" PRId64,
                         __func__, mem_offset_out[0], target_file_offset_out[0], io_block_len_out[0]);
#endif

        H5_SUBFILING_GOTO_DONE(SUCCEED);
    }
    else {
        int64_t next_mem_offset  = start_mem_offset + block_size;
        int64_t next_file_offset = start_file_offset + block_size;
        int64_t i;

        /*
         * If the last I/O size doesn't cover the target data
         * size, there is at least one full stripe preceding
         * the last I/O block
         */
        io_block_len_out[0] = stripe_size;

#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id,
                         "%s: mem_offset[0] = %" PRId64 ", file_offset[0] = %" PRId64
                         ", io_block_len[0] = %" PRId64,
                         __func__, mem_offset_out[0], target_file_offset_out[0], io_block_len_out[0]);
#endif

        total_bytes = stripe_size;

        for (i = 1; i < iovec_depth - 1;) {
            mem_offset_out[i]         = next_mem_offset;
            target_file_offset_out[i] = next_file_offset;
            io_block_len_out[i]       = stripe_size;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%" PRId64 "] = %" PRId64 ", file_offset[%" PRId64 "] = %" PRId64
                             ", io_block_len[%" PRId64 "] = %" PRId64,
                             __func__, i, mem_offset_out[i], i, target_file_offset_out[i], i,
                             io_block_len_out[i]);
#endif

            next_mem_offset += block_size;
            next_file_offset += block_size;
            total_bytes += stripe_size;

            i++;
        }

        mem_offset_out[i]         = next_mem_offset;
        target_file_offset_out[i] = next_file_offset;
        io_block_len_out[i]       = last_io_len;

#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id,
                         "%s: mem_offset[%" PRId64 "] = %" PRId64 ", file_offset[%" PRId64 "] = %" PRId64
                         ", io_block_len[%" PRId64 "] = %" PRId64,
                         __func__, i, mem_offset_out[i], i, target_file_offset_out[i], i,
                         io_block_len_out[i]);
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
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_first_last(subfiling_context_t *sf_context, int64_t iovec_depth, int64_t target_datasize,
                      int64_t start_mem_offset, int64_t start_file_offset, int64_t first_io_len,
                      int64_t last_io_len, int64_t *mem_offset_out, int64_t *target_file_offset_out,
                      int64_t *io_block_len_out)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    HDassert(sf_context);
    HDassert(mem_offset_out);
    HDassert(target_file_offset_out);
    HDassert(io_block_len_out);
    HDassert(iovec_depth > 0);

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", first_io_len = %" PRId64 ", last_io_len = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, first_io_len, last_io_len);
#endif

    mem_offset_out[0]         = start_mem_offset;
    target_file_offset_out[0] = start_file_offset;
    io_block_len_out[0]       = first_io_len;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: mem_offset[0] = %" PRId64 ", file_offset[0] = %" PRId64
                     ", io_block_len[0] = %" PRId64,
                     __func__, mem_offset_out[0], target_file_offset_out[0], io_block_len_out[0]);
#endif

    if (first_io_len == target_datasize)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    if (first_io_len > 0) {
        int64_t offset_in_stripe = start_file_offset % stripe_size;
        int64_t next_mem_offset  = block_size - offset_in_stripe;
        int64_t next_file_offset = start_file_offset + (block_size - offset_in_stripe);
        int64_t i;

        total_bytes = first_io_len;

        for (i = 1; i < iovec_depth - 1;) {
            mem_offset_out[i]         = next_mem_offset;
            target_file_offset_out[i] = next_file_offset;
            io_block_len_out[i]       = stripe_size;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%" PRId64 "] = %" PRId64 ", file_offset[%" PRId64 "] = %" PRId64
                             ", io_block_len[%" PRId64 "] = %" PRId64,
                             __func__, i, mem_offset_out[i], i, target_file_offset_out[i], i,
                             io_block_len_out[i]);
#endif

            next_mem_offset += block_size;
            next_file_offset += block_size;
            total_bytes += stripe_size;

            i++;
        }

        mem_offset_out[i]         = next_mem_offset;
        target_file_offset_out[i] = next_file_offset;
        io_block_len_out[i]       = last_io_len;

#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id,
                         "%s: mem_offset[%" PRId64 "] = %" PRId64 ", file_offset[%" PRId64 "] = %" PRId64
                         ", io_block_len[%" PRId64 "] = %" PRId64,
                         __func__, i, mem_offset_out[i], i, target_file_offset_out[i], i,
                         io_block_len_out[i]);
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
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 *-------------------------------------------------------------------------
 */
static herr_t
iovec_fill_uniform(subfiling_context_t *sf_context, int64_t iovec_depth, int64_t target_datasize,
                   int64_t start_mem_offset, int64_t start_file_offset, int64_t *mem_offset_out,
                   int64_t *target_file_offset_out, int64_t *io_block_len_out)
{
    int64_t stripe_size;
    int64_t block_size;
    int64_t total_bytes = 0;
    herr_t  ret_value   = SUCCEED;

    HDassert(sf_context);
    HDassert(mem_offset_out);
    HDassert(target_file_offset_out);
    HDassert(io_block_len_out);
    HDassert((iovec_depth > 0) || (target_datasize == 0));

    stripe_size = sf_context->sf_stripe_size;
    block_size  = sf_context->sf_blocksize_per_stripe;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: start_mem_offset = %" PRId64 ", start_file_offset = %" PRId64
                     ", segment size = %" PRId64,
                     __func__, start_mem_offset, start_file_offset, stripe_size);
#endif

    mem_offset_out[0]         = start_mem_offset;
    target_file_offset_out[0] = start_file_offset;
    io_block_len_out[0]       = stripe_size;

#ifdef H5_SUBFILING_DEBUG
    H5_subfiling_log(sf_context->sf_context_id,
                     "%s: mem_offset[0] = %" PRId64 ", file_offset[0] = %" PRId64
                     ", io_block_len[0] = %" PRId64,
                     __func__, mem_offset_out[0], target_file_offset_out[0], io_block_len_out[0]);
#endif

    if (target_datasize == 0) {
#ifdef H5_SUBFILING_DEBUG
        H5_subfiling_log(sf_context->sf_context_id, "%s: target_datasize = 0", __func__);
#endif

        io_block_len_out[0] = 0;
        H5_SUBFILING_GOTO_DONE(SUCCEED);
    }

    if (target_datasize > stripe_size) {
        int64_t next_mem_offset  = start_mem_offset + block_size;
        int64_t next_file_offset = start_file_offset + block_size;

        total_bytes = stripe_size;

        for (int64_t i = 1; i < iovec_depth; i++) {
            mem_offset_out[i]         = next_mem_offset;
            target_file_offset_out[i] = next_file_offset;
            io_block_len_out[i]       = stripe_size;

#ifdef H5_SUBFILING_DEBUG
            H5_subfiling_log(sf_context->sf_context_id,
                             "%s: mem_offset[%" PRId64 "] = %" PRId64 ", file_offset[%" PRId64 "] = %" PRId64
                             ", io_block_len[%" PRId64 "] = %" PRId64,
                             __func__, i, mem_offset_out[i], i, target_file_offset_out[i], i,
                             io_block_len_out[i]);
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
