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

#ifndef NDEBUG
FILE *sf_logfile = NULL;
FILE *client_log = NULL;
#endif

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
    MPI_Info info;
    int      mpi_rank;
    int      mpi_size;

    H5FD_t *sf_file;

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
    haddr_t        eoa; /* end of allocated region          */
    haddr_t        eof; /* end of file; current file size   */
    haddr_t        pos; /* current file I/O position        */
    H5FD_file_op_t op;  /* last operation                   */
                        /* Copy of file name from open operation    */
    char filename[H5FD_MAX_FILENAME_LEN];
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
static herr_t  H5FD__subfiling_lock(H5FD_t *_file, hbool_t rw);
static herr_t  H5FD__subfiling_unlock(H5FD_t *_file);
static herr_t  H5FD__subfiling_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags, const void *input,
                                   void **output);

static herr_t H5FD__subfiling_get_default_config(hid_t fapl_id, H5FD_subfiling_config_t *config_out);
static herr_t H5FD__subfiling_validate_config(const H5FD_subfiling_config_t *fa);
static int    H5FD__copy_plist(hid_t fapl_id, hid_t *id_out_ptr);

static const H5FD_class_t H5FD_subfiling_g = {
    H5FD_CLASS_VERSION,              /* VFD interface version */
    H5_VFD_SUBFILING,                /* value                 */
    H5FD_SUBFILING_NAME,             /* name                  */
    MAXADDR,                         /* maxaddr               */
    H5F_CLOSE_WEAK,                  /* fc_degree             */
    H5FD__subfiling_term,            /* terminate             */
    NULL,                            /* sb_size               */
    NULL,                            /* sb_encode             */
    NULL,                            /* sb_decode             */
    sizeof(H5FD_subfiling_config_t), /* fapl_size             */
    H5FD__subfiling_fapl_get,        /* fapl_get              */
    H5FD__subfiling_fapl_copy,       /* fapl_copy             */
    H5FD__subfiling_fapl_free,       /* fapl_free             */
    0,                               /* dxpl_size             */
    NULL,                            /* dxpl_copy             */
    NULL,                            /* dxpl_free             */
    H5FD__subfiling_open,            /* open                  */
    H5FD__subfiling_close,           /* close                 */
    H5FD__subfiling_cmp,             /* cmp                   */
    H5FD__subfiling_query,           /* query                 */
    NULL,                            /* get_type_map          */
    NULL,                            /* alloc                 */
    NULL,                            /* free                  */
    H5FD__subfiling_get_eoa,         /* get_eoa               */
    H5FD__subfiling_set_eoa,         /* set_eoa               */
    H5FD__subfiling_get_eof,         /* get_eof               */
    H5FD__subfiling_get_handle,      /* get_handle            */
    H5FD__subfiling_read,            /* read                  */
    H5FD__subfiling_write,           /* write                 */
    H5FD__subfiling_read_vector,     /* read_vector           */
    H5FD__subfiling_write_vector,    /* write_vector          */
    NULL,                            /* read_selection        */
    NULL,                            /* write_selection       */
    NULL,                            /* flush                 */
    H5FD__subfiling_truncate,        /* truncate              */
    H5FD__subfiling_lock,            /* lock                  */
    H5FD__subfiling_unlock,          /* unlock                */
    NULL,                            /* del                   */
    H5FD__subfiling_ctl,             /* ctl                   */
    H5FD_FLMAP_DICHOTOMY             /* fl_map                */
};

/* Declare a free list to manage the H5FD_subfiling_t struct */
H5FL_DEFINE_STATIC(H5FD_subfiling_t);

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

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    /* Register the Subfiling VFD, if it isn't already registered */
    if (H5I_VFL != H5I_get_type(H5FD_SUBFILING_g)) {
        char *env_var;

        if ((H5FD_SUBFILING_g = H5FD_register(&H5FD_subfiling_g, sizeof(H5FD_class_t), FALSE)) < 0)
            HGOTO_ERROR(H5E_ID, H5E_CANTREGISTER, H5I_INVALID_HID, "can't register subfiling VFD");

        /* Check if Subfiling VFD has been loaded dynamically */
        env_var = HDgetenv(HDF5_DRIVER);
        if (env_var && !HDstrcmp(env_var, H5FD_SUBFILING_NAME)) {
            int mpi_initialized = 0;
            int provided        = 0;
            int mpi_code;

            /* Initialize MPI if not already initialized */
            if (MPI_SUCCESS != (mpi_code = MPI_Initialized(&mpi_initialized)))
                HMPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Initialized failed", mpi_code)
            if (mpi_initialized) {
                /* If MPI is initialized, validate that it was initialized with MPI_THREAD_MULTIPLE */
                if (MPI_SUCCESS != (mpi_code = MPI_Query_thread(&provided)))
                    HMPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Query_thread failed", mpi_code)
                if (provided != MPI_THREAD_MULTIPLE)
                    HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                "Subfiling VFD requires the use of MPI_Init_thread with MPI_THREAD_MULTIPLE")
            }
            else {
                int required = MPI_THREAD_MULTIPLE;

                /* Otherwise, initialize MPI */
                if (MPI_SUCCESS != (mpi_code = MPI_Init_thread(NULL, NULL, required, &provided)))
                    HMPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Init_thread failed", mpi_code)

                H5FD_mpi_self_initialized = TRUE;

                if (provided != required)
                    HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE")
            }
        }
    }

    /* Set return value */
    ret_value = H5FD_SUBFILING_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    if (H5FD_SUBFILING_g >= 0) {
        /* Terminate MPI if the driver initialized it */
        if (H5FD_mpi_self_initialized) {
            int mpi_finalized = 0;
            int mpi_code;

            if (MPI_SUCCESS != (mpi_code = MPI_Finalized(&mpi_finalized)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Finalized failed", mpi_code)
            if (!mpi_finalized) {
                if (MPI_SUCCESS != (mpi_code = MPI_Finalize()))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Finalize failed", mpi_code)
            }

            H5FD_mpi_self_initialized = FALSE;
        }
    }

done:
    /* Reset VFL ID */
    H5FD_SUBFILING_g = H5I_INVALID_HID;

    FUNC_LEAVE_NOAPI(ret_value)
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
H5Pset_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *vfd_config)
{
    H5FD_subfiling_config_t *subfiling_conf = NULL;
    H5P_genplist_t *         plist          = NULL;
    herr_t                   ret_value      = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, fa);

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if (vfd_config == NULL) {
        if (NULL == (subfiling_conf = HDcalloc(1, sizeof(*subfiling_conf))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfiling VFD configuration")

        /* Get subfiling VFD defaults */
        if (H5FD__subfiling_get_default_config(fapl_id, subfiling_conf) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get default subfiling VFD configuration")

        vfd_config = subfiling_conf;
    }

    if (H5FD__subfiling_validate_config(vfd_config) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid subfiling VFD configuration")

    ret_value = H5P_set_driver(plist, H5FD_SUBFILING, (void *)vfd_config, NULL);

done:
    HDfree(subfiling_conf);

    FUNC_LEAVE_API(ret_value)
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

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, config_out);

    if (config_out == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL")

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if (H5FD_SUBFILING != H5P_peek_driver(plist))
        use_default_config = TRUE;
    else {
        config_ptr = H5P_peek_driver_info(plist);
        if (NULL == config_ptr)
            use_default_config = TRUE;
    }

    if (use_default_config) {
        if (H5FD__subfiling_get_default_config(fapl_id, config_out) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get default Subfiling VFD configuration")
    }
    else {
        /* Copy the subfiling fapl data out */
        HDmemcpy(config_out, config_ptr, sizeof(H5FD_subfiling_config_t));

        /* Copy the driver info value */
        if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(config_out->ioc_fapl_id)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't copy IOC FAPL");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_subfiling() */

static herr_t
H5FD__subfiling_get_default_config(hid_t fapl_id, H5FD_subfiling_config_t *config_out)
{
    char * h5_require_ioc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(config_out);

    HDmemset(config_out, 0, sizeof(*config_out));

    config_out->magic         = H5FD_SUBFILING_FAPL_MAGIC;
    config_out->version       = H5FD_CURR_SUBFILING_FAPL_VERSION;
    config_out->ioc_fapl_id   = H5I_INVALID_HID;
    config_out->stripe_count  = 0;
    config_out->stripe_depth  = H5FD_DEFAULT_STRIPE_DEPTH;
    config_out->ioc_selection = SELECT_IOC_ONE_PER_NODE;
    config_out->context_id    = -1;

    HDsnprintf(config_out->file_dir, H5FD_SUBFILING_PATH_MAX, ".");

    config_out->require_ioc = TRUE;

    if ((h5_require_ioc = HDgetenv("H5_REQUIRE_IOC")) != NULL) {
        int value_check = HDatoi(h5_require_ioc);
        if (value_check == 0)
            config_out->require_ioc = FALSE;
    }

    /* Create a default FAPL and choose an appropriate underlying driver */
    if ((config_out->ioc_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "can't create default FAPL")

    if (config_out->require_ioc) {
        MPI_Comm comm = MPI_COMM_NULL;
        MPI_Info info = MPI_INFO_NULL;

        /* Propagate MPI Info down to IOC FAPL if any is available */
        if (H5Pget_mpi_params(fapl_id, &comm, &info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI Comm/Info")
        if (comm == MPI_COMM_NULL)
            comm = MPI_COMM_WORLD;

        if (H5Pset_mpi_params(config_out->ioc_fapl_id, comm, info) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get MPI Comm/Info on IOC FAPL")

        if (H5Pset_fapl_ioc(config_out->ioc_fapl_id, NULL) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set IOC VFD on IOC FAPL")
    }
    else {
        if (H5Pset_fapl_sec2(config_out->ioc_fapl_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set Sec2 VFD on IOC FAPL")
    }

done:
    if (ret_value < 0) {
        if (config_out->ioc_fapl_id >= 0 && H5Pclose(config_out->ioc_fapl_id) < 0)
            HDONE_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, FAIL, "can't close FAPL")
    }

    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    HDassert(fa != NULL);

    if (fa->version != H5FD_CURR_SUBFILING_FAPL_VERSION)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown H5FD_subfiling_config_t version")

    if (fa->magic != H5FD_SUBFILING_FAPL_MAGIC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid H5FD_subfiling_config_t magic value")

    /* TODO: add extra subfiling configuration validation code */

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    fa = (H5FD_subfiling_config_t *)H5MM_calloc(sizeof(H5FD_subfiling_config_t));

    if (fa == NULL) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    }

    /* Copy the fields of the structure */
    HDmemcpy(fa, &(file->fa), sizeof(H5FD_subfiling_config_t));

    /* Set return value */
    ret_value = fa;

done:
    if (ret_value == NULL) {

        if (fa != NULL) {
            H5MM_xfree(fa);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_SUBFILING_LOG_CALL(FUNC);

    HDassert(id_out_ptr != NULL);

    if (FALSE == H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, -1, "not a file access property list");

    plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id);
    if (NULL == plist_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, -1, "unable to get property list");

    *id_out_ptr = H5P_copy_plist(plist_ptr, FALSE);
    if (H5I_INVALID_HID == *id_out_ptr)
        HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, -1, "unable to copy file access property list");

done:
    FUNC_LEAVE_NOAPI(ret_value);
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

    FUNC_ENTER_STATIC

    new_fa = (H5FD_subfiling_config_t *)H5MM_malloc(sizeof(H5FD_subfiling_config_t));
    if (new_fa == NULL) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    }

    HDmemcpy(new_fa, old_fa, sizeof(H5FD_subfiling_config_t));
    ret_value = new_fa;

done:
    if (ret_value == NULL) {

        if (new_fa != NULL) {
            H5MM_xfree(new_fa);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
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
    H5FD_subfiling_config_t *fa = (H5FD_subfiling_config_t *)_fa;

    FUNC_ENTER_STATIC_NOERR

    HDassert(fa != NULL); /* sanity check */

    H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(SUCCEED)
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
    H5FD_driver_prop_t             driver_prop;              /* Property for driver ID & info */
    int                            mpi_code;                 /* MPI return code */
    H5FD_t *                       ret_value = NULL;

    FUNC_ENTER_STATIC

    /* Check arguments */
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    if (ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr")

    if (NULL == (file_ptr = (H5FD_subfiling_t *)H5FL_CALLOC(H5FD_subfiling_t)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate file struct")
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;

    /* Get the driver-specific file access properties */
    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")

    if (H5FD_mpi_self_initialized) {
        file_ptr->comm = MPI_COMM_WORLD;
        file_ptr->info = MPI_INFO_NULL;
    }
    else {
        /* Get the MPI communicator and info object from the property list */
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_COMM_NAME, &file_ptr->comm) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI communicator")
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_INFO_NAME, &file_ptr->info) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI info object")

        if (file_ptr->comm == MPI_COMM_NULL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid or unset MPI communicator in FAPL")
    }

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_ptr->comm, &file_ptr->mpi_rank)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code)
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(file_ptr->comm, &file_ptr->mpi_size)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code)

    config_ptr = H5P_peek_driver_info(plist_ptr);
    if (!config_ptr || (H5P_FILE_ACCESS_DEFAULT == fapl_id)) {
        if (H5FD__subfiling_get_default_config(fapl_id, &default_config) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get default subfiling VFD configuration")
        config_ptr = &default_config;
    }

    HDmemcpy(&file_ptr->fa, config_ptr, sizeof(H5FD_subfiling_config_t));

    /*
     * Extend the config info with file_path and file_dir
     * TODO: revise how this is handled
     */
    if (HDrealpath(name, file_ptr->fa.file_path) != NULL) {
        char *path      = HDstrdup(file_ptr->fa.file_path);
        char *directory = dirname(path);
        HDstrcpy(file_ptr->fa.file_dir, directory);
        HDfree(path);
    }

    /* Copy the FAPL from the config structure */
    /* JRM:  Why is this necessary?  If it is necessary, must close the property list on file close. */
    if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(file_ptr->fa.ioc_fapl_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy IOC FAPL");

    file_ptr->sf_file = H5FD_open(name, flags, config_ptr->ioc_fapl_id, HADDR_UNDEF);
    if (!file_ptr->sf_file)
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "unable to open IOC file")

    /* Check the "native" driver (IOC/sec2/etc.) */
    if (NULL == (plist_ptr = H5I_object(config_ptr->ioc_fapl_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "invalid IOC FAPL")

    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver ID & info")
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid driver ID in file access property list")

    if (driver->value != H5_VFD_IOC && driver->value != H5_VFD_SEC2)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                    "unable to open file '%s' - only IOC and Sec2 VFDs are currently supported for subfiles",
                    name)

    if (driver->value == H5_VFD_IOC) {
        /* We've already opened the subfiles... */
        H5FD_subfiling_t *ioc_file = (H5FD_subfiling_t *)(file_ptr->sf_file);

        /* Get a copy of the context ID for later use */
        file_ptr->fa.context_id  = ioc_file->fa.context_id;
        file_ptr->fa.require_ioc = true;
    }
    else if (driver->value == H5_VFD_SEC2) {
        uint64_t inode_id  = (uint64_t)-1;
        int      ioc_flags = O_RDWR;

        /* Translate the HDF5 file open flags into standard POSIX open flags */
        if (flags & H5F_ACC_TRUNC)
            ioc_flags |= O_TRUNC;
        if (flags & H5F_ACC_CREAT)
            ioc_flags |= O_CREAT;

        /* Let MPI rank 0 to the file stat operation and broadcast a result */
        if (file_ptr->mpi_rank == 0) {
            if (file_ptr->sf_file) {
                h5_stat_t sb;
                void *    file_handle = NULL;

                if (H5FDget_vfd_handle(file_ptr->sf_file, config_ptr->ioc_fapl_id, &file_handle) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get file handle")

                /* We create a new file descriptor for our file structure.
                 * Basically, we want these separate so that sec2 can
                 * deal with the opened file for additional operations
                 * (especially close) without interfering with subfiling.
                 */
                file_ptr->fd = HDdup(*(int *)file_handle);

                if (HDfstat(*(int *)file_handle, &sb) < 0)
                    HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file")
                inode_id = sb.st_ino;
            }
        }

        if (MPI_SUCCESS == MPI_Bcast(&inode_id, 1, MPI_UNSIGNED_LONG_LONG, 0, file_ptr->comm)) {
            file_ptr->inode = inode_id;
        }

        /* All ranks can now detect an error and fail. */
        if (inode_id == (uint64_t)-1)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file = %s\n", name)

        /* See: H5FDsubfile_int.c:
         * Note that the user defined HDF5 file is also considered subfile(0) */
        if (H5FD__open_subfiles(file_ptr->fa.file_path, (void *)&file_ptr->fa, inode_id, ioc_flags) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiling files = %s\n", name)
    }

    ret_value = (H5FD_t *)file_ptr;

done:
    if (NULL == ret_value) {
        if (file_ptr) {
            /* TODO: FAPL ID will likely never be H5I_INVALID_HID since it's currently initialized to 0 */
            if (H5I_INVALID_HID != file_ptr->fa.ioc_fapl_id)
                H5I_dec_ref(file_ptr->fa.ioc_fapl_id);
            if (file_ptr->sf_file)
                H5FD_close(file_ptr->sf_file);
            H5FL_FREE(H5FD_subfiling_t, file_ptr);
        }
    } /* end if error */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__subfiling_open() */

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
    H5FD_subfiling_t *   file_ptr   = (H5FD_subfiling_t *)_file;
    herr_t               ret_value  = SUCCEED; /* Return value */
    subfiling_context_t *sf_context = NULL;

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(file_ptr);

    sf_context = (subfiling_context_t *)get__subfiling_object(file_ptr->fa.context_id);

#if H5FD_SUBFILING_DEBUG_OP_CALLS
    if (sf_context->topology->rank_is_ioc)
        printf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
    else
        printf("[%s %d] fd=*\n", __func__, file_ptr->mpi_rank);
    fflush(stdout);
#endif
    if (H5FD_close(file_ptr->sf_file) != SUCCEED) {
        HSYS_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close file")
    }

    if (sf_context != NULL) {
        if (sf_context->subfile_prefix) {
            HDfree(sf_context->subfile_prefix);
            sf_context->subfile_prefix = NULL;
        }
        if (sf_context->sf_filename) {
            HDfree(sf_context->sf_filename);
            sf_context->sf_filename = NULL;
        }
        if (sf_context->h5_filename) {
            HDfree(sf_context->h5_filename);
            sf_context->h5_filename = NULL;
        }
    }

    /* if set, close the copy of the plist for the underlying VFD. */
    if ((H5I_INVALID_HID != file_ptr->fa.ioc_fapl_id) && (H5I_dec_ref(file_ptr->fa.ioc_fapl_id) < 0))
        HGOTO_ERROR(H5E_VFL, H5E_ARGS, FAIL, "can't close ioc FAPL")

    /* Release the file info */
    file_ptr = H5FL_FREE(H5FD_subfiling_t, file_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC_NOERR

    HDassert(f1);
    HDassert(f2);

    ret_value = H5FD_cmp(f1->sf_file, f2->sf_file);

    FUNC_LEAVE_NOAPI(ret_value)
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
    FUNC_ENTER_STATIC_NOERR

    /* Set the VFL feature flags that this driver supports */
    if (flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;     /* OK to aggregate metadata allocations  */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA;    /* OK to aggregate "small" raw data allocations */
        *flags |= H5FD_FEAT_HAS_MPI;                /* This driver uses MPI */
        *flags |= H5FD_FEAT_ALLOCATE_EARLY;         /* Allocate space early instead of late  */
        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE; /* VFD creates a file which can be opened with the default
                                                       VFD */
                                                    /* TODO: this is false -- delete the flag eventually */
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
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
    const H5FD_subfiling_t *file = (const H5FD_subfiling_t *)_file;

    FUNC_ENTER_STATIC_NOERR

    FUNC_LEAVE_NOAPI(file->eoa)
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
    H5FD_subfiling_t *file_ptr = (H5FD_subfiling_t *)_file;

    FUNC_ENTER_STATIC_NOERR

    file_ptr->eoa = addr;

    H5FD_set_eoa(file_ptr->sf_file, type, addr);

    FUNC_LEAVE_NOAPI(SUCCEED)
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
    const H5FD_subfiling_t *file        = (const H5FD_subfiling_t *)_file;
    int64_t                 logical_eof = -1;
    haddr_t                 ret_value   = HADDR_UNDEF;

    FUNC_ENTER_STATIC

#if 0 /* TODO */
    int64_t local_eof = H5FDget_eof(file->sf_file, type);

    if (MPI_SUCCESS != MPI_Allreduce(&local_eof, &logical_eof, 1, MPI_INT64_t, MPI_MAX, file->comm))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, HADDR_UNDEF, "MPI_Allreduce failed")
#endif

    /*
     * TODO: this is a heavy weight implementation.  We need something like this
     * for file open, and probably for file close.  However, in between, something
     * similar to the current solution in the MPIIO VFD might be more appropriate.
     */
    if (H5FD__subfiling__get_real_eof(file->fa.context_id, &logical_eof) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, HADDR_UNDEF, "can't get EOF")

    /* Return the global max of all the subfile EOF values */
    ret_value = (haddr_t)(logical_eof);

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    if (!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    *file_handle = &(file->fd);

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
    hbool_t              addrs_cooked       = FALSE;
    int                  ioc_total;
    herr_t               ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(file_ptr && file_ptr->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if (!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %" PRIuHADDR, addr)
    if (REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %" PRIuHADDR ", size = %" PRIuHADDR,
                    addr, size)

    /*
     * Apply the base address offset to the address for the read call.
     * Must then undo this addition before updating the file position
     */
    addr += _file->base_addr;
    addrs_cooked = TRUE;

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
    sf_context = (subfiling_context_t *)get__subfiling_object(file_ptr->fa.context_id);
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
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid number of I/O concentrators (%d)", ioc_total)
    }
    else if (ioc_total == 1) {
        /***********************************
         * No striping - just a single IOC *
         ***********************************/

        /* Make vector read call to subfile */
        if (H5FDread_vector(file_ptr->sf_file, dxpl_id, 1, &type, &addr, &size, &buf) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "read from subfile failed")
    }
    else {
        int64_t file_offset;
        int64_t io_size;
        int64_t block_size;
        size_t  max_depth;
        int     max_io_req_per_ioc;
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
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate source data offset I/O vector")
        if (NULL == (sf_data_size = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_data_size))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile data size I/O vector")
        if (NULL == (sf_offset = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_offset))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile offset I/O vector")

        H5_CHECKED_ASSIGN(file_offset, int64_t, addr, haddr_t);
        H5_CHECKED_ASSIGN(io_size, int64_t, size, size_t);

        /*
         * Get the potential set of IOC transactions; e.g., data sizes,
         * offsets and datatypes. These can all be used by either the
         * underlying IOC or by the sec2 driver.
         *
         * For now, assume we're dealing with contiguous datasets. Vector
         * I/O will probably handle the non-contiguous case.
         */
        max_io_req_per_ioc = init__indep_io(sf_context,         /* IN: Context used to look up config info */
                                            max_depth,          /* IN: Maximum stripe depth */
                                            ioc_total,          /* IN: Total number of IOCs */
                                            source_data_offset, /* OUT: Memory offset */
                                            sf_data_size,       /* OUT: Length of this contiguous block */
                                            sf_offset,          /* OUT: File offset */
                                            &ioc_start,  /* OUT: IOC index corresponding to starting offset */
                                            &ioc_count,  /* OUT: Number of actual IOCs used */
                                            file_offset, /* IN: Starting file offset */
                                            io_size,     /* IN: I/O size */
                                            1);          /* IN: Data extent of the 'type' assumes byte */

        if (max_io_req_per_ioc > 0) {
            uint32_t vector_len;

            H5_CHECKED_ASSIGN(vector_len, uint32_t, ioc_count, int);

            /* Allocate I/O vectors */
            if (NULL == (io_types = HDmalloc(vector_len * sizeof(*io_types))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O types vector")
            if (NULL == (io_addrs = HDmalloc(vector_len * sizeof(*io_addrs))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O addresses vector")
            if (NULL == (io_sizes = HDmalloc(vector_len * sizeof(*io_sizes))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O sizes vector")
            if (NULL == (io_bufs = HDmalloc(vector_len * sizeof(*io_bufs))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O buffers vector")

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

            for (int i = 0; i < max_io_req_per_ioc; i++) {
                int next = ioc_start;

                /* Fill in I/O types, offsets, sizes and buffers vectors */
                for (uint32_t k = 0; k < vector_len; k++) {
                    size_t idx = (size_t)next * max_depth + (size_t)i;

                    io_types[k] = type;
                    H5_CHECKED_ASSIGN(io_addrs[k], haddr_t, sf_offset[idx], int64_t);
                    H5_CHECKED_ASSIGN(io_sizes[k], size_t, sf_data_size[idx], int64_t);
                    io_bufs[k] = ((char *)buf + source_data_offset[idx]);

                    /*
                     * TODO: this seems suspicious. may chop off last I/O
                     * if a 0 I/O appears in middle of vectors?
                     */
                    if (io_sizes[k] == 0)
                        vector_len--;

                    next = (next + 1) % ioc_count;

                    /* TODO: Reconcile between writes and reads here */
#if 0
                    next++;
                    if (next == ioc_total)
                        next = 0;
#endif
                }

                /* Make vector read call to subfile */
                if (H5FDread_vector(file_ptr->sf_file, dxpl_id, vector_len, io_types, io_addrs, io_sizes,
                                    io_bufs) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "read from subfile failed")
            }

            /* TODO: The following is left for future work */
            /* H5FDctl(file_ptr->sf_file, op_code_complete, flags, input, &output); */
        }
    }

    /* Point to the end of the current I/O */
    addr += (haddr_t)size;

    /*
     * If we applied the base address offset to the
     * address for reading, undo that operation now.
     */
    if (addrs_cooked)
        addr -= _file->base_addr;

    /* Update current file position and EOF */
    file_ptr->pos = addr;
    file_ptr->op  = OP_READ;
    /* TODO: this seems suspicious. Shouldn't need to update EOF on read */
    if (file_ptr->pos > file_ptr->eof)
        file_ptr->eof = file_ptr->pos;

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

    FUNC_LEAVE_NOAPI(ret_value)
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
    hbool_t              addrs_cooked       = FALSE;
    int                  ioc_total;
    herr_t               ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(file_ptr && file_ptr->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if (!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %" PRIuHADDR, addr)
    if (REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %" PRIuHADDR ", size = %" PRIuHADDR,
                    addr, size)

    /*
     * Apply the base address offset to the address for the write call.
     * Must then undo this addition before updating the file position
     */
    addr += _file->base_addr;
    addrs_cooked = TRUE;

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
    sf_context = (subfiling_context_t *)get__subfiling_object(file_ptr->fa.context_id);
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
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid number of I/O concentrators (%d)", ioc_total)
    }
    else if (ioc_total == 1) {
        /***********************************
         * No striping - just a single IOC *
         ***********************************/

        /* Make vector write call to subfile */
        if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, 1, &type, &addr, &size, &buf) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "write to subfile failed")
    }
    else {
        int64_t file_offset;
        int64_t io_size;
        int64_t block_size;
        size_t  max_depth;
        int     max_io_req_per_ioc;
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
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate source data offset I/O vector")
        if (NULL == (sf_data_size = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_data_size))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile data size I/O vector")
        if (NULL == (sf_offset = HDcalloc(1, (size_t)ioc_total * max_depth * sizeof(*sf_offset))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile offset I/O vector")

        H5_CHECKED_ASSIGN(file_offset, int64_t, addr, haddr_t);
        H5_CHECKED_ASSIGN(io_size, int64_t, size, size_t);

        /*
         * Get the potential set of IOC transactions; e.g., data sizes,
         * offsets and datatypes. These can all be used by either the
         * underlying IOC or by the sec2 driver.
         *
         * For now, assume we're dealing with contiguous datasets. Vector
         * I/O will probably handle the non-contiguous case.
         */
        max_io_req_per_ioc = init__indep_io(sf_context,         /* IN: Context used to look up config info */
                                            max_depth,          /* IN: Maximum stripe depth */
                                            ioc_total,          /* IN: Total number of IOCs */
                                            source_data_offset, /* OUT: Memory offset */
                                            sf_data_size,       /* OUT: Length of this contiguous block */
                                            sf_offset,          /* OUT: File offset */
                                            &ioc_start,  /* OUT: IOC index corresponding to starting offset */
                                            &ioc_count,  /* OUT: Number of actual IOCs used */
                                            file_offset, /* IN: Starting file offset */
                                            io_size,     /* IN: I/O size */
                                            1);          /* IN: Data extent of the 'type' assumes byte */

        if (max_io_req_per_ioc > 0) {
            uint32_t vector_len;

            H5_CHECKED_ASSIGN(vector_len, uint32_t, ioc_count, int);

            /* Allocate I/O vectors */
            if (NULL == (io_types = HDmalloc(vector_len * sizeof(*io_types))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O types vector")
            if (NULL == (io_addrs = HDmalloc(vector_len * sizeof(*io_addrs))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O addresses vector")
            if (NULL == (io_sizes = HDmalloc(vector_len * sizeof(*io_sizes))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O sizes vector")
            if (NULL == (io_bufs = HDmalloc(vector_len * sizeof(*io_bufs))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate subfile I/O buffers vector")

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

            for (int i = 0; i < max_io_req_per_ioc; i++) {
                int next = ioc_start;

                /* Fill in I/O types, offsets, sizes and buffers vectors */
                for (uint32_t k = 0; k < vector_len; k++) {
                    size_t idx = (size_t)next * max_depth + (size_t)i;

                    io_types[k] = type;
                    H5_CHECKED_ASSIGN(io_addrs[k], haddr_t, sf_offset[idx], int64_t);
                    H5_CHECKED_ASSIGN(io_sizes[k], size_t, sf_data_size[idx], int64_t);
                    io_bufs[k] = ((const char *)buf + source_data_offset[idx]);

                    /*
                     * TODO: this seems suspicious. may chop off last I/O
                     * if a 0 I/O appears in middle of vectors?
                     */
                    if (io_sizes[k] == 0)
                        vector_len--;

                    next++;
                    if (next == ioc_total)
                        next = 0;
                }

                /* Make vector write call to subfile */
                if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, vector_len, io_types, io_addrs, io_sizes,
                                     io_bufs) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "write to subfile failed")
            }

            /* TODO: The following is left for future work */
            /* H5FDctl(file_ptr->sf_file, op_code_complete, flags, input, &output); */
        }
    }

    /* Point to the end of the current I/O */
    addr += (haddr_t)size;

    /*
     * If we applied the base address offset to the
     * address for writing, undo that operation now.
     */
    if (addrs_cooked)
        addr -= _file->base_addr;

    /* Update current file position and EOF */
    file_ptr->pos = addr;
    file_ptr->op  = OP_WRITE;
    if (file_ptr->pos > file_ptr->eof)
        file_ptr->eof = file_ptr->pos;

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

    FUNC_LEAVE_NOAPI(ret_value)
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
    herr_t            ret_value = SUCCEED; /* Return value             */

    FUNC_ENTER_STATIC

    /* Check arguments
     * RAW - Do we really need to check arguments once again?
     * These have already been checked in H5FD__subfiling_read_vector (see below)!
     */
    if (!file_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL")

    if ((!types) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "types parameter can't be NULL if count is positive")

    if ((!addrs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addrs parameter can't be NULL if count is positive")

    if ((!sizes) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "sizes parameter can't be NULL if count is positive")

    if ((!bufs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bufs parameter can't be NULL if count is positive")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")
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
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

            if ((addrs[k] + size) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, eoa = %llu", (int)k,
                            (unsigned long long)(addrs[k]), (int)k, (unsigned long long)size,
                            (unsigned long long)eoa)

            if (H5FD__subfiling_read(_file, type, dxpl_id, addrs[k], size, bufs[k]) != SUCCEED)
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "file vector read request failed")
        }
    }
    else {
        /* sec2 driver..
         * Call the subfiling 'direct write' version
         * of subfiling.
         */
        if (H5FD_read_vector(_file, count, types, addrs, sizes, bufs) != SUCCEED)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "file vector read request failed")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
    herr_t            ret_value = SUCCEED; /* Return value             */

    FUNC_ENTER_STATIC

    HDassert(file_ptr != NULL); /* sanity check */

    /* Check arguments
     * RAW - Do we really need to check arguments once again?
     * These have already been checked in H5FD__subfiling_write_vector (see below)!
     */
    if (!file_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL")

    if ((!types) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "types parameter can't be NULL if count is positive")

    if ((!addrs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addrs parameter can't be NULL if count is positive")

    if ((!sizes) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "sizes parameter can't be NULL if count is positive")

    if ((!bufs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bufs parameter can't be NULL if count is positive")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")
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
                HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver get_eoa request failed")

            if ((addrs[k] + size) > eoa)

                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "addr overflow, addrs[%d] = %llu, sizes[%d] = %llu, eoa = %llu", (int)k,
                            (unsigned long long)(addrs[k]), (int)k, (unsigned long long)size,
                            (unsigned long long)eoa)

            if (H5FD__subfiling_write(_file, type, dxpl_id, addrs[k], size, bufs[k]) != SUCCEED)
                HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file vector write request failed")
        }
    }
    else {
        /* sec2 driver..
         * Call the subfiling 'direct write' version
         * of subfiling.
         */
        if (H5FD_write_vector(_file, count, types, addrs, sizes, bufs) != SUCCEED)
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file vector write request failed")
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)

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

    FUNC_ENTER_STATIC

    HDassert(file);

    /* Extend the file to make sure it's large enough */
    if (!H5F_addr_eq(file->eoa, file->eof)) {

        /* Update the eof value */
        file->eof = file->eoa;

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op  = OP_UNKNOWN;
    } /* end if */

    /* truncate sub-files */
    /* This is a hack.  We should be doing the truncate of the sub-files via calls to
     * H5FD_truncate() with the IOC.  However, that system is messed up at present.
     * thus the following hack.
     *                                                 JRM -- 12/18/21
     */
#if 1 /* JRM */
    if (H5FD__subfiling__truncate_sub_files(file->eof, file->fa.context_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "sub-file truncate request failed")
#endif /* JRM */

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
static herr_t
H5FD__subfiling_lock(H5FD_t *_file, hbool_t rw)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file; /* VFD file struct  */
    herr_t            ret_value = SUCCEED;                   /* Return value       */

    FUNC_ENTER_STATIC

    HDassert(file);

    /* TODO: Consider lock only on IOC ranks for one IOC per subfile case */
    if (file->fa.require_ioc) {
#ifdef VERBOSE
        HDputs("Subfiling driver doesn't support file locking");
#endif
    }
    else {
        if (H5FD_lock(file->sf_file, rw) < 0)
            HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to lock file")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    HDassert(file);

    if (H5FD_unlock(file->sf_file) < 0)
        HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to lock file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__subfiling_unlock() */

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
 *                  H5FD_CTL__GET_MPI_COMMUNICATOR_OPCODE
 *                  H5FD_CTL__GET_MPI_RANK_OPCODE
 *                  H5FD_CTL__GET_MPI_SIZE_OPCODE
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

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(file);
    HDassert(H5FD_SUBFILING == file->pub.driver_id);

    switch (op_code) {

        case H5FD_CTL__GET_MPI_COMMUNICATOR_OPCODE:
            HDassert(output);
            HDassert(*output);
            **((MPI_Comm **)output) = file->comm;
            break;

        case H5FD_CTL__GET_MPI_RANK_OPCODE:
            HDassert(output);
            HDassert(*output);
            **((int **)output) = file->mpi_rank;
            break;

        case H5FD_CTL__GET_MPI_SIZE_OPCODE:
            HDassert(output);
            HDassert(*output);
            **((int **)output) = file->mpi_size;
            break;

        default: /* unknown op code */
            if (flags & H5FD_CTL__FAIL_IF_UNKNOWN_FLAG) {

                HGOTO_ERROR(H5E_VFL, H5E_FCNTL, FAIL, "unknown op_code and fail if unknown")
            }
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__subfiling_ctl() */
