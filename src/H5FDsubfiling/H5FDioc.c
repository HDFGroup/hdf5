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
 * Purpose:     The IOC VFD implements a file driver which relays all the
 *              VFD calls to an underlying VFD, and send all the write calls to
 *              another underlying VFD. Maintains two files simultaneously.
 */

/* This source code file is part of the H5FD driver module */
#include "H5FDdrvr_module.h"

#include "H5private.h"    /* Generic Functions        */
#include "H5FDpublic.h"   /* Basic H5FD definitions   */
#include "H5Eprivate.h"   /* Error handling           */
#include "H5FDprivate.h"  /* File drivers             */
#include "H5FDioc.h"      /* IOC file driver          */
#include "H5FDioc_priv.h" /* IOC file driver          */
#include "H5FDsec2.h"     /* Sec2 VFD                 */
#include "H5FLprivate.h"  /* Free Lists               */
#include "H5Fprivate.h"   /* File access              */
#include "H5Iprivate.h"   /* IDs                      */
#include "H5MMprivate.h"  /* Memory management        */
#include "H5Pprivate.h"   /* Property lists           */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_IOC_g = H5I_INVALID_HID;

/* Whether the driver initialized MPI on its own */
static hbool_t H5FD_mpi_self_initialized = FALSE;

/* Pointer to value for MPI_TAG_UB */
int *H5FD_IOC_tag_ub_val_ptr = NULL;

/* The information of this ioc */
typedef struct H5FD_ioc_t {
    H5FD_t            pub; /* public stuff, must be first    */
    int               fd;  /* the filesystem file descriptor */
    H5FD_ioc_config_t fa;  /* driver-specific file access properties */

    /* MPI Info */
    MPI_Comm comm;
    MPI_Info info;
    int      mpi_rank;
    int      mpi_size;

    H5FD_t *ioc_file; /* native HDF5 file pointer (sec2) */

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
} H5FD_ioc_t;

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

#ifdef H5FD_IOC_DEBUG
#define H5FD_IOC_LOG_CALL(name)                                                                              \
    do {                                                                                                     \
        HDprintf("called %s()\n", (name));                                                                   \
        HDfflush(stdout);                                                                                    \
    } while (0)
#else
#define H5FD_IOC_LOG_CALL(name) /* no-op */
#endif

/* Private functions */
/* Prototypes */
static herr_t  H5FD__ioc_term(void);
static hsize_t H5FD__ioc_sb_size(H5FD_t *_file);
static herr_t  H5FD__ioc_sb_encode(H5FD_t *_file, char *name /*out*/, unsigned char *buf /*out*/);
static herr_t  H5FD__ioc_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf);
static void *  H5FD__ioc_fapl_get(H5FD_t *_file);
static void *  H5FD__ioc_fapl_copy(const void *_old_fa);
static herr_t  H5FD__ioc_fapl_free(void *_fapl);
static H5FD_t *H5FD__ioc_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t  H5FD__ioc_close(H5FD_t *_file);
static int     H5FD__ioc_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t  H5FD__ioc_query(const H5FD_t *_file, unsigned long *flags /* out */);
static herr_t  H5FD__ioc_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map);
static haddr_t H5FD__ioc_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static herr_t  H5FD__ioc_free(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size);
static haddr_t H5FD__ioc_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type);
static herr_t  H5FD__ioc_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr);
static haddr_t H5FD__ioc_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type);
static herr_t  H5FD__ioc_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void **file_handle);
static herr_t  H5FD__ioc_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                              void *buf);
static herr_t  H5FD__ioc_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                               const void *buf);
static herr_t  H5FD__ioc_read_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                     haddr_t addrs[], size_t sizes[], void *bufs[] /* out */);
static herr_t  H5FD__ioc_write_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                      haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */);
static herr_t  H5FD__ioc_flush(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t  H5FD__ioc_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t  H5FD__ioc_lock(H5FD_t *_file, hbool_t rw);
static herr_t  H5FD__ioc_unlock(H5FD_t *_file);
static herr_t  H5FD__ioc_del(const char *name, hid_t fapl);
/*
static herr_t H5FD__ioc_ctl(H5FD_t *file, uint64_t op_code, uint64_t flags,
                            const void *input, void **result);
*/

static herr_t H5FD__ioc_get_default_config(H5FD_ioc_config_t *config_out);
static herr_t H5FD__ioc_validate_config(const H5FD_ioc_config_t *fa);
static int    H5FD__copy_plist(hid_t fapl_id, hid_t *id_out_ptr);

static herr_t H5FD__ioc_close_int(H5FD_ioc_t *file_ptr);

static herr_t H5FD__ioc_write_vector_internal(H5FD_t *_file, uint32_t count, H5FD_mem_t types[],
                                              haddr_t addrs[], size_t sizes[],
                                              const void *bufs[] /* data_in */);
static herr_t H5FD__ioc_read_vector_internal(H5FD_t *_file, uint32_t count, haddr_t addrs[], size_t sizes[],
                                             void *bufs[] /* data_out */);

static const H5FD_class_t H5FD_ioc_g = {
    H5FD_CLASS_VERSION,        /* VFD interface version */
    H5_VFD_IOC,                /* value                 */
    H5FD_IOC_NAME,             /* name                  */
    MAXADDR,                   /* maxaddr               */
    H5F_CLOSE_WEAK,            /* fc_degree             */
    H5FD__ioc_term,            /* terminate             */
    H5FD__ioc_sb_size,         /* sb_size               */
    H5FD__ioc_sb_encode,       /* sb_encode             */
    H5FD__ioc_sb_decode,       /* sb_decode             */
    sizeof(H5FD_ioc_config_t), /* fapl_size             */
    H5FD__ioc_fapl_get,        /* fapl_get              */
    H5FD__ioc_fapl_copy,       /* fapl_copy             */
    H5FD__ioc_fapl_free,       /* fapl_free             */
    0,                         /* dxpl_size             */
    NULL,                      /* dxpl_copy             */
    NULL,                      /* dxpl_free             */
    H5FD__ioc_open,            /* open                  */
    H5FD__ioc_close,           /* close                 */
    H5FD__ioc_cmp,             /* cmp                   */
    H5FD__ioc_query,           /* query                 */
    H5FD__ioc_get_type_map,    /* get_type_map          */
    H5FD__ioc_alloc,           /* alloc                 */
    H5FD__ioc_free,            /* free                  */
    H5FD__ioc_get_eoa,         /* get_eoa               */
    H5FD__ioc_set_eoa,         /* set_eoa               */
    H5FD__ioc_get_eof,         /* get_eof               */
    H5FD__ioc_get_handle,      /* get_handle            */
    H5FD__ioc_read,            /* read                  */
    H5FD__ioc_write,           /* write                 */
    H5FD__ioc_read_vector,     /* read_vector           */
    H5FD__ioc_write_vector,    /* write_vector          */
    NULL,                      /* read_selection        */
    NULL,                      /* write_selection       */
    H5FD__ioc_flush,           /* flush                 */
    H5FD__ioc_truncate,        /* truncate              */
    H5FD__ioc_lock,            /* lock                  */
    H5FD__ioc_unlock,          /* unlock                */
    H5FD__ioc_del,             /* del                   */
    NULL,                      /* ctl                   */
    H5FD_FLMAP_DICHOTOMY       /* fl_map                */
};

/* Declare a free list to manage the H5FD_ioc_t struct */
H5FL_DEFINE_STATIC(H5FD_ioc_t);

/* Declare a free list to manage the H5FD_ioc_config_t struct */
H5FL_DEFINE_STATIC(H5FD_ioc_config_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD_ioc_init
 *
 * Purpose:     Initialize the IOC driver by registering it with the
 *              library.
 *
 * Return:      Success:    The driver ID for the ioc driver.
 *              Failure:    Negative
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_ioc_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;

    H5FD_IOC_LOG_CALL(__func__);

    /* Register the IOC VFD, if it isn't already registered */
    if (H5I_VFL != H5I_get_type(H5FD_IOC_g)) {
        char *env_var;
        int   key_val_retrieved = 0;
        int   mpi_code;

        if ((H5FD_IOC_g = H5FD_register(&H5FD_ioc_g, sizeof(H5FD_class_t), FALSE)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_ID, H5E_CANTREGISTER, H5I_INVALID_HID, "can't register IOC VFD");

        /* Check if IOC VFD has been loaded dynamically */
        env_var = HDgetenv(HDF5_DRIVER);
        if (env_var && !HDstrcmp(env_var, H5FD_IOC_NAME)) {
            int mpi_initialized = 0;
            int provided        = 0;

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
                        "IOC VFD requires the use of MPI_Init_thread with MPI_THREAD_MULTIPLE");
            }
            else {
                int required = MPI_THREAD_MULTIPLE;

                /* Otherwise, initialize MPI */
                if (MPI_SUCCESS != (mpi_code = MPI_Init_thread(NULL, NULL, required, &provided)))
                    H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Init_thread failed", mpi_code);

                H5FD_mpi_self_initialized = TRUE;

                if (provided != required)
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                            "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE");
            }
        }

        /* Retrieve upper bound for MPI message tag value */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_TAG_UB, &H5FD_IOC_tag_ub_val_ptr,
                                                         &key_val_retrieved)))
            H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Comm_get_attr failed", mpi_code);

        if (!key_val_retrieved)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                    "couldn't retrieve value for MPI_TAG_UB");
    }

    ret_value = H5FD_IOC_g;

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD_ioc_init() */

/*---------------------------------------------------------------------------
 * Function:    H5FD__ioc_term
 *
 * Purpose:     Shut down the IOC VFD.
 *
 * Returns:     SUCCEED (Can't fail)
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_term(void)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (H5FD_IOC_g >= 0) {
        /* Terminate MPI if the driver initialized it */
        if (H5FD_mpi_self_initialized) {
            int mpi_finalized = 0;
            int mpi_code;

            if (MPI_SUCCESS != (mpi_code = MPI_Finalized(&mpi_finalized)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalized failed", mpi_code);
            if (!mpi_finalized) {
                if (MPI_SUCCESS != (mpi_code = MPI_Finalize()))
                    H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalize failed", mpi_code);
            }

            H5FD_mpi_self_initialized = FALSE;
        }
    }

done:
    /* Reset VFL ID */
    H5FD_IOC_g = H5I_INVALID_HID;

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_term() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_ioc
 *
 * Purpose:     Sets the file access property list to use the
 *              ioc driver.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *vfd_config)
{
    H5FD_ioc_config_t *ioc_conf  = NULL;
    H5P_genplist_t *   plist_ptr = NULL;
    herr_t             ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (NULL == (plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (vfd_config == NULL) {
        if (NULL == (ioc_conf = HDcalloc(1, sizeof(*ioc_conf))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate IOC VFD configuration");
        ioc_conf->ioc_fapl_id = H5I_INVALID_HID;

        /* Get IOC VFD defaults */
        if (H5FD__ioc_get_default_config(ioc_conf) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get default IOC VFD configuration");

        vfd_config = ioc_conf;
    }

    if (H5FD__ioc_validate_config(vfd_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid IOC VFD configuration");

    ret_value = H5P_set_driver(plist_ptr, H5FD_IOC, vfd_config, NULL);

done:
    if (ioc_conf) {
        if (ioc_conf->ioc_fapl_id >= 0 && H5I_dec_ref(ioc_conf->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTDEC, FAIL, "can't close IOC FAPL");
        HDfree(ioc_conf);
    }

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5Pset_fapl_ioc() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_ioc
 *
 * Purpose:     Returns information about the ioc file access property
 *              list through the structure config_out.
 *
 *              Will fail if config_out is received without pre-set valid
 *              magic and version information.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *config_out)
{
    const H5FD_ioc_config_t *config_ptr         = NULL;
    H5P_genplist_t *         plist_ptr          = NULL;
    hbool_t                  use_default_config = FALSE;
    herr_t                   ret_value          = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    if (config_out == NULL)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL");

    if (NULL == (plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (H5FD_IOC != H5P_peek_driver(plist_ptr))
        use_default_config = TRUE;
    else {
        config_ptr = H5P_peek_driver_info(plist_ptr);
        if (NULL == config_ptr)
            use_default_config = TRUE;
    }

    if (use_default_config) {
        if (H5FD__ioc_get_default_config(config_out) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get default IOC VFD configuration");
    }
    else {
        /* Copy the IOC fapl data out */
        HDmemcpy(config_out, config_ptr, sizeof(H5FD_ioc_config_t));

        /* Copy the driver info value */
        if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(config_out->ioc_fapl_id)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't copy IOC FAPL");
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5Pget_fapl_ioc() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_default_config
 *
 * Purpose:     This is called by H5Pset/get_fapl_ioc when called with no
 *              established configuration info.  This simply fills in
 *              the basics.   This avoids the necessity of having the
 *              user write code to initialize the config structure.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_get_default_config(H5FD_ioc_config_t *config_out)
{
    herr_t ret_value = SUCCEED;

    HDassert(config_out);

    HDmemset(config_out, 0, sizeof(*config_out));

    config_out->magic         = H5FD_IOC_FAPL_MAGIC;
    config_out->version       = H5FD_CURR_IOC_FAPL_VERSION;
    config_out->ioc_fapl_id   = H5I_INVALID_HID;
    config_out->stripe_count  = 0;
    config_out->stripe_depth  = H5FD_DEFAULT_STRIPE_DEPTH;
    config_out->ioc_selection = SELECT_IOC_ONE_PER_NODE;

    /* Create a default FAPL and choose an appropriate underlying driver */
    if ((config_out->ioc_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "can't create default FAPL");

    /* Currently, only sec2 vfd supported */
    if (H5Pset_fapl_sec2(config_out->ioc_fapl_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set Sec2 VFD on IOC FAPL");

    /* Specific to this I/O Concentrator */
    config_out->thread_pool_count = H5FD_IOC_THREAD_POOL_SIZE;

done:
    if (ret_value < 0) {
        if (config_out->ioc_fapl_id >= 0 && H5Pclose(config_out->ioc_fapl_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_PLIST, H5E_CANTCLOSEOBJ, FAIL, "can't close FAPL");
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_validate_config()
 *
 * Purpose:     Test to see if the supplied instance of
 *              H5FD_ioc_config_t contains internally consistent data.
 *              Return SUCCEED if so, and FAIL otherwise.
 *
 *              Note the difference between internally consistent and
 *              correct.  As we will have to try to setup the IOC to
 *              determine whether the supplied data is correct,
 *              we will settle for internal consistency at this point
 *
 * Return:      SUCCEED if instance of H5FD_ioc_config_t contains
 *              internally consistent data, FAIL otherwise.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_validate_config(const H5FD_ioc_config_t *fa)
{
    herr_t ret_value = SUCCEED;

    HDassert(fa != NULL);

    if (fa->version != H5FD_CURR_IOC_FAPL_VERSION)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown H5FD_ioc_config_t version");

    if (fa->magic != H5FD_IOC_FAPL_MAGIC)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid H5FD_ioc_config_t magic value");

    /* TODO: add extra IOC configuration validation code */

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_validate_config() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_sb_size
 *
 * Purpose:     Obtains the number of bytes required to store the driver file
 *              access data in the HDF5 superblock.
 *
 * Return:      Success:    Number of bytes required.
 *
 *              Failure:    0 if an error occurs or if the driver has no
 *                          data to store in the superblock.
 *
 * NOTE: no public API for H5FD_sb_size, it needs to be added
 *-------------------------------------------------------------------------
 */
static hsize_t
H5FD__ioc_sb_size(H5FD_t *_file)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    hsize_t     ret_value = 0;

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if (file->ioc_file)
        ret_value = H5FD_sb_size(file->ioc_file);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_sb_size */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_sb_encode
 *
 * Purpose:     Encode driver-specific data into the output arguments.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_sb_encode(H5FD_t *_file, char *name /*out*/, unsigned char *buf /*out*/)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if (file->ioc_file && H5FD_sb_encode(file->ioc_file, name, buf) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTENCODE, FAIL, "unable to encode the superblock in R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_sb_encode */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_sb_decode
 *
 * Purpose:     Decodes the driver information block.
 *
 * Return:      SUCCEED/FAIL
 *
 * NOTE: no public API for H5FD_sb_size, need to add
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if (H5FD_sb_load(file->ioc_file, name, buf) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "unable to decode the superblock in R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_sb_decode */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_fapl_get
 *
 * Purpose:     Returns a file access property list which indicates how the
 *              specified file is being accessed. The return list could be
 *              used to access another file the same way.
 *
 * Return:      Success:    Ptr to new file access property list with all
 *                          members copied from the file struct.
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__ioc_fapl_get(H5FD_t *_file)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    void *      ret_value = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    ret_value = H5FD__ioc_fapl_copy(&(file->fa));

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__copy_plist
 *
 * Purpose:     Sanity-wrapped H5P_copy_plist() for each channel.
 *              Utility function for operation in multiple locations.
 *
 * Return:      0 on success, -1 on error.
 *-------------------------------------------------------------------------
 */
static int
H5FD__copy_plist(hid_t fapl_id, hid_t *id_out_ptr)
{
    int             ret_value = 0;
    H5P_genplist_t *plist_ptr = NULL;

    H5FD_IOC_LOG_CALL(__func__);

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
 * Function:    H5FD__ioc_fapl_copy
 *
 * Purpose:     Copies the file access properties.
 *
 * Return:      Success:    Pointer to a new property list info structure.
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__ioc_fapl_copy(const void *_old_fa)
{
    const H5FD_ioc_config_t *old_fa_ptr = (const H5FD_ioc_config_t *)_old_fa;
    H5FD_ioc_config_t *      new_fa_ptr = NULL;
    void *                   ret_value  = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    HDassert(old_fa_ptr);

    new_fa_ptr = H5FL_CALLOC(H5FD_ioc_config_t);
    if (NULL == new_fa_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate log file FAPL");

    HDmemcpy(new_fa_ptr, old_fa_ptr, sizeof(H5FD_ioc_config_t));

    /* Copy the FAPL */
    if (H5FD__copy_plist(old_fa_ptr->ioc_fapl_id, &(new_fa_ptr->ioc_fapl_id)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy the IOC FAPL");

    ret_value = (void *)new_fa_ptr;

done:
    if (NULL == ret_value)
        if (new_fa_ptr)
            new_fa_ptr = H5FL_FREE(H5FD_ioc_config_t, new_fa_ptr);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_fapl_copy() */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_fapl_free
 *
 * Purpose:     Releases the file access lists
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_fapl_free(void *_fapl)
{
    H5FD_ioc_config_t *fapl      = (H5FD_ioc_config_t *)_fapl;
    herr_t             ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    HDassert(fapl);

    if (fapl->ioc_fapl_id >= 0 && H5I_dec_ref(fapl->ioc_fapl_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close FAPL ID");

    /* Free the property list */
    fapl = H5FL_FREE(H5FD_ioc_config_t, fapl);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_fapl_free() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_open
 *
 * Purpose:     Create and/or opens a file as an HDF5 file.
 *
 * Return:      Success:    A pointer to a new file data structure. The
 *                          public fields will be initialized by the
 *                          caller, which is always H5FD_open().
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__ioc_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_ioc_t *             file_ptr   = NULL; /* Ioc VFD info */
    const H5FD_ioc_config_t *config_ptr = NULL; /* Driver-specific property list */
    H5FD_ioc_config_t        default_config;
    H5FD_class_t *           driver    = NULL; /* VFD for file */
    H5P_genplist_t *         plist_ptr = NULL;
    H5FD_driver_prop_t       driver_prop; /* Property for driver ID & info */
    int                      mpi_inited = 0;
    int                      mpi_code; /* MPI return code */
    H5FD_t *                 ret_value = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    if (!name || !*name)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name");
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr");
    if (ADDR_OVERFLOW(maxaddr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr");

    if (NULL == (file_ptr = (H5FD_ioc_t *)H5FL_CALLOC(H5FD_ioc_t)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate file struct");
    file_ptr->comm           = MPI_COMM_NULL;
    file_ptr->info           = MPI_INFO_NULL;
    file_ptr->context_id     = -1;
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;

    /* Get the driver-specific file access properties */
    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");

    if (H5FD_mpi_self_initialized) {
        file_ptr->comm = MPI_COMM_WORLD;
        file_ptr->info = MPI_INFO_NULL;

        mpi_inited = 1;
    }
    else {
        /* Get the MPI communicator and info object from the property list */
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_COMM_NAME, &file_ptr->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI communicator");
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_INFO_NAME, &file_ptr->info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI info object");

        if (file_ptr->comm == MPI_COMM_NULL)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid or unset MPI communicator in FAPL");

        /* Get the status of MPI initialization */
        if (MPI_SUCCESS != (mpi_code = MPI_Initialized(&mpi_inited)))
            H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Initialized failed", mpi_code);
        if (!mpi_inited)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_UNINITIALIZED, NULL, "MPI has not been initialized");
    }

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_ptr->comm, &file_ptr->mpi_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code);
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(file_ptr->comm, &file_ptr->mpi_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code);

    config_ptr = H5P_peek_driver_info(plist_ptr);
    if (!config_ptr || (H5P_FILE_ACCESS_DEFAULT == fapl_id)) {
        if (H5FD__ioc_get_default_config(&default_config) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get default IOC VFD configuration");
        config_ptr = &default_config;
    }

    /* Fill in the file config values */
    HDmemcpy(&file_ptr->fa, config_ptr, sizeof(H5FD_ioc_config_t));

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

    /* Copy the ioc FAPL. */
    if (H5FD__copy_plist(config_ptr->ioc_fapl_id, &(file_ptr->fa.ioc_fapl_id)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy FAPL");

    /* Check the underlying driver (sec2/mpio/etc.) */
    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(config_ptr->ioc_fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");

    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver ID & info");
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL,
                                "invalid driver ID in file access property list");

    if (driver->value != H5_VFD_SEC2) {
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                                "unable to open file '%s' - only Sec2 VFD is currently supported", name);
    }
    else {
        subfiling_context_t *sf_context = NULL;
        uint64_t             inode_id   = UINT64_MAX;
        int                  ioc_flags;
        int                  l_error = 0;
        int                  g_error = 0;

        /* Translate the HDF5 file open flags into standard POSIX open flags */
        ioc_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;
        if (H5F_ACC_TRUNC & flags)
            ioc_flags |= O_TRUNC;
        if (H5F_ACC_CREAT & flags)
            ioc_flags |= O_CREAT;
        if (H5F_ACC_EXCL & flags)
            ioc_flags |= O_EXCL;

        file_ptr->ioc_file = H5FD_open(file_ptr->file_path, flags, config_ptr->ioc_fapl_id, HADDR_UNDEF);
        if (file_ptr->ioc_file) {
            h5_stat_t sb;
            void *    file_handle = NULL;

            if (file_ptr->mpi_rank == 0) {
                if (H5FDget_vfd_handle(file_ptr->ioc_file, config_ptr->ioc_fapl_id, &file_handle) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get file handle");

                if (HDfstat(*(int *)file_handle, &sb) < 0)
                    H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file");

                HDcompile_assert(sizeof(uint64_t) >= sizeof(ino_t));
                file_ptr->inode = sb.st_ino;
                inode_id        = (uint64_t)sb.st_ino;
            }

            if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&inode_id, 1, MPI_UINT64_T, 0, file_ptr->comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code);

            if (file_ptr->mpi_rank != 0)
                file_ptr->inode = (ino_t)inode_id;
        }
        else {
            /* The two-step file opening approach may be
             * the root cause for the sec2 open to return a NULL.
             * It is prudent then, to collectively fail (early) in this case.
             */
            l_error = 1;
        }

        /* Check if any ranks had an issue opening the file */
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allreduce(&l_error, &g_error, 1, MPI_INT, MPI_SUM, file_ptr->comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Allreduce failed", mpi_code);
        if (g_error)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                                    "one or more MPI ranks were unable to open file '%s'", name);

        /*
         * Open the subfiles for this HDF5 file. A subfiling
         * context ID will be returned, which is used for
         * further interactions with this file's subfiles.
         */
        if (H5_open_subfiles(file_ptr->file_path, inode_id, file_ptr->fa.ioc_selection, ioc_flags,
                             file_ptr->comm, &file_ptr->context_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiles for file '%s'",
                                    name);

        /* Initialize I/O concentrator threads if this MPI rank is an I/O concentrator */
        sf_context = H5_get_subfiling_object(file_ptr->context_id);
        if (sf_context && sf_context->topology->rank_is_ioc) {
            if (initialize_ioc_threads(sf_context) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL,
                                        "unable to initialize I/O concentrator threads");
        }
    }

    ret_value = (H5FD_t *)file_ptr;

done:
    /* run a barrier just before exit.  The objective is to
     * ensure that the IOCs are fully up and running before
     * we proceed.  Note that this barrier is not sufficient
     * by itself -- we also need code in initialize_ioc_threads()
     * to wait until the main IOC thread has finished its
     * initialization.
     */
    if (mpi_inited) {
        MPI_Comm barrier_comm = MPI_COMM_WORLD;

        if (file_ptr && (file_ptr->comm != MPI_COMM_NULL))
            barrier_comm = file_ptr->comm;

        if (MPI_SUCCESS != (mpi_code = MPI_Barrier(barrier_comm)))
            H5_SUBFILING_MPI_DONE_ERROR(NULL, "MPI_Barrier failed", mpi_code);
    }

    if (NULL == ret_value) {
        if (file_ptr) {
            if (H5FD__ioc_close_int(file_ptr) < 0)
                H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close IOC file");
        }
    } /* end if error */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_open() */

static herr_t
H5FD__ioc_close_int(H5FD_ioc_t *file_ptr)
{
    herr_t ret_value = SUCCEED;

    HDassert(file_ptr);

#ifdef H5FD_IOC_DEBUG
    {
        subfiling_context_t *sf_context = H5_get_subfiling_object(file_ptr->fa.context_id);
        if (sf_context) {
            if (sf_context->topology->rank_is_ioc)
                HDprintf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
            else
                HDprintf("[%s %d] fd=*\n", __func__, file_ptr->mpi_rank);
        }
        else
            HDprintf("[%s %d] invalid subfiling context", __func__, file_ptr->mpi_rank);
        HDfflush(stdout);
    }
#endif

    if (file_ptr->fa.ioc_fapl_id >= 0 && H5I_dec_ref(file_ptr->fa.ioc_fapl_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_ARGS, FAIL, "can't close FAPL");
    file_ptr->fa.ioc_fapl_id = H5I_INVALID_HID;

    /* Close underlying file */
    if (file_ptr->ioc_file) {
        if (H5FD_close(file_ptr->ioc_file) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close HDF5 file");
        file_ptr->ioc_file = NULL;
    }

    if (file_ptr->context_id >= 0) {
        subfiling_context_t *sf_context = H5_get_subfiling_object(file_ptr->context_id);
        int                  mpi_code;

        /* Don't allow IOC threads to be finalized until everyone gets here */
        if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file_ptr->comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);

        if (sf_context && sf_context->topology->rank_is_ioc) {
            if (finalize_ioc_threads(sf_context) < 0)
                /* Note that closing of subfiles is collective */
                H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to finalize IOC threads");
        }

        if (H5_close_subfiles(file_ptr->context_id) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close subfiling file(s)");
        file_ptr->context_id = -1;
    }

    if (H5_mpi_comm_free(&file_ptr->comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Communicator");
    if (H5_mpi_info_free(&file_ptr->info) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Info object");

done:
    HDfree(file_ptr->file_path);
    file_ptr->file_path = NULL;

    HDfree(file_ptr->file_dir);
    file_ptr->file_dir = NULL;

    /* Release the file info */
    file_ptr = H5FL_FREE(H5FD_ioc_t, file_ptr);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_close
 *
 * Purpose:     Closes files
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL, file not closed.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_close(H5FD_t *_file)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (H5FD__ioc_close_int(file) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close IOC file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_cmp
 *
 * Purpose:     Compare the keys of two files.
 *
 * Return:      Success:    A value like strcmp()
 *              Failure:    Must never fail
 *-------------------------------------------------------------------------
 */
static int
H5FD__ioc_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_ioc_t *f1        = (const H5FD_ioc_t *)_f1;
    const H5FD_ioc_t *f2        = (const H5FD_ioc_t *)_f2;
    herr_t            ret_value = 0; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    HDassert(f1);
    HDassert(f2);

    ret_value = H5FD_cmp(f1->ioc_file, f2->ioc_file);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_cmp */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_query(const H5FD_t *_file, unsigned long *flags /* out */)
{
    const H5FD_ioc_t *file_ptr  = (const H5FD_ioc_t *)_file;
    herr_t            ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (file_ptr == NULL) {
        if (flags)
            *flags = 0;
    }
    else if (file_ptr->ioc_file) {
        if (H5FDquery(file_ptr->ioc_file, flags) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTLOCK, FAIL, "unable to query R/W file");
    }
    else {
        /* There is no file. Because this is a pure passthrough VFD,
         * it has no features of its own.
         */
        if (flags)
            *flags = 0;
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_type_map
 *
 * Purpose:     Retrieve the memory type mapping for this file
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map)
{
    const H5FD_ioc_t *file      = (const H5FD_ioc_t *)_file;
    herr_t            ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    /* Retrieve memory type mapping for R/W channel only */
    if (H5FD_get_fs_type_map(file->ioc_file, type_map) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to allocate for R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_type_map() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_alloc
 *
 * Purpose:     Allocate file memory.
 *
 * Return:      Address of allocated space (HADDR_UNDEF if error).
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ioc_alloc(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file; /* VFD file struct */
    haddr_t     ret_value = HADDR_UNDEF;         /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    /* Allocate memory for each file, only return the return value for R/W file.
     */
    if ((ret_value = H5FDalloc(file->ioc_file, type, dxpl_id, size)) == HADDR_UNDEF)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "unable to allocate for R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_alloc() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_free
 *
 * Purpose:     Free the resources for the ioc VFD.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_free(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file; /* VFD file struct */
    herr_t      ret_value = SUCCEED;             /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    if (H5FDfree(file->ioc_file, type, dxpl_id, addr, size) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free for R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_free() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_eoa
 *
 * Purpose:     Returns the end-of-address marker for the file. The EOA
 *              marker is the first address past the last byte allocated in
 *              the format address space.
 *
 * Return:      Success:    The end-of-address-marker
 *
 *              Failure:    HADDR_UNDEF
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ioc_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_ioc_t *file      = (const H5FD_ioc_t *)_file;
    haddr_t           ret_value = HADDR_UNDEF;

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if ((ret_value = H5FD_get_eoa(file->ioc_file, type)) == HADDR_UNDEF)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, HADDR_UNDEF, "unable to get eoa");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_eoa */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file. This function is
 *              called shortly after an existing HDF5 file is opened in order
 *              to tell the driver where the end of the HDF5 data is located.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);
    HDassert(file->ioc_file);

    if (H5FD_set_eoa(file->ioc_file, type, addr) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "H5FDset_eoa failed for R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_eof
 *
 * Purpose:     Returns the end-of-address marker for the file. The EOA
 *              marker is the first address past the last byte allocated in
 *              the format address space.
 *
 * Return:      Success:    The end-of-address-marker
 *
 *              Failure:    HADDR_UNDEF
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ioc_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_ioc_t *   file       = (const H5FD_ioc_t *)_file;
    haddr_t              ret_value  = HADDR_UNDEF; /* Return value */
    subfiling_context_t *sf_context = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    sf_context = H5_get_subfiling_object(file->context_id);
    if (sf_context) {
        ret_value = sf_context->sf_eof;
        goto done;
    }

    if (HADDR_UNDEF == (ret_value = H5FD_get_eof(file->ioc_file, type)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, HADDR_UNDEF, "unable to get eof");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_eof */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_handle
 *
 * Purpose:     Returns a pointer to the file handle of low-level virtual
 *              file driver.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void **file_handle)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);
    HDassert(file_handle);

    if (H5FD_get_vfd_handle(file->ioc_file, file->fa.ioc_fapl_id, file_handle) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to get handle of R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_handle */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_read
 *
 * Purpose:     Reads SIZE bytes of data from the R/W channel, beginning at
 *              address ADDR into buffer BUF according to data transfer
 *              properties in DXPL_ID.
 *
 * Return:      Success:    SUCCEED
 *                          The read result is written into the BUF buffer
 *                          which should be allocated by the caller.
 *              Failure:    FAIL
 *                          The contents of BUF are undefined.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr,
               size_t size, void *buf)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if (!H5F_addr_defined(addr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %" PRIuHADDR, addr);
    if (REGION_OVERFLOW(addr, size))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %" PRIuHADDR, addr);

    /* Public API for dxpl "context" */
    if (H5FDread(file->ioc_file, type, dxpl_id, addr, size, buf) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "Reading from R/W channel failed");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_write
 *
 * Purpose:     Writes SIZE bytes of data to IOC file, beginning at address
 *              ADDR from buffer BUF according to data transfer properties
 *              in DXPL_ID.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size, const void *buf)
{
    H5P_genplist_t *plist_ptr = NULL;
    herr_t          ret_value = SUCCEED;

    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(dxpl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");

    addr += _file->base_addr;

    ret_value = H5FD__ioc_write_vector_internal(_file, 1, &type, &addr, &size, &buf);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_write() */

static herr_t
H5FD__ioc_read_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                      size_t sizes[], void *bufs[] /* out */)
{
    H5FD_ioc_t *file_ptr  = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    /* Check arguments */
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

    /* Get the default dataset transfer property list if the user didn't provide
     * one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    ret_value = H5FD__ioc_read_vector_internal(_file, count, addrs, sizes, bufs);

done:
    H5_SUBFILING_FUNC_LEAVE;
}

static herr_t
H5FD__ioc_write_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                       size_t sizes[], const void *bufs[] /* in */)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    /* Check arguments */
    if (!file)
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

    /* Get the default dataset transfer property list if the user didn't provide
     * one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    ret_value = H5FD__ioc_write_vector_internal(_file, count, types, addrs, sizes, bufs);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FDioc__write_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_flush
 *
 * Purpose:     Flushes all data to disk for underlying VFD.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_flush(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t closing)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    if (H5FDflush(file->ioc_file, dxpl_id, closing) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFLUSH, FAIL, "unable to flush R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_flush() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_truncate
 *
 * Purpose:     Notify driver to truncate the file back to the allocated size.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    HDassert(file);
    HDassert(file->ioc_file);
    HDassert(file->ioc_file);

    if (H5FDtruncate(file->ioc_file, dxpl_id, closing) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "unable to truncate R/W file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_truncate */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_lock
 *
 * Purpose:     Sets a file lock.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_lock(H5FD_t *_file, hbool_t rw)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file; /* VFD file struct */
    herr_t      ret_value = SUCCEED;             /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    HDassert(file);
    HDassert(file->ioc_file);

    if (H5FD_lock(file->ioc_file, rw) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTLOCKFILE, FAIL, "unable to lock file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_lock */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_unlock
 *
 * Purpose:     Removes a file lock.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_unlock(H5FD_t *_file)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file; /* VFD file struct */
    herr_t      ret_value = SUCCEED;             /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    if (H5FD_unlock(file->ioc_file) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTUNLOCKFILE, FAIL, "unable to unlock file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_unlock */

static herr_t
H5FD__ioc_del(const char *name, hid_t fapl)
{
    herr_t ret_value = SUCCEED;

    (void)name;
    (void)fapl;

    /* TODO: implement later */

    H5_SUBFILING_FUNC_LEAVE;
}

/*--------------------------------------------------------------------------
 * Function:   H5FD__ioc_write_vector_internal
 *
 * Purpose:    This function takes 'count' vector entries
 *             and initiates an asynch write operation for each.
 *             By asynchronous, we mean that MPI_Isends are utilized
 *             to communicate the write operations to the 'count'
 *             IO Concentrators.  The calling function will have
 *             decomposed the actual user IO request into the
 *             component segments, each IO having a maximum size
 *             of "stripe_depth", which is recorded in the
 *             subfiling_context_t 'sf_context' structure.
 *
 * Return:     SUCCEED if no errors, FAIL otherwise.
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_write_vector_internal(H5FD_t *_file, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                                size_t sizes[], const void *bufs[] /* in */)
{
    subfiling_context_t *sf_context    = NULL;
    MPI_Request *        active_reqs   = NULL;
    H5FD_ioc_t *         file_ptr      = (H5FD_ioc_t *)_file;
    io_req_t **          sf_async_reqs = NULL;
    int64_t              sf_context_id = -1;
    herr_t               ret_value     = SUCCEED;
    struct __mpi_req {
        int          n_reqs;
        MPI_Request *active_reqs;
    } *mpi_reqs = NULL;

    HDassert(_file);
    HDassert(addrs);
    HDassert(sizes);
    HDassert(bufs);

    if (count == 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    sf_context_id = file_ptr->context_id;

    if (NULL == (sf_context = H5_get_subfiling_object(sf_context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "can't get subfiling context from ID");
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->n_io_concentrators);

    if (NULL == (active_reqs = HDcalloc((size_t)(count + 2), sizeof(struct __mpi_req))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "can't allocate active I/O requests array");

    if (NULL == (sf_async_reqs = HDcalloc((size_t)count, sizeof(*sf_async_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate I/O request array");

    /*
     * Note: We allocated extra space in the active_requests (above).
     * The extra should be enough for an integer plus a pointer.
     */
    mpi_reqs              = (struct __mpi_req *)&active_reqs[count];
    mpi_reqs->n_reqs      = (int)count;
    mpi_reqs->active_reqs = active_reqs;

    /* Each pass thru the following should queue an MPI write
     * to a new IOC. Both the IOC selection and offset within the
     * particular subfile are based on the combination of striping
     * factors and the virtual file offset (addrs[i]).
     */
    for (size_t i = 0; i < (size_t)count; i++) {
        herr_t write_status;

        if (sizes[i] == 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "invalid size argument of 0");

        H5_CHECK_OVERFLOW(addrs[i], haddr_t, int64_t);
        H5_CHECK_OVERFLOW(sizes[i], size_t, int64_t);
        write_status =
            ioc__write_independent_async(sf_context_id, sf_context->topology->n_io_concentrators,
                                         (int64_t)addrs[i], (int64_t)sizes[i], bufs[i], &sf_async_reqs[i]);

        if (write_status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't queue write operation");

        mpi_reqs->active_reqs[i] = sf_async_reqs[i]->completion_func.io_args.io_req;
    }

    /*
     * Mirror superblock writes to the stub file so that
     * legacy HDF5 applications can check what type of
     * file they are reading
     */
    for (size_t i = 0; i < (size_t)count; i++) {
        if (types[i] == H5FD_MEM_SUPER) {
            if (H5FDwrite(file_ptr->ioc_file, H5FD_MEM_SUPER, H5P_DEFAULT, addrs[i], sizes[i], bufs[i]) < 0)
                H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
                                        "couldn't write superblock information to stub file");
        }
    }

    /* Here, we should have queued 'count' async requests.
     * We can can now try to complete those before returning
     * to the caller for the next set of IO operations.
     */
    if (sf_async_reqs[0]->completion_func.io_function)
        ret_value = (*sf_async_reqs[0]->completion_func.io_function)(mpi_reqs);

done:
    if (active_reqs)
        HDfree(active_reqs);

    if (sf_async_reqs) {
        for (size_t i = 0; i < (size_t)count; i++) {
            if (sf_async_reqs[i]) {
                HDfree(sf_async_reqs[i]);
            }
        }
        HDfree(sf_async_reqs);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

static herr_t
H5FD__ioc_read_vector_internal(H5FD_t *_file, uint32_t count, haddr_t addrs[], size_t sizes[],
                               void *bufs[] /* out */)
{
    subfiling_context_t *sf_context    = NULL;
    MPI_Request *        active_reqs   = NULL;
    H5FD_ioc_t *         file_ptr      = (H5FD_ioc_t *)_file;
    io_req_t **          sf_async_reqs = NULL;
    int64_t              sf_context_id = -1;
    herr_t               ret_value     = SUCCEED;
    struct __mpi_req {
        int          n_reqs;
        MPI_Request *active_reqs;
    } *mpi_reqs = NULL;

    HDassert(_file);
    HDassert(addrs);
    HDassert(sizes);
    HDassert(bufs);

    if (count == 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    sf_context_id = file_ptr->context_id;

    if (NULL == (sf_context = H5_get_subfiling_object(sf_context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "can't get subfiling context from ID");
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->n_io_concentrators);

    if (NULL == (active_reqs = HDcalloc((size_t)(count + 2), sizeof(struct __mpi_req))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "can't allocate active I/O requests array");

    if (NULL == (sf_async_reqs = HDcalloc((size_t)count, sizeof(*sf_async_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate I/O request array");

    /*
     * Note: We allocated extra space in the active_requests (above).
     * The extra should be enough for an integer plus a pointer.
     */
    mpi_reqs              = (struct __mpi_req *)&active_reqs[count];
    mpi_reqs->n_reqs      = (int)count;
    mpi_reqs->active_reqs = active_reqs;

    for (size_t i = 0; i < (size_t)count; i++) {
        int read_status;

        H5_CHECK_OVERFLOW(addrs[i], haddr_t, int64_t);
        H5_CHECK_OVERFLOW(sizes[i], size_t, int64_t);
        read_status =
            ioc__read_independent_async(sf_context_id, sf_context->topology->n_io_concentrators,
                                        (int64_t)addrs[i], (int64_t)sizes[i], bufs[i], &sf_async_reqs[i]);

        if (read_status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't queue read operation");

        mpi_reqs->active_reqs[i] = sf_async_reqs[i]->completion_func.io_args.io_req;
    }

    /* Here, we should have queued 'count' async requests
     * (one to each required IOC).
     *
     * We can can now try to complete those before returning
     * to the caller for the next set of IO operations.
     */
    if (sf_async_reqs[0]->completion_func.io_function)
        ret_value = (*sf_async_reqs[0]->completion_func.io_function)(mpi_reqs);

done:
    if (active_reqs)
        HDfree(active_reqs);

    if (sf_async_reqs) {
        for (size_t i = 0; i < count; i++) {
            if (sf_async_reqs[i]) {
                HDfree(sf_async_reqs[i]);
            }
        }
        HDfree(sf_async_reqs);
    }

    H5_SUBFILING_FUNC_LEAVE;
}
