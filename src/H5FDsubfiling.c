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
 * Programmer:  Richard Warren
 *
 *
 * Purpose: An initial implementation of a subfiling VFD which is
 *          derived from other "stacked" VFDs such as the splitter,
 *          mirror, and family VFDs.
 */

#define H5S_FRIEND           /*suppress error about including H5Spkg	  */
#include "H5FDdrvr_module.h" /* This source code file is part of the H5FD driver module */

#include "H5CXprivate.h"   /* API contexts, etc.       */
#include "H5Dprivate.h"    /* Dataset stuff            */
#include "H5Eprivate.h"    /* Error handling           */
#include "H5FDprivate.h"   /* File drivers             */
#include "H5FDsubfiling.h" /* Subfiling file driver    */
#include "H5FLprivate.h"   /* Free Lists               */
#include "H5Fprivate.h"    /* File access              */
#include "H5Iprivate.h"    /* IDs                      */
#include "H5MMprivate.h"   /* Memory management        */
#include "H5Pprivate.h"    /* Property lists           */
#include "H5Spkg.h"        /* For selections and creation of subfiling vectors */
#include "H5private.h"     /* Generic Functions        */
#include "H5FDioc.h"       /* IOC                      */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_SUBFILING_g = 0;

#ifndef NDEBUG
FILE *sf_logfile = NULL;
FILE *client_log = NULL;
#endif

/* These are used for the creation of read or write vectors */
static haddr_t * sf_offsets = NULL;
static hssize_t *sf_sizes   = NULL;
static void **   sf_bufs    = NULL;

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

    /* the following fields are inherited from the sec2 VFD, and will
     * likely be deleted.
     */
    int     mpi_rank; /* useful MPI information           */
    int     mpi_size;
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
    char     filename[H5FD_MAX_FILENAME_LEN];
    MPI_Info info;
    MPI_Comm comm;

    /* Information from properties set by 'h5repart' tool
     *
     * Whether to eliminate the family driver info and convert this file to
     * a single file.
     */
    hbool_t fam_to_single;
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

#define H5FD_IOC_DEBUG_OP_CALLS 0 /* debugging print toggle; 0 disables */

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
extern herr_t  H5Pset_fapl_sec2(hid_t fapl_id);
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

static herr_t H5FD__subfiling_read_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                          haddr_t addrs[], size_t sizes[], void *bufs[] /* out */);
static herr_t H5FD__subfiling_write_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                           haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */);

static herr_t H5FD__subfiling_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);

static herr_t H5FD__subfiling_lock(H5FD_t *_file, hbool_t rw);
static herr_t H5FD__subfiling_unlock(H5FD_t *_file);
static herr_t H5FD__subfiling_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags,
                                  const void H5_ATTR_UNUSED *input, void **output);

static herr_t H5FD__subfiling_validate_config(const H5FD_subfiling_config_t *fa);

#if 0 /* JRM */ /* delete if all goes well */
static int H5FD__subfiling_mpi_rank(const H5FD_t *_file);
static int H5FD__subfiling_mpi_size(const H5FD_t *_file);
static MPI_Comm H5FD__subfiling_communicator(const H5FD_t *_file);
#endif          /* JRM */
#if 0 /* JRM */ /* unused?  delete if so */
static herr_t H5FD__subfiling_get_info(H5FD_t *_file, void **mpi_info);
#endif          /* JRM */

static const H5FD_class_t H5FD_subfiling_g = {
    H5FD_SUBFILING_VALUE,            /* value                */
    "subfiling",                     /* name                 */
    MAXADDR,                         /* maxaddr              */
    H5F_CLOSE_WEAK,                  /* fc_degree            */
    H5FD__subfiling_term,            /* terminate            */
    NULL,                            /* sb_size              */
    NULL,                            /* sb_encode            */
    NULL,                            /* sb_decode            */
    sizeof(H5FD_subfiling_config_t), /* fapl_size            */
    H5FD__subfiling_fapl_get,        /* fapl_get             */
    H5FD__subfiling_fapl_copy,       /* fapl_copy            */
    H5FD__subfiling_fapl_free,       /* fapl_free            */
    0,                               /* dxpl_size            */
    NULL,                            /* dxpl_copy            */
    NULL,                            /* dxpl_free            */
    H5FD__subfiling_open,            /* open                 */
    H5FD__subfiling_close,           /* close                */
    H5FD__subfiling_cmp,             /* cmp                  */
    H5FD__subfiling_query,           /* query                */
    NULL,                            /* get_type_map         */
    NULL,                            /* alloc                */
    NULL,                            /* free                 */
    H5FD__subfiling_get_eoa,         /* get_eoa              */
    H5FD__subfiling_set_eoa,         /* set_eoa              */
    H5FD__subfiling_get_eof,         /* get_eof              */
    H5FD__subfiling_get_handle,      /* get_handle           */
    H5FD__subfiling_read,            /* read                 */
    H5FD__subfiling_write,           /* write                */
    H5FD__subfiling_read_vector,     /* read_vector          */
    H5FD__subfiling_write_vector,    /* write_vector         */
    NULL,                            /* read_selection       */
    NULL,                            /* write_selection      */
    NULL,                            /* flush                */
    H5FD__subfiling_truncate,        /* truncate             */
    H5FD__subfiling_lock,            /* lock                 */
    H5FD__subfiling_unlock,          /* unlock               */
    NULL,                            /* del                  */
    H5FD__subfiling_ctl,             /* ctl                  */
    H5FD_FLMAP_DICHOTOMY             /* fl_map               */
};

/* Declare a free list to manage the H5FD_subfiling_t struct */
H5FL_DEFINE_STATIC(H5FD_subfiling_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD__init_package
 *
 * Purpose:     Initializes any interface-specific data or routines.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__init_package(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if (H5FD_subfiling_init() < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to initialize subfiling VFD")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__init_package() */

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

#if 1 /* JRM */
    if (H5I_VFL != H5I_get_type(H5FD_SUBFILING_g))
        H5FD_SUBFILING_g = H5FD_register(&H5FD_subfiling_g, sizeof(H5FD_class_t), FALSE);
#else  /* JRM */
    if (H5I_VFL != H5I_get_type(H5FD_SUBFILING_g)) {
        HDfprintf(stdout, "H5FD_subfiling_init(): calling H5FD_register()\n");
        H5FD_SUBFILING_g = H5FD_register(&H5FD_subfiling_g, sizeof(H5FD_class_t), FALSE);
    }
#endif /* JRM */

#if 0  /* JRM */
    HDfprintf(stdout, "H5FD_subfiling_init() subfiling registered.  id = %lld \n", (int64_t)H5FD_SUBFILING_g);
#endif /* JRM */

    /* Set return value */
    ret_value = H5FD_SUBFILING_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_init() */

/*---------------------------------------------------------------------------
 * Function:    H5FD_subfiling_term
 *
 * Purpose:     Shut down the VFD
 *
 * Returns:     SUCCEED (Can't fail)
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

#if 0  /* JRM */
    HDfprintf(stdout, "Entering H5FD__subfiling_term().\n");
#endif /* JRM */

    /* Reset VFL ID */
    H5FD_SUBFILING_g = 0;

#if 0  /* JRM */
    HDfprintf(stdout, "Exiting H5FD__subfiling_term().\n");
#endif /* JRM */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_subfiling_term() */

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

static herr_t
fapl__get_subfiling_defaults(H5FD_subfiling_config_t *fa)
{
    herr_t ret_value = SUCCEED;
    char * envValue  = NULL;

    HDassert(fa);

    fa->common.magic         = H5FD_SUBFILING_FAPL_T_MAGIC;
    fa->common.version       = H5FD_CURR_SUBFILING_FAPL_T_VERSION;
    fa->common.ioc_fapl_id   = H5P_DEFAULT;
    fa->common.stripe_count  = 0;
    fa->common.stripe_depth  = H5FD_DEFAULT_STRIPE_DEPTH;
    fa->common.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    /* VFD specific */
    fa->require_ioc = TRUE;

    if ((envValue = getenv("H5_REQUIRE_IOC")) != NULL) {
        int value_check = atoi(envValue);
        if (value_check == 0) {
            fa->require_ioc = FALSE;
        }
        else if (value_check > 0) {
            fa->require_ioc = TRUE;
        }
    }
    return (ret_value);
}

/*-------------------------------------------------------------------------
 *
 * Function:    H5Pset_fapl_subfiling
 *
 * Purpose:     Modify the file access property list to use the
 *              H5FD_SUBFILING driver defined in this source file.  All
 *              driver specific properties are passed in as a pointer to
 *              a suitably initialized instance of H5FD_subfiling_config_t
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
H5Pset_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *fa)
{
    H5P_genplist_t *        plist    = NULL; /* Property list pointer */
    hid_t                   ioc_fapl = H5I_INVALID_HID;
    H5FD_ioc_config_t       ioc_config;
    H5FD_subfiling_config_t subfiling_conf;
    herr_t                  ret_value = FAIL;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, fa);

    if (NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    if (fa == NULL) {
        /* Create IOC fapl */
        ioc_fapl = H5Pcreate(H5P_FILE_ACCESS);
        if (H5I_INVALID_HID == ioc_fapl)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't create ioc fapl")

        /* Get subfiling VFD defaults */
        if (fapl__get_subfiling_defaults(&subfiling_conf) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get subfiling fapl")

        if (subfiling_conf.require_ioc) {
            /* Get IOC VFD defaults */
            if (H5Pget_fapl_ioc(ioc_fapl, &ioc_config) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get ioc fapl")

            /* Now we can set the IOC fapl. */
            if (H5Pset_fapl_ioc(ioc_fapl, &ioc_config) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set ioc fapl")
        }
        else {
            if (H5Pset_fapl_sec2(ioc_fapl) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set sec2 fapl")
        }

        /* Assign the IOC fapl as the underlying VPD */
        subfiling_conf.common.ioc_fapl_id = ioc_fapl;

        fa = &subfiling_conf;
    }

    if (FAIL == H5FD__subfiling_validate_config(fa)) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid subfiling config")
    }

    ret_value = H5P_set_driver(plist, H5FD_SUBFILING, (void *)fa, NULL);

done:
    FUNC_LEAVE_API(ret_value)

} /* end H5Pset_fapl_subfiling() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_validate_config()
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
 * Programmer:  Jacob Smith
 *              9/10/17
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_validate_config(const H5FD_subfiling_config_t *fa)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(fa != NULL);

    if (fa->common.version != H5FD_CURR_SUBFILING_FAPL_T_VERSION) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown H5FD_subfiling_config_t version");
    }

    /* add subfiling configuration validation code here */

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_subfiling_validate_config() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_subfiling
 *
 * Purpose:     Returns information about the subfiling file access
 *              property list though the function arguments.
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              9/10/17
 * Modifications:
 *              Richard Warren
 *              If the fapl has yet to be set, we return an instance
 *              with default values for most fields.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *config_out)
{
    const H5FD_subfiling_config_t *config_ptr = NULL;
    H5P_genplist_t *               plist      = NULL;
    herr_t                         ret_value  = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, config_out);

    if (config_out == NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL")
    }

    plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS);
    if (plist == NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
    }

    config_ptr = (const H5FD_subfiling_config_t *)H5P_peek_driver_info(plist);
    if (config_ptr == NULL) {
        ret_value = fapl__get_subfiling_defaults(config_out);
    }
    else {
        /* Copy the subfiling fapl data out */
        HDmemcpy(config_out, config_ptr, sizeof(H5FD_subfiling_config_t));

        /* Copy the driver info value */
        if (H5FD__copy_plist(config_ptr->common.ioc_fapl_id, &(config_out->common.ioc_fapl_id)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't copy IOC FAPL");
    }

done:
    FUNC_LEAVE_API(ret_value)

} /* end H5Pget_fapl_subfiling() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_fapl_get
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

    FUNC_ENTER_NOAPI_NOINIT

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

} /* end H5FD_subfiling_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_fapl_copy
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

    FUNC_ENTER_NOAPI_NOINIT

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

} /* end H5FD_subfiling_fapl_copy() */

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

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(fa != NULL); /* sanity check */

    H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5FD_subfiling_fapl_free() */

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
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__subfiling_open(const char *name, unsigned flags, hid_t subfiling_fapl_id, haddr_t maxaddr)
{
    H5FD_subfiling_t *             file_ptr   = NULL; /* Subfiling VFD info */
    const H5FD_subfiling_config_t *config_ptr = NULL; /* Driver-specific property list */
    H5FD_class_t *                 driver     = NULL; /* VFD for file */
    H5P_genplist_t *               plist_ptr  = NULL;
    H5FD_driver_prop_t             driver_prop; /* Property for driver ID & info */

#if 0  /* JRM */
  hbool_t err_occurred = FALSE;
  uint64_t h5_file_id = (uint64_t)-1;
#endif /* JRM */
    H5FD_t *ret_value = NULL;
#if 0  /* JRM */
  hid_t fapl_check;
  hid_t ioc_fapl_id;
#endif /* JRM */

    FUNC_ENTER_STATIC

    /* Check arguments */
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    if (ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr")

    file_ptr = (H5FD_subfiling_t *)H5FL_CALLOC(H5FD_subfiling_t);
    if (NULL == file_ptr)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate file struct")

    /* Get the driver-specific file access properties */
    plist_ptr = (H5P_genplist_t *)H5I_object(subfiling_fapl_id);
    if (NULL == plist_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    config_ptr = (const H5FD_subfiling_config_t *)H5P_peek_driver_info(plist_ptr);
    if (NULL == config_ptr)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "unable to get VFL driver info")

    memcpy(&file_ptr->fa, config_ptr, sizeof(config_common_t));

    /* Copy the FAPL from the config structure */
    /* JRM:  Why is this necessary?  If it is necessary, must close the property list on file close. */
    if (H5FD__copy_plist(config_ptr->common.ioc_fapl_id, &(file_ptr->fa.common.ioc_fapl_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy IOC FAPL");

    file_ptr->sf_file = H5FD_open(name, flags, config_ptr->common.ioc_fapl_id, HADDR_UNDEF);
    if (!file_ptr->sf_file)
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "unable to open IOC file")

    /* Check the "native" driver (sec2 or mpio) */
    plist_ptr = (H5P_genplist_t *)H5I_object(config_ptr->common.ioc_fapl_id);

    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver ID & info")
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid driver ID in file access property list")

    if (strncmp(driver->name, "ioc", 3) == 0) {
        /* We've already opened the subfiles... */
        H5FD_subfiling_t *ioc_file = (H5FD_subfiling_t *)(file_ptr->sf_file);
        /* Get a copy of the context ID for later use */
        file_ptr->fa.common.context_id = ioc_file->fa.common.context_id;
        file_ptr->fa.require_ioc       = true;
    }
    else if (strncmp(driver->name, "sec2", 4) == 0) {
        uint64_t inode_id = (uint64_t)-1;
        int      mpi_rank, mpi_size;
        int      ioc_flags = O_RDWR;

        /* Translate the HDF5 file open flags into standard POSIX open flags */
        if (flags & H5F_ACC_TRUNC)
            ioc_flags |= O_TRUNC;
        if (flags & H5F_ACC_CREAT)
            ioc_flags |= O_CREAT;

        /* Get some basic MPI information */
        MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
        MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

        /* Let MPI rank 0 to the file stat operation and broadcast a result */
        if (mpi_rank == 0) {
            if (file_ptr->sf_file) {
                H5FD_sec2_t *hdf_file = (H5FD_sec2_t *)file_ptr->sf_file;
                h5_stat_t    sb;
                /* We create a new file descriptor for our file structure.
                 * Basically, we want these separate so that sec2 can
                 * deal with the opened file for additional operations
                 * (especially close) without interfering with subfiling.
                 */
                file_ptr->fd = HDdup(hdf_file->fd);
                if (HDfstat(hdf_file->fd, &sb) < 0)
                    HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file")
                inode_id = sb.st_ino;
            }
        }

        if (MPI_SUCCESS == MPI_Bcast(&inode_id, 1, MPI_UNSIGNED_LONG_LONG, 0, MPI_COMM_WORLD)) {
            file_ptr->inode = inode_id;
        }

        /* All ranks can now detect an error and fail. */
        if (inode_id == (uint64_t)-1)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file = %s\n", name)

        /* See: H5FDsubfile_int.c:
         * Note that the user defined HDF5 file is also considered subfile(0) */
        if (H5FD__open_subfiles((void *)&file_ptr->fa, inode_id, ioc_flags) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiling files = %s\n", name)
    }
    else {
        HDputs("We only support ioc and sec2 file opens at the moment.");
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file = %s\n", name)
    }
    ret_value = (H5FD_t *)file_ptr;

done:
    if (NULL == ret_value) {
        if (file_ptr) {
            if (H5I_INVALID_HID != file_ptr->fa.common.ioc_fapl_id)
                H5I_dec_ref(file_ptr->fa.common.ioc_fapl_id);
            if (file_ptr->sf_file)
                H5FD_close(file_ptr->sf_file);
            H5FL_FREE(H5FD_subfiling_t, file_ptr);
        }
    } /* end if error */

    // return ret_value;
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
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_close(H5FD_t *_file)
{
    H5FD_subfiling_t *   file_ptr   = (H5FD_subfiling_t *)_file;
    herr_t               ret_value  = SUCCEED; /* Return value */
    subfiling_context_t *sf_context = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(file_ptr);

    sf_context = (subfiling_context_t *)get__subfiling_object(file_ptr->fa.common.context_id);

#ifdef VERBOSE
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
    if ((H5I_INVALID_HID != file_ptr->fa.common.ioc_fapl_id) &&
        (H5I_dec_ref(file_ptr->fa.common.ioc_fapl_id) < 0))
        HGOTO_ERROR(H5E_VFL, H5E_ARGS, FAIL, "can't close ioc FAPL")

    /* Release the file info */
    file_ptr = H5FL_FREE(H5FD_subfiling_t, file_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_cmp
 *
 * Purpose:     Compares two files belonging to this driver using an
 *              arbitrary (but consistent) ordering.
 *
 * Return:      Success:    A value like strcmp()
 *              Failure:    never fails (arguments were checked by the
 *                          caller).
 *
 * Programmer:  Richard Warren
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD__subfiling_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_subfiling_t *f1        = (const H5FD_subfiling_t *)_f1;
    const H5FD_subfiling_t *f2        = (const H5FD_subfiling_t *)_f2;
    int                     ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

#ifdef H5_HAVE_WIN32_API
    if (f1->dwVolumeSerialNumber < f2->dwVolumeSerialNumber)
        HGOTO_DONE(-1)
    if (f1->dwVolumeSerialNumber > f2->dwVolumeSerialNumber)
        HGOTO_DONE(1)

    if (f1->nFileIndexHigh < f2->nFileIndexHigh)
        HGOTO_DONE(-1)
    if (f1->nFileIndexHigh > f2->nFileIndexHigh)
        HGOTO_DONE(1)

    if (f1->nFileIndexLow < f2->nFileIndexLow)
        HGOTO_DONE(-1)
    if (f1->nFileIndexLow > f2->nFileIndexLow)
        HGOTO_DONE(1)
#else /* H5_HAVE_WIN32_API */
#ifdef H5_DEV_T_IS_SCALAR
    if (f1->device < f2->device)
        HGOTO_DONE(-1)
    if (f1->device > f2->device)
        HGOTO_DONE(1)
#else  /* H5_DEV_T_IS_SCALAR */
    /* If dev_t isn't a scalar value on this system, just use memcmp to
     * determine if the values are the same or not.  The actual return value
     * shouldn't really matter...
     */
    if (HDmemcmp(&(f1->device), &(f2->device), sizeof(dev_t)) < 0)
        HGOTO_DONE(-1)
    if (HDmemcmp(&(f1->device), &(f2->device), sizeof(dev_t)) > 0)
        HGOTO_DONE(1)
#endif /* H5_DEV_T_IS_SCALAR */
    if (f1->inode < f2->inode)
        HGOTO_DONE(-1)
    if (f1->inode > f2->inode)
        HGOTO_DONE(1)
#endif /* H5_HAVE_WIN32_API */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_cmp() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_query
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
#if 0 /* JRM */ /* original version -- delete if all goes well */ 
static herr_t
H5FD__subfiling_query(const H5FD_t *_file, unsigned long *flags /* out */)
{
    const H5FD_subfiling_t *file = (const H5FD_subfiling_t *)_file; /* subfiling VFD info */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    /* Notice: the Mirror VFD Writer currently uses only the Sec2 driver as
     * the underying driver -- as such, the Mirror VFD implementation copies
     * these feature flags as its own. Any modifications made here must be
     * reflected in H5FDmirror.c
     * -- JOS 2020-01-13
     */
    if (flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata
                                                    allocations  */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA; /* OK to accumulate metadata for
                                                    faster writes */
        *flags |= H5FD_FEAT_DATA_SIEVE;          /* OK to perform data sieving for faster raw
                                                    data reads & writes    */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data
                                                    allocations */
        *flags |= H5FD_FEAT_POSIX_COMPAT_HANDLE; /* get_handle callback returns a
                                                    POSIX file descriptor */
        *flags |= H5FD_FEAT_SUPPORTS_SWMR_IO;    /* VFD supports the
                                                    single-writer/multiple-readers
                                                    (SWMR) pattern   */
        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE;
        /* Check for flags that are set by h5repart */
        if (file && file->fam_to_single)
            *flags |= H5FD_FEAT_IGNORE_DRVRINFO; /* Ignore the driver info when file
                                                    is opened (which eliminates it) */
    }                                            /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_subfiling_query() */
#else /* JRM */ /* new version copied from MPIO VFD */

static herr_t
H5FD__subfiling_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_STATIC_NOERR

    /* Set the VFL feature flags that this driver supports */
    if (flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations  */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
        //*flags |= H5FD_FEAT_HAS_MPI;                 /* This driver uses MPI */
        *flags |= H5FD_FEAT_ALLOCATE_EARLY; /* Allocate space early instead of late                        */
        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE; /* VFD creates a file which can be opened with the default
                                                       VFD */
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__mpio_query() */

#endif /* JRM */ /* new version copied from MPIO VFD */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_get_eoa
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

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(file->eoa)
} /* end H5FD_subfiling_get_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_set_eoa
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

    FUNC_ENTER_NOAPI_NOINIT_NOERR
    file_ptr->eoa = addr;

    H5FD_set_eoa(file_ptr->sf_file, type, addr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_subfiling_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_get_eof
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
 *              well as a stripe_size.  The combiniation of these two
 *              variables determines the "address" (a combination of IOC
 *              and a file offset) of any storage operation.
 *
 *              Having a defined storage layout, the virtual file EOF
 *              calculation should be the MAXIMUM value returned by the
 *              collection of IOCs.  Every MPI rank which hosts an IOC
 *              maintains it's own EOF by updating that value for each
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
    H5FD_subfiling_t *file      = (const H5FD_subfiling_t *)_file;
    haddr_t           ret_value = HADDR_UNDEF;
    haddr_t           local_eof, global_eof = 0;
    FUNC_ENTER_STATIC

    local_eof = H5FD_get_eof(file->sf_file, type);
    if (MPI_SUCCESS != MPI_Allreduce(&local_eof, &global_eof, 1, MPI_LONG_LONG, MPI_MAX, MPI_COMM_WORLD))
        HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, HADDR_UNDEF, "mpi_allreduce failed")
    /* Return the global max of all the subfile EOF values */

    ret_value = global_eof;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_get_eof() */

/*-------------------------------------------------------------------------
 * Function:       H5FD_subfiling_get_handle
 *
 * Purpose:        Returns the file handle of subfiling file driver.
 *
 * Returns:        SUCCEED/FAIL
 *
 * Programmer:     Raymond Lu
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void **file_handle)
{
    H5FD_subfiling_t *file      = (H5FD_subfiling_t *)_file;
    herr_t            ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if (!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    *file_handle = &(file->fd);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_get_handle() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_read
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
H5FD__subfiling_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id,
                     haddr_t addr, size_t size, void *buf /*out*/)
{
    H5FD_subfiling_t *   file_ptr     = (H5FD_subfiling_t *)_file;
    herr_t               ret_value    = SUCCEED; /* Return value */
    hbool_t              addrs_cooked = FALSE;
    subfiling_context_t *sf_context   = NULL;
    int                  ioc_total, count;
    int64_t              blocksize;
    HDoff_t              offset;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file_ptr && file_ptr->pub.cls);
    HDassert(buf);

    sf_context = (subfiling_context_t *)get__subfiling_object(file_ptr->fa.common.context_id);

    HDassert(sf_context);
    HDassert(sf_context->topology);

    /* Given the current IO and the IO concentrator info
     * we can determine some IO transaction parameters.
     * In particular, for large IO operations, each IOC
     * may require multiple IOs to fulfill the user IO
     * request. The 'max_depth' variable and number of
     * IOCs are used to size the vectors that will be
     * used to invoke the underlying IO operations.
     */
    ioc_total = sf_context->topology->n_io_concentrators;
#ifdef VERBOSE
    printf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
    fflush(stdout);
#endif

    if (ioc_total > 1) {
        size_t max_depth;
        blocksize = sf_context->sf_blocksize_per_stripe;
#if 0  /* JRM */
    size_t max_depth = (size_t)(size / blocksize) + 2;
#else  /* JRM */
        max_depth = (size / (size_t)blocksize) + 2;
#endif /* JRM */
        int next, ioc_count = 0, ioc_start = -1;

        int64_t source_data_offset[ioc_total][max_depth], sf_data_size[ioc_total][max_depth],
            sf_offset[ioc_total][max_depth];

        size_t varsize = sizeof(sf_offset);

        memset(source_data_offset, 0, varsize);
        memset(sf_data_size, 0, varsize);
        memset(sf_offset, 0, varsize);

        /* Check for overflow conditions */
        if (!H5F_addr_defined(addr))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
        if (REGION_OVERFLOW(addr, size))
            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu",
                        (unsigned long long)addr, (unsigned long long)size)

        addr += _file->base_addr;

        /* Follow the example of read_vector (see H5FDint.c) */
        addrs_cooked = TRUE;

        offset = (HDoff_t)addr;

        /* Given the number of io concentrators, we allocate vectors (one per-ioc)
         * to contain the translation of the IO request into a collection of io
         * requests. The translation is accomplished in the init__indep_io function.
         */

        /* Get the potential set of ioc transactions, i.e. data sizes,
         * offsets, and datatypes.  These can all be used by either the
         * underlying IOC or by sec2.
         *
         * For now, we assume we're dealing with contiguous datasets.
         * Vector IO will probably handle the non-contiguous condition
         */
        count = init__indep_io(sf_context, /* We use the context to look up config info */
#if 0                                      /* JRM */
        max_depth, ioc_total, source_data_offset, /* (out) Memory offset */
        sf_data_size, /* (out) Length of this contiguous block */
        sf_offset,    /* (out) File offset */
#else                                      /* JRM */
                               max_depth, ioc_total, (int64_t *)source_data_offset, /* (out) Memory offset */
                               (int64_t *)sf_data_size, /* (out) Length of this contiguous block */
                               (int64_t *)sf_offset,    /* (out) File offset */
#endif                                     /* JRM */
                               &ioc_start, /* (out) IOC index corresponding to starting offset */
                               &ioc_count, /* (out) number of actual IOCs used */
                               offset,     /* (in)  Starting file offset */
#if 0                                      /* JRM */
        size,         /* (in)  IO size */
#else                                      /* JRM */
                               (int64_t)size,           /* (in)  IO size */
#endif                                     /* JRM */
                               1);         /* (in)  data extent of the 'type' assumes byte */

        if (count > 0) {
            int i, k;

            /* Set ASYNC MODE:
            H5FD_class_aio_t *async_file_ptr = (H5FD_class_aio_t *)file_ptr->sf_file;
            uint64_t op_code_begin = xxx;
            uint64_t op_code_complete = zzz;
            const void *input = NULL;
            void *output = NULL;
            (*async_file_ptr->h5fdctl)(file_ptr->sf_file, op_code_begin, flags, input,
            &output);
             */

#if 0
      printf("[%s] addr=%ld, size=%ld, depth=%d, ioc_count=%d, ioc_start=%d\n", 
             __func__, offset, size, count, ioc_count, ioc_start);
      fflush(stdout);
#endif

            /* The 'count' variable captures the max number of IO requests to a single
             * IOC whereas the ioc_count is the number of IOC requests per outer loop
             * (i) and also represents the vector length being used in the call to
             * H5FDread_vector.
             */

            for (i = 0; i < count; i++) {
                H5FD_mem_t type_in[ioc_count];
                int64_t    data_size[ioc_count];
                int64_t    offset_in[ioc_count];
                void *     data_in[ioc_count];
                char *     databuf = (char *)buf;
#if 0  /* JRM */
        int vectorlen = ioc_count;
#else  /* JRM */
                uint32_t vectorlen = (uint32_t)ioc_count;
#endif /* JRM */

                /*
                 * Fill vector variables 'data_in' and 'type_in'
                 */
                for (next = ioc_start, k = 0; k < ioc_count; k++) {
                    offset_in[k] = sf_offset[next][i];
                    type_in[k]   = type;
                    data_in[k]   = databuf + source_data_offset[next][i];
                    if ((data_size[k] = sf_data_size[next][i]) == 0) {
                        vectorlen--;
                    }
                    next = (next + 1) % ioc_count;
                }

                /* And make the read_vector call.  Under normal circumstances this
                 * should invoke H5FD__ioc_read_vector() (see H5FDioc.c)
                 */
#if 0
        for (k=0; k < vectorlen; k++) {
			printf("%s (%d): v_len=%d, offset=%ld, data_size=%ld\n",
                   __func__, k,vectorlen, offset_in[k], data_size[k]);
            fflush(stdout);
        }
#endif
#if 1 /* JRM */
                if (H5FDread_vector(file_ptr->sf_file, dxpl_id, vectorlen, type_in, (uint64_t *)offset_in,
                                    (uint64_t *)data_size, data_in) < 0) {
#else  /* JRM */
                if (H5FDread_vector(file_ptr->sf_file, dxpl_id, vectorlen, type_in, offset_in, data_size,
                                    data_in) < 0) {
#endif /* JRM */
                    HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "IOC file write failed")
                }
            }

            /*
             (*async_file_ptr->h5fdctl)(file_ptr->sf_file, op_code_complete, flags, input, &output);
             */
        }
    }
    else { /* NO STRIPING:: Just a single IOC */

        /* Check for overflow conditions */
        if (!H5F_addr_defined(addr))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
        if (REGION_OVERFLOW(addr, size))
            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu",
                        (unsigned long long)addr, (unsigned long long)size)

        addr += _file->base_addr;

        /* Follow the example of read_vector (see H5FDint.c) */
        addrs_cooked = TRUE;

        offset = (HDoff_t)addr;
#if 0 /* JRM */
    if (H5FDread_vector(file_ptr->sf_file, dxpl_id, 1, &type, &offset, &size,
                         &buf) < 0) {
#else /* JRM */
        if (H5FDread_vector(file_ptr->sf_file, dxpl_id, 1, &type, &addr, &size, &buf) < 0) {

#endif /* JRM */
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "IOC file write failed")
    }
}

addr += (haddr_t)size; /* Point to the end of the current IO */

if (addrs_cooked)
    addr -= _file->base_addr;

/* Update current position and eof */
file_ptr->pos = addr;
file_ptr->op  = OP_READ;
if (file_ptr->pos > file_ptr->eof)
    file_ptr->eof = file_ptr->pos;

done : if (ret_value < 0)
{
    /* Reset last file I/O information */
    file_ptr->pos = HADDR_UNDEF;
    file_ptr->op  = OP_UNKNOWN;
} /* end if */

FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_read() */

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
H5FD__subfiling_write(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id,
                      haddr_t addr, size_t size, const void *buf /*in*/)
{
    H5FD_subfiling_t *   file_ptr     = (H5FD_subfiling_t *)_file;
    herr_t               ret_value    = SUCCEED; /* Return value */
    hbool_t              addrs_cooked = FALSE;
    subfiling_context_t *sf_context   = NULL;
    int                  ioc_total, count;
    int64_t              blocksize;
    HDoff_t              offset;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file_ptr && file_ptr->pub.cls);
    HDassert(buf);

    sf_context = (subfiling_context_t *)get__subfiling_object(file_ptr->fa.common.context_id);
    HDassert(sf_context);
    HDassert(sf_context->topology);

    /* Given the current IO and the IO concentrator info
     * we can determine some IO transaction parameters.
     * In particular, for large IO operations, each IOC
     * may require multiple IOs to fulfill the user IO
     * request. The 'max_depth' variable and number of
     * IOCs are used to size the vectors that will be
     * used to invoke the underlying IO operations.
     */
    ioc_total = sf_context->topology->n_io_concentrators;

#ifdef VERBOSE
    if (sf_context->topology->rank_is_ioc)
        printf("[%s %d] fd=%d\n", __func__, file_ptr->mpi_rank, sf_context->sf_fid);
    else
        printf("[%s %d] fd=*\n", __func__, file_ptr->mpi_rank);
    fflush(stdout);
#endif

    if (ioc_total > 1) {
        size_t max_depth;
        blocksize = sf_context->sf_blocksize_per_stripe;
#if 0  /* JRM */
        size_t max_depth = (size_t)(size / blocksize) + 2;
#else  /* JRM */
        max_depth = (size_t)(size / (size_t)blocksize) + 2;
#endif /* JRM */
        int next, ioc_count = 0, ioc_start = -1;

        int64_t source_data_offset[ioc_total][max_depth], sf_data_size[ioc_total][max_depth],
            sf_offset[ioc_total][max_depth];

        size_t varsize = sizeof(sf_offset);

        memset(source_data_offset, 0, varsize);
        memset(sf_data_size, 0, varsize);
        memset(sf_offset, 0, varsize);

        /* Check for overflow conditions */
        if (!H5F_addr_defined(addr))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
        if (REGION_OVERFLOW(addr, size))
            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu",
                        (unsigned long long)addr, (unsigned long long)size)

        addr += _file->base_addr;

#ifdef VERBOSE
        printf("[%s %d] addr=%ld, size=%ld\n", __func__, file_ptr->mpi_rank, addr, size);
        fflush(stdout);
#endif

        /* Follow the example of read_vector (see H5FDint.c) */
        addrs_cooked = TRUE;

        offset = (HDoff_t)addr;

        /* Given the number of io concentrators, we allocate vectors (one per-ioc)
         * to contain the translation of the IO request into a collection of io
         * requests. The translation is accomplished in the init__indep_io function.
         */

        /* Get the potential set of ioc transactions, i.e. data sizes,
         * offsets, and datatypes.  These can all be used by either the
         * underlying IOC or by sec2.
         *
         * For now, we assume we're dealing with contiguous datasets.
         * Vector IO will probably handle the non-contiguous condition
         */
#if 0  /* JRM */
    count = init__indep_io(
        sf_context, /* We use the context to look up config info */
        max_depth, ioc_total, source_data_offset, /* (out) Memory offset */
        sf_data_size, /* (out) Length of this contiguous block */
        sf_offset,    /* (out) File offset */
        &ioc_start,       /* (out) IOC index corresponding to starting offset */
        &ioc_count,       /* (out) number of actual IOCs used */
        offset,       /* (in)  Starting file offset */
        size,         /* (in)  IO size */
        1);           /* (in)  data extent of the 'type' assumes byte */
#else  /* JRM */
        count = init__indep_io(sf_context, /* We use the context to look up config info */
                               max_depth, ioc_total, (int64_t *)source_data_offset, /* (out) Memory offset */
                               (int64_t *)sf_data_size, /* (out) Length of this contiguous block */
                               (int64_t *)sf_offset,    /* (out) File offset */
                               &ioc_start,              /* (out) IOC index corresponding to starting offset */
                               &ioc_count,              /* (out) number of actual IOCs used */
                               offset,                  /* (in)  Starting file offset */
                               (int64_t)size,           /* (in)  IO size */
                               1);                      /* (in)  data extent of the 'type' assumes byte */
#endif /* JRM */

        next = ioc_start;
        if (count > 0) {
            int i, k;

            /* Set ASYNC MODE:
            H5FD_class_aio_t *async_file_ptr = (H5FD_class_aio_t *)file_ptr->sf_file;
            uint64_t op_code_begin = xxx;
            uint64_t op_code_complete = zzz;
            const void *input = NULL;
            void *output = NULL;
            (*async_file_ptr->h5fdctl)(file_ptr->sf_file, op_code_begin, flags, input,
            &output);
             */

#if 0
      printf("[%s] addr=%ld, size=%ld, depth=%d, ioc_count=%d, ioc_start=%d\n", 
             __func__, offset, size, count, ioc_count, ioc_start);
      fflush(stdout);
#endif
            /* The 'count' variable captures the max number of IO requests to a single
             * IOC whereas the ioc_count is the number of IOC requests per outer loop
             * (i) and also represents the vector length being used in the call to
             * H5FDwrite_vector.
             */

            for (i = 0; i < count; i++) {
                H5FD_mem_t type_in[ioc_count];
                int64_t    data_size[ioc_count];
                int64_t    offset_in[ioc_count];
#if 0  /* JRM */
        void *data_in[ioc_count];
#else  /* JRM */
                const void *data_in[ioc_count];
#endif /* JRM */
                const char *databuf = buf;
#if 0  /* JRM */
        int vectorlen = ioc_count;
#else  /* JRM */
                uint32_t vectorlen = (uint32_t)ioc_count;
#endif /* JRM */

                /*
                 * Fill vector variables 'data_in' and 'type_in'
                 */
                for (next = ioc_start, k = 0; k < ioc_count; k++) {
                    offset_in[k] = sf_offset[next][i];
                    type_in[k]   = type;
                    data_in[k]   = databuf + source_data_offset[next][i];
                    if ((data_size[k] = sf_data_size[next][i]) == 0) {
                        vectorlen--;
                    }
                    next++;
                    if (next == ioc_total)
                        next = 0;
                }

                /* And make the write_vector call.  Under normal circumstances this
                 * should invoke H5FD__ioc_write_vector() (see H5FDioc.c)
                 */
#if 0
        for (k=0; k < vectorlen; k++) {
			printf("%s (%d): v_len=%d, offset=%ld, data_size=%ld\n",
                   __func__, k, vectorlen, offset_in[k], data_size[k]);
            fflush(stdout);
        }
#endif
#if 0  /* JRM */
        if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, vectorlen, type_in,
                             offset_in, data_size, data_in) < 0) {
          HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "IOC file write failed")
        }
      }
#else  /* JRM */

                if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, vectorlen, type_in, (uint64_t *)offset_in,
                                     (uint64_t *)data_size, data_in) < 0) {
                    HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "IOC file write failed")
                }
            }
#endif /* JRM */

                /*
            (*async_file_ptr->h5fdctl)(file_ptr->sf_file, op_code_complete, flags, input,
            &output);
                 */
            }
        }
        else { /* NO STRIPING:: Just a single IOC */

            /* Check for overflow conditions */
            if (!H5F_addr_defined(addr))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu",
                            (unsigned long long)addr)
            if (REGION_OVERFLOW(addr, size))
                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu",
                            (unsigned long long)addr, (unsigned long long)size)

            addr += _file->base_addr;

            /* Follow the example of read_vector (see H5FDint.c) */
            addrs_cooked = TRUE;

            offset = (HDoff_t)addr;
#if 0  /* JRM */
    if (H5FDwrite_vector(file_ptr->sf_file, dxpl_id, 1, &type, &offset, &size,
                         &buf) < 0) {
#else  /* JRM */
        if (H5FD_write_vector(file_ptr->sf_file, 1, &type, &addr, &size, &buf) < 0) {
#endif /* JRM */
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "IOC file write failed")
        }
    }

    addr += (haddr_t)size; /* Point to the end of the current IO */

    if (addrs_cooked)
        addr -= _file->base_addr;

    /* Update current position and eof */
    file_ptr->pos = addr;
    file_ptr->op  = OP_WRITE;
    if (file_ptr->pos > file_ptr->eof)
        file_ptr->eof = file_ptr->pos;

done:
    if (ret_value < 0) {
        /* Reset last file I/O information */
        file_ptr->pos = HADDR_UNDEF;
        file_ptr->op  = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_write() */

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
     * These have already been checked in H5FD_subfiling_read_vector (see below)!
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

    /* Get the default dataset transfer property list if the user didn't provide
     * one */
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
     * These have already been checked in H5FD_subfiling_write_vector (see below)!
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

    /* Get the default dataset transfer property list if the user didn't provide
     * one */
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
 * Function:    H5FD_subfiling_truncate
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

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    /* Extend the file to make sure it's large enough */
    if (!H5F_addr_eq(file->eoa, file->eof)) {

        /* Update the eof value */
        file->eof = file->eoa;

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op  = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_truncate() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_lock
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

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    if (file->fa.require_ioc)
        puts("Subfiling driver doesn't support file locking");
    else {
        if (H5FD_lock(file->sf_file, rw) < 0)
            HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to lock file")
    } /* end if */
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_lock() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_unlock
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

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    if (H5FD_unlock(file->sf_file) < 0)
        HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to lock file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_unlock() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__get_file_ino
 *
 * Purpose:     Given a filename input, we HDstat the file to retrieve
 *              the inode value.  The was principally used for the VOL
 *              implementation of subfiling
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD__get_file_ino(const char *name, uint64_t *st_ino)
{
    herr_t    ret_value = SUCCEED; /* Return value */
    h5_stat_t sb;

    FUNC_ENTER_PACKAGE

    if (HDstat(name, &sb) < 0)
        HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to fstat file")

    *st_ino = sb.st_ino;

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__get_file_ino() */

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

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(file);
    HDassert(H5FD_SUBFILING == file->pub.driver_id);

    switch (op_code) {

        case H5FD_CTL__GET_MPI_COMMUNICATOR_OPCODE:
            HDassert(output);
#if 0 /* JRM */  /* remove eventually */
            if (*output == NULL) {
                HDfprintf(stdout,
                          "H5FD__subfiling_ctl:H5FD_CTL__GET_MPI_COMMUNICATOR_OPCODE: *output is NULL\n");
            }
#endif /* JRM */ /* remove eventually */
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

static herr_t
create__simple_vector(hid_t H5_ATTR_UNUSED file_space_id, void *memDataBuf, haddr_t addrBase,
                      hssize_t elements, size_t type_extent, hssize_t *vlen, haddr_t **_offsets,
                      hssize_t **_blocklens, void ***_bufs)
{
    haddr_t * offsets   = *_offsets;
    hssize_t *blocklens = *_blocklens;
    void **   bufs      = *_bufs;
    void *    nextBuf   = memDataBuf;

    assert(vlen);
    assert(_offsets);
    assert(_blocklens);
    assert(_bufs);

    if (*vlen < 0) {
        offsets = (haddr_t *)malloc((sizeof(haddr_t)));
        assert(offsets);

        blocklens = (hssize_t *)malloc((sizeof(hssize_t)));
        assert(blocklens);

        bufs = (void **)malloc((sizeof(void **)));
        assert(bufs);

        bufs[0]      = nextBuf;
        offsets[0]   = addrBase;
        blocklens[0] = (hssize_t)((hssize_t)elements * (hssize_t)type_extent);

        if (*vlen < 0) {
            *_offsets   = offsets;
            *_blocklens = blocklens;
            *_bufs      = bufs;
        }
        *vlen = 1;
        return 0;
    }
    return -1;
}

static herr_t
create__vector_from_hyperslab(hid_t file_space_id, void *memDataBuf, haddr_t addrBase, size_t type_extent,
                              hssize_t *vlen, haddr_t **_offsets, hsize_t **_blocklens, void ***_bufs)
{
    herr_t   ret_value = SUCCEED;
    hssize_t k, n_blocks = H5Sget_select_hyper_nblocks(file_space_id);

    // USE THIS (when we get around to using calling here).
    // htri_t check = H5Sget_regular_hyperslab(file_space_id,)
    char *nextBuf = memDataBuf;

    hsize_t stride[H5S_MAX_RANK];
    hsize_t count[H5S_MAX_RANK];

    hsize_t *strides = stride;
    hsize_t *counts  = count;

    haddr_t *offsets   = *_offsets;
    hsize_t *blocklens = *_blocklens;
    void **  bufs      = *_bufs;

    assert(vlen);
    assert(_offsets);
    assert(_blocklens);
    assert(_bufs);
    assert(n_blocks > 0);

    if (n_blocks > H5S_MAX_RANK) {
        /* Allocate a temp for the H5Sget_regular_hyperslab function call */
        if ((strides = (hsize_t *)malloc((size_t)n_blocks * sizeof(hsize_t))) == NULL) {
            perror("unable to allocate storage for vector creation");
            return -1;
        }
        if ((counts = (hsize_t *)malloc((size_t)n_blocks * sizeof(hsize_t))) == NULL) {
            perror("unable to allocate storage for vector creation");
            return -1;
        }
    }

    /* Allocate storage for the vector elements */
    if (*vlen < n_blocks) {
        if (offsets) {
            offsets = (haddr_t *)realloc(offsets, ((size_t)n_blocks * sizeof(haddr_t)));
        }
        else {
            offsets = (haddr_t *)malloc(((size_t)n_blocks * sizeof(haddr_t)));
        }
        assert(offsets);
        if (blocklens) {
            blocklens = (hsize_t *)realloc(blocklens, ((size_t)n_blocks * sizeof(hsize_t)));
        }
        else {
            blocklens = (hsize_t *)malloc(((size_t)n_blocks * sizeof(hsize_t)));
        }
        assert(blocklens);
        if (bufs) {
            bufs = (void **)realloc(bufs, ((size_t)n_blocks * sizeof(void **)));
        }
        else {
            bufs = (void **)malloc(((size_t)n_blocks * sizeof(void **)));
        }
        assert(bufs);
        *vlen = n_blocks;
    }
    /* Fill vector elements */
    if ((ret_value =
             H5Sget_regular_hyperslab(file_space_id, (hsize_t *)offsets, strides, counts, blocklens)) < 0) {
        puts("H5Sget_regular_hyperslab failed");
        return -1;
    }

    for (k = 0; k < n_blocks; k++) {
        bufs[k] = nextBuf;
        offsets[k] *= type_extent;
        offsets[k] += addrBase;
        blocklens[k] *= type_extent;
        nextBuf += (strides[k] * type_extent);
    }
    if (strides != stride)
        free(strides);
    if (counts != count)
        free(counts);

    *_offsets   = offsets;
    *_blocklens = blocklens;
    *_bufs      = bufs;

    return ret_value;
}

static herr_t
check__dims(int ndims, hsize_t *mem_dims, hsize_t *file_dims, int *diff_index)
{
    int    i;
    herr_t ret_value = SUCCEED;
    for (i = 0; i < ndims; i++) {
        if (mem_dims[i] != file_dims[i]) {
            *diff_index = i;
            return 0;
        }
    }
    /* ndims +1 == no differences */
    *diff_index = i;
    return ret_value;
}

#ifdef UNUSED
static haddr_t
get__data_offset(int mpi_rank, int mpi_size, size_t dtype_extent, const H5S_t *mem_space,
                 const H5S_t *file_space)
{
    haddr_t this_base = 0;
    return this_base;
}
#endif

static haddr_t
get__base_offset(int mpi_rank, int mpi_size, size_t dtype_extent, hid_t mem_space_id, hid_t file_space_id)
{
    haddr_t this_base = 0;
    int     n_dims;
    int     is_simple = H5Sis_simple(file_space_id);
    /* The 'is_simple' variable is actually a tri value type:
     *  -1 == failed
     *   0 == NOT_SIMPLE
     *   1 == SIMPLE
     */
    if (is_simple > 0) {
        n_dims = H5Sget_simple_extent_ndims(mem_space_id);
        if (n_dims > 0) {
            hsize_t mem_stride[n_dims];
            hsize_t mem_dims[n_dims];
            hsize_t file_stride[n_dims];
            hsize_t file_dims[n_dims];

            if (H5Sget_simple_extent_dims(mem_space_id, mem_dims, mem_stride) < 0)
                puts("H5Sget_simple_extent_dims returned an error");
            if (H5Sget_simple_extent_dims(file_space_id, file_dims, file_stride) < 0)
                puts("H5Sget_simple_extent_dims returned an error");

            if (n_dims == 1) {
                if (mpi_rank == (mpi_size - 1))
                    this_base = (file_dims[0] - mem_dims[0]) * dtype_extent;
                else
                    this_base = (mem_dims[0] * dtype_extent * (hsize_t)mpi_rank);
            }
            else {
                int diff_index = -1;
                if (check__dims(n_dims, mem_dims, file_dims, &diff_index) < 0)
                    puts("check_dims returned an error");
                else { /* CHECK-THIS!  What is the correct way?
                        * if the diff_index isn't 0, then we probably need
                        * to do the multiplication of the dimensions...
                        */
                    this_base = (mem_dims[diff_index] * (hsize_t)mpi_rank);
                }
            }
        }
    }

    return this_base;
}

herr_t
H5FD__dataset_write_contiguous(hid_t H5_ATTR_UNUSED h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
                               int mpi_rank, int mpi_size, void H5_ATTR_UNUSED *_dset,
                               hid_t H5_ATTR_UNUSED mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                               hid_t H5_ATTR_UNUSED plist_id, const void *buf)
{
    herr_t       ret_value     = SUCCEED; /* Return value */
    hssize_t     num_elem_file = (hssize_t)-1, num_elem_mem = (hssize_t)-1;
    hssize_t     s_dtype_extent = (hssize_t)dtype_extent;
    H5S_sel_type sel_type;
    hssize_t     sf_vlen = -1;

    const H5S_t *mem_space;
    const H5S_t *file_space;

    FUNC_ENTER_PACKAGE

    if ((num_elem_file = H5Sget_select_npoints(file_space_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't get number of points in file selection")

    if ((num_elem_mem = H5Sget_select_npoints(mem_space_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't get number of points in memory selection")

    if (num_elem_file != num_elem_mem)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                    "number of elements selected"
                    " in file and memory dataspaces is different")

    if (H5S_get_validated_dataspace(mem_space_id, &mem_space) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not get a validated dataspace from mem_space_id")

    if (H5S_get_validated_dataspace(file_space_id, &file_space) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not get a validated dataspace from file_space_id")

    if (num_elem_file > 0) {
        sel_type = H5Sget_select_type(file_space_id);
        switch (sel_type) {
            case H5S_SEL_NONE:
                printf("[%d] H5S_SEL_NONE\n", mpi_rank);
                break;
            case H5S_SEL_POINTS: {
                haddr_t rank_baseAddr;
                rank_baseAddr =
                    get__base_offset(mpi_rank, mpi_size, dtype_extent, mem_space_id, file_space_id);
                rank_baseAddr += dataset_baseAddr;
                printf("[%d] H5S_SEL_POINTS - num_elem_file: %lld: UNSUPPORTED (for now)\n", mpi_rank,
                       num_elem_file);
                ret_value = -1;
                goto done;

                break;
            }
            case H5S_SEL_HYPERSLABS: {
                int     status;
                haddr_t rank_baseAddr;
                rank_baseAddr =
                    get__base_offset(mpi_rank, mpi_size, dtype_extent, mem_space_id, file_space_id);
                rank_baseAddr += dataset_baseAddr;

                if ((status = H5Sis_regular_hyperslab(file_space_id)) < 0) {
                    puts("H5Sis_regular_hyperslab returned an error");
                    ret_value = -1;
                    goto done;
                }
                if (status > 0) {
                    if (sf_offsets == NULL)
                        sf_offsets = (haddr_t *)malloc(sizeof(haddr_t));
                    if (sf_sizes == NULL)
                        sf_sizes = (hssize_t *)malloc(sizeof(hssize_t *));
                    if (sf_bufs == NULL)
                        sf_bufs = (void **)malloc(sizeof(void *));
                    sf_vlen = 1;
                    assert(sf_offsets);
                    assert(sf_sizes);
                    assert(sf_bufs);

                    sf_offsets[0] = rank_baseAddr;
                    sf_sizes[0]   = num_elem_mem * s_dtype_extent;
                    sf_bufs[0]    = buf;
                }
                break;
            }
            case H5S_SEL_ALL: {
                int     status;
                haddr_t rank_baseAddr;
                rank_baseAddr =
                    get__base_offset(mpi_rank, mpi_size, dtype_extent, mem_space_id, file_space_id);
                rank_baseAddr += dataset_baseAddr;
                if (num_elem_mem > 0) {
                    status = H5Sis_simple(file_space_id);
                    if (status > 0) {
                        if (create__simple_vector(file_space_id, buf, rank_baseAddr, num_elem_mem,
                                                  dtype_extent, &sf_vlen, &sf_offsets, &sf_sizes,
                                                  &sf_bufs) < 0) {
                            puts("Unable to create simple vectors");
                            goto done;
                        }
                    }
                }
                break;
            }
            default:
                printf("[%d] UNSUPPORTED selection type\n", mpi_rank);
                ret_value = -1;
        } /* END switch (sel_type) */

    } /* if (num_elem_file > 0) */

done:

    FUNC_LEAVE_NOAPI(ret_value)
}

herr_t
H5FD__dataset_read_contiguous(hid_t H5_ATTR_UNUSED h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
                              int mpi_rank, int mpi_size, void H5_ATTR_UNUSED *_dset,
                              hid_t H5_ATTR_UNUSED mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                              hid_t H5_ATTR_UNUSED plist_id, void *buf)
{
    herr_t       ret_value     = SUCCEED; /* Return value */
    hssize_t     num_elem_file = -1, num_elem_mem = -1;
    H5S_sel_type sel_type;
    hssize_t     sf_vlen = -1;
    int          status  = 0;

    FUNC_ENTER_PACKAGE
    if ((num_elem_file = H5Sget_select_npoints(file_space_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't get number of points in file selection")
    if ((num_elem_mem = H5Sget_select_npoints(mem_space_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't get number of points in memory selection")

    if (num_elem_file != num_elem_mem)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                    "number of elements selected"
                    " in file and memory dataspaces is different")

    if (num_elem_file > 0) {
        sel_type = H5Sget_select_type(file_space_id);
        switch (sel_type) {
            case H5S_SEL_NONE:
                // printf("[%d] H5S_SEL_NONE\n", mpi_rank);
                break;
            case H5S_SEL_POINTS: {
                haddr_t rank_baseAddr;
                rank_baseAddr =
                    get__base_offset(mpi_rank, mpi_size, dtype_extent, mem_space_id, file_space_id);
                rank_baseAddr += dataset_baseAddr;
                // printf("[%d] H5S_SEL_POINTS - num_elem_file: %lld: UNSUPPORTED (for
                // now)\n", mpi_rank, num_elem_file);
                ret_value = -1;
                goto done;

                break;
            }
            case H5S_SEL_HYPERSLABS: {
                haddr_t      rank_baseAddr;
                const H5S_t *mem_space;
                const H5S_t *file_space;
                rank_baseAddr =
                    get__base_offset(mpi_rank, mpi_size, dtype_extent, mem_space_id, file_space_id);
                rank_baseAddr += dataset_baseAddr;
                if (H5S_get_validated_dataspace(mem_space_id, &mem_space) < 0) {
                    puts("could not get a validated dataspace from mem_space_id");
                }
                if (H5S_get_validated_dataspace(file_space_id, &file_space) < 0) {
                    puts("could not get a validated dataspace from file_space_id");
                }

                if ((status = H5Sis_regular_hyperslab(file_space_id)) < 0) {
                    puts("H5Sis_regular_hyperslab returned an error");
                    ret_value = -1;
                    goto done;
                }
                if (status > 0) {
                    if (sf_offsets == NULL)
                        sf_offsets = (haddr_t *)malloc(sizeof(haddr_t));
                    if (sf_sizes == NULL)
                        sf_sizes = (hssize_t *)malloc(sizeof(hsize_t));
                    if (sf_bufs == NULL)
                        sf_bufs = (void **)malloc(sizeof(void *));
                    sf_vlen = 1;
                    assert(sf_offsets);
                    assert(sf_sizes);
                    assert(sf_bufs);

                    sf_offsets[0] = rank_baseAddr;
                    sf_sizes[0]   = (hssize_t)((hssize_t)num_elem_mem * (hssize_t)dtype_extent);
                    sf_bufs[0]    = buf;
                }
                break;
            }
            case H5S_SEL_ALL: {
                haddr_t rank_baseAddr;
                rank_baseAddr =
                    get__base_offset(mpi_rank, mpi_size, dtype_extent, mem_space_id, file_space_id);
                rank_baseAddr += dataset_baseAddr;
                if (num_elem_mem > 0) {
                    status = H5Sis_simple(file_space_id);
                    if (status > 0) {
                        if (create__simple_vector(file_space_id, buf, rank_baseAddr, num_elem_mem,
                                                  dtype_extent, &sf_vlen, &sf_offsets, &sf_sizes,
                                                  &sf_bufs) < 0) {
                            puts("Unable to create simple vectors");
                            goto done;
                        }
                    }
                }
                break;
            }
            default:
                printf("[%d] UNSUPPORTED selection type\n", mpi_rank);
                ret_value = -1;
        } /* END switch (sel_type) */

    } /* if (num_elem_file > 0) */

done:

    FUNC_LEAVE_NOAPI(ret_value)
}

#if 0 /* JRM */ /* delete if all goes well */
static int H5FD__subfiling_mpi_rank(const H5FD_t *_file) {
  const H5FD_subfiling_t *file = (const H5FD_subfiling_t *)_file;

  FUNC_ENTER_STATIC_NOERR

  /* Sanity checks */
  HDassert(file);

  FUNC_LEAVE_NOAPI(file->mpi_rank)
} /* end H5FD__subfiling_mpi_rank() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_mpi_size
 *
 * Purpose:     Returns the number of MPI processes
 *
 * Return:      Success: non-negative
 *              Failure: negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, May 16, 2002
 *
 *-------------------------------------------------------------------------
 */
static int H5FD__subfiling_mpi_size(const H5FD_t *_file) {
  const H5FD_subfiling_t *file = (const H5FD_subfiling_t *)_file;

  FUNC_ENTER_STATIC_NOERR

  /* Sanity checks */
  HDassert(file);

  FUNC_LEAVE_NOAPI(file->mpi_size)
} /* end H5FD__subfiling_mpi_size() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_communicator
 *
 * Purpose:     Returns the MPI communicator for the file.
 *
 * Return:      Success:    The communicator
 *              Failure:    Can't fail
 *
 * Programmer:  Richard Warren
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm H5FD__subfiling_communicator(const H5FD_t *_file) {
  const H5FD_subfiling_t *file = (const H5FD_subfiling_t *)_file;

  FUNC_ENTER_STATIC_NOERR

  /* Sanity checks */
  HDassert(file);

  FUNC_LEAVE_NOAPI(file->comm)
} /* end H5FD__subfiling_communicator() */

#endif /* JRM */ /* delete if all goes well */

#if 0 /* JRM */ /* unused?? delete if so */
/*-------------------------------------------------------------------------
 * Function:       H5FD_subfiling_get_info
 *
 * Purpose:        Returns the file info of SUBFILING file driver.
 *
 * Returns:        Non-negative if succeed or negative if fails.
 *
 * Programmer:     John Mainzer
 *                 April 4, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t H5FD__subfiling_get_info(H5FD_t *_file, void **mpi_info) {
  H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;
  herr_t ret_value = SUCCEED;

  FUNC_ENTER_STATIC

  if (!mpi_info)
    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mpi info not valid")

  *mpi_info = &(file->info);

done:
  FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__subfiling_get_info() */

#endif /* JRM */

void
manage_client_logfile(int H5_ATTR_UNUSED client_rank, int H5_ATTR_UNUSED flag_value)
{
#ifndef NDEBUG
    if (flag_value) {
        char logname[64];
        sprintf(logname, "sf_client_%d.log", client_rank);
        client_log = fopen(logname, "a+");
    }
    else if (client_log) {
        fclose(client_log);
        client_log = 0;
    }
#endif
    return;
}
