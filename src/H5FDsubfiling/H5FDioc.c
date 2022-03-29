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

#include "H5FDpublic.h"  /* Basic H5FD definitions   */
#include "H5Eprivate.h"  /* Error handling           */
#include "H5FDprivate.h" /* File drivers             */
#include "H5FDioc.h"     /* IOC file driver          */
#include "H5FLprivate.h" /* Free Lists               */
#include "H5Fprivate.h"  /* File access              */
#include "H5Iprivate.h"  /* IDs                      */
#include "H5MMprivate.h" /* Memory management        */
#include "H5Pprivate.h"  /* Property lists           */
#include "H5private.h"   /* Generic Functions        */

#if 1 /* JRM */ /* For now, H5FDsubfiling_priv.h needs mercury.  Since the code that needs it will           \
                 * move to its own header, just hack it for now.                                             \
                 */
#include "mercury_thread.h"
#include "mercury_thread_mutex.h"
#include "mercury_thread_pool.h"
#endif /* JRM */

#include "H5FDsubfiling_priv.h"

/* The driver identification number, initialized at runtime */
static hid_t H5FD_IOC_g = 0;
#if 0 /* JRM */ /* delete if all goes well */
extern volatile int sf_shutdown_flag;
#endif          /* JRM */

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

#if H5FD_IOC_DEBUG_OP_CALLS
#define H5FD_IOC_LOG_CALL(name)                                                                              \
    do {                                                                                                     \
        HDprintf("called %s()\n", (name));                                                                   \
        HDfflush(stdout);                                                                                    \
    } while (0)
#else
#define H5FD_IOC_LOG_CALL(name) /* no-op */
#endif                          /* H5FD_IOC_DEBUG_OP_CALLS */

/* Public functions which are referenced but not found in this file */
extern herr_t H5FD__write_vector_internal(hid_t h5_fid, hssize_t count, haddr_t addrs[], size_t sizes[],
                                          const void *bufs[] /* data_in */);
extern herr_t H5FD__read_vector_internal(hid_t h5_fid, hssize_t count, haddr_t addrs[], size_t sizes[],
                                         void *bufs[] /* data_out */);
extern int    H5FD__close_subfiles(int64_t context_id);
extern int    H5FD__open_subfiles(void *_config_info, uint64_t h5_file_id, int flags);
extern hid_t  fid_map_to_context(hid_t sf_fid);
extern subfiling_context_t *get__subfiling_object(int64_t context_id);

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
/*
static herr_t H5FD__ioc_ctl(H5FD_t *file, uint64_t op_code, uint64_t flags,
                            const void *input, void **result);
*/

static const H5FD_class_t H5FD_ioc_g = {
    H5FD_CLASS_VERSION,        /* VFD interface version */
    H5FD_IOC_VALUE,            /* value                 */
    "ioc",                     /* name                  */
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
    NULL,                      /* del                   */
    NULL,                      /* ctl                   */
    H5FD_FLMAP_DICHOTOMY       /* fl_map                */
};

/* Declare a free list to manage the H5FD_ioc_t struct */
H5FL_DEFINE_STATIC(H5FD_ioc_t);

/* Declare a free list to manage the H5FD_ioc_fapl_t struct */
H5FL_DEFINE_STATIC(H5FD_ioc_config_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD__init_package
 *
 * Purpose:     Initializes any interface-specific data or routines.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__init_package(void)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER_NOAPI(FAIL)

    H5FD_IOC_LOG_CALL(FUNC);

#if 1 /* JRM */
    if (H5I_VFL != H5I_get_type(H5FD_IOC_g))
        H5FD_IOC_g = H5FD_register(&H5FD_ioc_g, sizeof(H5FD_class_t), FALSE);
#else  /* JRM */
    if (H5I_VFL != H5I_get_type(H5FD_IOC_g)) {
        HDfprintf(stdout, "H5FD_ioc_init(): calling H5FD_register()\n");
        H5FD_IOC_g = H5FD_register(&H5FD_ioc_g, sizeof(H5FD_class_t), FALSE);
    }
#endif /* JRM */

#if 0  /* JRM */
  HDfprintf(stdout, "H5FD_ioc_init() IOC registered.  id = %lld \n", (int64_t)H5FD_IOC_g);
#endif /* JRM */

    if (H5I_INVALID_HID == H5FD_IOC_g)
        HGOTO_ERROR(H5E_ID, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register file driver ID")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__init_package() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_ioc_init
 *
 * Purpose:     Initialize the ioc driver by registering it with the
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

    FUNC_ENTER_NOAPI(FAIL)

    H5FD_IOC_LOG_CALL(FUNC);

    if (H5I_VFL != H5I_get_type(H5FD_IOC_g))
        H5FD_IOC_g = H5FDregister(&H5FD_ioc_g);

    ret_value = H5FD_IOC_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_ioc_init() */

#if 0 /* JRM */ /* delete if all goes well */
/*-------------------------------------------------------------------------
 * Function:    H5FD_ioc_set_shutdown_flag
 *
 * Purpose:     IO Concentrator threads are told to terminate their service
 *              loop and exit by setting 'shutdown_flag' to a non-zero
 *              value.
 *
 * Return:      None
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_ioc_set_shutdown_flag(int flag)
{
    sf_shutdown_flag = flag;
    if (H5FD_IOC_g > 0)
        usleep(100);
    return;
} /* end H5FD_ioc_set_shutdown_flag() */
#endif          /* JRM */

/*---------------------------------------------------------------------------
 * Function:    H5FD__ioc_term
 *
 * Purpose:     Shut down the ioc VFD.
 *
 * Returns:     SUCCEED (Can't fail)
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR
    // FUNC_ENTER_STATIC_NOERR

#if 0  /* JRM */
    HDfprintf(stdout, "Entering H5FD__ioc_term().\n");
#endif /* JRM */

    H5FD_IOC_LOG_CALL(FUNC);

    /* Reset VFL ID */
    H5FD_IOC_g = 0;

#if 0  /* JRM */
    HDfprintf(stdout, "Exiting H5FD__ioc_term().\n");
#endif /* JRM */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__ioc_term() */

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

    H5FD_IOC_LOG_CALL(FUNC);

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
    H5FD_ioc_config_t *info      = NULL;
    H5P_genplist_t *   plist_ptr = NULL;
    herr_t             ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, vfd_config);

    H5FD_IOC_LOG_CALL(FUNC);

    if (H5FD_IOC_FAPL_T_MAGIC != vfd_config->common.magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid configuration (magic number mismatch)")
    if (H5FD_CURR_IOC_FAPL_T_VERSION != vfd_config->common.version)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid config (version number mismatch)")
    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a valid property list")

    info = H5FL_CALLOC(H5FD_ioc_config_t);
    if (NULL == info)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to allocate file access property list struct")

    memcpy(info, vfd_config, sizeof(H5FD_ioc_config_t));
    info->common.ioc_fapl_id = fapl_id;
    ret_value                = H5P_set_driver(plist_ptr, H5FD_IOC, info, NULL);

done:
    if (info)
        info = H5FL_FREE(H5FD_ioc_config_t, info);

    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_ioc() */

/*-------------------------------------------------------------------------
 * Function:    fapl_get_ioc_defaults
 *
 * Purpose:     This is called by H5Pget_fapl_ioc when called with no
 *              established configuration info.  This simply fills in
 *              in the basics.   This avoids the necessity of having
 *              the user write code to initialize the config structure.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
fapl_get_ioc_defaults(H5FD_ioc_config_t *fa)
{
    herr_t ret_value = SUCCEED;

    fa->common.magic         = H5FD_IOC_FAPL_T_MAGIC;
    fa->common.version       = H5FD_CURR_IOC_FAPL_T_VERSION;
    fa->common.ioc_fapl_id   = H5P_DEFAULT;
    fa->common.stripe_count  = 0;
    fa->common.stripe_depth  = H5FD_DEFAULT_STRIPE_DEPTH;
    fa->common.ioc_selection = SELECT_IOC_ONE_PER_NODE;

    /* Specific to this IO Concentrator */
    fa->thread_pool_count = H5FD_IOC_THREAD_POOL_SIZE;
    return (ret_value);
} /* end fapl_get_ioc_defaults() */

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
    const H5FD_ioc_config_t *config_ptr = NULL;
    H5P_genplist_t *         plist_ptr  = NULL;
    herr_t                   ret_value  = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, config_out);

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    if (config_out == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL")

    plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS);
    if (plist_ptr == NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
    }

    config_ptr = (const H5FD_ioc_config_t *)H5P_peek_driver_info(plist_ptr);
    if (config_ptr == NULL) {
        memset(config_out, 0, sizeof(H5FD_ioc_config_t));
        ret_value = fapl_get_ioc_defaults(config_out);
    }
    else {
        /* Copy the subfiling fapl data out */
        HDmemcpy(config_out, config_ptr, sizeof(H5FD_ioc_config_t));

        /* Copy the driver info value */
        if (H5FD__copy_plist(config_ptr->common.ioc_fapl_id, &(config_out->common.ioc_fapl_id)) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't copy IOC FAPL");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_ioc() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_flush
 *
 * Purpose:     Flushes all data to disk for both channels.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_flush(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t closing)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Public API for dxpl "context" */
    if (H5FDflush(file->ioc_file, dxpl_id, closing) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTFLUSH, FAIL, "unable to flush R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_flush() */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if (!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
    if (REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu", (unsigned long long)addr)

    /* Public API for dxpl "context" */
    if (H5FDread(file->ioc_file, type, dxpl_id, addr, size, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "Reading from R/W channel failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_write
 *
 * Purpose:     Writes SIZE bytes of data to R/W and W/O channels, beginning
 *              at address ADDR from buffer BUF according to data transfer
 *              properties in DXPL_ID.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_write(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t dxpl_id, haddr_t addr, size_t size,
                const void *buf)
{
    H5FD_ioc_t *    file_ptr  = (H5FD_ioc_t *)_file;
    H5P_genplist_t *plist_ptr = NULL;
    herr_t          ret_value = SUCCEED;
    hid_t           h5_fid;

    FUNC_ENTER_STATIC

    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    addr += _file->base_addr;
    h5_fid    = (hid_t)file_ptr->inode;
    ret_value = H5FD__write_vector_internal(h5_fid, 1, &addr, &size, &buf);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_write() */

static herr_t
H5FD__ioc_read_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                      size_t sizes[], void *bufs[] /* out */)
{
    H5FD_ioc_t *file_ptr  = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */
    hid_t       h5_fid;

    FUNC_ENTER_STATIC

    /* Check arguments */
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

    h5_fid    = (hid_t)file_ptr->inode;
    ret_value = H5FD__read_vector_internal(h5_fid, count, addrs, sizes, bufs);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

static herr_t
H5FD__ioc_write_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                       size_t sizes[], const void *bufs[] /* in */)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */
    hid_t       h5_fid;

    FUNC_ENTER_STATIC

    /* Check arguments */
    if (!file)
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
    h5_fid    = (hid_t)file->inode;
    ret_value = H5FD__write_vector_internal(h5_fid, count, addrs, sizes, bufs);

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FDioc__write_vector() */

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

    FUNC_ENTER_STATIC_NOERR

    H5FD_IOC_LOG_CALL(FUNC);

    ret_value = H5FD__ioc_fapl_copy(&(file->fa));

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_fapl_get() */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    HDassert(old_fa_ptr);

    new_fa_ptr = H5FL_CALLOC(H5FD_ioc_config_t);
    if (NULL == new_fa_ptr)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate log file FAPL")

    HDmemcpy(new_fa_ptr, old_fa_ptr, sizeof(H5FD_ioc_config_t));
    HDstrncpy(new_fa_ptr->common.file_path, old_fa_ptr->common.file_path, H5FD_IOC_PATH_MAX);

    /* Copy the FAPL */
    if (H5FD__copy_plist(old_fa_ptr->common.ioc_fapl_id, &(new_fa_ptr->common.ioc_fapl_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy the IOC FAPL");

    ret_value = (void *)new_fa_ptr;

done:
    if (NULL == ret_value)
        if (new_fa_ptr)
            new_fa_ptr = H5FL_FREE(H5FD_ioc_config_t, new_fa_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    HDassert(fapl);

    if (H5I_dec_ref(fapl->common.ioc_fapl_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDEC, FAIL, "can't close W/O FAPL ID")

    /* Free the property list */
    fapl = H5FL_FREE(H5FD_ioc_config_t, fapl);

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
H5FD__ioc_open(const char *name, unsigned flags, hid_t ioc_fapl_id, haddr_t maxaddr)
{
    H5FD_ioc_t *             file_ptr = NULL; /* Ioc VFD info */
    const H5FD_ioc_config_t *fapl_ptr = NULL; /* Driver-specific property list */
    H5FD_class_t *           driver   = NULL; /* VFD for file */
    H5FD_driver_prop_t       driver_prop;     /* Property for driver ID & info */
    H5P_genplist_t *         plist_ptr = NULL;
    H5FD_t *                 ret_value = NULL;
    int                      l_error = 0, g_error = 0, mpi_enabled = 0;
    int                      mpi_code; /* MPI return code */

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

#if 0 /* JRM */  /* delete this eventually */
    HDfprintf(stdout, "\n\nH5FD__ioc_open: entering.\n\n");
    HDfflush(stdout);
#endif /* JRM */ /* delete this eventually */

    /* Check arguments */
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    if (ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr")
    if ((H5P_FILE_ACCESS_DEFAULT == ioc_fapl_id) || (H5FD_IOC != H5Pget_driver(ioc_fapl_id)))
        /* presupposes that H5P_FILE_ACCESS_DEFAULT is not a ioc */
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "driver is not ioc")

    /* We should validate that the application has been initialized
     * with MPI_Init_thread and that the library supports
     * MPI_THREAD_MULTIPLE
     */
    if (MPI_Initialized(&mpi_enabled) == MPI_SUCCESS) {
        int mpi_provides = 0;
        MPI_Query_thread(&mpi_provides);
        if (mpi_provides != MPI_THREAD_MULTIPLE) {
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "Subfiling requires the use of MPI_THREAD_MULTIPLE")
        }
    }

    file_ptr = (H5FD_ioc_t *)H5FL_CALLOC(H5FD_ioc_t);
    if (NULL == file_ptr)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate file struct")

    /* Get some basic MPI information */
    MPI_Comm_size(MPI_COMM_WORLD, &file_ptr->mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &file_ptr->mpi_rank);

    /* Get the driver-specific file access properties */
    plist_ptr = (H5P_genplist_t *)H5I_object(ioc_fapl_id);
    if (NULL == plist_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")

    fapl_ptr = (const H5FD_ioc_config_t *)H5P_peek_driver_info(plist_ptr);
    if (NULL == fapl_ptr)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "unable to get VFL driver info")

    /* Fill in the file config values */
    memcpy(&file_ptr->fa, fapl_ptr, sizeof(H5FD_ioc_config_t));

    /* Extend the config info with file_path and file_dir */
    if (HDrealpath(name, file_ptr->fa.common.file_path) != NULL) {
        char *path      = HDstrdup(file_ptr->fa.common.file_path);
        char *directory = dirname(path);
        HDstrcpy(file_ptr->fa.common.file_dir, directory);
        HDfree(path);
    }

    /* Copy the ioc FAPL. */
    if (H5FD__copy_plist(fapl_ptr->common.ioc_fapl_id, &(file_ptr->fa.common.ioc_fapl_id)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't copy W/O FAPL");

    /* Check the "native" driver (sec2 or mpio) */
    plist_ptr = (H5P_genplist_t *)H5I_object(fapl_ptr->common.ioc_fapl_id);

    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get driver ID & info")
    if (NULL == (driver = (H5FD_class_t *)H5I_object(driver_prop.driver_id)))
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid driver ID in file access property list")

    if (strncmp(driver->name, "sec2", 4) == 0) {
        uint64_t inode_id  = (uint64_t)-1;
        int      ioc_flags = O_RDWR;

        /* Translate the HDF5 file open flags into standard POSIX open flags */
        if (flags & H5F_ACC_TRUNC)
            ioc_flags |= O_TRUNC;
        if (flags & H5F_ACC_CREAT)
            ioc_flags |= O_CREAT;

        /* sec2 open the file */
        file_ptr->ioc_file =
            H5FD_open(file_ptr->fa.common.file_path, flags, fapl_ptr->common.ioc_fapl_id, HADDR_UNDEF);
        if (file_ptr->ioc_file) {
            h5_stat_t    sb;
            H5FD_sec2_t *hdf_file = (H5FD_sec2_t *)file_ptr->ioc_file;
            if (HDfstat(hdf_file->fd, &sb) < 0)
                HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file")
            /* Get the inode info and copy the open file descriptor
             * The latter is used to pass to the subfiling code to use
             * as an alternative to opening a new subfiling file, e.g. nnn_0_of_N.h5
             */
            file_ptr->inode = inode_id = sb.st_ino;
        }
        else {
            /* The two-step file opening approach may be
             * the root cause for the sec2 open to return a NULL.
             * It is prudent then, to collectively fail (early) in this case.
             */
            l_error = 1;
        }
        MPI_Allreduce(&l_error, &g_error, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
        if (g_error) {
            if (file_ptr->ioc_file)
                H5FD_close(file_ptr->ioc_file);
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file = %s\n", name)
        }

        /* See: H5FDsubfile_int.c:  returns error count! */
        if (H5FD__open_subfiles((void *)&file_ptr->fa, inode_id, ioc_flags) > 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiling files = %s\n", name)

        else if (file_ptr->inode > 0) { /* No errors opening the subfiles */
            subfiling_context_t *sf_context = get__subfiling_object(file_ptr->fa.common.context_id);
            if (sf_context && sf_context->topology->rank_is_ioc) {
                if (initialize_ioc_threads(sf_context) < 0) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Unable to initialize IOC threads")
                }
            }
        }
    }
    else {
        HDputs("We only support sec2 file opens at the moment.");
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file = %s\n", name)
    }

    ret_value = (H5FD_t *)file_ptr;

done:
    if (NULL == ret_value) {
        if (file_ptr) {
            if (file_ptr->ioc_file)
                H5FD_close(file_ptr->ioc_file);
            H5FL_FREE(H5FD_ioc_t, file_ptr);
        }
    } /* end if error */
#if 1 /* JRM */
    /* run a barrier just before exit.  The objective is to
     * ensure that the IOCs are fully up and running before
     * we proceed.  Note that this barrier is not sufficient
     * by itself -- we also need code in initialize_ioc_threads()
     * to wait until the main IOC thread has finished its
     * initialization.
     */
    /* TODO: don't use MPI_COMM_WORLD here -- use communicator supplied in the open instead */
    /* Adendum:  Consider creating a copy of the supplied communicator for exclusing use by
     *           the VFD.  I can't say that this is necessary, but it is a plausible cause
     *           of the hangs observed with sub-filing.           -- JRM
     */

#if 0 /* JRM */  /* remove eventually */
    HDfprintf(stdout, "\nH5FD__ioc_open: entering terminal barrier.\n");
    HDfflush(stdout);
#endif /* JRM */ /* remove eventually */

    if ((mpi_code = MPI_Barrier(MPI_COMM_WORLD)) != MPI_SUCCESS) {
        HMPI_DONE_ERROR(NULL, "Barrier failed", mpi_code)
    }
#endif /* JRM */
#if 0  /* JRM */
    HDfprintf(stdout, "\n\nH5FD__ioc_open: exiting.\n\n");
    HDfflush(stdout);
#endif /* JRM */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_open() */

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
    // subfiling_context_t *sf_context = NULL;

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Sanity check */
    HDassert(file);
#ifdef VERBOSE
    sf_context = (subfiling_context_t *)get__subfiling_object(file->fa.common.context_id);
    if (sf_context->topology->rank_is_ioc)
        printf("[%s %d] fd=%d\n", __func__, file->mpi_rank, sf_context->sf_fid);
    else
        printf("[%s %d] fd=*\n", __func__, file->mpi_rank);
    fflush(stdout);
#endif

    if (H5I_dec_ref(file->fa.common.ioc_fapl_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_ARGS, FAIL, "can't close W/O FAPL")

    /* Call the sec2 close */
    if (file->ioc_file) {
        if (H5FD_close(file->ioc_file) == FAIL)
            HGOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close HDF5 file")
    }

    /* See: H5FDsubfile_int.c */
    if (H5FD__close_subfiles(file->fa.common.context_id) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close subfiling file(s)")

    /* dup'ed in the H5FD__ioc_open function (see above) */
    HDclose(file->hdf_fd_dup);
    /* Release the file info */
    file = H5FL_FREE(H5FD_ioc_t, file);
    file = NULL;

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_close() */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if ((ret_value = H5FD_get_eoa(file->ioc_file, type)) == HADDR_UNDEF)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, HADDR_UNDEF, "unable to get eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC)

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);
    HDassert(file->ioc_file);

    if (H5FD_set_eoa(file->ioc_file, type, addr) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "H5FDset_eoa failed for R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    sf_context = get__subfiling_object(file->fa.common.context_id);
    if (sf_context) {
        ret_value = (haddr_t)sf_context->sf_eof;
        goto done;
    }

    if (HADDR_UNDEF == (ret_value = H5FD_get_eof(file->ioc_file, type)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, HADDR_UNDEF, "unable to get eof")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_get_eof */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    HDassert(file);
    HDassert(file->ioc_file);
    HDassert(file->ioc_file);

    if (H5FDtruncate(file->ioc_file, dxpl_id, closing) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTUPDATE, FAIL, "unable to truncate R/W file")
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_truncate */

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

    FUNC_ENTER_STATIC_NOERR

    H5FD_IOC_LOG_CALL(FUNC);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if (file->ioc_file)
        ret_value = H5FD_sb_size(file->ioc_file);

    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if (file->ioc_file && H5FD_sb_encode(file->ioc_file, name, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTENCODE, FAIL, "unable to encode the superblock in R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Sanity check */
    HDassert(file);
    HDassert(file->ioc_file);

    if (H5FD_sb_load(file->ioc_file, name, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "unable to decode the superblock in R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_sb_decode */

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

    FUNC_ENTER_STATIC_NOERR

    H5FD_IOC_LOG_CALL(FUNC);

    HDassert(f1);
    HDassert(f2);

    ret_value = H5FD_cmp(f1->ioc_file, f2->ioc_file);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_cmp */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);
    HDassert(file_handle);

    if (H5FD_get_vfd_handle(file->ioc_file, file->fa.common.ioc_fapl_id, file_handle) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to get handle of R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_get_handle */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_lock
 *
 * Purpose:     Sets a file lock.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_lock(H5FD_t *_file, hbool_t H5_ATTR_UNUSED rw)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file; /* VFD file struct */
    herr_t      ret_value = SUCCEED;             /* Return value */

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    HDassert(file);
    HDassert(file->ioc_file);

#if 1
    if (HDflock(file->hdf_fd_dup, LOCK_SH) < 0) {
        perror("flock");
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOCKFILE, FAIL, "unable to lock R/W file")
    }
#else
    /* Place the lock on each file */
    if (H5FD_lock(file->ioc_file, rw) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOCKFILE, FAIL, "unable to lock R/W file")
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    HDassert(file);
#if 1
    if (HDflock(file->hdf_fd_dup, LOCK_UN) < 0) {
        perror("flock");
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOCKFILE, FAIL, "unable to lock R/W file")
    }
#else
    if (file->ioc_file != NULL)
        if (H5FD_unlock(file->ioc_file) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTUNLOCKFILE, FAIL, "unable to unlock W/O file")
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_unlock */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    if (file_ptr == NULL) {
        if (flags)
            *flags = 0;
    }
    else if (file_ptr->ioc_file) {
        if (H5FDquery(file_ptr->ioc_file, flags) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOCK, FAIL, "unable to query R/W file");
    }
    else {
        /* There is no file. Because this is a pure passthrough VFD,
         * it has no features of its own.
         */
        if (flags)
            *flags = 0;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_query() */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    /* Allocate memory for each file, only return the return value for R/W file.
     */
    if ((ret_value = H5FDalloc(file->ioc_file, type, dxpl_id, size)) == HADDR_UNDEF)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "unable to allocate for R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_alloc() */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    /* Retrieve memory type mapping for R/W channel only */
    if (H5FD_get_fs_type_map(file->ioc_file, type_map) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to allocate for R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_get_type_map() */

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

    FUNC_ENTER_STATIC

    H5FD_IOC_LOG_CALL(FUNC);

    /* Check arguments */
    HDassert(file);
    HDassert(file->ioc_file);

    if (H5FDfree(file->ioc_file, type, dxpl_id, addr, size) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free for R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__ioc_free() */

void
H5FD_ioc_wait_thread_main(void)
{
    return;
}

void
H5FD_ioc_finalize_threads(void)
{

    return;
}
