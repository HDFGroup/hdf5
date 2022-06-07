/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
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
 * Purpose:  VFD SWMR driver for the reader
 */

#include "H5FDdrvr_module.h" /* This source code file is part of the H5FD driver module */
#define H5F_FRIEND           /* Suppress error about including H5Fpkg            */

#include "H5Eprivate.h"      /* Error handling           */
#include "H5Fpkg.h"          /* Files                    */
#include "H5FDprivate.h"     /* File drivers             */
#include "H5FDvfd_swmr.h"    /* VFD SWMR file driver     */
#include "H5FLprivate.h"     /* Free Lists               */
#include "H5Iprivate.h"      /* IDs                      */
#include "H5MMprivate.h"     /* Memory management        */
#include "H5Pprivate.h"      /* Property lists           */
#include "H5retry_private.h" /* Retry loops.		     */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_VFD_SWMR_g = 0;

/* The description of a file belonging to this driver */
typedef struct H5FD_vfd_swmr_t {
    H5FD_t pub; /* public stuff, must be first      */

    H5FD_vfd_swmr_reader_fapl_t fa; /* driver-specific file access properties */

    /* HDF5 file */
    char    hdf5_filename[H5FD_MAX_FILENAME_LEN]; /* Name of the HDF5 file from open */
    H5FD_t *hdf5_file_lf;                         /* Driver info for the HDF5 file */

    /* Metadata file */
    int                     md_fd;             /* File descriptor for the metadata file */
    uint32_t                md_pages_reserved; /* # of pages reserved at the head of the metadata file */
    char                    md_file_path_name[H5FD_MAX_FILENAME_LEN + 1]; /* Name of the metadate file */
    H5FD_vfd_swmr_md_header md_header;                                    /* Metadata file header */
    H5FD_vfd_swmr_md_index  md_index;                                     /* Metadata file index */

    /* Histogram of ticks elapsed inside the API (reader only).
     * api_elapsed_ticks[elapsed] is the number of times
     * `elapsed' ticks passed during an API call in the
     * program's lifetime.
     */
    uint64_t *api_elapsed_ticks;    /* Array of histogram buckets */
    uint32_t  api_elapsed_nbuckets; /* Number of histogram buckets. */

    hbool_t               pb_configured; /* Sanity-checking flag set when page buffer is configured */
    H5F_vfd_swmr_config_t config;        /* VFD SWMR configuration */

    /*
     * Indicate whether we are in make_believe state or not
     */
    hbool_t make_believe;

} H5FD_vfd_swmr_t;

#define MAXADDR            (((haddr_t)1 << (8 * sizeof(HDoff_t) - 1)) - 1)
#define VFD_SWMR_MD_SUFFIX ".md"

/* Prototypes */
static herr_t  H5FD__vfd_swmr_term(void);
static void *  H5FD__vfd_swmr_fapl_get(H5FD_t *_file);
static void *  H5FD__vfd_swmr_fapl_copy(const void *_old_fa);
static herr_t  H5FD__vfd_swmr_fapl_free(void *_fapl);
static H5FD_t *H5FD__vfd_swmr_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t  H5FD__vfd_swmr_close(H5FD_t *_file);
static int     H5FD__vfd_swmr_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t  H5FD__vfd_swmr_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD__vfd_swmr_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD__vfd_swmr_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD__vfd_swmr_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD__vfd_swmr_get_handle(H5FD_t *_file, hid_t fapl, void **file_handle);
static herr_t  H5FD__vfd_swmr_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                                   void *buf);
static herr_t  H5FD__vfd_swmr_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr, size_t size,
                                    const void *buf);
static herr_t  H5FD__vfd_swmr_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t  H5FD__vfd_swmr_lock(H5FD_t *_file, hbool_t rw);
static herr_t  H5FD__vfd_swmr_unlock(H5FD_t *_file);
static herr_t  H5FD__vfd_swmr_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags, const void *input,
                                  void **output);

/* VFD SWMR */
static htri_t H5FD__vfd_swmr_header_deserialize(H5FD_vfd_swmr_t *, H5FD_vfd_swmr_md_header *);
static htri_t H5FD__vfd_swmr_index_deserialize(const H5FD_vfd_swmr_t *file, H5FD_vfd_swmr_md_index *md_index,
                                               const H5FD_vfd_swmr_md_header *md_header);
static herr_t H5FD__vfd_swmr_load_hdr_and_idx(H5FD_vfd_swmr_t *, hbool_t);

static const H5FD_class_t H5FD_vfd_swmr_g = {
    H5FD_VFD_SWMR_VALUE,                 /* value                */
    "vfd_swmr",                          /* name                 */
    MAXADDR,                             /* maxaddr              */
    H5F_CLOSE_WEAK,                      /* fc_degree            */
    H5FD__vfd_swmr_term,                 /* terminate            */
    NULL,                                /* sb_size              */
    NULL,                                /* sb_encode            */
    NULL,                                /* sb_decode            */
    sizeof(H5FD_vfd_swmr_reader_fapl_t), /* fapl_size            */
    H5FD__vfd_swmr_fapl_get,             /* fapl_get             */
    H5FD__vfd_swmr_fapl_copy,            /* fapl_copy            */
    H5FD__vfd_swmr_fapl_free,            /* fapl_free            */
    0,                                   /* dxpl_size            */
    NULL,                                /* dxpl_copy            */
    NULL,                                /* dxpl_free            */
    H5FD__vfd_swmr_open,                 /* open                 */
    H5FD__vfd_swmr_close,                /* close                */
    H5FD__vfd_swmr_cmp,                  /* cmp                  */
    H5FD__vfd_swmr_query,                /* query                */
    NULL,                                /* get_type_map         */
    NULL,                                /* alloc                */
    NULL,                                /* free                 */
    H5FD__vfd_swmr_get_eoa,              /* get_eoa              */
    H5FD__vfd_swmr_set_eoa,              /* set_eoa              */
    H5FD__vfd_swmr_get_eof,              /* get_eof              */
    H5FD__vfd_swmr_get_handle,           /* get_handle           */
    H5FD__vfd_swmr_read,                 /* read                 */
    H5FD__vfd_swmr_write,                /* write                */
    NULL,                                /* flush                */
    H5FD__vfd_swmr_truncate,             /* truncate             */
    H5FD__vfd_swmr_lock,                 /* lock                 */
    H5FD__vfd_swmr_unlock,               /* unlock               */
    NULL,                                /* del                  */
    H5FD__vfd_swmr_ctl,                  /* ctl                  */
    H5FD_FLMAP_DICHOTOMY                 /* fl_map               */
};

/* Declare a free list to manage the H5FD_vfd_swmr_t struct */
H5FL_DEFINE_STATIC(H5FD_vfd_swmr_t);

/* Declare a free list to manage the H5FD_vfd_swmr_reader_config_t struct */
H5FL_DEFINE_STATIC(H5FD_vfd_swmr_reader_fapl_t);

/* Declare a free list to manage the H5FD_vfd_swmr_idx_entry_t sequence information */
H5FL_SEQ_DEFINE(H5FD_vfd_swmr_idx_entry_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:    The driver ID for the VFD SWMR driver.
 *              Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_vfd_swmr_init(void)
{
    hid_t ret_value = H5I_INVALID_HID; /* Return value */

    FUNC_ENTER_NOAPI_NOERR

    if (H5I_VFL != H5I_get_type(H5FD_VFD_SWMR_g))
        H5FD_VFD_SWMR_g = H5FD_register(&H5FD_vfd_swmr_g, sizeof(H5FD_class_t), FALSE);

    /* Set return value */
    ret_value = H5FD_VFD_SWMR_g;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_init() */

/*---------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_term
 *
 * Purpose:     Shut down the VFD
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_term(void)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Reset VFL ID */
    H5FD_VFD_SWMR_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__vfd_swmr_term() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_fapl_get
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
H5FD__vfd_swmr_fapl_get(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file;
    void *           ret_value = NULL;

    FUNC_ENTER_PACKAGE_NOERR

    ret_value = H5FD__vfd_swmr_fapl_copy(&(file->fa));

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__vfd_swmr_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_fapl_copy
 *
 * Purpose:     Copies the file access properties.
 *
 * Return:      Success:    Pointer to a new property list info structure.
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__vfd_swmr_fapl_copy(const void *_old_fa)
{
    const H5FD_vfd_swmr_reader_fapl_t *old_fa_ptr = (const H5FD_vfd_swmr_reader_fapl_t *)_old_fa;
    H5FD_vfd_swmr_reader_fapl_t *      new_fa_ptr = NULL;
    void *                             ret_value  = NULL;

    FUNC_ENTER_PACKAGE

    HDassert(old_fa_ptr);
    HDassert(old_fa_ptr->magic == H5FD_VFD_SWMR_READER_MAGIC);

    new_fa_ptr = H5FL_CALLOC(H5FD_vfd_swmr_reader_fapl_t);
    if (NULL == new_fa_ptr)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate vfd swmr FAPL")

    H5MM_memcpy(new_fa_ptr, old_fa_ptr, sizeof(H5FD_vfd_swmr_reader_fapl_t));

    ret_value = (void *)new_fa_ptr;

done:
    if (NULL == ret_value) {
        if (new_fa_ptr) {
            new_fa_ptr->magic = 0;
            new_fa_ptr        = H5FL_FREE(H5FD_vfd_swmr_reader_fapl_t, new_fa_ptr);
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__vfd_swmr_fapl_copy() */

/*--------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_fapl_free
 *
 * Purpose:     Releases the file access lists
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_fapl_free(void *_fapl)
{
    H5FD_vfd_swmr_reader_fapl_t *fapl      = (H5FD_vfd_swmr_reader_fapl_t *)_fapl;
    herr_t                       ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    if ((NULL == fapl) || (fapl->magic != H5FD_VFD_SWMR_READER_MAGIC))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL or invalid H5FD_vfd_swmr_reader_fapl_t *")

    /* Free the property list */
    fapl->magic = 0;
    fapl        = H5FL_FREE(H5FD_vfd_swmr_reader_fapl_t, fapl);

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__vfd_swmr_fapl_free() */

/*-------------------------------------------------------------------------
 * Function:    H5P_pop_vfd_swmr_reader_vfd_off_fapl
 *
 * Purpose:     After a file has been opened in VFD SWMR reader mode, we
 *              must pop the vfd swmr reader driver entry off the supplied
 *              fapl.  If we don't, and the fapl is used to open a second
 *              file (i.e. via virtual data sets), we would have multiple
 *              vfd swmr reader driver entries pushed on the vfd stack.
 *
 *              Do this as follows:
 *
 *              1) Read the file driver entry from the supplied fapl.  Verify
 *                 that it specifies the vfd swmr reader VFD.
 *
 *              2) Read the file driver entry from the sub fapl specified
 *                 in the vfd swmr reader vfd fapl entry.  Set the file
 *                 driver entry on the supplied fapl equal to that on
 *                 the sub-fapl.
 *
 *              3) Discard the sub-fapl?  Not sure if this is necessary.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer   JRM -- 4/28/22
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_pop_vfd_swmr_reader_vfd_off_fapl(hid_t fapl_id)
{
    H5FD_driver_prop_t driver_prop; /* Property for driver ID & info */
    H5P_genplist_t *   plist_ptr = NULL;
    hid_t              sub_fapl_id;
    H5P_genplist_t *   sub_plist_ptr = NULL;
    herr_t             ret_value     = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks -- get ptr to plist in passing */
    if (NULL == (plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* 1) Read the file driver entry from the supplied fapl.  Verify
     *    that it specifies the vfd swmr reader VFD.
     */

    /* get the driver property from the supplied fapl */
    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file driver ID & info")

    /* verify that it specifies the vfd swrm reader vfd */
    if (driver_prop.driver_id != H5FD_VFD_SWMR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "fapl driver prop not vfd swmr reader")

    if ((((const H5FD_vfd_swmr_reader_fapl_t *)(driver_prop.driver_info)) == NULL) ||
        (((const H5FD_vfd_swmr_reader_fapl_t *)(driver_prop.driver_info))->magic !=
         H5FD_VFD_SWMR_READER_MAGIC))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "fapl driver info invalid")

    sub_fapl_id = ((const H5FD_vfd_swmr_reader_fapl_t *)(driver_prop.driver_info))->fapl_id;

    /* 2) Read the file driver entry from the sub fapl specified
     *    in the vfd swmr reader vfd fapl entry.  Set the file
     *    driver entry on the supplied fapl equal to that on
     *    the sub-fapl.
     */
    /* get a pointer to the sub-fapl */
    if (NULL == (sub_plist_ptr = (H5P_genplist_t *)H5I_object(sub_fapl_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get sub-fapl pointer")

    /* get the driver property from the sub-fapl */
    if (H5P_peek(sub_plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get sub-fapl file driver ID & info")

    /* insert the driver info from the sub-fapl into the supplied fapl.  There is
     * some question in my mind as to whether I should be making a copy of the
     * info and string obtained above.  While I don't think it is necessary,
     * if we get occult failures, this is a good place to look.
     *
     * Note that for now, the driver info on the sub-fapl should only specify the
     * sec2 VFD -- which has NULL info and config string.  Thus, if it is an
     * issue, it may not appear immediately.
     */
    if (H5P_set_driver(plist_ptr, driver_prop.driver_id, driver_prop.driver_info,
                       driver_prop.driver_config_str) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "can't set driver on supplied fapl")

    /* 3) Discard the sub-fapl?  Not sure if this is necessary.  Will wait on this for now. */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5P_pop_vfd_swmr_reader_vfd_off_fapl() */

/*-------------------------------------------------------------------------
 * Function:    H5P_push_vfd_swmr_reader_vfd_on_fapl
 *
 * Purpose:     When a file is opened in VFD SWMR reader mode, we must
 *              adjust the fapl so as to push the VFD SWMR reader vfd on
 *              the VFD stack specified in the fapl.
 *
 *              Do this as follows:
 *
 *              1) Copy the file driver from the supplied fapl.  Note
 *                 that due to potential VFD stacking, we can't verify
 *                 that this VFD supports vfd swmr.  This will have to
 *                 wait until after the file is opened.
 *
 *              2) Create a new FAPL, and set the file driver obtained
 *                 in 1) in the new FAPL.
 *
 *              3) Allocate a new instance of H5FD_vfd_swmr_reader_config_t,
 *                 load it with the ID of the FAPL created in 2, and use
 *                 it to overwrite the file driver entry in the supplied
 *                 FAPL.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer   JRM -- 4/28/22
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5P_push_vfd_swmr_reader_vfd_on_fapl(hid_t fapl_id)
{
    H5FD_driver_prop_t           driver_prop; /* Property for driver ID & info */
    H5FD_vfd_swmr_reader_fapl_t *info      = NULL;
    H5P_genplist_t *             plist_ptr = NULL;
    hid_t                        sub_fapl_id;
    H5P_genplist_t *             sub_plist_ptr = NULL;
    H5P_genclass_t *             pclass        = NULL;
    herr_t                       ret_value     = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* sanity checks -- get ptr to plist in passing */
    if (NULL == (plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* 1) Copy the file driver from the supplied fapl.  Note
     *    that due to potential VFD stacking, we can't verify
     *    that this VFD supports vfd swmr.  This will have to
     *    wait until after the file is opened.
     */

    /* get the driver property from the supplied fapl */
    if (H5P_peek(plist_ptr, H5F_ACS_FILE_DRV_NAME, &driver_prop) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get init driver ID & info")

    /* 2) Create a new FAPL, and set the file driver obtained in 1) in the new FAPL. */

    /* create a new FAPL */
    if (NULL == (pclass = (H5P_genclass_t *)H5I_object_verify(H5P_FILE_ACCESS, H5I_GENPROP_CLS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5I_INVALID_HID, "not a property list class");

    if ((sub_fapl_id = H5P_create_id(pclass, TRUE)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, H5I_INVALID_HID, "unable to create fapl");

    /* get a pointer to it */
    if (NULL == (sub_plist_ptr = (H5P_genplist_t *)H5I_object(sub_fapl_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get sub-fapl pointer")

    /* insert the driver info from the base fapl into the sub-fapl.  There is
     * some question in my mind as to whether I should be making a copy of the
     * info and string obtained above.  While I don't think it is necessary,
     * if we get occult failures, this is a good place to look.
     *
     * Note that for now, the driver info being inserted in sub-fapl should only specify
     * the sec2 VFD -- which has NULL info and config string.  Thus, if it is an
     * issue, it may not appear immediately.
     */
    if (H5P_set_driver(sub_plist_ptr, driver_prop.driver_id, driver_prop.driver_info,
                       driver_prop.driver_config_str) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "can't set driver on sub-fapl")

    /* 3) Allocate a new instance of H5FD_vfd_swmr_reader_config_t,
     *    load it with the ID of the FAPL created in 2, and use
     *    it to overwrite the file driver entry in the supplied
     *    FAPL.
     */

    info = H5FL_CALLOC(H5FD_vfd_swmr_reader_fapl_t);
    if (NULL == info)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "unable to allocate vfd swmr fapl struct")

    /* initialize the vfd swmr reader vfd info */
    info->magic   = H5FD_VFD_SWMR_READER_MAGIC;
    info->fapl_id = sub_fapl_id;

    /* set the driver on the main fapl */
    ret_value = H5P_set_driver(plist_ptr, H5FD_VFD_SWMR, info, NULL);

done:
    if (info)
        info = H5FL_FREE(H5FD_vfd_swmr_reader_fapl_t, info);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5P_push_vfd_swmr_reader_vfd_on_fapl() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__swmr_reader_open
 *
 * Purpose:     Perform the reader-only aspects of opening in VFD SWMR mode:
 *              initialize histogram of ticks spent in API calls, wait for
 *              the shadow file to appear, load the header and index.
 *
 * Return:      SUCCEED/FAIL
 *
 * Modifications:
 *  Vailin Choi: 2/18/2022
 *  VDS changes: Try opening metadata file and loading header/index if make_believe is FALSE.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__swmr_reader_open(H5FD_vfd_swmr_t *file)
{
    h5_retry_t retry;
    hbool_t    do_try; /* more tries remain */
    herr_t     ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    file->api_elapsed_nbuckets = file->config.max_lag + 1;

    file->api_elapsed_ticks = H5MM_calloc(file->api_elapsed_nbuckets * sizeof(*file->api_elapsed_ticks));

    if (file->api_elapsed_ticks == NULL)
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "could not allocate API elapsed ticks")

    if (!file->make_believe) {
        /* Retry on opening the metadata file */
        for (do_try         = H5_retry_init(&retry, H5FD_VFD_SWMR_MD_FILE_RETRY_MAX, H5_RETRY_DEFAULT_MINIVAL,
                                    H5_RETRY_DEFAULT_MAXIVAL);
             do_try; do_try = H5_retry_next(&retry)) {
            if ((file->md_fd = HDopen(file->md_file_path_name, O_RDONLY)) >= 0)
                break;
        }

        /* Exhaust all retries for opening the md file */
        if (!do_try)
            HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, FAIL,
                        "unable to open the metadata file after all retry attempts");

        /* Retry on loading and decoding the header and index in the
         *  metadata file
         */
        if (H5FD__vfd_swmr_load_hdr_and_idx(file, TRUE) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to load/decode the md file header/index");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__swmr_reader_open() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_create_make_believe_data
 *
 * Purpose:     Set up pretend data when make_believe is true
 *
 * Return:      VOID
 *
 * Programmer:  Vailin Choi -- 1/13/2022
 *
 *-------------------------------------------------------------------------
 */
static void
H5FD__vfd_swmr_create_make_believe_data(H5FD_vfd_swmr_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;

    FUNC_ENTER_PACKAGE_NOERR

    HDassert(file->make_believe);

    /* Create make_believe data: empty header and index */
    file->md_header.fs_page_size = 0;
    file->md_header.tick_num     = 0;
    file->md_header.index_offset = H5FD_MD_HEADER_SIZE;
    file->md_header.index_length = H5FD_MD_INDEX_SIZE(0);

    file->md_index.tick_num    = 0;
    file->md_index.num_entries = 0;

    FUNC_LEAVE_NOAPI_VOID

} /* H5FD__vfd_swmr_create_make_believe_data() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_open
 *
 * Purpose:     Open the metadata file and the underlying HDF5 file
 *
 * Return:      Success:    A pointer to a new file data structure. The
 *                          public fields will be initialized by the
 *                          caller, which is always H5FD_open().
 *              Failure:    NULL
 *
 * Modifications:
 *  Vailin Choi: 2/18/2022
 *  VDS changes: --Build metadata file name
 *               --Determine make_believe or not
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__vfd_swmr_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_vfd_swmr_t *                  file = NULL;
    size_t                             page_buf_size;
    H5P_genplist_t *                   plist;
    H5F_vfd_swmr_config_t *            vfd_swmr_config;
    const H5FD_vfd_swmr_reader_fapl_t *fa_ptr    = NULL;
    H5FD_t *                           ret_value = NULL; /* Return value     */
    htri_t                             is_hdf5;

    FUNC_ENTER_PACKAGE

    /* VFD SWMR reader VFD should only be called to open a file read only */
    HDassert((H5F_ACC_RDWR & flags) == 0);

    /* Get file access property list */
    if (NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")

    if (H5P_get(plist, H5F_ACS_PAGE_BUFFER_SIZE_NAME, &page_buf_size) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get page buffer size");

    /* Paged allocation, too, has to be enabled, but the page buffer
     * initialization (H5PB_create) will detect a conflicting configuration
     * and return an error.
     */
    if (page_buf_size == 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "page buffering must be enabled")

    /* Create the new driver struct */
    if (NULL == (file = H5FL_CALLOC(H5FD_vfd_swmr_t)))
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, NULL, "unable to allocate file struct")

    /* get the vfd swrm reader fapl entry. */
    if (H5P_peek_driver(plist) != H5FD_VFD_SWMR)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "incorrect VFL driver");

    fa_ptr = (const H5FD_vfd_swmr_reader_fapl_t *)H5P_peek_driver_info(plist);
    if (NULL == fa_ptr)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "bad VFL driver info");

    HDassert(fa_ptr->magic == H5FD_VFD_SWMR_READER_MAGIC); /* sanity check */

    /* the fapl id stored in fa_ptr->fapl_id should contain a driver entry that
     * specifies a VFD that supports VFD SWMR.  Since there may be a stack of
     * VFDs, we can't check this until after file open.  Further, the vfd swmr
     * reader vfd is currently hard coded to use the sec2 vfd as its underlying
     * vfd.  Thus we just save a copy of the H5FD_vfd_swmr_reader_fapl_t for
     * now.
     */
    H5MM_memcpy(&(file->fa), fa_ptr, sizeof(H5FD_vfd_swmr_reader_fapl_t));

    vfd_swmr_config = &file->config;

    /* Get VFD SWMR configuration */
    if (H5P_get(plist, H5F_ACS_VFD_SWMR_CONFIG_NAME, vfd_swmr_config) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get VFD SWMR config info")

    HDassert(!vfd_swmr_config->writer);

    file->md_fd             = -1;
    file->hdf5_file_lf      = NULL;
    file->md_pages_reserved = vfd_swmr_config->md_pages_reserved;

    /* Retain a copy of the name used to open the HDF5 file */
    HDstrncpy(file->hdf5_filename, name, sizeof(file->hdf5_filename));
    file->hdf5_filename[sizeof(file->hdf5_filename) - 1] = '\0';

    /* Retain a copy of the metadata file name */
    if (H5F_vfd_swmr_build_md_path_name(vfd_swmr_config, name, file->md_file_path_name) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, NULL, "building md_file_path and md_file_name failed")

    file->md_file_path_name[sizeof(file->md_file_path_name) - 1] = '\0';

    /* Make sure the hdf5 file exists and is valid */
    is_hdf5 = H5F__is_hdf5(name, H5P_FILE_ACCESS_DEFAULT);

    /* Ensure reader */
    if (!vfd_swmr_config->writer) {
        /* Metadata file does not exist, presume_posix is true, HDF5 file exist */
        if (HDaccess(file->md_file_path_name, F_OK) < 0 && vfd_swmr_config->presume_posix_semantics &&
            is_hdf5 == TRUE) {

            file->make_believe = TRUE;
            H5FD__vfd_swmr_create_make_believe_data(file);
        }
        if (H5FD__swmr_reader_open(file) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, NULL, "perform reader-specific opening steps failed")
    }

    /* Hard-wired to open the underlying HDF5 file with SEC2 */
    /* H5FD_SEC2 is the default driver for H5P_FILE_ACCESS_DEFAULT except when
       the environment variable HDF5_DRIVER is set to otherwise */
    if ((file->hdf5_file_lf = H5FD_open(name, flags, H5P_FILE_ACCESS_DEFAULT, HADDR_UNDEF)) == NULL)
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "can't set driver info");

    /* set pb_configured to FALSE.  This field should not exist, but
     * until we modify the file open procedure to create the page buffer
     * before there is any file I/O when opening a file VFD SWMR reader,
     * we need to be able to turn off sanity checking in the read function
     * until the page buffer is enabled.  This field exists for this
     * purpose, and should be removed when it is no longer necessary.
     *
     *                                            JRM -- 1/29/19
     */
    file->pb_configured = FALSE;

    /* Set return value */
    ret_value = &file->pub;

done:
    /* Handle closing if error */
    if (NULL == ret_value && file)
        if (H5FD__vfd_swmr_close(&file->pub) < 0)
            HDONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, NULL, "error from closing")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_open() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_close
 *
 * Purpose:     Handle closing for VFD SWMR driver
 *              --close the underlying HDF5 file
 *              --close the metadata file if open
 *              --free the index entries if available
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_close(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file;
    herr_t           ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    if (file->hdf5_file_lf != NULL) {

        /* Close the underlying file */
        if (H5FD_close(file->hdf5_file_lf) < 0)
            /* Push error, but keep going */
            HDONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close the HDF5 file")
    }

    if (file->api_elapsed_ticks != NULL)
        H5MM_xfree(file->api_elapsed_ticks);

    /* Close the metadata file */
    if (file->md_fd >= 0 && HDclose(file->md_fd) < 0)
        /* Push error, but keep going */
        HDONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close the metadata file");

    /* Free the index entries */
    if (file->md_index.num_entries && file->md_index.entries)
        file->md_index.entries = H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, file->md_index.entries);

    /* Release the driver info */
    file = H5FL_FREE(H5FD_vfd_swmr_t, file);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__vfd_swmr_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_cmp
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
H5FD__vfd_swmr_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_vfd_swmr_t *f1        = (const H5FD_vfd_swmr_t *)_f1;
    const H5FD_vfd_swmr_t *f2        = (const H5FD_vfd_swmr_t *)_f2;
    int                    ret_value = 0;

    FUNC_ENTER_PACKAGE_NOERR

    ret_value = H5FD_cmp(f1->hdf5_file_lf, f2->hdf5_file_lf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd__swmr_cmp() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      SUCCEED (Can't fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Set the VFL feature flags that this driver supports */
    if (flags) {
        *flags = 0;

        /* OK to aggregate metadata allocations */
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;

        /* OK to accumulate metadata for faster writes */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA;

        /* OK to perform data sieving for faster raw data reads & writes */
        *flags |= H5FD_FEAT_DATA_SIEVE;

        /* OK to aggregate "small" raw data allocations */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA;

        /* get_handle callback returns a POSIX file descriptor */
        *flags |= H5FD_FEAT_POSIX_COMPAT_HANDLE;

        /* VFD supports the single-writer / multiple readers (SWMR) pattern */
        *flags |= H5FD_FEAT_SUPPORTS_SWMR_IO;

        /* VFD creates a file that can be opened with the default VFD
         *
         * NOTE: When this VFD becomes a true passthrough, this flag will
         *       probably need to go away.
         */
        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE;
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD__vfd_swmr_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file for the
 *              underlying HDF5 file. The EOA marker is the first address
 *              past the last byte allocated in the format address space.
 *
 * Return:      The end-of-address marker.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__vfd_swmr_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_vfd_swmr_t *file      = (const H5FD_vfd_swmr_t *)_file;
    haddr_t                ret_value = HADDR_UNDEF;

    FUNC_ENTER_PACKAGE

    if ((ret_value = H5FD_get_eoa(file->hdf5_file_lf, type)) == HADDR_UNDEF)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "unable to get HDF5 file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_get_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the underlying HDF5 file.
 *              This function is called shortly after an existing HDF5 file
 *              is opened in order to tell the driver where the end of the
 *              HDF5 data is located.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file;
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (H5FD_set_eoa(file->hdf5_file_lf, type, addr) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to set HDF5 file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_get_eof
 *
 * Purpose:     Returns the end-of-file marker, which is the greater of
 *              either the filesystem end-of-file or the HDF5 end-of-address
 *              markers for the underlying HDF5 file
 *
 * Return:      End of file address, the first address past the end of the
 *              "file", either the filesystem file or the HDF5 file.
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__vfd_swmr_get_eof(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_vfd_swmr_t *file      = (const H5FD_vfd_swmr_t *)_file;
    haddr_t                ret_value = HADDR_UNDEF;

    FUNC_ENTER_PACKAGE

    /* LATER: need to determine the metadata file or underlying HDF5 file ? */
    if ((ret_value = H5FD_get_eof(file->hdf5_file_lf, type)) == HADDR_UNDEF)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "unable to set file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_get_eof() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_get_handle
 *
 * Purpose:     Returns the file handle for the underling HDF5 file
 *
 * Returns:     SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_get_handle(H5FD_t *_file, hid_t fapl, void **file_handle)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file;
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    /* LATER? H5P_get(plist, H5F_ACS_SWMR_FILE_NAME, &type) */

    if ((ret_value = H5FD_get_vfd_handle(file->hdf5_file_lf, fapl, file_handle)) < 0)

        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to get handle for HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_get_handle() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_read
 *
 * Purpose:     If the target page or multi-page metadata entry is
 *              defined in the current metadata file index, satisfy
 *              the read from the metadata file.  Otherwise, pass the
 *              read through to the underlying VFD.
 *
 *              Under normal operating conditions, the size of the
 *              read must always match the size supplied in the
 *              metadata file index.  However, until we modify the
 *              file open process for VFD SWMR readers to create the
 *              page buffer before any reads, we must allow non
 *              full page / non full multi-page metadata entry reads
 *              until the page buffer is created.
 *
 *              This is tracked by the pb_configured flag in
 *              H5FD_vfd_swmr_t.  If this field is FALSE, the function
 *              must allow reads smaller than the size listed in the
 *              index, and possibly starting anywhere in the page.
 *              Note, however, that these reads must not cross page
 *              boundaries.
 *
 *              Once we modify the file open code to start up the
 *              page buffer before we attempt any reads, this exception
 *              will not longer be necessary, and should be removed.
 *
 *                                            JRM -- 1/29/19
 *
 * Return:      Success:    SUCCEED. Result is stored in caller-supplied
 *                          buffer BUF.
 *              Failure:    FAIL, Contents of buffer BUF are undefined.
 *
 * Modifications:
 *  Vailin Choi: 2/18/2022
 *  VDS changes: If fs_page_size is 0, i.e. in make_believe state,
 *               read from the underlying HDF5 file
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_read(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id, const haddr_t addr,
                    size_t size, void *const buf /*out*/)
{
    const size_t               init_size = size;
    haddr_t                    target_page;
    haddr_t                    page_offset;
    H5FD_vfd_swmr_t *          file = (H5FD_vfd_swmr_t *)_file;
    H5FD_vfd_swmr_idx_entry_t *index, *entry;
    uint32_t                   num_entries = 0;
    uint32_t                   fs_page_size;
    herr_t                     ret_value = SUCCEED;
    char *                     p         = buf;

    FUNC_ENTER_PACKAGE

    HDassert(file && file->pub.cls);
    HDassert(buf);

    index        = file->md_index.entries;
    num_entries  = file->md_index.num_entries;
    fs_page_size = file->md_header.fs_page_size;

    if (!fs_page_size) {
        HDassert(!num_entries);
        HDassert(file->make_believe);
        entry = NULL;
    }
    else {
        /* Try finding the addr from the index */
        target_page = addr / fs_page_size;

        entry = H5FD_vfd_swmr_pageno_to_mdf_idx_entry(index, num_entries, target_page, FALSE);
    }

    if (entry == NULL) {
        /* Cannot find addr in index, read from the underlying hdf5 file */
        if (H5FD_read(file->hdf5_file_lf, type, addr, size, buf) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "file read request failed")

        HGOTO_DONE(SUCCEED);
    }

    /* Found in index, read from the metadata file */
    HDassert(addr >= target_page * fs_page_size);

    page_offset = addr - (target_page * fs_page_size);

    HDassert((page_offset == 0) || ((!file->pb_configured) && (page_offset + size <= fs_page_size)));

    HDassert(entry->hdf5_page_offset * fs_page_size <= addr);
    HDassert(addr < (entry->hdf5_page_offset + 1) * fs_page_size);
    HDassert(page_offset + init_size <= entry->length);

    if (HDlseek(file->md_fd, (HDoff_t)((entry->md_file_page_offset * fs_page_size) + page_offset), SEEK_SET) <
        0)
        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

    /* Coding borrowed from sec2 read */
    while (size > 0) {

        h5_posix_io_t     bytes_in;   /* # of bytes to read */
        h5_posix_io_ret_t bytes_read; /* # of bytes actually read */

        /* Trying to read more bytes than the return type can handle is
         * undefined behavior in POSIX.
         */
        if (size > H5_POSIX_MAX_IO_BYTES)
            bytes_in = MIN(H5_POSIX_MAX_IO_BYTES, size);
        else
            bytes_in = (h5_posix_io_t)size;

        do {
            bytes_read = HDread(file->md_fd, p, bytes_in);
        } while (-1 == bytes_read && EINTR == errno);

        if (-1 == bytes_read)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL,
                        "error reading the page/multi-page entry from the md file")

        HDassert(0 <= bytes_read && (size_t)bytes_read <= size);

        size -= (size_t)bytes_read;
        p += bytes_read;
    }

    /* Verify stored and computed checksums are equal.
     *
     * Ignore the checksum if the buffer (buf, size) is not large enough
     * to hold the entire shadow image.  Assume that the caller will
     * read the entry fully, later.
     *
     * Ignore checksum if the page buffer is not configured---this
     * is John's hack to allow the library to find the superblock
     * signature.
     */
    if (file->pb_configured && entry->length == init_size &&
        H5_checksum_metadata(buf, entry->length, 0) != entry->checksum) {
        H5FD_vfd_swmr_md_header tmp_header;

        if (H5FD__vfd_swmr_header_deserialize(file, &tmp_header) != TRUE)
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL,
                        "checksum error in shadow file entry; could not load header")

        HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, "checksum error in shadow file entry")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_write
 *
 * Purpose:     As the VFD SWMR reader VFD is only use on files that are
 *              opened read only, this function should be unreachable.
 *
 * Return:      FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_write(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type,
                     hid_t H5_ATTR_UNUSED dxpl_id, haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED size,
                     const void H5_ATTR_UNUSED *buf)
{
    FUNC_ENTER_PACKAGE_NOERR /* Yes, even though this pushes an error on the stack */

        HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)

} /* end H5FD__vfd_swmr_write() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_truncate
 *
 * Purpose:     As the VFD SWMR reader VFD is only use on files that are
 *              opened read only, this function should be unreachable.
 *
 * Return:      FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_truncate(H5FD_t H5_ATTR_UNUSED *_file, hid_t H5_ATTR_UNUSED dxpl_id,
                        hbool_t H5_ATTR_UNUSED closing)
{
    FUNC_ENTER_PACKAGE_NOERR /* Yes, even though this pushes an error on the stack */

        HERROR(H5E_CACHE, H5E_SYSTEM, "called unreachable fcn.");

    FUNC_LEAVE_NOAPI(FAIL)
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_lock
 *
 * Purpose:     To place an advisory lock on the underlying HDF5 file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_lock(H5FD_t *_file, hbool_t rw)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t           ret_value = SUCCEED;                  /* Return value  */

    FUNC_ENTER_PACKAGE

    HDassert(file);

    if (H5FD_lock(file->hdf5_file_lf, rw) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOCK, FAIL, "unable to lock the HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__vfd_swmr_lock() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_unlock
 *
 * Purpose:     To remove the existing lock on the underlying HDF5 file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_unlock(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t           ret_value = SUCCEED;                  /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(file);

    if (H5FD_unlock(file->hdf5_file_lf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTUNLOCK, FAIL, "unable to unlock the HDF5 file")

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__vfd_swmr_unlock() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_ctl
 *
 * Purpose:     VFD SWMR reader VFD version of the ctl callback.
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
 *              At present, this VFD supports no op codes of its own and
 *              simply passes ctl calls on to the underlying VFD.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_ctl(H5FD_t *_file, uint64_t op_code, uint64_t flags, const void *input, void **output)
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file;
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(file);

    switch (op_code) {

        /* Unknown op code */
        default:
            if (flags & H5FD_CTL__ROUTE_TO_TERMINAL_VFD_FLAG) {
                /* Pass ctl call down to R/W channel VFD */
                if (H5FDctl(file->hdf5_file_lf, op_code, flags, input, output) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_FCNTL, FAIL, "VFD ctl request failed")
            }
            else {
                /* If no valid VFD routing flag is specified, fail for unknown op code
                 * if H5FD_CTL__FAIL_IF_UNKNOWN_FLAG flag is set.
                 */
                if (flags & H5FD_CTL__FAIL_IF_UNKNOWN_FLAG)
                    HGOTO_ERROR(H5E_VFL, H5E_FCNTL, FAIL,
                                "VFD ctl request failed (unknown op code and fail if unknown flag is set)")
            }

            break;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__vfd_swmr_ctl() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_load_hdr_and_idx()
 *
 * Purpose:     Load and decode the header and index in the metadata file
 *
 * In H5FD__vfd_swmr_load_hdr_and_idx(), we follow this protocol for reading
 * the shadow file:
 *
 * 0 If the maximum number of retries have been attempted, then exit
 *   with an error.
 *
 * 1 Try to read the shadow file *header*.  If successful, continue to 2.
 *
 *   If there is a hard failure, then return an error.  If there is a failure
 *   that may be transient, then sleep and retry at 0.
 *
 * 2 If the tick number in the header is less than the tick last read by the
 *   VFD, then return an error.
 *
 * 3 If the tick number in the header is equal to the last tick read by the
 *   VFD, then exit without doing anything.
 *
 * 4 Try to read the shadow file *index*.  If successful, continue to 5.
 *
 *   If there is a hard failure, then return an error.  If there is a failure
 *   that may be transient, then sleep and retry at 0.
 *
 * 5 If a different tick number was read from the index than from the index,
 *   then continue at 0.
 *
 * 6 Try to *re-read* the shadow file *header*.  If successful, continue to 7.
 *
 *   If there is a hard failure, then return an error.  If there is a failure
 *   that may be transient, then sleep and retry at 0.
 *
 * 7 Compare the header that was read previously with the new header.  If
 *   the new header is different than the old, then we may not have read
 *   the index at the right shadow-file offset, or the index may have been
 *   read in an inconsistent state, so sleep and retry at 0.  Otherwise,
 *   return success.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Modifications:
 *  Vailin Choi: 2/18/2022
 *  VDS changes: Update the header's fs_page size if it is still 0
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__vfd_swmr_load_hdr_and_idx(H5FD_vfd_swmr_t *file, hbool_t open)
{
    hbool_t                 do_try;
    h5_retry_t              retry;
    H5FD_vfd_swmr_md_header md_header;     /* Metadata file header, take 1 */
    H5FD_vfd_swmr_md_header md_header_two; /* Metadata file header, take 2 */
    H5FD_vfd_swmr_md_index  md_index;      /* Metadata file index           */
    htri_t                  rc;
    static uint64_t         last_index_offset = 0;
    herr_t                  ret_value         = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    for (do_try         = H5_retry_init(&retry, H5FD_VFD_SWMR_MD_LOAD_RETRY_MAX, H5_RETRY_ONE_SECOND / 10,
                                H5_RETRY_ONE_SECOND);
         do_try; do_try = H5_retry_next(&retry)) {

        /* Load and decode the header.  Go around again on a temporary
         * failure (FALSE).  Bail on an irrecoverable failure (FAIL).
         */
        rc = H5FD__vfd_swmr_header_deserialize(file, &md_header);

        /* Temporary failure, try again. */
        if (rc == FALSE)
            continue;

        if (rc != TRUE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not read header")

        if (md_header.index_offset != last_index_offset)
            last_index_offset = md_header.index_offset;

        if (open)
            ; // ignore tick number on open
        else if (md_header.tick_num == file->md_header.tick_num) {
            /* If the tick number in the header hasn't increased since last
             * time, then there is not a complete new index to read, so
             * get out.
             */
            HGOTO_DONE(SUCCEED)
        }
        else if (md_header.tick_num < file->md_header.tick_num)
            /* The tick number must not move backward. */
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "tick number in header moved backwards")

        HDassert(md_header.tick_num > file->md_header.tick_num || open);

        /* Load and decode the index.  Go around again on a temporary
         * failure (FALSE).  Bail on an irrecoverable failure (FAIL).
         */
        rc = H5FD__vfd_swmr_index_deserialize(file, &md_index, &md_header);

        if (rc == FALSE)
            continue;

        if (rc != TRUE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not read index")

        /* If the tick_num is the same in both header and index,
         * and the header reads the same the second time as the first time,
         * then we should have a consistent picture of the index.
         */
        if (md_header.tick_num == md_index.tick_num &&
            (rc = H5FD__vfd_swmr_header_deserialize(file, &md_header_two)) == TRUE &&
            md_header.tick_num == md_header_two.tick_num &&
            md_header.index_length == md_header_two.index_length &&
            md_header.index_offset == md_header_two.index_offset)
            break;

        if (md_index.entries != NULL) {

            HDassert(md_index.num_entries);
            md_index.entries =
                (H5FD_vfd_swmr_idx_entry_t *)H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, md_index.entries);
        }

        if (rc == FAIL)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not re-read header")
    }

    /* Exhaust all retries for loading and decoding the md file header
     * and index
     */
    if (!do_try)
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL,
                    "error in loading/decoding the metadata file header and index")

    /* Free VFD local entries */
    if (file->md_index.entries != NULL) {

        HDassert(file->md_index.num_entries);

        file->md_index.entries =
            (H5FD_vfd_swmr_idx_entry_t *)H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, file->md_index.entries);
    }

    /* Copy header and index to VFD */
    file->md_header  = md_header;
    file->md_index   = md_index;
    md_index.entries = NULL;

done:
    /* Need to update the header's fs_page_size if it is still 0
       because it is possible that md_header.tick_num == file->md_header.tick_num
       and the loading is not done */
    if (ret_value == SUCCEED && !file->md_header.fs_page_size) {
        HDassert(md_header.fs_page_size);
        HDassert(file->make_believe);
        file->md_header.fs_page_size = md_header.fs_page_size;
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD__vfd_swmr_load_hdr_and_idx() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_header_deserialize()
 *
 * Purpose:     To load and decode the header in the metadata file
 *              --Retry to get a file with size at least the size of the header
 *              --Retry on loading the valid magic and checksum for the header
 *              --Decode the header
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:  Vailin Choi
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5FD__vfd_swmr_header_deserialize(H5FD_vfd_swmr_t *file, H5FD_vfd_swmr_md_header *md_header)
{
    uint8_t  image[H5FD_MD_HEADER_SIZE]; /* Buffer for element data */
    uint32_t stored_chksum;              /* Stored metadata checksum */
    uint32_t computed_chksum;            /* Computed metadata checksum */
    uint8_t *p;
    htri_t   ret_value = FAIL;
    uint64_t index_length;
    ssize_t  nread;

    FUNC_ENTER_PACKAGE

    /* Set file pointer to the beginning the file */
    if (HDlseek(file->md_fd, H5FD_MD_HEADER_OFF, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

    /* Read the header */
    nread = HDread(file->md_fd, image, H5FD_MD_HEADER_SIZE);

    /* Try again if a signal interrupted the read. */
    if (nread == -1 && errno == EINTR)
        HGOTO_DONE(FALSE)

    /* We cannot recover from any other error by trying again,
     * so bail out.
     */
    if (nread == -1)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "error in reading the shadow header")

    if ((uint64_t)nread < H5FD_MD_HEADER_SIZE)
        HGOTO_DONE(FALSE)

    /* Verify magic number */
    if (HDmemcmp(image, H5FD_MD_HEADER_MAGIC, H5_SIZEOF_MAGIC) != 0)
        HGOTO_DONE(FALSE)

    /* Verify stored and computed checksums are equal */
    H5F_get_checksums(image, H5FD_MD_HEADER_SIZE, &stored_chksum, &computed_chksum);

    if (stored_chksum != computed_chksum)
        HGOTO_DONE(FALSE);

    /* Header magic is already valid */
    p = image + H5_SIZEOF_MAGIC;

    /* Deserialize page size, tick number, index offset, index length */
    UINT32DECODE(p, md_header->fs_page_size);
    UINT64DECODE(p, md_header->tick_num);
    UINT64DECODE(p, md_header->index_offset);
    UINT64DECODE(p, index_length);
    if (index_length > SIZE_MAX)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "index is too large to hold in core")

    md_header->index_length = (size_t)index_length;

    /* Checksum is already valid */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - image) <= H5FD_MD_HEADER_SIZE);

    ret_value = TRUE;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD__vfd_swmr_header_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__vfd_swmr_index_deserialize()
 *
 * Purpose:     Load and decode the index in the metadata file
 *              --Retry to get a file with size at least the size of the
 *                (header+index)
 *              --Retry on loading the valid magic and checksum for the index
 *              --Decode the index
 *              --Decode the index entries if the tick number in the header and
 *                the index match
 *
 * Return:      TRUE/FALSE/FAIL
 *-------------------------------------------------------------------------
 */
static htri_t
H5FD__vfd_swmr_index_deserialize(const H5FD_vfd_swmr_t *file, H5FD_vfd_swmr_md_index *md_index,
                                 const H5FD_vfd_swmr_md_header *md_header)
{
    uint8_t *image;           /* Buffer */
    uint8_t *p = NULL;        /* Pointer to buffer */
    uint32_t stored_chksum;   /* Stored metadata checksum value */
    uint32_t computed_chksum; /* Computed metadata checksum value */
    unsigned i;               /* Local index variable */
    htri_t   ret_value = TRUE;
    ssize_t  nread;

    FUNC_ENTER_PACKAGE

    /* Allocate buffer for reading index */
    if (NULL == (image = H5MM_malloc(md_header->index_length)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "memory allocation failed for index's on disk image buffer")

    /* We may seek past EOF.  That's ok, the read(2) will catch that. */
    if (HDlseek(file->md_fd, (HDoff_t)md_header->index_offset, SEEK_SET) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

    nread = HDread(file->md_fd, image, md_header->index_length);

    /* Try again if a signal interrupted the read. */
    if (nread == -1 && errno == EINTR)
        HGOTO_DONE(FALSE)

    /* We cannot recover from any other error by trying again,
     * so bail out.
     */
    if (nread == -1)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "error in reading the header in metadata file")

    /* Try again if the read was not full.
     *
     * XXX XXX XXX
     * A short read should not be possible under the protocol that
     * I intend to adopt: the writer will write(2) the new index.
     * In a second write(2), the header describing that index
     * will be written.  POSIX will guarantee that the former
     * write is visible before the latter.  Under the protocol,
     * there should always be `index_length` bytes available to
     * read at `index_offset`.  If not, the reader should treat it
     * like an unrecoverable error instead of retrying.
     */
    if ((size_t)nread < md_header->index_length)
        HGOTO_DONE(FALSE)

    /* If the index magic is incorrect, then assume that is a
     * temporary error and try again.
     *
     * XXX XXX XXX
     * Under the new protocol, where the index is written in
     * one write(2), and the header is written in a distinct
     * second write(2), and the header and index are read in
     * the reverse order, the index magic usually will be intact.
     *
     * It is possible under the new protocol that we read
     * the header on tick `t`, then an arbitrary delay
     * occurs (the user taps Control-Z, say), and then we
     * read the index on tick `t + max_lag + 1` or later.
     * In the mean time, the index may have moved, and its
     * storage may have been reused.  In that case, we could
     * read bad magic.  It's possible to recover by
     * re-reading the header.
     */
    if (HDmemcmp(image, H5FD_MD_INDEX_MAGIC, H5_SIZEOF_MAGIC) != 0)
        HGOTO_DONE(FALSE)

    /* Verify stored and computed checksums are equal */
    H5F_get_checksums(image, md_header->index_length, &stored_chksum, &computed_chksum);

    if (stored_chksum != computed_chksum)
        HGOTO_DONE(FALSE);

    p = image + H5_SIZEOF_MAGIC;

    /* Deserialize the index info: tick number, number of entries, entries,
     * checksum
     */
    UINT64DECODE(p, md_index->tick_num);
    UINT32DECODE(p, md_index->num_entries);

    /* Read index entries */
    if (md_index->num_entries) {
        /* Allocate memory for index entries */
        md_index->entries = H5FL_SEQ_CALLOC(H5FD_vfd_swmr_idx_entry_t, md_index->num_entries);
        if (NULL == md_index->entries)
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "memory allocation failed for index entries")

        /* Decode index entries */
        for (i = 0; i < md_index->num_entries; i++) {
            UINT32DECODE(p, md_index->entries[i].hdf5_page_offset);
            UINT32DECODE(p, md_index->entries[i].md_file_page_offset);
            UINT32DECODE(p, md_index->entries[i].length);
            UINT32DECODE(p, md_index->entries[i].checksum);
        }
    }
    else
        md_index->entries = NULL;

    /* Checksum is already valid */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - image) <= md_header->index_length);

done:
    if (image != NULL)
        image = H5MM_xfree(image);

    if (ret_value == FAIL && md_index->entries != NULL) {

        HDassert(md_index->num_entries != 0);

        md_index->entries = H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, md_index->entries);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD__vfd_swmr_index_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_tick_and_idx()
 *
 * Purpose:     Retrieve tick_num, num_entries and index from the metadata
 *              file
 *
 *              --If the parameter "reload_hdr_and_index" is true, load and
 *                decode the header and index via
 *                H5FD__vfd_swmr_load_hdr_and_idx(), which may replace the
 *                VFD's local copies of header and index with the
 *                latest info read.
 *
 *              --Return tick_num, num_entries and index from the VFD's
 *                local copies.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_vfd_swmr_get_tick_and_idx(H5FD_t *_file, hbool_t reload_hdr_and_index, uint64_t *tick_ptr,
                               uint32_t *num_entries_ptr, H5FD_vfd_swmr_idx_entry_t index[])
{
    H5FD_vfd_swmr_t *file      = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t           ret_value = SUCCEED;                  /* Return value  */

    HDassert(index == NULL || num_entries_ptr != NULL);

    FUNC_ENTER_NOAPI(FAIL)

    /* Load and decode the header and index as indicated */
    if (reload_hdr_and_index && H5FD__vfd_swmr_load_hdr_and_idx(file, FALSE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, "unable to load/decode md header and index")

    /* Return tick_num */
    if (tick_ptr != NULL)
        *tick_ptr = file->md_header.tick_num;

    if (index != NULL) {

        if (*num_entries_ptr < file->md_index.num_entries)
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, "not enough space to copy index")

        HDmemcpy(index, file->md_index.entries,
                 (file->md_index.num_entries * sizeof(file->md_index.entries[0])));
    }

    if (num_entries_ptr != NULL)
        *num_entries_ptr = file->md_index.num_entries;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD_vfd_swmr_get_tick_and_idx() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_dump_status
 *
 * Purpose:     Dump a variety of information about the vfd swmr reader
 *              vfd to stderr for debugging purposes.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_vfd_swmr_dump_status(H5FD_t *_file, uint64_t page)
{
    hbool_t                    in_index = FALSE;
    int                        i        = 0;
    uint32_t                   num_entries;
    H5FD_vfd_swmr_idx_entry_t *index;
    H5FD_vfd_swmr_t *          file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    index       = file->md_index.entries;
    num_entries = file->md_index.num_entries;

    while ((!in_index) && (i < (int)num_entries)) {

        if (index[i].hdf5_page_offset == page)
            in_index = TRUE;

        HDassert((i == 0) || (index[i - 1].hdf5_page_offset < index[i].hdf5_page_offset));

        i++;
    }

    HDfprintf(stderr, "fd: tick = %" PRIu64 ", index_len = %" PRIu32 ", page %" PRIu64 " in index = %s.\n",
              file->md_index.tick_num, num_entries, page, in_index ? "true" : "false");

    FUNC_LEAVE_NOAPI_VOID

} /* H5FD_vfd_swmr_dump_status() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_set_pb_configured
 *
 * Purpose:     Set the pb_configured field.
 *
 *              This notifies the VFD that the page buffer is configured,
 *              and that therefore all reads to the metadata file should
 *              read complete pages or multi-page metadata entries.
 *
 *              This function in necessary because we haven't modified
 *              the file open code to configure the page buffer prior
 *              to any file I/O when opening a file VFD SWMR reader.
 *              Once this is done, this function should be removed.
 *
 * Return:      VOID
 *
 * Programmer:  JRM -- 1/29/19
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_vfd_swmr_set_pb_configured(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    file->pb_configured = TRUE;

    FUNC_LEAVE_NOAPI_VOID

} /* H5FD_vfd_swmr_set_pb_configured() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_record_elapsed_ticks
 *
 * Purpose:     In the histogram of ticks spent in API calls, increase
 *              the bucket for `elapsed` ticks by one.
 *
 * Return:      VOID
 *
 * Programmer:  JRM -- 1/29/19
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_vfd_swmr_record_elapsed_ticks(H5FD_t *_file, uint64_t elapsed)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;

    uint32_t elapsed_idx = MIN(elapsed, file->api_elapsed_nbuckets);

    file->api_elapsed_ticks[elapsed_idx]++;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5FD_vfd_swmr_record_elapsed_ticks() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_md_name
 *
 * Purpose:     To retrieve the metadata file's full name
 *
 * Return:      VOID
 *
 * Programmer:  Vailin Choi; 02/18/2022
 *
 *-------------------------------------------------------------------------
 *
 */
void
H5FD_vfd_swmr_get_md_path_name(H5FD_t *_file, char **name)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;

    *name = H5MM_xstrdup(file->md_file_path_name);

    FUNC_LEAVE_NOAPI_VOID

} /* H5FD_vfd_swmr_get_md_path_name() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_make_believe
 *
 * Purpose:     To retrieve the value of make_believe
 *
 * Return:      TRUE/FALSE
 *
 * Programmer:  Vailin Choi; 02/18/2022
 *
 *-------------------------------------------------------------------------
 *
 */
hbool_t
H5FD_vfd_swmr_get_make_believe(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    FUNC_LEAVE_NOAPI(file->make_believe)

} /* H5FD_vfd_swmr_get_make_believe() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_set_make_believe
 *
 * Purpose:     To set the VFD's make believe to the
 *              parameter "make_believe"
 *
 * Return:      VOID
 *
 * Programmer:  Vailin Choi; 02/18/2022
 *
 *-------------------------------------------------------------------------
 *
 */
void
H5FD_vfd_swmr_set_make_believe(H5FD_t *_file, hbool_t make_believe)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;

    HDassert(file);

    /* Set return value */
    file->make_believe = make_believe;

    FUNC_LEAVE_NOAPI_VOID

} /* H5FD_vfd_swmr_make_believe() */

/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_assess_make_believe
 *
 * Purpose:     To determine whether continuing with make_believe or not.
 *              Return TRUE:
 *              --if metadata file does not exist, continue with make_believe
 *              Return FALSE:
 *              --if metadata file exists and can be opened successfully,
 *                discontinue with make_believe
 *              Return FAIL:
 *              --error in opening the metadata file
 *
 *
 * Return:      TRUE/FALSE/FAIL
 *
 * Programmer:  Vailin Choi; 02/18/2022
 *-------------------------------------------------------------------------
 */
htri_t
H5FD_vfd_swmr_assess_make_believe(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    h5_retry_t       retry;
    hbool_t          do_try;           /* more tries remain */
    htri_t           ret_value = TRUE; /* Return value  */

    FUNC_ENTER_NOAPI(FALSE)

    HDassert(file->make_believe);

    if (HDaccess(file->md_file_path_name, F_OK) >= 0) {
        /* MD file exists now, proceed to open it */
        HDassert(file->md_fd < 0);

        /* Retry on opening the metadata file */
        for (do_try         = H5_retry_init(&retry, H5FD_VFD_SWMR_MD_FILE_RETRY_MAX, H5_RETRY_DEFAULT_MINIVAL,
                                    H5_RETRY_DEFAULT_MAXIVAL);
             do_try; do_try = H5_retry_next(&retry)) {
            if ((file->md_fd = HDopen(file->md_file_path_name, O_RDONLY)) >= 0)
                break;
        }

        /* Exhaust all retries for opening the md file */
        if (!do_try)
            HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, FAIL,
                        "unable to open the metadata file after all retry attempts");

        /* Succeed in opening the MD file, discontinue make_believe */
        ret_value = FALSE;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_vfd_swmr_assess_make_believe() */
