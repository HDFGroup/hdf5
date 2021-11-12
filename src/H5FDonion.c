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
 * Onion Virtual File Driver (VFD)
 *
 * Purpose:     Provide in-file provenance and revision/version control.
 */

/* This source code file is part of the H5FD driver module */
#include "H5FDdrvr_module.h"

#include "H5private.h"      /* Generic Functions           */
#include "H5Eprivate.h"     /* Error handling              */
#include "H5Fprivate.h"     /* Files                       */
#include "H5FDprivate.h"    /* File drivers                */
#include "H5FDonion.h"      /* Onion file driver           */
#include "H5FDonion_priv.h" /* Onion file driver internals */
#include "H5FLprivate.h"    /* Free Lists                  */
#include "H5Iprivate.h"     /* IDs                         */
#include "H5MMprivate.h"    /* Memory management           */

/* The driver identification number, initialized at runtime
 */
static hid_t H5FD_ONION_g = 0;

/******************************************************************************
 *
 * Structure:   H5FD_onion_t
 *
 * Purpose:     Store information required to manage an onionized file.
 *              This structure is created when such a file is "opened" and
 *              discarded when it is "closed".
 *
 * Programmer:  Jacob "Jake" Smith
 *              6 July 2020
 *
 * `pub` (H5FD_t)
 *
 *      Instance of H5FD_t which contains all fields common to all VFDs.
 *      It must be the first item in this structure, since at higher levels,
 *      this structure will be treated as an instance of H5FD_t.
 *
 * `fa` (H5FD_onion_fapl_info_t)
 *
 *      Instance of `H5FD_onion_fapl_info_t` containing the configuration data
 *      needed to "open" the HDF5 file.
 *
 * `backing_canon` (H5FD_t *)
 *
 *      Virtual file handle for the canonical (i.e., logical HDF5) file in the
 *      backing store.
 *
 * `backing_onion` (H5FD_t *)
 *
 *      Virtual file handle for the onion file in the backing store.
 *      NULL if not set to use the single, separate storage target. (TODO)
 *
 * `backing_recov` (H5FD_t *)
 *
 *      Virtual file handle for the whole-history recovery file.
 *
 * `name_recov` (char *)
 *
 *      String allocated and populated on file-open in write mode and freed on
 *      file-close, stores the path/name of the 'recovery' file. The file
 *      created at this location is to be removed upon succesful file-close
 *      from write mode.
 *
 * `is_open_rw` (hbool_t)
 *
 *      Remember whether the file was opened in a read-write mode.
 *
 * `page_align_history` (hbool_t)
 *
 *      Remember whether onion-writes must be aligned to page boundaries.
 *
 * `header` (struct H5FD__onion_history_header)
 *
 *      In-memory copy of the onion history data header.
 *
 * `summary` (struct H5FD__onion_whole_history)
 *
 *      In-memory copy of the onion history "whole-history".
 *
 * `rev_record` (struct H5FD__onion_revision_record)
 *
 * `history_eof` (haddr_t)
 *
 *      Last byte in the onion history backing file.
 *
 * `rev_index` (struct H5FD__onion_revision_index *)
 *
 *      Index for maintaining modified pages.
 *      Pointer is NULL when the file is not opened in write mode.
 *      Pointer is allocated on open and must be freed on close.
 *      Contents must be merged with the revision record's archival index prior
 *      to commitment of history to backing store.
 *
 * `history_eof` (haddr_t)
 *
 *     Address of first byte past in-use onion history data.
 *
 * `origin_eof` (haddr_t)
 *
 *     Size of the origin canonical file.
 *
 * `logi_eoa` (haddr_t)
 *
 *     Address of first byte past addressed space in logical 'canonical' file.
 *
 * `logi_eof` (haddr_t)
 *
 *     Address of first byte past Last byte in the logical 'canonical' file.
 *     Must be copied into the revision record on close to write onion data.
 *
 ******************************************************************************
 */
typedef struct H5FD_onion_t {
    H5FD_t                             pub;
    H5FD_onion_fapl_info_t             fa;
    H5FD_t *                           backing_canon;
    H5FD_t *                           backing_onion;
    H5FD_t *                           backing_recov;
    char *                             name_recov;
    hbool_t                            is_open_rw;
    hbool_t                            page_align_history;
    struct H5FD__onion_history_header  header;
    struct H5FD__onion_whole_history   summary;
    struct H5FD__onion_revision_record rev_record;
    struct H5FD__onion_revision_index *rev_index;
    haddr_t                            history_eof;
    haddr_t                            origin_eof;
    haddr_t                            logi_eoa;
    haddr_t                            logi_eof;
} H5FD_onion_t;

H5FL_DEFINE_STATIC(H5FD_onion_t);

#define MAXADDR (((haddr_t)1 << (8 * sizeof(HDoff_t) - 1)) - 1)

/* 2^n for uint64_t types -- H5_EXP2 unsafe past 32 bits */
#define U64_EXP2(n) ((uint64_t)1 << (n))

/* Prototypes */
static herr_t  H5FD__onion_close(H5FD_t *);
static haddr_t H5FD__onion_get_eoa(const H5FD_t *, H5FD_mem_t);
static haddr_t H5FD__onion_get_eof(const H5FD_t *, H5FD_mem_t);
static H5FD_t *H5FD__onion_open(const char *, unsigned int, hid_t, haddr_t);
static herr_t  H5FD__onion_read(H5FD_t *, H5FD_mem_t, hid_t, haddr_t, size_t, void *);
static herr_t  H5FD__onion_set_eoa(H5FD_t *, H5FD_mem_t, haddr_t);
static herr_t  H5FD__onion_term(void);
static herr_t  H5FD__onion_write(H5FD_t *, H5FD_mem_t, hid_t, haddr_t, size_t, const void *);

static int      H5FD__onion_archival_index_list_sort_cmp(const void *, const void *);
static herr_t   H5FD__onion_ingest_whole_history(struct H5FD__onion_whole_history *whs_out, H5FD_t *raw_file,
                                                 haddr_t addr, haddr_t size);
static herr_t   H5FD__onion_open_rw(H5FD_onion_t *, unsigned int, haddr_t);
static herr_t   H5FD__onion_revision_index_resize(H5FD__onion_revision_index_t *);
static uint64_t H5FD__onion_whole_history_write(struct H5FD__onion_whole_history *whs, H5FD_t *file_dest,
                                                haddr_t off_start, haddr_t filesize_curr);

static herr_t  H5FD__onion_sb_encode(H5FD_t *_file, char *name /*out*/, unsigned char *buf /*out*/);
static herr_t  H5FD__onion_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf);
static hsize_t H5FD__onion_sb_size(H5FD_t *_file);

static const H5FD_class_t H5FD_onion_g = {
    "onion",                        /* name                 */
    MAXADDR,                        /* maxaddr              */
    H5F_CLOSE_WEAK,                 /* fc_degree            */
    H5FD__onion_term,               /* terminate            */
    H5FD__onion_sb_size,            /* sb_size              */
    H5FD__onion_sb_encode,          /* sb_encode            */
    H5FD__onion_sb_decode,          /* sb_decode            */
    sizeof(H5FD_onion_fapl_info_t), /* fapl_size            */
    NULL,                           /* fapl_get             */
    NULL,                           /* fapl_copy            */
    NULL,                           /* fapl_free            */
    0,                              /* dxpl_size            */
    NULL,                           /* dxpl_copy            */
    NULL,                           /* dxpl_free            */
    H5FD__onion_open,               /* open                 */
    H5FD__onion_close,              /* close                */
    NULL,                           /* cmp                  */
    NULL,                           /* query                */
    NULL,                           /* get_type_map         */
    NULL,                           /* alloc                */
    NULL,                           /* free                 */
    H5FD__onion_get_eoa,            /* get_eoa              */
    H5FD__onion_set_eoa,            /* set_eoa              */
    H5FD__onion_get_eof,            /* get_eof              */
    NULL,                           /* get_handle           */
    H5FD__onion_read,               /* read                 */
    H5FD__onion_write,              /* write                */
    NULL,                           /* flush                */
    NULL,                           /* truncate             */
    NULL,                           /* lock                 */
    NULL,                           /* unlock               */
    NULL,                           /* del */
    H5FD_FLMAP_DICHOTOMY            /* fl_map               */
};

/*-----------------------------------------------------------------------------
 * Function:    H5FD__init_package
 *
 * Purpose:     Initializes any interface-specific data or routines.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__init_package(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if (H5FD_onion_init() < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to initialize Onion VFD");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD__init_package() */

/*-----------------------------------------------------------------------------
 * Function:    H5FD_onion_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:    The driver ID for the onion driver.
 *              Failure:    Negative
 *
 *-----------------------------------------------------------------------------
 */
hid_t
H5FD_onion_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI(FAIL)

    if (H5I_VFL != H5I_get_type(H5FD_ONION_g)) {
        H5FD_ONION_g = H5FD_register(&H5FD_onion_g, sizeof(H5FD_class_t), FALSE);
    }

    /* Set return value */
    ret_value = H5FD_ONION_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_onion_init() */

/*-----------------------------------------------------------------------------
 * Function:    H5FD__onion_term
 *
 * Purpose:     Shut down the Onion VFD.
 *
 * Returns:     SUCCEED (Can't fail)
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_term(void)
{
    FUNC_ENTER_STATIC_NOERR;

    /* Reset VFL ID */
    H5FD_ONION_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED);

} /* end H5FD__onion_term() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5Pget_fapl_onion
 *
 * Purpose:     Copy the Onion configuraiton information from the FAPL at
 *              `fapl_id` to the destination pointer `fa_out`.
 *
 * Return:      Success: Non-negative value (SUCCEED).
 *              Failure: Negative value (FAIL).
 *
 *-----------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_onion(hid_t fapl_id, H5FD_onion_fapl_info_t *fa_out)
{
    const H5FD_onion_fapl_info_t *info_ptr  = NULL;
    H5P_genplist_t *              plist     = NULL;
    herr_t                        ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, fa_out);

    if (NULL == fa_out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL info-out pointer");

    plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS);
    if (NULL == plist)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Not a valid FAPL ID");

    if (H5FD_ONION != H5P_peek_driver(plist))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Incorrect VFL driver");

    info_ptr = (const H5FD_onion_fapl_info_t *)H5P_peek_driver_info(plist);
    if (NULL == info_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad VFL driver info");
    if (H5FD_ONION_FAPL_INFO_MAGIC != info_ptr->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad VFL driver info");

    HDmemcpy(fa_out, info_ptr, sizeof(H5FD_onion_fapl_info_t));

done:
    FUNC_LEAVE_API(ret_value)

} /* end H5Pget_fapl_onion() */

/*-----------------------------------------------------------------------------
 * Function:    H5Pset_fapl_onion
 *
 * Purpose      Set the file access property list at `fapl_id` to use the
 *              Onion virtual file driver with the given configuration.
 *              The info structure may be modified or deleted after this call,
 *              as its contents are copied into the FAPL.
 *
 * Return:      Success: Non-negative value (SUCCEED).
 *              Failure: Negative value (FAIL).
 *
 *-----------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_onion(hid_t fapl_id, const H5FD_onion_fapl_info_t *fa)
{
    H5P_genplist_t *plist     = NULL;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*!", fapl_id, fa);

    plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS);
    if (NULL == plist)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Not a valid FAPL ID");

    if (NULL == fa)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL info pointer");

    if (H5FD_ONION_FAPL_INFO_MAGIC != fa->magic)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid info magic");

    if (H5FD_ONION_FAPL_INFO_VERSION_CURR != fa->version)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid info version");

    if (!POWER_OF_TWO(fa->page_size))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid info page size");

    if (fa->page_size < 1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid info page size");

    if (H5P_DEFAULT != fa->backing_fapl_id) {
        H5P_genplist_t *_plist_ret = NULL;

        H5E_BEGIN_TRY
        {
            _plist_ret = H5P_object_verify(fa->backing_fapl_id, H5P_FILE_ACCESS);
        }
        H5E_END_TRY;
        if (_plist_ret == NULL) {
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid backing fapl id");
        }
    }

    ret_value = H5P_set_driver(plist, H5FD_ONION, (const void *)fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_onion() */

// TODO: comment
static hsize_t
H5FD__onion_sb_size(H5FD_t *_file)
{
    H5FD_onion_t *file      = (H5FD_onion_t *)_file;
    hsize_t       ret_value = 0;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(file);
    HDassert(file->backing_canon);

    if (file->backing_canon)
        ret_value = H5FD_sb_size(file->backing_canon);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__onion_sb_size */

// TODO: comment
static herr_t
H5FD__onion_sb_encode(H5FD_t *_file, char *name /*out*/, unsigned char *buf /*out*/)
{
    H5FD_onion_t *file      = (H5FD_onion_t *)_file;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(file);
    HDassert(file->backing_canon);

    if (file->backing_canon && H5FD_sb_encode(file->backing_canon, name, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTENCODE, FAIL, "unable to encode the superblock in R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__onion_sb_encode */

// TODO: comment
static herr_t
H5FD__onion_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    H5FD_onion_t *file      = (H5FD_onion_t *)_file;
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(file);
    HDassert(file->backing_canon);

    if (H5FD_sb_load(file->backing_canon, name, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "unable to decode the superblock in R/W file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__onion_sb_decode */

/*-----------------------------------------------------------------------------
 *
 * Write in-memory history header to appropriate backing file.
 * Overwrites existing header data.
 *
 * 11 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_update_and_write_header(H5FD_onion_t *file)
{
    uint32_t       _sum      = 0; /* required */
    uint64_t       size      = 0;
    unsigned char *buf       = NULL;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_STATIC;

    /* unset write-lock flag */
    if (file->is_open_rw)
        file->header.flags &= (uint32_t)~H5FD__ONION_HEADER_FLAG_WRITE_LOCK;

    buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_HEADER);
    if (NULL == buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer for updated history header");
    }
    size = H5FD_onion_history_header_encode(&file->header, buf, &_sum);
    if (0 == size) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem encoding updated history header");
    }
    if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, 0, (haddr_t)size, buf) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write updated history header");
    }

done:
    if (buf != NULL)
        H5MM_xfree(buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_update_and_write_header()*/

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_whole_history_write()
 *
 * Purpose:     Encode and write whole-history to file at the given address.
 *
 * Returns:     Success: Number of bytes written to destination file. (nonzero)
 *              Failure: Zero. (0)
 *
 *-----------------------------------------------------------------------------
 */
static uint64_t
H5FD__onion_whole_history_write(struct H5FD__onion_whole_history *whs, H5FD_t *file_dest, haddr_t off_start,
                                haddr_t filesize_curr)
{
    uint32_t       _sum      = 0; /* required */
    uint64_t       size      = 0;
    unsigned char *buf       = NULL;
    uint64_t       ret_value = 0;

    FUNC_ENTER_STATIC;

    buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY +
                      (H5FD__ONION_ENCODED_SIZE_RECORD_POINTER * whs->n_revisions));
    if (NULL == buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, 0, "can't allocate buffer for updated whole-history");
    }
    size = H5FD_onion_whole_history_encode(whs, buf, &_sum);
    if (0 == size) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, 0, "problem encoding updated whole-history");
    }
    if ((size + off_start > filesize_curr) &&
        (H5FD_set_eoa(file_dest, H5FD_MEM_DRAW, off_start + size) < 0)) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, 0, "can't modify EOA for updated whole-history");
    }
    if (H5FDwrite(file_dest, H5FD_MEM_DRAW, H5P_DEFAULT, off_start, size, buf) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, 0, "can't write whole-history as intended");
    }

    ret_value = size;

done:
    if (buf != NULL)
        H5MM_xfree(buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_whole_history_write() */

/*-----------------------------------------------------------------------------
 *
 * Write in-memory whole-history summary to appropriate backing file.
 * Update information in other in-memory components.
 *
 * 11 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_update_and_write_whole_history(H5FD_onion_t *file)
{
    uint64_t size      = 0;
    herr_t   ret_value = SUCCEED;

    FUNC_ENTER_STATIC;

    /* TODO: history EOF may not be correct (under what circumstances?) */

    size = H5FD__onion_whole_history_write(&file->summary, file->backing_onion, file->history_eof,
                                           file->history_eof);
    if (0 == size) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write updated whole-history");
    }
    if (size != file->header.whole_history_size) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "written whole-history differed from expected size");
    }

    /* Is last write operation to history file; no need to extend to page
     * boundary if set to page-align.
     */
    file->history_eof += size;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_update_and_write_whole_history() */

/*-----------------------------------------------------------------------------
 *
 * Write in-memory revision record to appropriate backing file.
 * Update information in other in-memory components.
 *
 * 11 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_commit_new_revision_record(H5FD_onion_t *file)
{
    uint32_t                            _sum      = 0; /* required */
    uint64_t                            size      = 0;
    uint64_t                            phys_addr = 0; /* offset in history file to record start */
    unsigned char *                     buf       = NULL;
    herr_t                              ret_value = SUCCEED;
    struct H5FD__onion_revision_record *rec_p     = &file->rev_record;
    struct H5FD__onion_whole_history *  whs_p     = &file->summary;
    struct H5FD__onion_record_pointer * new_list  = NULL;

    FUNC_ENTER_STATIC;

    time_t     rawtime;
    struct tm *info;
    time(&rawtime);
    info = gmtime(&rawtime);
    strftime(rec_p->time_of_creation, sizeof(rec_p->time_of_creation), "%Y%m%dT%H%M%SZ", info);
    // HDmemcpy(rec_p->time_of_creation, "19411207T190643Z", 16);

    rec_p->logi_eof = file->logi_eof;

    if ((TRUE == file->is_open_rw) && (H5FD_onion_merge_revision_index_into_archival_index(
                                           file->rev_index, &file->rev_record.archival_index) < 0)) {
        HGOTO_ERROR(H5E_VFL, H5E_INTERNAL, FAIL, "unable to update index to write");
    }

    buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_REVISION_RECORD + (size_t)rec_p->comment_size +
                      (size_t)rec_p->username_size +
                      (H5FD__ONION_ENCODED_SIZE_INDEX_ENTRY * rec_p->archival_index.n_entries));
    if (NULL == buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer for encoded revision record");
    }
    size = H5FD_onion_revision_record_encode(rec_p, buf, &_sum);
    if (0 == size) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem encoding revision record");
    }
    phys_addr = file->history_eof;
    if (H5FD_set_eoa(file->backing_onion, H5FD_MEM_DRAW, phys_addr + size) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "can't modify EOA for new revision record");
    }
    if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, phys_addr, size, buf) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write new revision record");
    }

    file->history_eof = phys_addr + size;
    if (TRUE == file->page_align_history)
        file->history_eof =
            (file->history_eof + (file->header.page_size - 1)) & (~(file->header.page_size - 1));

    /* Update whole-history info to accommodate new revision
     */

    if (whs_p->n_revisions == 0) {
        unsigned char *ptr = buf; /* re-use buffer space to compute checksum */

        HDassert(whs_p->record_pointer_list == NULL);
        whs_p->n_revisions         = 1;
        whs_p->record_pointer_list = H5MM_calloc(sizeof(struct H5FD__onion_record_pointer));
        if (NULL == whs_p->record_pointer_list) {
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate temporary record pointer list");
        }
        whs_p->record_pointer_list[0].phys_addr   = phys_addr;
        whs_p->record_pointer_list[0].record_size = size;
        UINT64ENCODE(ptr, phys_addr);
        UINT64ENCODE(ptr, size);
        whs_p->record_pointer_list[0].checksum = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));
        /* TODO: size-reset belongs where? */
        file->header.whole_history_size += H5FD__ONION_ENCODED_SIZE_RECORD_POINTER;
    } /* end if no extant revisions in history */
    else {
        unsigned char *ptr = buf; /* re-use buffer space to compute checksum */

        HDassert(whs_p->record_pointer_list != NULL);

        new_list = H5MM_calloc((whs_p->n_revisions + 1) * sizeof(struct H5FD__onion_record_pointer));
        if (NULL == new_list) {
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "unable to resize record pointer list");
        }
        HDmemcpy(new_list, whs_p->record_pointer_list,
                 sizeof(struct H5FD__onion_record_pointer) * whs_p->n_revisions);
        H5MM_xfree(whs_p->record_pointer_list);
        whs_p->record_pointer_list                                 = new_list;
        new_list                                                   = NULL;
        whs_p->record_pointer_list[whs_p->n_revisions].phys_addr   = phys_addr;
        whs_p->record_pointer_list[whs_p->n_revisions].record_size = size;
        UINT64ENCODE(ptr, phys_addr);
        UINT64ENCODE(ptr, size);
        whs_p->record_pointer_list[whs_p->n_revisions].checksum =
            H5_checksum_fletcher32(buf, (size_t)(ptr - buf));

        file->header.whole_history_size += H5FD__ONION_ENCODED_SIZE_RECORD_POINTER;
        whs_p->n_revisions += 1;
    } /* end if one or more revisions present in history */

    file->header.whole_history_addr = file->history_eof;

done:
    if (buf != NULL)
        H5MM_xfree(buf);
    if (new_list != NULL)
        H5MM_xfree(new_list);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_commit_new_revision_record() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_close
 *
 * Purpose:     Close an onionized file
 *
 * Return:      SUCCEED/FAIL
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_close(H5FD_t *_file)
{
    H5FD_onion_t *file      = (H5FD_onion_t *)_file;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(file);

    if (H5FD_ONION_STORE_TARGET_ONION == file->fa.store_target) {

        HDassert(file->backing_onion);

        if (file->is_open_rw) {

            HDassert(file->backing_recov);

            if (H5FD__onion_commit_new_revision_record(file) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "Can't write revision record to backing store")

            if (H5FD__onion_update_and_write_whole_history(file) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "Can't write whole-history to backing store")

            if (H5FD__onion_update_and_write_header(file) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "Can't write updated header to backing store")
        }
    }
    else if (H5FD_ONION_STORE_TARGET_H5 == file->fa.store_target)
        HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, FAIL, "hdf5 store-target not supported")
    else
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid history target")

done:

    /* Destroy things as best we can, even if there were earlier errors */
    if (file->backing_canon)
        if (H5FD_close(file->backing_canon) < 0)
            HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, FAIL, "can't close backing canon file")
    if (file->backing_onion)
        if (H5FD_close(file->backing_onion) < 0)
            HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, FAIL, "can't close backing onion file")
    if (file->backing_recov) {
        if (H5FD_close(file->backing_recov) < 0)
            HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, FAIL, "can't close backing recovery file")
        /* TODO: Use the VFD's del callback instead of remove (this requires
         *       storing a copy of the fapl that was used to open it)
         */
        // if (HDremove(file->name_recov) < 0)
        //    HDONE_ERROR(H5E_VFL, H5E_CANTDELETE, FAIL, "can't remove delete backing recovery file")
        HDremove(file->name_recov);
    }
    if (file->rev_index)
        if (H5FD_onion_revision_index_destroy(file->rev_index) < 0)
            HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, FAIL, "can't close revision index")

    H5MM_xfree(file->name_recov);
    H5MM_xfree(file->summary.record_pointer_list);
    H5MM_xfree(file->rev_record.username);
    H5MM_xfree(file->rev_record.comment);
    H5MM_xfree(file->rev_record.archival_index.list);

    file = H5FL_FREE(H5FD_onion_t, file);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD__onion_close() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_get_eoa
 *
 * Purpose:     Get end-of-address address.
 *
 * Return:      Address of first byte past the addressed space
 *
 *-----------------------------------------------------------------------------
 */
static haddr_t
H5FD__onion_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_onion_t *file = (const H5FD_onion_t *)_file;

    FUNC_ENTER_STATIC_NOERR;

    FUNC_LEAVE_NOAPI(file->logi_eoa)
} /* end H5FD__onion_get_eoa() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_get_eof
 *
 * Purpose:     Get end-of-file address.
 *
 * Return:      Address of first byte past the file-end.
 *
 *-----------------------------------------------------------------------------
 */
static haddr_t
H5FD__onion_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_onion_t *file = (const H5FD_onion_t *)_file;

    FUNC_ENTER_STATIC_NOERR;

    FUNC_LEAVE_NOAPI(file->logi_eof)
} /* end H5FD__onion_get_eof() */

/*-----------------------------------------------------------------------------
 *
 * Sanitize the backing FAPL ID
 *
 *-----------------------------------------------------------------------------
 */
static hid_t
get_legit_fapl_id(hid_t fapl_id)
{
    if (H5P_DEFAULT == fapl_id)
        return H5P_FILE_ACCESS_DEFAULT;
    else if (TRUE == H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
        return fapl_id;
    else
        return H5I_INVALID_HID;
}

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_history_header_decode
 *
 * Purpose:     Attempt to read a buffer and store it as a history-header
 *              structure.
 *
 *              Implementation must correspond with
 *              H5FD_onion_history_header_encode().
 *
 * Return:      Success: Number of bytes read from buffer.
 *              Failure: Zero (0).
 *
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_history_header_decode(unsigned char *buf, struct H5FD__onion_history_header *header)
{
    uint32_t       ui32      = 0;
    uint32_t       sum       = 0;
    uint64_t       ui64      = 0;
    uint8_t *      ui8p      = NULL;
    unsigned char *ptr       = NULL;
    uint64_t       ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(buf != NULL);
    HDassert(header != NULL);
    HDassert(H5FD__ONION_HEADER_MAGIC == header->magic);
    HDassert(H5FD__ONION_HEADER_VERSION_CURR == header->version);

    if (HDstrncmp((const char *)buf, H5FD__ONION_HEADER_SIGNATURE, 4))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid header signature");

    if (buf[4] != H5FD__ONION_HEADER_VERSION_CURR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid header version");

    ptr  = buf + 5;
    ui32 = 0;
    HDmemcpy(&ui32, ptr, 3);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, header->flags);
    ptr += 3;

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, header->page_size);
    ptr += 4;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT32DECODE(ui8p, header->origin_eof);
    ptr += 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT32DECODE(ui8p, header->whole_history_addr);
    ptr += 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT32DECODE(ui8p, header->whole_history_size);
    ptr += 8;

    sum = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, header->checksum);
    ptr += 4;

    if (sum != header->checksum)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "checksum mismatch");

    ret_value = (uint64_t)(ptr - buf);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_history_header_decode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_history_header_encode
 *
 * Purpose:     Write history-header structure to the given buffer.
 *              All multi-byte elements are stored in little-endian word order.
 *
 *              Implementation must correspond with
 *              H5FD_onion_history_header_decode().
 *
 *              The destination buffer must be sufficiently large to hold the
 *              encoded contents (H5FD__ONION_ENCODED_SIZE_HEADER).
 *
 * Return:      Number of bytes written to buffer.
 *              The checksum of the generated buffer contents (excluding the
 *              checksum itself) is stored in the pointer `sum_out`).
 *
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_history_header_encode(struct H5FD__onion_history_header *header, unsigned char *buf,
                                 uint32_t *sum_out)
{
    unsigned char *ptr       = buf;
    uint64_t       ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(buf != NULL);
    HDassert(sum_out != NULL);
    HDassert(header != NULL);
    HDassert(H5FD__ONION_HEADER_MAGIC == header->magic);
    HDassert(H5FD__ONION_HEADER_VERSION_CURR == header->version);
    HDassert(0 == (header->flags & 0xFF000000)); /* max three bits long */

    HDmemcpy(ptr, H5FD__ONION_HEADER_SIGNATURE, 4);
    ptr += 4;
    HDmemcpy(ptr, (unsigned char *)&header->version, 1);
    ptr += 1;
    UINT32ENCODE(ptr, header->flags);
    ptr -= 1; /* truncate to three bytes */
    UINT32ENCODE(ptr, header->page_size);
    UINT64ENCODE(ptr, header->origin_eof);
    UINT64ENCODE(ptr, header->whole_history_addr);
    UINT64ENCODE(ptr, header->whole_history_size);
    *sum_out = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));
    UINT32ENCODE(ptr, *sum_out);
    ret_value = (uint64_t)(ptr - buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_history_header_encode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_revision_record_decode
 *
 * Purpose:     Attempt to read a buffer and store it as a revision record
 *              structure.
 *
 *              Implementation must correspond with
 *              H5FD_onion_revision_record_encode().
 *
 *              MUST BE CALLED TWICE:
 *              On the first call, n_entries, comment_size, and username_size
 *              in the destination structure must all all be zero, and their
 *              respective variable-length components (index_entry_list,
 *              comment, username) must all be NULL.
 *
 *              If the buffer is well-formed, the destinatino structure is
 *              tentatively populated with fixed-size values, and the number of
 *              bytes read are returned.
 *
 *              Prior to the second call, the user must allocate space for the
 *              variable-length components, in accordance with the associated
 *              indicators (array of index-entry structures for
 *              index_entry_list, of size n_entries; character arrays for
 *              username and comment, allocated with the *_size number of
 *              bytes -- space for NULL-terminator is included in _size).
 *
 *              Then the decode operation is called a second time, and all
 *              components will be populated (and again number of bytes read is
 *              returned).
 *
 * Return:      Success: Number of bytes read from buffer.
 *              Failure: Zero (0).
 *
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_revision_record_decode(unsigned char *buf, struct H5FD__onion_revision_record *record)
{
    uint32_t       ui32          = 0;
    uint32_t       page_size     = 0;
    uint32_t       sum           = 0;
    uint64_t       ui64          = 0;
    uint64_t       n_entries     = 0;
    uint32_t       username_size = 0;
    uint32_t       comment_size  = 0;
    uint8_t *      ui8p          = NULL;
    unsigned char *ptr           = NULL;
    uint64_t       ret_value     = 0;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(buf != NULL);
    HDassert(record != NULL);
    HDassert(H5FD__ONION_REVISION_RECORD_MAGIC == record->magic);
    HDassert(H5FD__ONION_REVISION_RECORD_VERSION_CURR == record->version);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_MAGIC == record->archival_index.magic);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR == record->archival_index.version);

    if (HDstrncmp((const char *)buf, H5FD__ONION_REVISION_RECORD_SIGNATURE, 4))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid signature");

    if (H5FD__ONION_REVISION_RECORD_VERSION_CURR != buf[4])
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid record version");

    ptr = buf + 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT64DECODE(ui8p, record->revision_id);
    ptr += 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT64DECODE(ui8p, record->parent_revision_id);
    ptr += 8;

    HDmemcpy(record->time_of_creation, ptr, 16);
    ptr += 16;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT64DECODE(ui8p, record->logi_eof);
    ptr += 8;

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, page_size);
    ptr += 4;

    if (page_size == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "page size is zero");
    if (!POWER_OF_TWO(page_size))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "page size not power of two");

    for (record->archival_index.page_size_log2 = 0;
         (((uint32_t)1 << record->archival_index.page_size_log2) & page_size) == 0;
         record->archival_index.page_size_log2++)
        ;

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, record->user_id);
    ptr += 4;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT64DECODE(ui8p, n_entries);
    ptr += 8;

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, username_size);
    ptr += 4;

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, comment_size);
    ptr += 4;

    if (record->archival_index.n_entries == 0) {
        record->archival_index.n_entries = n_entries;
        ptr += H5FD__ONION_ENCODED_SIZE_INDEX_ENTRY * n_entries;
    }
    else if (n_entries != record->archival_index.n_entries) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "n_entries in archival index does not match decoded");
    }
    else {
        struct H5FD__onion_index_entry *entry_p = NULL;
        size_t                          i       = 0;
        if (record->archival_index.list == NULL) {
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "no archival index entry list");
        }
        for (i = 0; i < n_entries; i++) {
            entry_p = &record->archival_index.list[i];

            HDmemcpy(&ui64, ptr, 8);
            ui8p = (uint8_t *)&ui64;
            UINT64DECODE(ui8p, entry_p->logi_page);
            ptr += 8;
            /* logi_page actually encoded as address; check and convert */
            if (entry_p->logi_page & (page_size - 1)) {
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "logical address does not align with page size");
            }
            entry_p->logi_page = entry_p->logi_page >> record->archival_index.page_size_log2;

            HDmemcpy(&ui64, ptr, 8);
            ui8p = (uint8_t *)&ui64;
            UINT64DECODE(ui8p, entry_p->phys_addr);
            ptr += 8;

            HDmemcpy(&ui32, ptr, 4);
            ui8p = (uint8_t *)&ui32;
            UINT32DECODE(ui8p, sum);
            ptr += 4;

            ui32 = H5_checksum_fletcher32((ptr - 20), 16);
            if (ui32 != sum) {
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "index entry checksum mismatch");
            }
        }
    }

    if (record->username_size == 0) {
        if (record->username != NULL) {
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "username pointer prematurely allocated");
        }
        record->username_size = username_size;
    }
    else {
        if (record->username == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "no username pointer");
        HDmemcpy(record->username, ptr, username_size);
    }
    ptr += username_size;

    if (record->comment_size == 0) {
        if (record->comment != NULL) {
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "comment pointer prematurely allocated");
        }
        record->comment_size = comment_size;
    }
    else {
        if (record->comment == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "no comment pointer");
        HDmemcpy(record->comment, ptr, comment_size);
    }
    ptr += comment_size;

    sum = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, record->checksum);
    ptr += 4;

    if (sum != record->checksum) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "checksum mismatch");
    }

    ret_value = (uint64_t)(ptr - buf);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_revision_record_decode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_revision_record_encode
 *
 * Purpose:     Write revision-record structure to the given buffer.
 *              All multi-byte elements are stored in little-endian word order.
 *
 *              Implementation must correspond with
 *              H5FD_onion_revision_record_decode().
 *
 *              The destination buffer must be sufficiently large to hold the
 *              encoded contents.
 *              (Hint: `sizeof(revision-record-struct) + comment-size +
 *              username-size + sizeof(index-entry-struct) * n_entries)`
 *              guarantees ample/excess space.)
 *
 * Return:      Number of bytes written to buffer.
 *              The checksum of the generated buffer contents (excluding the
 *              checksum itself) is stored in the pointer `sum_out`).
 *
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_revision_record_encode(struct H5FD__onion_revision_record *record, unsigned char *buf,
                                  uint32_t *sum_out)
{
    unsigned char *ptr       = buf;                       /* original pointer */
    uint32_t       vers_u32  = (uint32_t)record->version; /* pad out unused bytes */
    uint32_t       page_size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(sum_out != NULL);
    HDassert(buf != NULL);
    HDassert(record != NULL);
    HDassert(vers_u32 < 0x100);
    HDassert(H5FD__ONION_REVISION_RECORD_MAGIC == record->magic);
    HDassert(H5FD__ONION_REVISION_RECORD_VERSION_CURR == record->version);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_MAGIC == record->archival_index.magic);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR == record->archival_index.version);

    page_size = (uint32_t)(1 << record->archival_index.page_size_log2);

    HDmemcpy(ptr, H5FD__ONION_REVISION_RECORD_SIGNATURE, 4);
    ptr += 4;
    UINT32ENCODE(ptr, vers_u32);
    UINT64ENCODE(ptr, record->revision_id);
    UINT64ENCODE(ptr, record->parent_revision_id);
    HDmemcpy(ptr, record->time_of_creation, 16);
    ptr += 16;
    UINT64ENCODE(ptr, record->logi_eof);
    UINT32ENCODE(ptr, page_size);
    UINT32ENCODE(ptr, record->user_id);
    UINT64ENCODE(ptr, record->archival_index.n_entries);
    UINT32ENCODE(ptr, record->username_size);
    UINT32ENCODE(ptr, record->comment_size);

    if (record->archival_index.n_entries > 0) {
        uint64_t i              = 0;
        uint64_t page_size_log2 = record->archival_index.page_size_log2;

        HDassert(record->archival_index.list != NULL);
        for (i = 0; i < record->archival_index.n_entries; i++) {
            uint32_t                        sum       = 0;
            struct H5FD__onion_index_entry *entry_p   = NULL;
            uint64_t                        logi_addr = 0;

            entry_p   = &record->archival_index.list[i];
            logi_addr = entry_p->logi_page << page_size_log2;

            UINT64ENCODE(ptr, logi_addr);
            UINT64ENCODE(ptr, entry_p->phys_addr);
            sum = H5_checksum_fletcher32((ptr - 16), 16);
            UINT32ENCODE(ptr, sum);
        }
    }

    if (record->username_size > 0) {
        HDassert(record->username != NULL && *record->username != '\0');
        HDmemcpy(ptr, record->username, record->username_size);
        ptr += record->username_size;
    }

    if (record->comment_size > 0) {
        HDassert(record->comment != NULL && *record->comment != '\0');
        HDmemcpy(ptr, record->comment, record->comment_size);
        ptr += record->comment_size;
    }

    *sum_out = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));
    UINT32ENCODE(ptr, *sum_out);

    FUNC_LEAVE_NOAPI((uint64_t)(ptr - buf));
} /* end H5FD_onion_revision_record_encode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_whole_history_decode
 *
 * Purpose:     Attempt to read a buffer and store it as a whole-history
 *              structure.
 *
 *              Implementation must correspond with
 *              H5FD_onion_whole_history_encode().
 *
 *              MUST BE CALLED TWICE:
 *              On the first call, n_records in the destination structure must
 *              be zero, and record_pointer_list be NULL.
 *
 *              If the buffer is well-formed, the destination structure is
 *              tentatively populated with fixed-size values, and the number of
 *              bytes read are returned.
 *
 *              Prior to the second call, the user must allocate space for
 *              record_pointer_list to hold n_records record-pointer structs.
 *
 *              Then the decode operation is called a second time, and all
 *              components will be populated (and again number of bytes read is
 *              returned).
 *
 * Return:      Success: Number of bytes read from buffer.
 *              Failure: Zero (0).
 *
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_whole_history_decode(unsigned char *buf, struct H5FD__onion_whole_history *summary)
{
    uint32_t       ui32        = 0;
    uint32_t       sum         = 0;
    uint64_t       ui64        = 0;
    uint64_t       n_revisions = 0;
    uint8_t *      ui8p        = NULL;
    unsigned char *ptr         = NULL;
    uint64_t       ret_value   = 0;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(buf != NULL);
    HDassert(summary != NULL);
    HDassert(H5FD__ONION_WHOLE_HISTORY_MAGIC == summary->magic);
    HDassert(H5FD__ONION_WHOLE_HISTORY_VERSION_CURR == summary->version);

    if (HDstrncmp((const char *)buf, "OWHS", 4))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid signature");

    if (H5FD__ONION_WHOLE_HISTORY_VERSION_CURR != buf[4])
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "invalid version");

    ptr = buf + 8;

    HDmemcpy(&ui64, ptr, 8);
    ui8p = (uint8_t *)&ui64;
    UINT64DECODE(ui8p, n_revisions);
    ptr += 8;

    if (0 == summary->n_revisions) {
        summary->n_revisions = n_revisions;
        ptr += H5FD__ONION_ENCODED_SIZE_RECORD_POINTER * n_revisions;
    }
    else {
        uint64_t i = 0;

        if (summary->n_revisions != n_revisions)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0,
                        "summary argument suggests different revision count than encoded buffer");
        if (NULL == summary->record_pointer_list)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "list is NULL -- cannot populate");

        for (i = 0; i < n_revisions; i++) {
            struct H5FD__onion_record_pointer *rpp = &summary->record_pointer_list[i];

            HDmemcpy(&ui64, ptr, 8);
            ui8p = (uint8_t *)&ui64;
            UINT64DECODE(ui8p, rpp->phys_addr);
            ptr += 8;

            HDmemcpy(&ui64, ptr, 8);
            ui8p = (uint8_t *)&ui64;
            UINT64DECODE(ui8p, rpp->record_size);
            ptr += 8;

            HDmemcpy(&ui32, ptr, 4);
            ui8p = (uint8_t *)&ui32;
            UINT64DECODE(ui8p, rpp->checksum);
            ptr += 4;
        }
    }

    sum = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));

    HDmemcpy(&ui32, ptr, 4);
    ui8p = (uint8_t *)&ui32;
    UINT32DECODE(ui8p, summary->checksum);
    ptr += 4;

    if (sum != summary->checksum)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, 0, "checksum mismatch");

    ret_value = (uint64_t)(ptr - buf);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_whole_history_decode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_whole_history_encode
 *
 * Purpose:     Write whole-history structure to the given buffer.
 *              All multi-byte elements are stored in little-endian word order.
 *
 *              Implementation must correspond with
 *              H5FD_onion_whole_history_decode().
 *
 *              The destination buffer must be sufficiently large to hold the
 *              encoded contents.
 *              (Hint: `sizeof(whole-history-struct) +
 *              sizeof(record-pointer-struct) * n_records)` guarantees
 *              ample/excess space.)
 *
 * Return:      Number of bytes written to buffer.
 *              The checksum of the generated buffer contents (excluding the
 *              checksum itself) is stored in the pointer `sum_out`).
 *
 *-----------------------------------------------------------------------------
 */
uint64_t
H5FD_onion_whole_history_encode(struct H5FD__onion_whole_history *summary, unsigned char *buf,
                                uint32_t *sum_out)
{
    unsigned char *ptr      = buf;
    uint32_t       vers_u32 = (uint32_t)summary->version; /* pad out unused bytes */

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(summary != NULL);
    HDassert(H5FD__ONION_WHOLE_HISTORY_MAGIC == summary->magic);
    HDassert(H5FD__ONION_WHOLE_HISTORY_VERSION_CURR == summary->version);
    HDassert(buf != NULL);
    HDassert(sum_out != NULL);

    HDmemcpy(ptr, H5FD__ONION_WHOLE_HISTORY_SIGNATURE, 4);
    ptr += 4;
    UINT32ENCODE(ptr, vers_u32);
    UINT64ENCODE(ptr, summary->n_revisions);
    if (summary->n_revisions > 0) {
        uint64_t i = 0;

        HDassert(summary->record_pointer_list != NULL); /* TODO: error? */
        for (i = 0; i < summary->n_revisions; i++) {
            UINT64ENCODE(ptr, summary->record_pointer_list[i].phys_addr);
            UINT64ENCODE(ptr, summary->record_pointer_list[i].record_size);
            UINT32ENCODE(ptr, summary->record_pointer_list[i].checksum);
        }
    }
    *sum_out = H5_checksum_fletcher32(buf, (size_t)(ptr - buf));
    UINT32ENCODE(ptr, *sum_out);

    FUNC_LEAVE_NOAPI((uint64_t)(ptr - buf));
} /* end H5FD_onion_whole_history_encode() */

/*-----------------------------------------------------------------------------
 *
 * Populate user_id and username (string) in revision record pointer.
 * Assumes that the username string pointer arrives as NULL;
 * Allocated username string must be manually freed when done.
 *
 * 11 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_set_userinfo_in_record(struct H5FD__onion_revision_record *rec_p)
{
    uid_t          uid       = 0;
    struct passwd *user_info = NULL;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_STATIC;

    uid = HDgetuid();

    HDassert(0 == ((uint64_t)uid & 0xFFFFFFFF00000000)); /* fits uint32_t */
    rec_p->user_id = (uint32_t)uid;

    if (NULL == (user_info = HDgetpwuid(uid)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "can't get user info")

    rec_p->username_size = (uint32_t)HDstrlen(user_info->pw_name) + 1;

    if (NULL == (rec_p->username = H5MM_malloc(sizeof(char) * rec_p->username_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate space for username string")

    HDmemcpy(rec_p->username, user_info->pw_name, rec_p->username_size);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_set_userinfo_in_record() */

/*-----------------------------------------------------------------------------
 *
 * Create/truncate HDF5 and onion data for a fresh file.
 *
 * Speical open operation required to instantiate the canonical file and
 * history simultaneously. If successful, the required backing files are
 * craeated and given initial population on the backing store, and the Onion
 * virtual file handle is set; open effects a write-mode open.
 *
 * Cannot create 'template' history and proceeed with normal write-mode open,
 * as this would in effect create an empty first revision, making the history
 * unintuitive. (create file -> initialize and commit empty first revision
 * (revision 0); any data written to file during the 'create' open, as seen by
 * the user, would be in the second revision (revision 1).)
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_create_truncate_onion(H5FD_onion_t *file, const char *filename, const char *name_onion,
                                  const char *name_recovery, unsigned int flags, haddr_t maxaddr)
{
    hid_t                               backing_fapl_id = H5I_INVALID_HID;
    struct H5FD__onion_history_header * hdr_p           = NULL;
    struct H5FD__onion_whole_history *  whs_p           = NULL;
    struct H5FD__onion_revision_record *rec_p           = NULL;
    unsigned char *                     buf             = NULL;
    uint64_t                            size            = 0;
    herr_t                              ret_value       = SUCCEED;

    FUNC_ENTER_STATIC;

    HDassert(file != NULL);

    hdr_p = &file->header;
    whs_p = &file->summary;
    rec_p = &file->rev_record;

    hdr_p->flags = H5FD__ONION_HEADER_FLAG_WRITE_LOCK;
    if (H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_DIVERGENT_HISTORY & file->fa.creation_flags)
        hdr_p->flags |= H5FD__ONION_HEADER_FLAG_DIVERGENT_HISTORY;
    if (H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT & file->fa.creation_flags)
        hdr_p->flags |= H5FD__ONION_HEADER_FLAG_PAGE_ALIGNMENT;

    hdr_p->origin_eof = 0;

    if (H5FD__onion_set_userinfo_in_record(rec_p) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Can't record user info");

    backing_fapl_id = get_legit_fapl_id(file->fa.backing_fapl_id);
    if (H5I_INVALID_HID == backing_fapl_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid backing FAPL ID");

    /* Create backing files for onion history
     */

    file->backing_canon = H5FD_open(filename, flags, backing_fapl_id, maxaddr);
    if (NULL == file->backing_canon) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "cannot open the backing file");
    }

    file->backing_onion = H5FD_open(name_onion, flags, backing_fapl_id, maxaddr);
    if (NULL == file->backing_onion) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "cannot open the backing onion file");
    }

    file->backing_recov = H5FD_open(name_recovery, flags, backing_fapl_id, maxaddr);
    if (NULL == file->backing_recov) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "cannot open the backing file");
    }

    /* Write "empty" .h5 file contents (signature ONIONEOF) */

    if (H5FD_set_eoa(file->backing_canon, H5FD_MEM_DRAW, 8) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't extend EOA");
    /* must use public API to correclty set DXPL context :( */
    if (H5FDwrite(file->backing_canon, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 8, "ONIONEOF") < 0) {
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "cannot write header to the backing h5 file");
    }

    /* Write nascent whole-history summary (with no revisions) to "recovery" */

    if (NULL == (buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer")
    size = H5FD_onion_whole_history_encode(whs_p, buf, &whs_p->checksum);
    if (H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY != size)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't encode whole-history");
    if (H5FD_set_eoa(file->backing_recov, H5FD_MEM_DRAW, size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't extend EOA");
    /* Must use public API to correclty set DXPL context :(
     * TODO: Revisit this...
     */
    if (H5FDwrite(file->backing_recov, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "cannot write summary to the backing recovery file")
    hdr_p->whole_history_size = size; /* record for later use */
    H5MM_xfree(buf);
    buf = NULL;

    /* Write history header with "no" whole-history summary to history.
     * Size of the "recovery" history recorded for later use on close.
     */

    if (NULL == (buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_HEADER)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer")
    size = H5FD_onion_history_header_encode(hdr_p, buf, &hdr_p->checksum);
    if (H5FD__ONION_ENCODED_SIZE_HEADER != size)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "can't encode history header");
    if (H5FD_set_eoa(file->backing_onion, H5FD_MEM_DRAW, size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't extend EOA");
    /* Must use public API to correctly set DXPL context :(
     * TODO: Revisit this...
     */
    if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "cannot write header to the backing onion file")
    file->history_eof = (haddr_t)size;
    if (TRUE == file->page_align_history)
        file->history_eof = (file->history_eof + (hdr_p->page_size - 1)) & (~(hdr_p->page_size - 1));

    rec_p->archival_index.list = NULL;

    if (NULL == (file->rev_index = H5FD_onion_revision_index_init(file->fa.page_size)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize revision index")

done:
    if (buf != NULL)
        H5MM_xfree(buf);

    if (FAIL == ret_value)
        HDremove(name_recovery); /* destroy new temp file, if 'twas created */

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_create_truncate_onion() */

/*-----------------------------------------------------------------------------
 *
 * Read and decode the history header information from `raw_file` at `addr`,
 * and store the decoded information in the structure at `hdr_out`.
 *
 * 12 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_ingest_history_header(struct H5FD__onion_history_header *hdr_out, H5FD_t *raw_file, haddr_t addr)
{
    unsigned char *buf       = NULL;
    herr_t         ret_value = SUCCEED;
    haddr_t        size      = (haddr_t)H5FD__ONION_ENCODED_SIZE_HEADER;
    uint32_t       sum       = 0;

    FUNC_ENTER_STATIC;

    if (H5FD_get_eof(raw_file, H5FD_MEM_DRAW) < (addr + size)) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "header indicates whole-history beyond EOF");
    }

    buf = H5MM_malloc(sizeof(char) * size);
    if (NULL == buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer space");
    }

    if (H5FD_set_eoa(raw_file, H5FD_MEM_DRAW, (addr + size)) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "can't modify EOA");
    }

    if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, addr, size, buf) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't read history header from file");
    }

    if (H5FD_onion_history_header_decode(buf, hdr_out) == 0) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "can't decode history header");
    }

    sum = H5_checksum_fletcher32(buf, size - 4);
    if (hdr_out->checksum != sum) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "checksum mismatch between buffer and stored");
    }

done:
    H5MM_xfree(buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_ingest_history_header() */

/*-----------------------------------------------------------------------------
 *
 * Read and decode the revision_record information from `raw_file` at
 * `addr` .. `addr + size` (taken from whole-history), and store the decoded
 * information in the structure at `r_out`.
 *
 * 13 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_ingest_revision_record(struct H5FD__onion_revision_record *r_out, H5FD_t *raw_file,
                                   const struct H5FD__onion_whole_history *whs, uint64_t revision_id)
{
    unsigned char *buf       = NULL;
    herr_t         ret_value = SUCCEED;
    uint64_t       n         = 0;
    uint64_t       high      = 0;
    uint64_t       low       = 0;
    uint64_t       range     = 0;
    uint32_t       sum       = 0;
    haddr_t        addr      = 0;
    haddr_t        size      = 0;

    FUNC_ENTER_STATIC;

    HDassert(r_out);
    HDassert(raw_file);
    HDassert(whs);
    HDassert(whs->record_pointer_list);
    HDassert(whs->n_revisions > 0);

    high  = whs->n_revisions - 1;
    range = high;
    addr  = (haddr_t)whs->record_pointer_list[high].phys_addr;
    size  = (haddr_t)whs->record_pointer_list[high].record_size;

    /* Initialize r_out
     *
     * TODO: This function should completely initialize r_out. Relying on
     *       other code to some of the work while we just paste over parts
     *       of the struct here is completely bananas.
     */
    r_out->comment             = H5MM_xfree(r_out->comment);
    r_out->username            = H5MM_xfree(r_out->username);
    r_out->archival_index.list = H5MM_xfree(r_out->archival_index.list);

    if (H5FD_get_eof(raw_file, H5FD_MEM_DRAW) < (addr + size)) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "at least one record extends beyond EOF");
    }

#if 0 /* TODO: recovery-open */
/* recovery-open may have EOA below revision record */
    if ((H5FD_get_eoa(raw_file, H5FD_MEM_DRAW) < (addr + size))
    &&  (H5FD_set_eoa(raw_file, H5FD_MEM_DRAW, (addr + size)) < 0))
    {
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL,
                "can't modify EOA");
    }
#endif

    /* Perform binary search on records to find target revision by ID.
     * As IDs are added sequentially, they are "guaranteed" to be sorted.
     */
    while (range > 0) {
        n    = (range / 2) + low;
        addr = (haddr_t)whs->record_pointer_list[n].phys_addr;
        size = (haddr_t)whs->record_pointer_list[n].record_size;

        buf = H5MM_malloc(sizeof(char) * size);
        if (NULL == buf) {
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer space");
        }

        if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, addr, size, buf) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't read revision record from file");
        }

        if (H5FD_onion_revision_record_decode(buf, r_out) != size) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "can't decode revision record (initial)");
        }

        sum = H5_checksum_fletcher32(buf, size - 4);
        if (r_out->checksum != sum) {
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "checksum mismatch between buffer and stored");
        }

        if (revision_id == r_out->revision_id)
            break;

        H5MM_xfree(buf);
        buf = NULL;

        r_out->archival_index.n_entries = 0;
        r_out->comment_size             = 0;
        r_out->username_size            = 0;

        if (r_out->revision_id < revision_id)
            low = (n == high) ? high : n + 1;
        else
            high = (n == low) ? low : n - 1;
        range = high - low;
    } /* end while 'non-leaf' binary search */

    if (range == 0) {
        n    = low;
        addr = (haddr_t)whs->record_pointer_list[n].phys_addr;
        size = (haddr_t)whs->record_pointer_list[n].record_size;

        buf = H5MM_malloc(sizeof(char) * size);
        if (NULL == buf) {
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer space");
        }

        if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, addr, size, buf) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't read revision record from file");
        }

        if (H5FD_onion_revision_record_decode(buf, r_out) != size) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "can't decode revision record (initial)");
        }

        sum = H5_checksum_fletcher32(buf, size - 4);
        if (r_out->checksum != sum) {
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "checksum mismatch between buffer and stored");
        }

        if (revision_id != r_out->revision_id) {
            HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL,
                        "could not find target revision!"); /* TODO: corrupted? */
        }
    } /* end if revision ID at 'leaf' in binary search */

    if (r_out->username_size > 0)
        if (NULL == (r_out->username = H5MM_malloc(sizeof(char) * r_out->username_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate username space")

    if (r_out->comment_size > 0)
        if (NULL == (r_out->comment = H5MM_malloc(sizeof(char) * r_out->comment_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate comment space")

    if (r_out->archival_index.n_entries > 0)
        if (NULL == (r_out->archival_index.list = H5MM_calloc(r_out->archival_index.n_entries *
                                                              sizeof(struct H5FD__onion_index_entry))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate index entry list")

    if (H5FD_onion_revision_record_decode(buf, r_out) != size)
        HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "can't decode revision record (final)")

done:
    H5MM_xfree(buf);
    if (ret_value == FAIL) {
        H5MM_xfree(r_out->comment);
        H5MM_xfree(r_out->username);
        H5MM_xfree(r_out->archival_index.list);
    }

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_ingest_revision_record() */

/*-----------------------------------------------------------------------------
 *
 * Read and decode the whole-history information from `raw_file` at
 * `addr` .. `addr + size` (taken from history header), and store the decoded
 * information in the structure at `whs_out`.
 *
 * If successful, `whs_out->record_pointer_list` is always allocated, even if
 * there is zero revisions.
 *
 * 12 August 2020
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_ingest_whole_history(struct H5FD__onion_whole_history *whs_out, H5FD_t *raw_file, haddr_t addr,
                                 haddr_t size)
{
    unsigned char *buf       = NULL;
    herr_t         ret_value = SUCCEED;
    uint32_t       sum       = 0;

    FUNC_ENTER_STATIC;

    if (H5FD_get_eof(raw_file, H5FD_MEM_DRAW) < (addr + size)) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "header indicates whole-history beyond EOF");
    }

    buf = H5MM_malloc(sizeof(char) * size);
    if (NULL == buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer space");
    }

    if (H5FD_set_eoa(raw_file, H5FD_MEM_DRAW, (addr + size)) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "can't modify EOA");
    }

    if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, addr, size, buf) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't read whole-history from file");
    }

    if (H5FD_onion_whole_history_decode(buf, whs_out) != size) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "can't decode whole-history (initial)");
    }

    sum = H5_checksum_fletcher32(buf, size - 4);
    if (whs_out->checksum != sum) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "checksum mismatch between buffer and stored");
    }

    whs_out->record_pointer_list =
        H5MM_calloc(whs_out->n_revisions * sizeof(struct H5FD__onion_record_pointer));
    if (whs_out->n_revisions > 0 && NULL == whs_out->record_pointer_list) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate record pointer list");
    }

    if (H5FD_onion_whole_history_decode(buf, whs_out) != size) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, FAIL, "can't decode whole-history (final)");
    }

done:
    H5MM_xfree(buf);
    if (ret_value == FAIL) {
        if (whs_out->record_pointer_list != NULL)
            H5MM_xfree(whs_out->record_pointer_list);
    }

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_ingest_whole_history() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_open
 *
 * Purpose:     Open an onionized file.
 *
 * Return:      Success:    A pointer to a new file data structure
 *              Failure:    NULL
 *
 *-----------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__onion_open(const char *filename, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5P_genplist_t *              plist = NULL;
    H5FD_onion_t *                file  = NULL;
    const H5FD_onion_fapl_info_t *fa    = NULL;
    ;
    hid_t   backing_fapl_id = H5I_INVALID_HID;
    char *  name_onion      = NULL;
    char *  name_recovery   = NULL;
    H5FD_t *ret_value       = NULL;

    FUNC_ENTER_STATIC

    /* Check arguments */
    if (!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    HDassert(H5P_DEFAULT != fapl_id);
    if (NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if (NULL == (fa = (const H5FD_onion_fapl_info_t *)H5P_peek_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "bad VFL driver info")

    /* Check for unsupported target values */
    if (H5FD_ONION_STORE_TARGET_H5 == fa->store_target)
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, NULL, "same-file storage not implemented")
    else if (H5FD_ONION_STORE_TARGET_ONION != fa->store_target)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid store target")

    /* Allocate space for the file struct */
    if (NULL == (file = H5FL_CALLOC(H5FD_onion_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate file struct")

    /* Allocate space for onion VFD file names */
    if (NULL == (name_onion = H5MM_malloc(sizeof(char) * (HDstrlen(filename) + 7))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate onion name string")
    HDsnprintf(name_onion, HDstrlen(filename) + 7, "%s.onion", filename);

    if (NULL == (name_recovery = H5MM_malloc(sizeof(char) * (HDstrlen(name_onion) + 10))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate recovery name string")
    HDsnprintf(name_recovery, HDstrlen(name_onion) + 10, "%s.recovery", name_onion);
    file->name_recov = name_recovery;

    if (NULL == (file->name_recov = H5MM_malloc(sizeof(char) * (HDstrlen(name_onion) + 10))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate recovery name string")
    HDsnprintf(file->name_recov, HDstrlen(name_onion) + 10, "%s.recovery", name_onion);

    /* Translate H5P_DEFAULT to a a real fapl ID, if necessary */
    backing_fapl_id = get_legit_fapl_id(file->fa.backing_fapl_id);
    if (H5I_INVALID_HID == backing_fapl_id)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid backing FAPL ID");

    /* Initialize file structure fields */

    HDmemcpy(&(file->fa), fa, sizeof(H5FD_onion_fapl_info_t));

    file->header.magic     = H5FD__ONION_HEADER_MAGIC;
    file->header.version   = H5FD__ONION_HEADER_VERSION_CURR;
    file->header.page_size = file->fa.page_size; /* guarded on FAPL-set */

    file->summary.magic   = H5FD__ONION_WHOLE_HISTORY_MAGIC;
    file->summary.version = H5FD__ONION_WHOLE_HISTORY_VERSION_CURR;

    file->rev_record.magic                  = H5FD__ONION_REVISION_RECORD_MAGIC;
    file->rev_record.version                = H5FD__ONION_REVISION_RECORD_VERSION_CURR;
    file->rev_record.archival_index.magic   = H5FD__ONION_ARCHIVAL_INDEX_MAGIC;
    file->rev_record.archival_index.version = H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR;

    /* Check that the page size is a power of two */
    if ((fa->page_size == 0) || ((fa->page_size & (fa->page_size - 1)) != 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "page size is not a power of two")

    /* Assign the page size */
    /* TODO: Is this really the best way to do this? Why not just store the
     *       page size directly? It looks like this is so we can do bit shifts
     *       instead of division, which is some severly premature optimization
     *       with a major hit on maintainability.
     */
    double log2_page_size                          = HDlog2((double)(fa->page_size));
    file->rev_record.archival_index.page_size_log2 = (uint32_t)log2_page_size;

    /* Proceed with open. */

    if ((H5F_ACC_CREAT | H5F_ACC_TRUNC) & flags) {

        /* Create a new onion file from scratch */

        /* Set flags */
        if (fa->creation_flags & H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT) {
            file->header.flags |= H5FD__ONION_HEADER_FLAG_PAGE_ALIGNMENT;
            file->page_align_history = TRUE;
        }

        /* Truncate and create everything as necessary */
        if (H5FD__onion_create_truncate_onion(file, filename, name_onion, file->name_recov, flags, maxaddr) <
            0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTCREATE, NULL, "unable to create/truncate onionized files")
        file->is_open_rw = TRUE;
    }
    else {

        /* Opening an existing onion file */

        /* Open the existing file using the specified fapl */
        if (NULL == (file->backing_canon = H5FD_open(filename, flags, backing_fapl_id, maxaddr)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "unable to open canonical file (does not exist?)")

        /* Try to open any existing onion file */
        H5E_BEGIN_TRY
        {
            file->backing_onion = H5FD_open(name_onion, flags, backing_fapl_id, maxaddr);
        }
        H5E_END_TRY;

        /* If that didn't work, create a new onion file */
        /* TODO: Move to a new function */
        if (NULL == file->backing_onion) {
            if (H5F_ACC_RDWR & flags) {
                struct H5FD__onion_history_header * hdr_p      = NULL;
                struct H5FD__onion_whole_history *  whs_p      = NULL;
                struct H5FD__onion_revision_record *rec_p      = NULL;
                unsigned char *                     head_buf   = NULL;
                unsigned char *                     wh_buf     = NULL;
                uint64_t                            size       = 0;
                uint64_t                            saved_size = 0;
                haddr_t                             canon_eof  = 0;

                HDassert(file != NULL);

                hdr_p = &file->header;
                whs_p = &file->summary;
                rec_p = &file->rev_record;

                // hdr_p->flags = H5FD__ONION_HEADER_FLAG_WRITE_LOCK;
                if (H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_DIVERGENT_HISTORY & file->fa.creation_flags)
                    hdr_p->flags |= H5FD__ONION_HEADER_FLAG_DIVERGENT_HISTORY;
                if (H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT & file->fa.creation_flags)
                    hdr_p->flags |= H5FD__ONION_HEADER_FLAG_PAGE_ALIGNMENT;

                if (HADDR_UNDEF == (canon_eof = H5FD_get_eof(file->backing_canon, H5FD_MEM_DEFAULT))) {
                    HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, NULL, "cannot get size of canonical file")
                }
                if (H5FD_set_eoa(file->backing_canon, H5FD_MEM_DRAW, canon_eof) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTSET, NULL, "can't extend EOA")
                hdr_p->origin_eof = canon_eof;
                file->logi_eof    = canon_eof;

                if (H5FD__onion_set_userinfo_in_record(rec_p) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "Can't record user info")

                backing_fapl_id = get_legit_fapl_id(file->fa.backing_fapl_id);
                if (H5I_INVALID_HID == backing_fapl_id)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid backing FAPL ID")

                /* Create backing files for onion history */

                if ((file->backing_onion =
                         H5FD_open(name_onion, (H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC),
                                   backing_fapl_id, maxaddr)) == NULL) {
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "cannot open the backing onion file")
                }

                // Write history header with "no" whole-history summary to history.
                hdr_p->whole_history_size = H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY; /* record for later use */
                hdr_p->whole_history_addr =
                    H5FD__ONION_ENCODED_SIZE_HEADER + 1; /* TODO: comment these 2 or do some other way */
                head_buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_HEADER);
                if (NULL == head_buf)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer")
                size = H5FD_onion_history_header_encode(hdr_p, head_buf, &hdr_p->checksum);
                if (H5FD__ONION_ENCODED_SIZE_HEADER != size)
                    HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't encode history header")
                /* must use public API to correclty set DXPL context :( */

                wh_buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY);
                if (NULL == wh_buf)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't allocate buffer")
                saved_size         = size;
                whs_p->n_revisions = 0;
                size = H5FD_onion_whole_history_encode(whs_p, wh_buf, &whs_p->checksum);
                file->header.whole_history_size = size; /* record for later use */
                if (H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY != size) {
                    HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "can't encode whole-history")
                }
                if (H5FD_set_eoa(file->backing_onion, H5FD_MEM_DRAW, saved_size + size + 1) < 0)
                    HGOTO_ERROR(H5E_FILE, H5E_CANTSET, NULL, "can't extend EOA")
                
				/* must use public API to correclty set DXPL context :( */
                if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, 0, saved_size, head_buf) < 0) {
                    HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, NULL,
                                "cannot write header to the backing onion file")
                }

                file->history_eof = (haddr_t)saved_size;
                if (TRUE == file->page_align_history)
                    file->history_eof =
                        (file->history_eof + (hdr_p->page_size - 1)) & (~(hdr_p->page_size - 1));

                rec_p->archival_index.list = NULL;

                file->header.whole_history_addr = file->history_eof;

                // Write nascent whole-history summary (with no revisions) to the backing onion file
                if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, saved_size + 1, size, wh_buf) <
                    0) {
                    HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, NULL,
                                "cannot write summary to the backing onion file")
                }
                
                file->header.whole_history_size = size; /* record for later use */

                H5MM_xfree(head_buf);
                H5MM_xfree(wh_buf);
            }
            else {
                HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "unable to open onion file (does not exist?).")
            }
        }

        /* Get the history header from the onion file */
        if (H5FD__onion_ingest_history_header(&file->header, file->backing_onion, 0) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, NULL, "can't get history header from backing store")
        file->page_align_history =
            (file->header.flags & H5FD__ONION_HEADER_FLAG_PAGE_ALIGNMENT) ? TRUE : FALSE;

        if (H5FD__ONION_HEADER_FLAG_WRITE_LOCK & file->header.flags) {
            /* Opening a file twice in write mode is an error */
            HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, NULL, "Can't open file already opened in write-mode")
        }
        else {
            /* Read in the history from the onion file */
            if (H5FD__onion_ingest_whole_history(&file->summary, file->backing_onion,
                                                 file->header.whole_history_addr,
                                                 file->header.whole_history_size) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, NULL, "can't get whole-history from backing store")

            /* Sanity check on revision ID */
            if (fa->revision_id >= file->summary.n_revisions &&
                fa->revision_id != H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "target revision ID out of range")

            if (file->summary.n_revisions > 0 &&
                H5FD__onion_ingest_revision_record(&file->rev_record, file->backing_onion, &file->summary,
                                                   MIN(fa->revision_id, (file->summary.n_revisions - 1))) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTDECODE, NULL, "can't get revision record from backing store")

            if (H5F_ACC_RDWR & flags)
                if (H5FD__onion_open_rw(file, flags, maxaddr) < 0)
                    HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "can't write-open write-locked file")
        }

    } /* End if opening existing file */

    /* Copy comment from FAPL info, if one is given */
    if ((H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC) & flags) {
        if (fa->comment) {
            /* Free the old comment */
            file->rev_record.comment = H5MM_xfree(file->rev_record.comment);

            /* TODO: Lengths of strings should be size_t */
            file->rev_record.comment_size = (uint32_t)HDstrlen(fa->comment) + 1;

            if (NULL == (file->rev_record.comment = H5MM_xstrdup(fa->comment)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "unable to allocate comment string")
        }
    }

    file->origin_eof = file->header.origin_eof;
    file->logi_eof   = MAX(file->rev_record.logi_eof, file->logi_eof);
    // file->logi_eof   = file->rev_record.logi_eof;
    file->logi_eoa = 0;

    file->history_eof = H5FD_get_eoa(file->backing_onion, H5FD_MEM_DRAW);
    if (TRUE == file->page_align_history)
        file->history_eof =
            (file->history_eof + (file->header.page_size - 1)) & (~(file->header.page_size - 1));

    ret_value = (H5FD_t *)file;

done:
    H5MM_xfree(name_onion);
    H5MM_xfree(name_recovery);

    if ((NULL == ret_value) && file) {

        if (file->backing_canon)
            if (H5FD_close(file->backing_canon) < 0)
                HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, NULL, "can't destroy backing canon")
        if (file->backing_onion)
            if (H5FD_close(file->backing_onion) < 0)
                HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, NULL, "can't destroy backing onion")
        if (file->backing_recov)
            if (H5FD_close(file->backing_recov) < 0)
                HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, NULL, "can't destroy backing recov")

        if (file->rev_index)
            if (H5FD_onion_revision_index_destroy(file->rev_index) < 0)
                HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, NULL, "can't destroy revision index")

        H5MM_xfree(file->summary.record_pointer_list);

        H5MM_xfree(file->name_recov);
        H5MM_xfree(file->rev_record.comment);
        H5MM_xfree(file->rev_record.username);

        H5FL_FREE(H5FD_onion_t, file);
    }

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_open() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_open_rw()
 *
 * Purpose:     Complete onion file-open, handling process for write mode.
 *
 *              Creates recovery file if one does not exist.
 *              Initializes 'live' revision index.
 *              Force write-open is not yet supported (recovery provision) TODO
 *              Establishes write-lock in history header (sets lock flag).
 *
 * Return:      Success: Non-negative value (SUCCEED).
 *              Failure: Negative value (FAIL).
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_open_rw(H5FD_onion_t *file, unsigned int flags, haddr_t maxaddr)
{
    unsigned char *buf       = NULL;
    uint64_t       size      = 0;
    uint32_t       _sum      = 0;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_STATIC;

    /* Guard against simultaneous write-open.
     * TODO: support recovery open with force-write-open flag in FAPL info.
     */

    if (file->header.flags & H5FD__ONION_HEADER_FLAG_WRITE_LOCK) {
        HGOTO_ERROR(H5E_VFL, H5E_UNSUPPORTED, FAIL, "can't write-open write-locked file");
    }

    /* Copy whole-history to recovery file.
     */

    file->backing_recov = H5FD_open(file->name_recov, (flags | H5F_ACC_CREAT | H5F_ACC_TRUNC),
                                    file->fa.backing_fapl_id, maxaddr);
    if (NULL == file->backing_recov) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL, "unable to create recovery file");
    }

    size = H5FD__onion_whole_history_write(&file->summary, file->backing_recov, 0, 0);
    if (0 == size) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write whole-history to recovery file");
    }
    if (size != file->header.whole_history_size) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "written whole-history differed from expected size");
    }

    /* Set write-lock flag in Onion header.
     */

    buf = H5MM_malloc(H5FD__ONION_ENCODED_SIZE_HEADER);
    if (NULL == buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate space for encoded buffer");
    }

    file->header.flags |= H5FD__ONION_HEADER_FLAG_WRITE_LOCK;

    size = H5FD_onion_history_header_encode(&file->header, buf, &_sum);
    if (0 == size) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "problem encoding history header");
    }

    if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, 0, (haddr_t)size, buf) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "can't write updated history header");
    }

    /* Prepare revision index and finalize write-mode open */

    if (NULL == (file->rev_index = H5FD_onion_revision_index_init(file->fa.page_size)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize revision index")
    file->rev_record.parent_revision_id = file->rev_record.revision_id;
    file->rev_record.revision_id += 1;
    file->is_open_rw = TRUE;

done:
    if (FAIL == ret_value) {
        if (file->backing_recov != NULL) {
            if (H5FD_close(file->backing_recov) < 0) {
                HDONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "can't close recovery file");
            }
            else {
                file->backing_recov = NULL;
            }
        }

        if (file->rev_index != NULL) {
            if (H5FD_onion_revision_index_destroy(file->rev_index) < 0) {
                HDONE_ERROR(H5E_VFL, H5E_CANTRELEASE, FAIL, "can't destroy revision index");
            }
            else {
                file->rev_index = NULL;
            }
        }
    } /* end if failure */

    if (buf != NULL)
        H5MM_xfree(buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_open_rw() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_read
 *
 * Purpose:     Read bytes from an onionized file.
 *
 * Return:      Success: Non-negative value (SUCCEED).
 *              Failure: Negative value (FAIL).
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_read(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t offset, size_t len,
                 void *_buf_out)
{
    H5FD_onion_t * file           = (H5FD_onion_t *)_file;
    uint64_t       page_0         = 0;
    size_t         n_pages        = 0;
    uint32_t       page_size      = 0;
    uint32_t       page_size_log2 = 0;
    size_t         i              = 0;
    size_t         j              = 0;
    size_t         bytes_to_read  = len;
    unsigned char *buf_out        = (unsigned char *)_buf_out;
    herr_t         ret_value      = SUCCEED;

    FUNC_ENTER_STATIC;

    HDassert(file != NULL);
    HDassert(buf_out != NULL);

    if ((uint64_t)(offset + len) > file->logi_eoa) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Read extends beyond addressed space");
    }

    if (0 == len)
        goto done;

    page_size      = file->header.page_size;
    page_size_log2 = file->rev_record.archival_index.page_size_log2;
    page_0         = offset >> page_size_log2;
    n_pages        = (len + page_size - 1) >> page_size_log2;

    /* Read, page-by-page */
    for (i = 0; i < n_pages; i++) {
        const struct H5FD__onion_index_entry *entry_out_p   = NULL;
        haddr_t                               page_gap_head = 0; /* start of page to start of buffer */
        haddr_t                               page_gap_tail = 0; /* end of buffer to end of page */
        size_t                                page_readsize = 0;
        uint64_t                              page_i        = page_0 + i;

        if (0 == i)
            page_gap_head = offset & (((uint32_t)1 << page_size_log2) - 1);
        if (n_pages - 1 == i)
            page_gap_tail = page_size - bytes_to_read - page_gap_head;
        page_readsize = (size_t)page_size - page_gap_head - page_gap_tail;

        if (TRUE == file->is_open_rw &&
            H5FD_onion_revision_index_find(file->rev_index, page_i, &entry_out_p)) {
            if (H5FDread(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT,
                         (haddr_t)entry_out_p->phys_addr + page_gap_head, page_readsize, buf_out) < 0) {
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't get working file data");
            }
        } /* end if page exists in 'live' revision index */
        else if (H5FD_onion_archival_index_find(&file->rev_record.archival_index, page_i, &entry_out_p)) {
            if (H5FDread(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT,
                         (haddr_t)entry_out_p->phys_addr + page_gap_head, page_readsize, buf_out) < 0) {
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't get previously-amended file data");
            }
        } /* end if page exists in 'dead' archival index */
        else {
            /* casts prevent truncation */
            haddr_t addr_start   = (haddr_t)page_i * (haddr_t)page_size + (haddr_t)page_gap_head;
            haddr_t overlap_size = (addr_start > file->origin_eof) ? 0 : file->origin_eof - addr_start;
            haddr_t read_size    = MIN(overlap_size, page_readsize);
#if 0
HDputs("READING from original file");
HDprintf("page_size: %llu, addr_start: %llu page_gap_head: %llu, page_gap_tail: %llu overlap_size: %llu page_readsize:%llu read_size: %llu\n", page_size, addr_start, page_gap_head, page_gap_tail, overlap_size, page_readsize, read_size);
#endif
            /* Get all original bytes in page range */
            if ((read_size > 0) &&
                H5FDread(file->backing_canon, type, H5P_DEFAULT, addr_start, read_size, buf_out) < 0) {
                HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't get original file data");
            }

            /* Fill with 0s any gaps after end of original bytes
             * and before end of page.
             */
            for (j = read_size; j < page_readsize; j++)
                buf_out[j] = 0;
        } /* end if page exists in neither index */

        buf_out += page_readsize;
        bytes_to_read -= page_readsize;
    } /* end for each page in range */

    HDassert(0 == bytes_to_read);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_read() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_set_eoa
 *
 * Purpose:     Set end-of-address marker of the logical file.
 *
 * Return:      Success: Non-negative value (SUCCEED).
 *              Failure: Negative value (FAIL).
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_onion_t *file = (H5FD_onion_t *)_file;

    FUNC_ENTER_STATIC_NOERR;

    file->logi_eoa = addr;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5FD__onion_set_eoa() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_write
 *
 * Purpose:     Write bytes to an onionized file.
 *
 * Return:      Success: Non-negative value (SUCCEED).
 *              Failure: Negative value (FAIL).
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_write(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t offset, size_t len,
                  const void *_buf)
{
    H5FD_onion_t *       file           = (H5FD_onion_t *)_file;
    uint64_t             page_0         = 0;
    size_t               n_pages        = 0;
    unsigned char *      page_buf       = NULL;
    uint32_t             page_size      = 0;
    uint32_t             page_size_log2 = 0;
    size_t               i              = 0;
    size_t               j              = 0;
    size_t               bytes_to_write = len;
    const unsigned char *buf            = (const unsigned char *)_buf;
    herr_t               ret_value      = SUCCEED;

    FUNC_ENTER_STATIC;

    HDassert(file != NULL);
    HDassert(buf != NULL);
    HDassert(file->rev_index != NULL);
    HDassert((uint64_t)(offset + len) <= file->logi_eoa);

    if (FALSE == file->is_open_rw) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Write not allowed if file not opened in write mode");
    }

    if (0 == len)
        goto done;

    page_size      = file->header.page_size;
    page_size_log2 = file->rev_record.archival_index.page_size_log2;
    page_0         = offset >> page_size_log2;
    n_pages        = (len + page_size - 1) >> page_size_log2;

    page_buf = H5MM_calloc(page_size * sizeof(unsigned char));
    if (NULL == page_buf) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "cannot allocate temporary buffer");
    }

    /* Write, page-by-page */
    for (i = 0; i < n_pages; i++) {
        const unsigned char *                 write_buf = buf;
        struct H5FD__onion_index_entry        new_entry;
        const struct H5FD__onion_index_entry *entry_out_p   = NULL;
        haddr_t                               page_gap_head = 0; /* start of page to start of buffer */
        haddr_t                               page_gap_tail = 0; /* end of buffer to end of page */
        size_t                                page_n_used   = 0; /* nbytes from buffer for this page-write */
        uint64_t                              page_i        = page_0 + i;

        if (0 == i)
            page_gap_head = offset & (((uint32_t)1 << page_size_log2) - 1);
        // If we have a page_gap_head and the number of bytes to write is evenly divisible by the page size we
        // need to add an additional page to make up for the page_gap_head
        if (page_gap_head > 0 && bytes_to_write % page_size == 0) {
            n_pages++;
        }
        if (n_pages - 1 == i) {
            page_gap_tail = page_size - bytes_to_write - page_gap_head;
        }
        page_n_used = page_size - page_gap_head - page_gap_tail;

        /* Modify page in revision index, if present */
        if (H5FD_onion_revision_index_find(file->rev_index, page_i, &entry_out_p)) {
            if (page_gap_head | page_gap_tail) {
                /* Copy existing page verbatim. */
                if (H5FDread(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)entry_out_p->phys_addr,
                             page_size, page_buf) < 0) {
                    HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't get working file data");
                }
                /* Overlay delta from input buffer onto page buffer. */
                HDmemcpy(page_buf, buf, page_n_used);
                write_buf = page_buf;
            } /* end if partial page */

            if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)entry_out_p->phys_addr,
                          page_size, write_buf) < 0) {
                HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "write amended page data to backing file");
            }

            buf += page_n_used; /* overflow never touched */
            bytes_to_write -= page_n_used;

            continue;
        } /* end if page exists in 'live' revision index */

        if (page_gap_head || page_gap_tail) {
            /* Fill gaps with existing data or zeroes. */
            if (H5FD_onion_archival_index_find(&file->rev_record.archival_index, page_i, &entry_out_p)) {
                /* Copy existing page verbatim. */
                if (H5FDread(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, (haddr_t)entry_out_p->phys_addr,
                             page_size, page_buf) < 0) {
                    HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't get previously-amended data");
                }
            } /* end if page exists in 'dead' archival index */
            else {
                haddr_t addr_start   = (haddr_t)(page_i * page_size);
                haddr_t overlap_size = (addr_start > file->origin_eof) ? 0 : file->origin_eof - addr_start;
                haddr_t read_size    = MIN(overlap_size, page_size);

                /* Get all original bytes in page range */
                if ((read_size > 0) &&
                    H5FDread(file->backing_canon, type, H5P_DEFAULT, addr_start, read_size, page_buf) < 0) {
                    HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "can't get original file data");
                }

                /* Fill with 0s any gaps after end of original bytes
                 * or start of page and before start of new data.
                 */
                for (j = read_size; j < page_gap_head; j++)
                    page_buf[j] = 0;

                /* Fill with 0s any gaps after end of original bytes
                 * or end of new data and before end of page.
                 */
                for (j = MAX(read_size, page_size - page_gap_tail); j < page_size; j++) {
                    page_buf[j] = 0;
                }
            } /* end if page exists in neither index */

            /* Copy input buffer to temporary page buffer */
            assert((page_size - page_gap_head) >= page_n_used);
            HDmemcpy(page_buf + page_gap_head, buf, page_n_used);
            write_buf = page_buf;

        } /* end if data range does not span entire page */

        new_entry.logi_page = page_i;
        new_entry.phys_addr = file->history_eof;

        if (H5FD_set_eoa(file->backing_onion, H5FD_MEM_DRAW, file->history_eof + page_size) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "can't modify EOA for new page amendment");
        }
        if (H5FDwrite(file->backing_onion, H5FD_MEM_DRAW, H5P_DEFAULT, file->history_eof, page_size,
                      write_buf) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "write amended page data to backing file");
        }
        if (H5FD_onion_revision_index_insert(file->rev_index, &new_entry) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTINSERT, FAIL, "can't insert new index entry into revision index");
        }

        file->history_eof += page_size;
        buf += page_n_used; /* possible overflow never touched */
        bytes_to_write -= page_n_used;

    } /* end for each page to write */

    HDassert(0 == bytes_to_write);

    file->logi_eof = MAX(file->logi_eof, (offset + len));

done:
    if (page_buf != NULL)
        H5MM_xfree(page_buf);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_write() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_archival_index_is_valid
 *
 * Purpose:     Determine whether an archival index structure is valid.
 *
 *              + Verify magic number and version (sanity checking).
 *              + Verify page size (power of two).
 *              + Verify list exists.
 *              + Verify list contents:
 *                + Sorted by increasing logical address (no duplicates)
 *                + Logical addresses are multiples of page size.
 *
 * Return:      TRUE if above creteria are met.
 *              FALSE otherwise.
 *
 *-----------------------------------------------------------------------------
 */
hbool_t
H5FD_onion_archival_index_is_valid(const struct H5FD__onion_archival_index *aix)
{
    hbool_t ret_value = TRUE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(aix);

    if (H5FD__ONION_ARCHIVAL_INDEX_MAGIC != aix->magic)
        HGOTO_DONE(FALSE)
    if (H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR != aix->version)
        HGOTO_DONE(FALSE)
    if (NULL == aix->list)
        HGOTO_DONE(FALSE)

    /* Ensure list is sorted on logi_page field */
    if (aix->n_entries > 1)
        for (uint64_t i = 1; i < aix->n_entries - 1; i++)
            if (aix->list[i + 1].logi_page <= aix->list[i].logi_page)
                HGOTO_DONE(FALSE)

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_archival_index_is_valid() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_archival_index_find
 *
 * Purpose:     Retrieve the archival index entry by logical page ID.
 *
 *              The archival index pointer must point to a valid index entry.
 *              The entry out pointer-pointer cannot be null.
 *
 * Return:      Success: Positive value (1) -- entry found.
 *                       Entry out pointer-pointer is set to point to entry.
 *              Failure: Zero (0) -- entry not found.
 *                       Entry out pointer-pointer is unmodified.
 *
 *-----------------------------------------------------------------------------
 */
int
H5FD_onion_archival_index_find(const struct H5FD__onion_archival_index *aix, uint64_t logi_page,
                               const struct H5FD__onion_index_entry **entry_out_p)
{
    uint64_t                        low       = 0;
    uint64_t                        high      = 0;
    uint64_t                        n         = 0;
    uint64_t                        range     = 0;
    struct H5FD__onion_index_entry *x         = NULL;
    int                             ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(aix);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_MAGIC == aix->magic);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR == aix->version);
    HDassert(entry_out_p);
    if (aix->n_entries != 0)
        HDassert(aix->list);

    high  = aix->n_entries - 1;
    range = high;

    /* Trivial cases */
    if (aix->n_entries == 0 || logi_page > aix->list[high].logi_page || logi_page < aix->list[0].logi_page)
        HGOTO_DONE(0)

    /*
     * Binary search on sorted list
     */

    /* Winnow down to first of found or one element */
    while (range > 0) {
        HDassert(high < aix->n_entries);
        n = low + (range / 2);
        x = &(aix->list[n]);
        if (x->logi_page == logi_page) {
            *entry_out_p = x; /* element found at fence */
            ret_value    = 1;
            goto done;
        }
        else if (x->logi_page < logi_page) {
            low = (n == high) ? high : n + 1;
        }
        else {
            high = (n == low) ? low : n - 1;
        }
        range = high - low;
    }

    HDassert(high == low); /* one element */

    /* n == low/high check because we may have tested it already above */
    if ((n != low || n != high) && (aix->list[low].logi_page == logi_page)) {
        *entry_out_p = &aix->list[low];
        ret_value    = 1;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_archival_index_find() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_revision_index_destroy
 *
 * Purpose:     Release all resources of a revision index.
 *
 * Return:      SUCCEED/FAIL
 *
 *-----------------------------------------------------------------------------
 */
herr_t
H5FD_onion_revision_index_destroy(H5FD__onion_revision_index_t *rix)
{
    size_t i         = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(rix);
    HDassert(H5FD__ONION_REVISION_INDEX_MAGIC == rix->magic);
    HDassert(H5FD__ONION_REVISION_INDEX_VERSION_CURR == rix->version);

    for (i = 0; 0 < rix->_hash_table_n_keys_populated && i < rix->_hash_table_size; i++) {
        struct H5FD__onion_revision_index_hash_chain_node *next_p = NULL;
        struct H5FD__onion_revision_index_hash_chain_node *node_p = rix->_hash_table[i];

        if (node_p != NULL)
            rix->_hash_table_n_keys_populated -= 1;

        while (node_p != NULL) {
            HDassert(H5FD__ONION_REVISION_INDEX_HASH_CHAIN_NODE_MAGIC == node_p->magic);
            HDassert(H5FD__ONION_REVISION_INDEX_HASH_CHAIN_NODE_VERSION_CURR == node_p->version);

            next_p = node_p->next;
            H5MM_xfree(node_p);
            node_p = next_p;
        }
    }
    H5MM_xfree(rix->_hash_table);
    H5MM_xfree(rix);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_revision_index_destroy() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_revision_index_init
 *
 * Purpose:     Initialize a revision index structure with a default starting
 *              size. A new structure is allocated and populated with initial
 *              values.
 *
 * Return:      Success:    Pointer to newly-allocated structure
 *              Failure:    NULL
 *
 *-----------------------------------------------------------------------------
 */
H5FD__onion_revision_index_t *
H5FD_onion_revision_index_init(uint32_t page_size)
{
    uint64_t                      table_size = U64_EXP2(H5FD__ONION_REVISION_INDEX_STARTING_SIZE_LOG2);
    H5FD__onion_revision_index_t *rix        = NULL;
    H5FD__onion_revision_index_t *ret_value  = NULL;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(0 != page_size);
    HDassert(POWER_OF_TWO(page_size));

    if (NULL == (rix = H5MM_calloc(sizeof(H5FD__onion_revision_index_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "cannot allocate index")

    if (NULL == (rix->_hash_table =
                     H5MM_calloc(table_size * sizeof(struct H5FD__onion_revision_index_hash_chain_node *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "cannot allocate hash table")

    rix->magic     = H5FD__ONION_REVISION_INDEX_MAGIC;
    rix->version   = H5FD__ONION_REVISION_INDEX_VERSION_CURR;
    rix->n_entries = 0;
    /* Compute and store log2(page_size) */
    for (rix->page_size_log2 = 0; (((uint32_t)1 << rix->page_size_log2) & page_size) == 0;
         rix->page_size_log2++)
        ;
    rix->_hash_table_size             = table_size;
    rix->_hash_table_size_log2        = H5FD__ONION_REVISION_INDEX_STARTING_SIZE_LOG2;
    rix->_hash_table_n_keys_populated = 0;

    ret_value = rix;

done:
    if (NULL == ret_value && NULL != rix)
        H5MM_xfree(rix);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_revision_index_init() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD__onion_revision_index_resize()
 *
 * Purpose:     Replace the hash table in the revision index.
 *
 *              Doubles the available number of keys, re-hashes table contents,
 *              and updates relevant components in the index structure.
 *
 *              Fails if unable to allocate space for larger hash table.
 *
 * Return:      SUCCEED/FAIL
 *
 *-----------------------------------------------------------------------------
 */
static herr_t
H5FD__onion_revision_index_resize(H5FD__onion_revision_index_t *rix)
{
    struct H5FD__onion_revision_index_hash_chain_node **new_table            = NULL;
    uint64_t                                            i                    = 0;
    uint64_t                                            new_size_log2        = rix->_hash_table_size_log2 + 1;
    uint64_t                                            new_size             = U64_EXP2(new_size_log2);
    uint64_t                                            new_n_keys_populated = 0;
    herr_t                                              ret_value            = SUCCEED;

    FUNC_ENTER_STATIC;

    HDassert(rix);
    HDassert(H5FD__ONION_REVISION_INDEX_MAGIC == rix->magic);
    HDassert(H5FD__ONION_REVISION_INDEX_VERSION_CURR == rix->version);
    HDassert(rix->_hash_table);

    if (NULL ==
        (new_table = H5MM_calloc(new_size * sizeof(struct H5FD__onion_revision_index_hash_chain_node *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "cannot allocate new hash table")

    for (i = 0; i < rix->_hash_table_size; i++) {
        while (rix->_hash_table[i] != NULL) {
            struct H5FD__onion_revision_index_hash_chain_node *node = NULL;
            uint64_t                                           key  = 0;

            /* Pop entry off of bucket stack and re-hash */
            node                = rix->_hash_table[i];
            rix->_hash_table[i] = node->next;
            node->next          = NULL;
            key                 = node->entry_data.logi_page & (new_size - 1);

            if (NULL == new_table[key]) {
                new_table[key] = node;
                new_n_keys_populated++;
            }
            else {
                node->next   = new_table[i];
                new_table[i] = node;
            }
        }
    }

    H5MM_xfree(rix->_hash_table);
    rix->_hash_table_size             = new_size;
    rix->_hash_table_size_log2        = new_size_log2;
    rix->_hash_table_n_keys_populated = new_n_keys_populated;
    rix->_hash_table                  = new_table;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD__onion_revision_index_resize() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_revision_index_insert()
 *
 * Purpose:     Add an entry to the revision index, or update an existing
 *              entry. Must be used to update entries as well as add --
 *              checksum value will change.
 *
 *              Entry data is copied into separate memory region; user pointer
 *              can be safley re-used or discarded after operation.
 *
 * Return:      SUCCEED/FAIL
 *
 *-----------------------------------------------------------------------------
 */
herr_t
H5FD_onion_revision_index_insert(H5FD__onion_revision_index_t *        rix,
                                 const struct H5FD__onion_index_entry *entry)
{
    uint64_t                                            key         = 0;
    struct H5FD__onion_revision_index_hash_chain_node * node        = NULL;
    struct H5FD__onion_revision_index_hash_chain_node **append_dest = NULL;
    herr_t                                              ret_value   = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(rix);
    HDassert(H5FD__ONION_REVISION_INDEX_MAGIC == rix->magic);
    HDassert(H5FD__ONION_REVISION_INDEX_VERSION_CURR == rix->version);
    HDassert(entry);

    /* Resize and re-hash table if necessary */
    if (rix->n_entries >= (rix->_hash_table_size * 2) ||
        rix->_hash_table_n_keys_populated >= (rix->_hash_table_size / 2)) {
        if (H5FD__onion_revision_index_resize(rix) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NONE_MINOR, FAIL, "unable to resize and hash table")
    }

    key = entry->logi_page & (rix->_hash_table_size - 1);
    HDassert(key < rix->_hash_table_size);

    if (NULL == rix->_hash_table[key]) {
        /* Key maps to empty bucket */

        append_dest = &rix->_hash_table[key];
        rix->_hash_table_n_keys_populated++;
    }
    else {
        /* Key maps to populated bucket */

        for (node = rix->_hash_table[key]; node != NULL; node = node->next) {
            append_dest = &node->next; /* look for bucket tail */
            if (entry->logi_page == node->entry_data.logi_page) {
                if (entry->phys_addr != node->entry_data.phys_addr) {
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "physical address mismatch");
                }
                HDmemcpy(&node->entry_data, entry, sizeof(struct H5FD__onion_index_entry));
                append_dest = NULL; /* Node updated, do not append */
                break;
            }
        }
    }

    /* Add new entry to bucket chain */
    if (append_dest != NULL) {
        if (NULL == (node = H5MM_malloc(sizeof(struct H5FD__onion_revision_index_hash_chain_node))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "cannot allocate new ash chain node")
        node->magic   = H5FD__ONION_REVISION_INDEX_HASH_CHAIN_NODE_MAGIC;
        node->version = H5FD__ONION_REVISION_INDEX_HASH_CHAIN_NODE_VERSION_CURR;
        node->next    = NULL;
        HDmemcpy(&node->entry_data, entry, sizeof(struct H5FD__onion_index_entry));
        *append_dest = node;
        rix->n_entries++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_revision_index_insert() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_revision_index_find()
 *
 *
 * Purpose:     Get pointer to revision index entry with the given page number,
 *              if it exists in the index.
 *
 * Return:      Success: Positive value (1) -- entry found.
 *                       Entry out pointer-pointer is set to point to entry.
 *              Failure: Zero (0) -- entry not found.
 *                       Entry out pointer-pointer is unmodified.
 *
 *-----------------------------------------------------------------------------
 */
int
H5FD_onion_revision_index_find(const H5FD__onion_revision_index_t *rix_p, uint64_t logi_page,
                               const struct H5FD__onion_index_entry **entry_out_p)
{
    uint64_t key       = 0;
    int      ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR;

    HDassert(rix_p);
    HDassert(H5FD__ONION_REVISION_INDEX_MAGIC == rix_p->magic);
    HDassert(H5FD__ONION_REVISION_INDEX_VERSION_CURR == rix_p->version);
    HDassert(rix_p->_hash_table);
    HDassert(entry_out_p);

    key = logi_page & (rix_p->_hash_table_size - 1);
    HDassert(key < rix_p->_hash_table_size);

    if (rix_p->_hash_table[key] != NULL) {
        struct H5FD__onion_revision_index_hash_chain_node *node = NULL;

        for (node = rix_p->_hash_table[key]; node != NULL; node = node->next) {
            if (logi_page == node->entry_data.logi_page) {
                *entry_out_p = &node->entry_data;
                ret_value    = 1;
                break;
            }
        }
    }

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_revision_index_find() */

/*-----------------------------------------------------------------------------
 *
 * Callback for comparisons in sorting archival index entries by logi_page.
 *
 *-----------------------------------------------------------------------------
 */
static int
H5FD__onion_archival_index_list_sort_cmp(const void *_a, const void *_b)
{
    const struct H5FD__onion_index_entry *a = (const struct H5FD__onion_index_entry *)_a;
    const struct H5FD__onion_index_entry *b = (const struct H5FD__onion_index_entry *)_b;

    if (a->logi_page < b->logi_page)
        return -1;
    else if (a->logi_page > b->logi_page)
        return 1;
    return 0;
} /* end H5FD__onion_archival_index_list_sort_cmp() */

/*-----------------------------------------------------------------------------
 *
 * Function:    H5FD_onion_merge_revision_index_into_archival_index()
 *
 * Purpose:     Merge index entries from revision index into archival index.
 *
 *              If successful, the archival index is expanded 'behind the
 *              scenes' and new entries from the revision index are inserted.
 *              The archival index remains sorted in ascending order of logical
 *              address.
 *
 *              The conversion to archival index changes logical pages in
 *              revision index entries to their logical addresses in-file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-----------------------------------------------------------------------------
 */
herr_t
H5FD_onion_merge_revision_index_into_archival_index(const H5FD__onion_revision_index_t *rix,
                                                    struct H5FD__onion_archival_index * aix)
{
    uint64_t                          i         = 0;
    uint64_t                          n_kept    = 0;
    struct H5FD__onion_index_entry *  kept_list = NULL;
    struct H5FD__onion_archival_index new_aix   = {
        H5FD__ONION_ARCHIVAL_INDEX_MAGIC,
        H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR,
        0,    /* page_size_log2 tbd */
        0,    /* n_entries */
        NULL, /* list pointer (allocated later) */
    };
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT;

    HDassert(rix);
    HDassert(aix);
    HDassert(H5FD__ONION_REVISION_INDEX_MAGIC == rix->magic);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_MAGIC == aix->magic);
    HDassert(H5FD__ONION_REVISION_INDEX_VERSION_CURR == rix->version);
    HDassert(H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR == aix->version);
    HDassert(aix->page_size_log2 == rix->page_size_log2);

    /* If the revision index is empty there is nothing to archive */
    if (rix->n_entries == 0)
        goto done;

    /* Add all revision index entries to new archival list */
    new_aix.page_size_log2 = aix->page_size_log2;

    if (NULL == (new_aix.list = H5MM_calloc(rix->n_entries * sizeof(struct H5FD__onion_index_entry))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "unable to allocate new archival index list")

    for (i = 0; i < rix->_hash_table_size; i++) {
        const struct H5FD__onion_revision_index_hash_chain_node *node_p = NULL;

        for (node_p = rix->_hash_table[i]; node_p != NULL; node_p = node_p->next) {
            HDmemcpy(&new_aix.list[new_aix.n_entries], &node_p->entry_data,
                     sizeof(struct H5FD__onion_index_entry));
            new_aix.n_entries++;
        }
    }

    /* Sort the new archival list */
    HDqsort(new_aix.list, new_aix.n_entries, sizeof(struct H5FD__onion_index_entry),
            H5FD__onion_archival_index_list_sort_cmp);

    /* Add the old archival index entries to a 'kept' list containing the
     * old archival list entries that are not also included in the revision
     * list.
     *
     * Note that kept_list will be NULL if there are no entries in the passed-in
     * archival list.
     */
    if (aix->n_entries > 0)
        if (NULL == (kept_list = H5MM_calloc(aix->n_entries * sizeof(struct H5FD__onion_index_entry))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "unable to allocate larger archival index list")

    for (i = 0; i < aix->n_entries; i++) {
        const struct H5FD__onion_index_entry *_p = NULL;

        /* Add only if page not already added from revision index */
        if (H5FD_onion_archival_index_find(&new_aix, aix->list[i].logi_page, &_p) == 0) {
            HDmemcpy(&kept_list[n_kept], &aix->list[i], sizeof(struct H5FD__onion_index_entry));
            n_kept++;
        }
    }

    /* Destroy the old archival list and replace with a list big enough to hold
     * the revision list entries and the kept list entries
     */
    H5MM_xfree(aix->list);
    if (NULL ==
        (aix->list = H5MM_calloc((new_aix.n_entries + n_kept) * sizeof(struct H5FD__onion_index_entry))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "unable to allocate exact-size archival index list")

    /* Copy (new) revision list entries to replacement list */
    HDmemcpy(aix->list, new_aix.list, sizeof(struct H5FD__onion_index_entry) * new_aix.n_entries);
    aix->n_entries = new_aix.n_entries;

    /* Copy (old) kept archival list entries to replacement list */
    if (n_kept > 0) {
        HDmemcpy(&aix->list[aix->n_entries], kept_list, sizeof(struct H5FD__onion_index_entry) * n_kept);
        aix->n_entries += n_kept;
    }

    /* Sort this list */
    HDqsort(aix->list, aix->n_entries, sizeof(struct H5FD__onion_index_entry),
            H5FD__onion_archival_index_list_sort_cmp);

done:
    /* Free the temporary lists */
    H5MM_xfree(kept_list);
    H5MM_xfree(new_aix.list);

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_onion_merge_revision_index_into_entry_list() */
