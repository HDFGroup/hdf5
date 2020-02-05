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
 * Purpose:  VFD SWMR driver for the reader
 */

#include "H5FDdrvr_module.h" /* This source code file is part of the H5FD driver module */


#include "H5Eprivate.h"     /* Error handling           */
#include "H5Fprivate.h"     /* File access              */
#include "H5FDprivate.h"    /* File drivers             */
#include "H5FDvfd_swmr.h"   /* VFD SWMR file driver     */
#include "H5FLprivate.h"    /* Free Lists               */
#include "H5Iprivate.h"     /* IDs                      */
#include "H5MMprivate.h"    /* Memory management        */
#include "H5Pprivate.h"     /* Property lists           */
#include "H5retry_private.h"/* Retry loops.		*/

/* The driver identification number, initialized at runtime */
static hid_t H5FD_VFD_SWMR_g = 0;

typedef struct H5FD_vfd_swmr_t {
    H5FD_t pub;                                 /* public stuff, must be      */
                                                /* first                      */

    /* HDF5 file */
    char hdf5_filename[H5FD_MAX_FILENAME_LEN];  /* Name of the HDF5 file from */
                                                /* open                       */
    H5FD_t *hdf5_file_lf;                       /* Driver info for the HDF5   */
                                                /* file                       */

    /* Metadata file */
    int md_fd;                                  /* File descriptor for the    */
                                                /* metadata file              */
    uint32_t md_pages_reserved;                 /* # of pages reserved at the */
                                                /* head of the metadata file  */
    char md_file_path[H5FD_MAX_FILENAME_LEN];   /* Name of the metadate file  */
    H5FD_vfd_swmr_md_header md_header;          /* Metadata file header       */
    H5FD_vfd_swmr_md_index md_index;            /* Metadata file index        */

    hbool_t pb_configured;                      /* boolean flag set to TRUE   */
                                                /* when the page buffer is    */
                                                /* and to FALSE otherwise.    */
                                                /* Used for sanity checking.  */
} H5FD_vfd_swmr_t;

#define MAXADDR (((haddr_t)1<<(8*sizeof(HDoff_t)-1))-1)

/* Prototypes */
static herr_t H5FD_vfd_swmr_term(void);
static H5FD_t *H5FD_vfd_swmr_open(const char *name, unsigned flags, 
    hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_vfd_swmr_close(H5FD_t *_file);
static int H5FD_vfd_swmr_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_vfd_swmr_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_vfd_swmr_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_vfd_swmr_set_eoa(H5FD_t *_file, H5FD_mem_t type, 
    haddr_t addr);
static haddr_t H5FD_vfd_swmr_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD_vfd_swmr_get_handle(H5FD_t *_file, hid_t fapl, 
    void** file_handle);
static herr_t H5FD_vfd_swmr_read(H5FD_t *_file, H5FD_mem_t type, 
    hid_t fapl_id, haddr_t addr, size_t size, void *buf);
static herr_t H5FD_vfd_swmr_write(H5FD_t *_file, H5FD_mem_t type, 
    hid_t fapl_id, haddr_t addr, size_t size, const void *buf);
static herr_t H5FD_vfd_swmr_truncate(H5FD_t *_file, hid_t dxpl_id, 
    hbool_t closing);
static herr_t H5FD_vfd_swmr_lock(H5FD_t *_file, hbool_t rw);
static herr_t H5FD_vfd_swmr_unlock(H5FD_t *_file);

/* VFD SWMR */
static htri_t H5FD__vfd_swmr_header_deserialize(H5FD_t *_file, 
    H5FD_vfd_swmr_md_header *md_header);
static htri_t H5FD__vfd_swmr_index_deserialize(const H5FD_t *_file, 
    H5FD_vfd_swmr_md_index *md_index, const H5FD_vfd_swmr_md_header *md_header);
static herr_t H5FD__vfd_swmr_load_hdr_and_idx(H5FD_t *_file, hbool_t open);

static const H5FD_class_t H5FD_vfd_swmr_g = {
    "vfd_swmr",                 /* name                 */
    MAXADDR,                    /* maxaddr              */
    H5F_CLOSE_WEAK,             /* fc_degree            */
    H5FD_vfd_swmr_term,         /* terminate            */
    NULL,                       /* sb_size              */
    NULL,                       /* sb_encode            */
    NULL,                       /* sb_decode            */
    0,                          /* fapl_size            */
    NULL,                       /* fapl_get             */
    NULL,                       /* fapl_copy            */
    NULL,                       /* fapl_free            */
    0,                          /* dxpl_size            */
    NULL,                       /* dxpl_copy            */
    NULL,                       /* dxpl_free            */
    H5FD_vfd_swmr_open,         /* open                 */
    H5FD_vfd_swmr_close,        /* close                */
    H5FD_vfd_swmr_cmp,          /* cmp                  */
    H5FD_vfd_swmr_query,        /* query                */
    NULL,                       /* get_type_map         */
    NULL,                       /* alloc                */
    NULL,                       /* free                 */
    H5FD_vfd_swmr_get_eoa,      /* get_eoa              */
    H5FD_vfd_swmr_set_eoa,      /* set_eoa              */
    H5FD_vfd_swmr_get_eof,      /* get_eof              */
    H5FD_vfd_swmr_get_handle,   /* get_handle           */
    H5FD_vfd_swmr_read,         /* read                 */
    H5FD_vfd_swmr_write,        /* write                */
    NULL,                       /* flush                */
    H5FD_vfd_swmr_truncate,     /* truncate             */
    H5FD_vfd_swmr_lock,         /* lock                 */
    H5FD_vfd_swmr_unlock,       /* unlock               */
    H5FD_FLMAP_DICHOTOMY        /* fl_map               */
};

/* Declare a free list to manage the H5FD_vfd_swmr_t struct */
H5FL_DEFINE_STATIC(H5FD_vfd_swmr_t);

/* Declare a free list to manage the H5FD_vfd_swmr_idx_entry_t sequence information */
H5FL_SEQ_DEFINE(H5FD_vfd_swmr_idx_entry_t);


/*-------------------------------------------------------------------------
 * Function:    H5FD__init_package
 *
 * Purpose:     Initializes any interface-specific data or routines.
 *
b
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__init_package(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if(H5FD_vfd_swmr_init() < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "unable to initialize swmr VFD")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__init_package() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:    The driver ID for the VFD SWMR driver.
 *              Failure:    Negative
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_vfd_swmr_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;          /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5I_VFL != H5I_get_type(H5FD_VFD_SWMR_g))
        H5FD_VFD_SWMR_g = H5FD_register(&H5FD_vfd_swmr_g, sizeof(H5FD_class_t), FALSE);

    /* Set return value */
    ret_value = H5FD_VFD_SWMR_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_init() */


/*---------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_term
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
H5FD_vfd_swmr_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VFL ID */
    H5FD_VFD_SWMR_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_vfd_swmr_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_vfd_swmr (Not yet)
 *
 * Purpose:     Modify the file access property list to use the H5FD_SWMR
 *              driver 
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_vfd_swmr(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_driver(plist, H5FD_VFD_SWMR, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_vfd_swmr() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_open
 *
 * Purpose:     Open the metadata file and the underlying HDF5 file
 *
 * Return:      Success:    A pointer to a new file data structure. The
 *                          public fields will be initialized by the
 *                          caller, which is always H5FD_open().
 *              Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_vfd_swmr_open(const char *name, unsigned flags, hid_t fapl_id, 
    haddr_t maxaddr)
{
    H5FD_vfd_swmr_t *file = NULL;            /* VFD SWMR driver info        */
    H5P_genplist_t *plist;                   /* Property list pointer       */
    H5F_vfd_swmr_config_t *vfd_swmr_config = /* Points to VFD SWMR          */
        NULL;                                /* configuration               */
    h5_retry_t retry;                        /* retry state */
    bool do_try;                             /* more tries remain */
    H5FD_t *ret_value  = NULL;               /* Return value     */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get file access property list */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))

        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, \
                    "not a file access property list")

    /* Allocate buffer for reading the VFD SWMR configuration */
    if(NULL == (vfd_swmr_config = (H5F_vfd_swmr_config_t *)
                                  H5MM_malloc(sizeof(H5F_vfd_swmr_config_t))))

        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, \
                    "memory allocation failed for H5F_vfd_swmr_config_t")

    /* Get VFD SWMR configuration */
    if(H5P_get(plist, H5F_ACS_VFD_SWMR_CONFIG_NAME, vfd_swmr_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, \
                    "can't get VFD SWMR config info")

    /* Ensure that this is the reader */
    HDassert(!vfd_swmr_config->writer);

    /* Create the new driver struct */
    if(NULL == (file = H5FL_CALLOC(H5FD_vfd_swmr_t)))

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, \
                    "unable to allocate file struct")

    file->hdf5_file_lf = NULL;
    file->md_pages_reserved = vfd_swmr_config->md_pages_reserved;

    /* Retain a copy of the name used to open the HDF5 file */
    HDstrncpy(file->hdf5_filename, name, sizeof(file->hdf5_filename));
    file->hdf5_filename[sizeof(file->hdf5_filename) - 1] = '\0';

    /* Retain a copy of the metadata file name */
    HDstrncpy(file->md_file_path, vfd_swmr_config->md_file_path, 
              sizeof(file->md_file_path));
    file->md_file_path[sizeof(file->md_file_path) - 1] = '\0';

    /* Retry on opening the metadata file */
    for (do_try = h5_retry_init(&retry, H5FD_VFD_SWMR_MD_FILE_RETRY_MAX,
                                 H5_RETRY_DEFAULT_MINIVAL,
                                 H5_RETRY_DEFAULT_MAXIVAL);
         do_try;
         do_try = h5_retry_next(&retry)) {
        if((file->md_fd = HDopen(file->md_file_path, O_RDONLY)) >= 0)
            break;
    }

    /* Exhaust all retries for opening the md file */
    if(!do_try)
        HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, NULL, \
                   "unable to open the metadata file after all retry attempts")

    /* Retry on loading and decoding the header and index in the
     *  metadata file 
     */
    if(H5FD__vfd_swmr_load_hdr_and_idx((H5FD_t *)file, TRUE) < 0)

        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, \
                    "unable to load/decode the md file header/index")

    /* Hard-wired to open the underlying HDF5 file with SEC2 */
    if((file->hdf5_file_lf = H5FD_open(name, flags, H5P_FILE_ACCESS_DEFAULT, 
                                       maxaddr)) == NULL)

        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "can't set driver info")

    /* set pb_configured to FALSE.  This field should not exist, but 
     * until we modify the file open proceedure to create the page buffer
     * before there is any file I/O when opening a file VFD SWMR reader, 
     * we need to be able to turn off sanity checking in the read function
     * until the page buffer is enabled.  This field exists for this 
     * purpose, and should be remove when it is no longer necessary.
     *
     *                                            JRM -- 1/29/19
     */
    file->pb_configured = FALSE;

    /* Set return value */
    ret_value = (H5FD_t*)file;

done:
    /* Free the buffer */
    if(vfd_swmr_config)
        vfd_swmr_config = (H5F_vfd_swmr_config_t *)H5MM_xfree(vfd_swmr_config);

    /* Handle closing if error */
    if(NULL == ret_value && file) {

        if(H5FD_vfd_swmr_close((H5FD_t *)file) < 0)

            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, NULL, "error from closing")

    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_open() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_close
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
H5FD_vfd_swmr_close(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;
    herr_t      ret_value = SUCCEED;                /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(file);

    /* Close the underlying file */
    if(file->hdf5_file_lf && H5FD_close(file->hdf5_file_lf) < 0)
         /* Push error, but keep going */
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, \
                    "unable to close the HDF5 file")

    /* Close the metadata file */
    if(file->md_fd >= 0 && HDclose(file->md_fd) < 0)
        /* Push error, but keep going */
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, \
                     "unable to close the metadata file")

    /* Free the index entries */
    if(file->md_index.num_entries && file->md_index.entries)
        file->md_index.entries = H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, 
                                               file->md_index.entries);

    /* Release the driver info */
    file = H5FL_FREE(H5FD_vfd_swmr_t, file);

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_vfd_swmr_close() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_cmp
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
H5FD_vfd_swmr_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_vfd_swmr_t   *f1 = (const H5FD_vfd_swmr_t *)_f1;
    const H5FD_vfd_swmr_t   *f2 = (const H5FD_vfd_swmr_t *)_f2;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    ret_value = H5FD_cmp(f1->hdf5_file_lf, f2->hdf5_file_lf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_cmp() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      SUCCEED (Can't fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;     /* OK to aggregate      */
                                                    /* metadata allocations */

        *flags |= H5FD_FEAT_ACCUMULATE_METADATA;    /* OK to accumulate     */
                                                    /* metadata for faster  */
                                                    /* writes               */

        *flags |= H5FD_FEAT_DATA_SIEVE;             /* OK to perform data   */ 
                                                    /* sieving for faster   */
                                                    /* raw data reads &     */
                                                    /* writes               */

        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA;    /* OK to aggregate      */
                                                    /* "small" raw data     */
                                                    /* allocations          */

        *flags |= H5FD_FEAT_POSIX_COMPAT_HANDLE;    /* get_handle callback  */
                                                    /* returns a POSIX file */
                                                    /* descriptor           */

        *flags |= H5FD_FEAT_SUPPORTS_SWMR_IO;       /* VFD supports the     */ 
                                                    /* single-writer/       */
                                                    /* multiple-readers     */ 
                                                    /* (SWMR) pattern       */

        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE; /* VFD creates a file   */ 
                                                    /* which can be opened  */
                                                    /* with the default VFD */

    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_vfd_swmr_query() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_eoa
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
H5FD_vfd_swmr_get_eoa(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_vfd_swmr_t	*file = (const H5FD_vfd_swmr_t *)_file;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT

    if((ret_value = H5FD_get_eoa(file->hdf5_file_lf, type)) == HADDR_UNDEF)

        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HADDR_UNDEF, \
                    "unable to get HDF5 file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the underlying HDF5 file. 
 *              This function is called shortly after an existing HDF5 file 
 *              is opened in order to tell the driver where the end of the 
 *              HDF5 data is located.
 *
 * Return:      SUCCEED (Can't fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr)
{
    H5FD_vfd_swmr_t	*file = (H5FD_vfd_swmr_t *)_file;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(H5FD_set_eoa(file->hdf5_file_lf, type, addr) < 0)

        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to set HDF5 file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_set_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_eof
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
H5FD_vfd_swmr_get_eof(const H5FD_t *_file, H5FD_mem_t type)
{
    const H5FD_vfd_swmr_t   *file = (const H5FD_vfd_swmr_t *)_file;
    haddr_t ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI_NOINIT

    /* LATER: need to determine the metadata file or underlying HDF5 file ? */
    if((ret_value = H5FD_get_eof(file->hdf5_file_lf, type)) == HADDR_UNDEF)

        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HADDR_UNDEF, \
                    "unable to set file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_get_eof() */


/*-------------------------------------------------------------------------
 * Function:       H5FD_vfd_swmr_get_handle
 *
 * Purpose:        Returns the file handle for the underling HDF5 file
 *
 * Returns:        SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_get_handle(H5FD_t *_file, hid_t fapl, void **file_handle)
{
    H5FD_vfd_swmr_t     *file = (H5FD_vfd_swmr_t *)_file;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    /* LATER? H5P_get(plist, H5F_ACS_SWMR_FILE_NAME, &type) */

    if((ret_value = H5FD_get_vfd_handle(file->hdf5_file_lf, 
                                        fapl, file_handle)) < 0)

        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, \
                    "unable to get handle for HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_get_handle() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_read
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
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_read(H5FD_t *_file, H5FD_mem_t type, 
    hid_t H5_ATTR_UNUSED dxpl_id,
    const haddr_t addr, size_t size, void *buf /*out*/)
{
    const size_t init_size = size;
    haddr_t target_page;
    haddr_t page_offset;
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;
    H5FD_vfd_swmr_idx_entry_t *index = NULL; /* Metadata file index */
    H5FD_vfd_swmr_idx_entry_t *entry;
    unsigned entry_retries =                 /* # of retries */
        H5FD_VFD_SWMR_MD_INDEX_RETRY_MAX;
    uint64_t nanosec = 1;                    /* # of nanoseconds to sleep */
                                             /* between retries           */
    uint32_t num_entries = 0;                /* Number of entries in index */
    uint32_t fs_page_size;                   /* Page size */
    uint32_t computed_chksum;                /* Computed checksum */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file && file->pub.cls);
    HDassert(buf);

    index        = file->md_index.entries;
    num_entries  = file->md_index.num_entries;
    fs_page_size = file->md_header.fs_page_size;

    /* Try finding the addr from the index */
    target_page = addr / fs_page_size;

#if 0 /* JRM */
    HDfprintf(stderr,
        "vfd swmr read target page/size = %" PRIu64 "/%" PRIu32 "\n",
        target_page, size);
    HDfprintf(stderr, "vfd swmr read index num_entries = %d\n", num_entries);
#endif /* JRM */

    entry = vfd_swmr_pageno_to_mdf_idx_entry(index, num_entries, target_page);

#if 0 /* JRM */
    if (entry != NULL) {
        HDfprintf(stderr,
            "vfd swmr read found target page/idx %" PRIu64 "/%td.\n",
            target_page, entry - index);
    } else {
        HDfprintf(stderr,
            "vfd swmr read passing through page / size = %" PRIu64 "/%zu\n",
            target_page, size);
    }
#endif /* JRM */

    if (entry == NULL) {
         /* Cannot find addr in index, read from the underlying hdf5 file */
        if(H5FD_read(file->hdf5_file_lf, type, addr, size, buf) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, \
                        "file read request failed");
        }
        HGOTO_DONE(SUCCEED);
    }

    /* Found in index, read from the metadata file */
    HDassert(addr >= target_page * fs_page_size);

    page_offset = addr - (target_page * fs_page_size);

#if 0 /* JRM */
    if ( ( page_offset != 0 ) &&
         ( ( file->pb_configured ) ||
           ( page_offset + size > fs_page_size ) ) ) {

        HDfprintf(stderr,
            "page_offset = %" PRIuHADDR ", size = %zu, "
            "page_size = %" PRIu32 "\n", page_offset, size, fs_page_size);
    }
#endif /* JRM */

    HDassert( ( page_offset == 0 ) ||
              ( ( ! file->pb_configured ) &&
                ( page_offset + size <= fs_page_size ) ) );

#if 0 /* JRM */
    HDfprintf(stderr,
        "addr = %" PRIuHADDR ", page = %" PRIuHADDR ", len = %zu\n",
        addr, addr / fs_page_size, size);
    HDfprintf(stderr,
        "reading index[%td]  fo/mdfo/l/chksum/fc/lc = %" PRIu64 "/%" PRIu64 "/%" PRIu32 "/%" PRIu32 "\n",
              entry - index,
              entry->hdf5_page_offset,
              entry->md_file_page_offset,
              entry->length,
              entry->chksum);


#endif /* JRM */

    HDassert(entry->hdf5_page_offset * fs_page_size <= addr);
    HDassert(addr < (entry->hdf5_page_offset + 1) * fs_page_size);
#if 0 /* JRM */
    if ( size != entry->length ) {

        HDfprintf(stderr,
            "size = %" PRIu32 ", index[%td].length = %" PRIu32 ".\n",
            size, entry - index, entry->length);
    }

    if ( ( init_size != 8 ) &&
         ( init_size != entry->length ) ) {

        HDfprintf(stderr,
            "ERROR: addr = %" PRIuHADDR ", page = %" PRIuHADDR
            ", len = %zu\n", addr, addr / fs_page_size, init_size);
    }

    HDassert((init_size == 8) ||
             (init_size == entry->length));
#endif /* JRM */

    HDassert( ( ! file->pb_configured ) ||
              ( init_size == entry->length ) );

    do {
        char *p = buf;

        if(HDlseek(file->md_fd, (HDoff_t)
                   ((entry->md_file_page_offset * fs_page_size)
                    + page_offset), SEEK_SET) < 0)

            HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, \
                        "unable to seek in metadata file")

        /* Coding borrowed from sec2 read */
        while(size > 0) {

            h5_posix_io_t     bytes_in    = 0;    /* # of bytes to read  */
            h5_posix_io_ret_t bytes_read  = -1;   /* # of bytes actually */
                                                  /* read                */

            /* Trying to read more bytes than the return type can handle is
             * undefined behavior in POSIX.
             */
            if(size > H5_POSIX_MAX_IO_BYTES)
                bytes_in = H5_POSIX_MAX_IO_BYTES;
            else
                bytes_in = (h5_posix_io_t)size;

            do {
                bytes_read = HDread(file->md_fd, p, bytes_in);
            } while(-1 == bytes_read && EINTR == errno);

            if(-1 == bytes_read) /* error */

                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, \
                "error reading the page/multi-page entry from the md file")

            HDassert(bytes_read >= 0);
            HDassert((size_t)bytes_read <= size);

            size -= (size_t)bytes_read;
            p += bytes_read;
        }

        /* Verify stored and computed checksums are equal */
#if 0 /* JRM */
        computed_chksum = H5_checksum_metadata(buf, entry->length, 0);
#else /* JRM */
        /* this is a hack to allow the library to find the superblock
         * signature -- clean this up.
         *                                   JRM -- 1/14/19
         */
        if ( file->pb_configured ) {
            computed_chksum = H5_checksum_metadata(buf, entry->length, 0);
        } else {
            computed_chksum = entry->chksum;
        }
#endif /* JRM */

#if 0 /* JRM */
        HDfprintf(stderr,
               "computed / actual chksum / fc / lc = 0x%" PRIx32 "/0x%" PRIx32 "/%x/%x\n",
               computed_chksum,
               entry->chksum,
               ((char *)buf)[0],
               ((char *)buf)[4095]);
#endif /* JRM */

        if(entry->chksum == computed_chksum)
            break;

         /* Double the sleep time next time */
         H5_nanosleep(nanosec);
         nanosec *= 2;

    } while(--entry_retries);

    /* Exhaust all retries for reading the page/multi-page entry */
    if(entry_retries == 0) {

        HDfprintf(stderr, "ERROR: addr = %" PRIuHADDR
            ", page = %" PRIuHADDR ", len = %zu\n",
                  addr, addr / fs_page_size, init_size);
        HDfprintf(stderr, " type = %d\n", type);
        HDfprintf(stderr,
        "reading index[%td]  fo/mdfo/l/chksum/fc/lc = %" PRIu64 "/%" PRIu64 "/%" PRIu32 "/%" PRIx32 "\n",
                  entry - index,
                  entry->hdf5_page_offset,
                  entry->md_file_page_offset,
                  entry->length,
                  entry->chksum);

        HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, \
                    "error in reading the page/multi-page entry")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_read() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_write
 *
 * Purpose:     Writes SIZE bytes of data to FILE beginning at address ADDR
 *              from buffer BUF according to data transfer properties in
 *              DXPL_ID.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_write(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id,
    haddr_t addr, size_t size, const void *buf)
{
    H5FD_vfd_swmr_t     *file       = (H5FD_vfd_swmr_t *)_file;
    herr_t              ret_value   = SUCCEED;                  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* This function should always fail.  For now assert FALSE */
    HDassert(FALSE);
    
    /* SHOULDN'T come here ?? */
    if(H5FD_write(file->hdf5_file_lf, type, addr, size, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file write request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_write() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_truncate
 *
 * Purpose:     Makes sure that the true file size is the same (or larger)
 *              than the end-of-address for the underlying HDF5 file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, 
   hbool_t closing)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;                       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* The VFD SWMR vfd should only be used by the VFD SWMR reader, 
     * and thus this file should only be opened R/O.
     *
     * Thus this function should never be called and should return error 
     *
     * For now, just assert FALSE.
     */
    HDassert(FALSE);

    HDassert(file);

    if(H5FD_truncate(file->hdf5_file_lf, closing) < 0)
        HGOTO_ERROR(H5E_IO, H5E_BADVALUE, FAIL, "unable to truncate the HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_truncate() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_lock
 *
 * Purpose:     To place an advisory lock on the underlying HDF5 file.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_lock(H5FD_t *_file, hbool_t rw)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;     /* Return value  */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    if(H5FD_lock(file->hdf5_file_lf, rw) < 0)

         HGOTO_ERROR(H5E_IO, H5E_CANTLOCK, FAIL, \
                     "unable to lock the HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_lock() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_unlock
 *
 * Purpose:     To remove the existing lock on the underlying HDF5 file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_unlock(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;                       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    
    if(H5FD_unlock(file->hdf5_file_lf) < 0)

        HGOTO_ERROR(H5E_IO, H5E_CANTUNLOCK, FAIL, \
                    "unable to unlock the HDF5 file")

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_vfd_swmr_unlock() */


/*
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
 * 2 If the tick number in the header is less than the tick last read by the VFD,
 *   then return an error.
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
 */
static herr_t
H5FD__vfd_swmr_load_hdr_and_idx(H5FD_t *_file, hbool_t open)
{
    H5FD_vfd_swmr_t *file =              /* VFD SWMR file struct          */
        (H5FD_vfd_swmr_t *)_file;  
    bool do_try;
    h5_retry_t retry;
    H5FD_vfd_swmr_md_header md_header;      /* Metadata file header, take 1 */
    H5FD_vfd_swmr_md_header md_header_two;  /* Metadata file header, take 2 */
    H5FD_vfd_swmr_md_index md_index;     /* Metadata file index           */
    herr_t ret_value  = SUCCEED;         /* Return value                  */
    htri_t rc;
    static uint64_t last_index_offset = 0;

    FUNC_ENTER_STATIC

    for (do_try = h5_retry_init(&retry, H5FD_VFD_SWMR_MD_LOAD_RETRY_MAX,
                      H5_RETRY_ONE_SECOND / 10, H5_RETRY_ONE_SECOND);
         do_try;
         do_try = h5_retry_next(&retry)) {

        /* Load and decode the header.  Go around again on a temporary
         * failure (FALSE).  Bail on an irrecoverable failure (FAIL).
         */
        rc = H5FD__vfd_swmr_header_deserialize(_file, &md_header);

        /* Temporary failure, try again. */
        if (rc == FALSE)
            continue;

        if (rc != TRUE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not read header");

        if (md_header.index_offset != last_index_offset) {
            fprintf(stderr, "index offset %" PRIu64 "\n",
                md_header.index_offset);
            last_index_offset = md_header.index_offset;
        }

        if (open)
            ; // ignore tick number on open
        else if (md_header.tick_num == file->md_header.tick_num) {
            /* If the tick number in the header hasn't increased since last
             * time, then there is not a complete new index to read, so
             * get out.
             */
            HGOTO_DONE(SUCCEED);
        } else if (md_header.tick_num < file->md_header.tick_num) {
            /* The tick number must not move backward. */
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                "tick number in header moved backwards");
        }

        HDassert(md_header.tick_num > file->md_header.tick_num || open);

        /* Load and decode the index.  Go around again on a temporary
         * failure (FALSE).  Bail on an irrecoverable failure (FAIL).
         */
        rc = H5FD__vfd_swmr_index_deserialize(_file, &md_index, &md_header);

        if (rc == FALSE)
            continue;

        if (rc != TRUE)
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "could not read index");

        /* If the tick_num is the same in both header and index,
         * and the header reads the same the second time as the first time,
         * then we should have a consistent picture of the index.
         */
        if (md_header.tick_num == md_index.tick_num &&
            (rc = H5FD__vfd_swmr_header_deserialize(_file,
                &md_header_two)) == TRUE &&
            md_header.tick_num == md_header_two.tick_num &&
            md_header.index_length == md_header_two.index_length &&
            md_header.index_offset == md_header_two.index_offset)
            break;

        if (md_index.entries != NULL) {

            HDassert(md_index.num_entries);
            md_index.entries = (H5FD_vfd_swmr_idx_entry_t *)
                               H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t,
                                             md_index.entries);
        }

        if (rc == FAIL) {
            HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                "could not re-read header");
        }
    }

    /* Exhaust all retries for loading and decoding the md file header 
     * and index 
     */
    if (!do_try) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, \
                "error in loading/decoding the metadata file header and index")
    }

    /* Free VFD local entries */
    if (file->md_index.entries != NULL) {

        HDassert(file->md_index.num_entries);

        file->md_index.entries = (H5FD_vfd_swmr_idx_entry_t *)
             H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, 
                           file->md_index.entries);
    }

    /* Copy header and index to VFD */
    file->md_header = md_header;
    file->md_index = md_index;
    md_index.entries = NULL;

done:

    FUNC_LEAVE_NOAPI(ret_value)

}  /* H5FD__vfd_swmr_load_hdr_and_idx() */


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
H5FD__vfd_swmr_header_deserialize(H5FD_t *_file, 
    H5FD_vfd_swmr_md_header *md_header)
{
    H5FD_vfd_swmr_t *file =                 /* VFD SWMR file struct */
        (H5FD_vfd_swmr_t *)_file; 
    uint8_t image[H5FD_MD_HEADER_SIZE];     /* Buffer for element data */
    uint32_t stored_chksum;                 /* Stored metadata checksum */
    uint32_t computed_chksum;               /* Computed metadata checksum */
    uint8_t *p;
    htri_t ret_value = TRUE;
    uint64_t index_length;
    ssize_t nread;

    FUNC_ENTER_STATIC

    /* Set file pointer to the beginning the file */
    if (lseek(file->md_fd, H5FD_MD_HEADER_OFF, SEEK_SET) < 0) {
        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, \
                    "unable to seek in metadata file");
    }

    /* Read the header */
    nread = read(file->md_fd, image, H5FD_MD_HEADER_SIZE);

    /* Try again if a signal interrupted the read. */
    if (nread == -1 && errno == EINTR)
        HGOTO_DONE(FALSE);

    /* We cannot recover from any other error by trying again,
     * so bail out.
     */
    if (nread == -1) {
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL,
                    "error in reading the shadow header");
    }

    if ((uint64_t)nread < H5FD_MD_HEADER_SIZE)
        HGOTO_DONE(FALSE);

    /* Verify magic number */
    if (memcmp(image, H5FD_MD_HEADER_MAGIC, H5_SIZEOF_MAGIC) != 0)
        HGOTO_DONE(FALSE);

    /* Verify stored and computed checksums are equal */
    H5F_get_checksums(image, H5FD_MD_HEADER_SIZE, &stored_chksum, 
                      &computed_chksum);

    if (stored_chksum != computed_chksum)
        HGOTO_DONE(FALSE);

    /* Header magic is already valid */
    p = image + H5_SIZEOF_MAGIC;

    /* Deserialize page size, tick number, index offset, index length */
    UINT32DECODE(p, md_header->fs_page_size);
    UINT64DECODE(p, md_header->tick_num);
    UINT64DECODE(p, md_header->index_offset);
    if ((index_length = uint64_decode(&p)) > SIZE_MAX) {
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                     "index is too large to hold in core");
    }

    md_header->index_length = (size_t)index_length;

    /* Checksum is already valid */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - image) <= H5FD_MD_HEADER_SIZE);

#if 0 /* JRM */
    HDfprintf(stderr, 
        "---read header ps/tick/idx_off/idx_len = %d / %lld / %lld / %lld\n", 
             md_header->fs_page_size, md_header->tick_num,
             md_header->index_offset, md_header->index_length);
#endif /* JRM */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5FD__vfd_swmr_header_deserialize() */



/*
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
 * Return:      Success:    TRUE
 *              Failure:    FAIL
 *              Retry:     FALSE
 *
 */
static htri_t
H5FD__vfd_swmr_index_deserialize(const H5FD_t *_file, 
    H5FD_vfd_swmr_md_index *md_index, const H5FD_vfd_swmr_md_header *md_header)
{
    const H5FD_vfd_swmr_t *file = (const H5FD_vfd_swmr_t *)_file;
    uint8_t *image;                 /* Buffer */
    uint8_t *p = NULL;              /* Pointer to buffer */
    uint32_t stored_chksum;         /* Stored metadata checksum value */
    uint32_t computed_chksum;       /* Computed metadata checksum value */
    unsigned i;                     /* Local index variable */
    htri_t ret_value = TRUE;
    ssize_t nread;

    FUNC_ENTER_STATIC

    /* Allocate buffer for reading index */
    if (NULL == (image = H5MM_malloc(md_header->index_length))) {
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL,
                   "memory allocation failed for index's on disk image buffer");
    }

    /* We may seek past EOF.  That's ok, the read(2) will catch that. */
    if (lseek(file->md_fd, (HDoff_t)md_header->index_offset, SEEK_SET) < 0){
        HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL,
                    "unable to seek in metadata file");
    }

    nread = read(file->md_fd, image, md_header->index_length);

    /* Try again if a signal interrupted the read. */
    if (nread == -1 && errno == EINTR)
        HGOTO_DONE(FALSE);

    /* We cannot recover from any other error by trying again,
     * so bail out.
     */
    if (nread == -1) {
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL,
                    "error in reading the header in metadata file");
    }

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
        HGOTO_DONE(FALSE);

    /* If the index magic is incorrect, then assume that is a
     * temporary error such as a "torn write."  Try again.
     *
     * XXX XXX XXX
     * Under the new protocol, where the index is written in
     * one write(2), and the header is written in a distinct
     * second write(2), it is reasonable to expect that the
     * index-write is complete when the index-read occurs.
     * So we should not read bad magic because we read a 
     * "torn" write.
     *
     * (I am not sure I believe any recent version of UNIX or
     * Linux suffers from torn writes on local filesystems!
     * Linux manual pages indicate that there was an issue, but
     * it was fixed.)
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
    if (memcmp(image, H5FD_MD_INDEX_MAGIC, H5_SIZEOF_MAGIC) != 0)
        HGOTO_DONE(FALSE);

    /* Verify stored and computed checksums are equal */
    H5F_get_checksums(image, md_header->index_length, &stored_chksum, 
                      &computed_chksum);

    if (stored_chksum != computed_chksum)
        HGOTO_DONE(FALSE);

    p = image + H5_SIZEOF_MAGIC;

    /* Deserialize the index info: tick number, number of entries, entries, 
     * checksum 
     */
    UINT64DECODE(p, md_index->tick_num);
    UINT32DECODE(p, md_index->num_entries);

    /* Read index entries */
    if(md_index->num_entries) {
        /* Allocate memory for index entries */
        md_index->entries = H5FL_SEQ_MALLOC(H5FD_vfd_swmr_idx_entry_t, 
                                    md_index->num_entries);
        if (NULL == md_index->entries) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL,
                "memory allocation failed for index entries");
        }

        /* Decode index entries */
        for (i = 0; i < md_index->num_entries; i++) {
            UINT32DECODE(p, md_index->entries[i].hdf5_page_offset);
            UINT32DECODE(p, md_index->entries[i].md_file_page_offset);
            UINT32DECODE(p, md_index->entries[i].length);
            UINT32DECODE(p, md_index->entries[i].chksum);
         }
     } else
        md_index->entries = NULL;

     /* Checksum is already valid */
     UINT32DECODE(p, stored_chksum);

     /* Sanity check */
     HDassert((size_t)(p - image) <= md_header->index_length);

#if 0 /* JRM */
    HDfprintf(stderr, 
             " ---- read index tick/num_entries = %lld / %d \n", 
             md_index->tick_num, md_index->num_entries);
#endif /* JRM */


done:
    if (image != NULL)
        image = H5MM_xfree(image);

    if (ret_value == FAIL && md_index->entries != NULL) {

        HDassert(md_index->num_entries != 0);

        md_index->entries =
            H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, md_index->entries);
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
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:  Vailin Choi
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_vfd_swmr_get_tick_and_idx(H5FD_t *_file, hbool_t reload_hdr_and_index,
    uint64_t *tick_ptr, uint32_t *num_entries_ptr, 
    H5FD_vfd_swmr_idx_entry_t index[])
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;                       /* Return value  */

    assert(index == NULL || num_entries_ptr != NULL);

    FUNC_ENTER_NOAPI(FAIL)

    /* Load and decode the header and index as indicated */
    if (reload_hdr_and_index &&
        H5FD__vfd_swmr_load_hdr_and_idx(_file, FALSE) < 0) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL,
                "unable to load/decode md header and index")
    }

    /* Return tick_num */
    if(tick_ptr != NULL)
        *tick_ptr = file->md_header.tick_num;

    if (index != NULL) {

        if (*num_entries_ptr < file->md_index.num_entries) {
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL,
                "not enough space to copy index");
        }

        HDmemcpy(index, file->md_index.entries, 
                 (file->md_index.num_entries *
                  sizeof(file->md_index.entries[0])));
    }

    if(num_entries_ptr != NULL)
        *num_entries_ptr = file->md_index.num_entries;

done:

    FUNC_LEAVE_NOAPI(ret_value)

}  /* H5FD_vfd_swmr_get_tick_and_idx() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_is_vfd_swmr_driver()
 *
 * Purpose:     Determine if the driver is a VFD SWMR driver
 *
 * Return:      Success:    TRUE/FALSE
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5FD_is_vfd_swmr_driver(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    FUNC_LEAVE_NOAPI(file->pub.driver_id == H5FD_VFD_SWMR)
}  /* H5FD_is_vfd_swmr_driver() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_underlying_vfd()
 *
 * Purpose:     Retrieve the underlying driver for the HDF5 file
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
H5FD_t *
H5FD_vfd_swmr_get_underlying_vfd(H5FD_t *_file)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    FUNC_LEAVE_NOAPI(file->hdf5_file_lf)
}  /* H5FD_vfd_swmr_get_underlying_vfd() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_dump_status
 *
 * Purpose:     Dump a variety of information about the vfd swmr reader 
 *              vfd to stderr for debugging purposes.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
void
H5FD_vfd_swmr_dump_status(H5FD_t *_file, uint64_t page)
{
    hbool_t in_index = FALSE;
    int i = 0;
    uint32_t num_entries;
    H5FD_vfd_swmr_idx_entry_t *index;
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file; /* VFD SWMR file struct */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);

    index        = file->md_index.entries;
    num_entries  = file->md_index.num_entries;

    while ( ( ! in_index ) && ( i < (int)num_entries ) ) {

        if ( index[i].hdf5_page_offset == page ) {

            in_index = TRUE;
        }

        HDassert( ( i == 0 ) || 
                  ( index[i-1].hdf5_page_offset < index[i].hdf5_page_offset ) );

        i++;
    }

    HDfprintf(stderr, 
              "fd: tick = %" PRIu64 ", index_len = %" PRIu32 ", page %" PRIu64
              " in index = %s.\n",
              file->md_index.tick_num, num_entries, page,
              in_index ? "true" : "false");

    FUNC_LEAVE_NOAPI_VOID

}  /* H5FD_vfd_swmr_dump_status() */


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

}  /* H5FD_vfd_swmr_set_pb_configured() */




