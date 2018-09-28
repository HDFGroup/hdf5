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
 * Purpose:  VFD SWMR driver
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

/* The driver identification number, initialized at runtime */
static hid_t H5FD_VFD_SWMR_g = 0;

typedef struct H5FD_vfd_swmr_t {
    H5FD_t pub;                                 /* public stuff, must be first */

    /* HDF5 file */
    char hdf5_filename[H5FD_MAX_FILENAME_LEN];  /* Name of the HDF5 file from open  */
    H5FD_t *hdf5_file_lf;                       /* Driver info for the HDF5 file */

    /* Metadata file */
    int md_fd;                                  /* File descriptor the metadata file */
    int32_t md_pages_reserved;                  /* # of pages reserved at the head of the metadata file */
    char md_file_path[H5FD_MAX_FILENAME_LEN];   /* Name of the metadate file */
    H5FD_vfd_swmr_md_header md_header;          /* Metadata file header */
    H5FD_vfd_swmr_md_index md_index;            /* Metadata file index */
} H5FD_vfd_swmr_t;

#define MAXADDR (((haddr_t)1<<(8*sizeof(HDoff_t)-1))-1)

/* Prototypes */
static herr_t H5FD_vfd_swmr_term(void);
static H5FD_t *H5FD_vfd_swmr_open(const char *name, unsigned flags, hid_t fapl_id,
            haddr_t maxaddr);
static herr_t H5FD_vfd_swmr_close(H5FD_t *_file);
static int H5FD_vfd_swmr_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_vfd_swmr_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_vfd_swmr_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_vfd_swmr_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_vfd_swmr_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD_vfd_swmr_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_vfd_swmr_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
            size_t size, void *buf);
static herr_t H5FD_vfd_swmr_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
            size_t size, const void *buf);
static herr_t H5FD_vfd_swmr_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t H5FD_vfd_swmr_lock(H5FD_t *_file, hbool_t rw);
static herr_t H5FD_vfd_swmr_unlock(H5FD_t *_file);

/* VFD SWMR */
static herr_t H5FD_vfd_swmr_header_deserialize(H5FD_t *_file, H5FD_vfd_swmr_md_header *md_header);
static herr_t H5FD_vfd_swmr_index_deserialize(H5FD_t *_file, H5FD_vfd_swmr_md_index *md_index, H5FD_vfd_swmr_md_header *md_header);
static herr_t H5FD_vfd_swmr_load_hdr_and_idx(H5FD_t *_file, hbool_t open);

static const H5FD_class_t H5FD_vfd_swmr_g = {
    "swmr",                     /* name                 */
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
H5FD_vfd_swmr_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_vfd_swmr_t *file = NULL;   /* VFD SWMR driver info    */
    H5P_genplist_t *plist;          /* Property list pointer */
    H5F_vfd_swmr_config_t *vfd_swmr_config = NULL;              /* Points to VFD SWMR configuration */
    unsigned file_retries = H5FD_VFD_SWMR_MD_FILE_RETRY_MAX;    /* Maximum retries for opening md file */
    uint64_t nanosec = 1;           /* # of nanoseconds to sleep between retries */
    H5FD_t *ret_value  = NULL;      /* Return value     */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get file access property list */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")

    /* Allocate buffer for reading the VFD SWMR configuration */
    if(NULL == (vfd_swmr_config = (H5F_vfd_swmr_config_t *)H5MM_malloc(sizeof(H5F_vfd_swmr_config_t))))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "memory allocation failed for H5F_vfd_swmr_config_t")

    /* Get VFD SWMR configuration */
    if(H5P_get(plist, H5F_ACS_VFD_SWMR_CONFIG_NAME, vfd_swmr_config) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get VFD SWMR config info")

    /* Ensure that this is the reader */
    HDassert(!vfd_swmr_config->vfd_swmr_writer);

    /* Create the new driver struct */
    if(NULL == (file = H5FL_CALLOC(H5FD_vfd_swmr_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate file struct")

    file->hdf5_file_lf = NULL;
    file->md_pages_reserved = vfd_swmr_config->md_pages_reserved;

    /* Retain a copy of the name used to open the HDF5 file */
    HDstrncpy(file->hdf5_filename, name, sizeof(file->hdf5_filename));
    file->hdf5_filename[sizeof(file->hdf5_filename) - 1] = '\0';

    /* Retain a copy of the metadata file name */
    HDstrncpy(file->md_file_path, vfd_swmr_config->md_file_path, sizeof(file->md_file_path));
    file->md_file_path[sizeof(file->md_file_path) - 1] = '\0';

    /* Retry on opening the metadata file */
    do {
        if((file->md_fd = HDopen(file->md_file_path, O_RDONLY)) >= 0)
            break;
        /* Sleep and double the sleep time on next retry */
        H5_nanosleep(nanosec);
        nanosec *= 2;               
    } while (--file_retries);

    /* Exhaust all retries for opening the md file */
    if(file_retries == 0)
        HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, NULL, "unable to open the metadata file after all retry attempts")

    /* Retry on loading and decoding the header and index in the metadata file */
    if(H5FD_vfd_swmr_load_hdr_and_idx((H5FD_t *)file, TRUE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "unable to load/decode the md file header/index")

    /* Hard-wired to open the underlying HDF5 file with SEC2 */
    if((file->hdf5_file_lf = H5FD_open(name, flags, H5P_FILE_ACCESS_DEFAULT, maxaddr)) == NULL)
        HGOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, NULL, "can't set driver info")

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
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close the HDF5 file")

    /* Close the metadata file */
    if(file->md_fd >= 0 && HDclose(file->md_fd) < 0)
        /* Push error, but keep going */
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "unable to close the metadata file")

    /* Free the index entries */
    if(file->md_index.num_entries && file->md_index.entries)
        file->md_index.entries = H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, file->md_index.entries);

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
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;     /* OK to aggregate metadata allocations                             */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA;    /* OK to accumulate metadata for faster writes                      */
        *flags |= H5FD_FEAT_DATA_SIEVE;             /* OK to perform data sieving for faster raw data reads & writes    */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA;    /* OK to aggregate "small" raw data allocations                     */
        *flags |= H5FD_FEAT_POSIX_COMPAT_HANDLE;    /* get_handle callback returns a POSIX file descriptor              */
        *flags |= H5FD_FEAT_SUPPORTS_SWMR_IO;       /* VFD supports the single-writer/multiple-readers (SWMR) pattern   */
        *flags |= H5FD_FEAT_DEFAULT_VFD_COMPATIBLE; /* VFD creates a file which can be opened with the default VFD      */

    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_vfd_swmr_query() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file for the underlying 
 *              HDF5 file. The EOA marker is the first address past the last byte
 *              allocated in the format address space.
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
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HADDR_UNDEF, "unable to get HDF5 file eoa")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the underlying HDF5 file. 
 *              This function is called shortly after an existing HDF5 file is opened
 *              in order to tell the driver where the end of the HDF5 data is located.
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
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, HADDR_UNDEF, "unable to set file eoa")

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

    if((ret_value = H5FD_get_vfd_handle(file->hdf5_file_lf, fapl, file_handle)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "unable to get handle for HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_get_handle() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_read
 *
 * Purpose:     TBD
 *
 * Return:      Success:    SUCCEED. Result is stored in caller-supplied
 *                          buffer BUF.
 *              Failure:    FAIL, Contents of buffer BUF are undefined.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_read(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id,
    haddr_t addr, size_t size, void *buf /*out*/)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;           /* VFD SWMR file struct */
    H5FD_vfd_swmr_idx_entry_t *index = NULL;                    /* Metadata file index */
    unsigned entry_retries = H5FD_VFD_SWMR_MD_INDEX_RETRY_MAX;  /* # of retries */
    uint64_t nanosec = 1;       /* # of nanoseconds to sleep between retries */
    uint32_t num_entries = 0;   /* Number of entries in index */
    uint32_t fs_page_size;      /* Page size */
    unsigned lo = 0, hi;        /* Low & high index values */
    unsigned my_idx = 0;        /* Final index value */
    int cmp;                    /* Return from comparison */
    uint32_t computed_chksum;   /* Computed checksum */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Try loading and decoding the header and index in the metadata file */
    if(H5FD_vfd_swmr_load_hdr_and_idx(_file, FALSE) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to load/decode the md file header/index")

    fs_page_size = file->md_header.fs_page_size;
    num_entries = file->md_index.num_entries;
    if(num_entries) {
        /* Allocate memory for index entries */
        if(NULL == (index = H5FL_SEQ_MALLOC(H5FD_vfd_swmr_idx_entry_t, num_entries)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "memory allocation failed for index entries")
        HDmemcpy(index, file->md_index.entries, num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t));
    }

    /* Try finding the addr from the index */
    cmp = -1;
    lo = 0;
    hi = num_entries;
    while(lo < hi && cmp) {
        my_idx = (lo + hi) / 2;
        cmp = H5F_addr_cmp(index[my_idx].hdf5_page_offset * fs_page_size, addr);
        if(cmp < 0)
            hi = my_idx;
        else
            lo = my_idx + 1;
    } /* end while */

    /* Found in index, read from the metadata file */
    if(cmp == 0) {
        HDassert(size == index[my_idx].length);

        do {
            if(HDlseek(file->md_fd, (HDoff_t)index[my_idx].md_file_page_offset * fs_page_size, SEEK_SET) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")

            /* Coding borrowed from sec2 read */
            while(size > 0) {

                h5_posix_io_t       bytes_in        = 0;    /* # of bytes to read       */
                h5_posix_io_ret_t   bytes_read      = -1;   /* # of bytes actually read */ 

                /* Trying to read more bytes than the return type can handle is
                 * undefined behavior in POSIX.
                 */
                if(size > H5_POSIX_MAX_IO_BYTES)
                    bytes_in = H5_POSIX_MAX_IO_BYTES;
                else
                    bytes_in = (h5_posix_io_t)size;

                do {
                    bytes_read = HDread(file->md_fd, buf, bytes_in);
                } while(-1 == bytes_read && EINTR == errno);
        
                if(-1 == bytes_read) /* error */
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "error reading the page/multi-page entry from the md file")
        
                HDassert(bytes_read >= 0);
                HDassert((size_t)bytes_read <= size);
        
                size -= (size_t)bytes_read;
                addr += (haddr_t)bytes_read;
                buf = (char *)buf + bytes_read;
            } /* end while size */

             /* Verify stored and computed checksums are equal */
             computed_chksum = H5_checksum_metadata(buf, index[my_idx].length, 0);
             if(index[my_idx].chksum == computed_chksum)
                break;

             /* Double the sleep time next time */
             H5_nanosleep(nanosec);
             nanosec *= 2;               

        } while(--entry_retries);

        /* Exhaust all retries for reading the page/multi-page entry */
        if(entry_retries == 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, "error in reading the page/multi-page entry")

    } else {  /* Cannot find addr in index, read from the underlying hdf5 file */
        if(H5FD_read(file->hdf5_file_lf, type, addr, size, buf) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "file read request failed")
    }

done:
    if(ret_value < 0) {
        if(index) {
            HDassert(num_entries);
            index = H5FL_SEQ_FREE(H5FD_vfd_swmr_idx_entry_t, num_entries);
        }
    } /* end if */

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
H5FD_vfd_swmr_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t closing)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;   /* VFD SWMR file struct  */
    herr_t ret_value = SUCCEED;                         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

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
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;   /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;     /* Return value  */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    if(H5FD_lock(file->hdf5_file_lf, rw) < 0)
         HGOTO_ERROR(H5E_IO, H5E_CANTLOCK, FAIL, "unable to lock the HDF5 file")

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
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;   /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;                         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    
    if(H5FD_unlock(file->hdf5_file_lf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTUNLOCK, FAIL, "unable to unlock the HDF5 file")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_vfd_swmr_unlock() */



/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_load_hdr_and_idx()
 *
 * Purpose:     Load and decode the header and index in the metadata file
 *              Try to load and decode the header:
 *              --If fail, RETRY
 *              --If succeed:
 *                  --If the size of header and index does not fit within md_pages_reserved, return error
 *                  --If NOT an initial open call:
 *                      --If tick_num just read is the same as the VFD's local copy, just return
 *                      --If tick_num just read is less than the VFD's local copy, return error
 *                  --If tick_num just read is greater than the VFD's local copy or an initial open call:
 *                      --Try to load and decode the index:
 *                          --If fail, RETRY
 *                          --If succeed:
 *                              --If tick_num in header matches that in index, replace the VFD's
 *                                local copy with the header and index just read
 *                              --If tick_num in header is 1 greater than that in index, RETRY
 *                              --Otherwise, return error
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_load_hdr_and_idx(H5FD_t *_file, hbool_t open)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;           /* VFD SWMR file struct */
    unsigned load_retries = H5FD_VFD_SWMR_MD_LOAD_RETRY_MAX;    /* Retries for loading header and index */
    uint64_t nanosec = 1;               /* # of nanoseconds to sleep between retries */
    H5FD_vfd_swmr_md_header md_header;  /* Metadata file header */
    H5FD_vfd_swmr_md_index md_index;    /* Metadata file index */
    herr_t ret_value  = SUCCEED;        /* Return value  */

    FUNC_ENTER_NOAPI_NOINIT

    do {
        HDmemset(&md_header, 0, sizeof(H5FD_vfd_swmr_md_header));
        HDmemset(&md_index, 0, sizeof(H5FD_vfd_swmr_md_index));

        /* Load and decode the header */
        if(H5FD_vfd_swmr_header_deserialize(_file, &md_header) >= 0) {

            /* Error if header + index fit does not within md_pages_reserved */
            if((H5FD_MD_HEADER_SIZE + md_header.index_length) > 
               (uint64_t)((hsize_t)file->md_pages_reserved * md_header.fs_page_size))
                HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "header + index does not fit within md_pages_reserved")

            if(!open) {
                if(md_header.tick_num == file->md_header.tick_num)
                    break; 
                else if(md_header.tick_num < file->md_header.tick_num)
                    HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "tick number read is less than local copy")
            }

            HDassert(md_header.tick_num > file->md_header.tick_num || open);

            /* Load and decode the index */
            if(H5FD_vfd_swmr_index_deserialize(_file, &md_index, &md_header) >= 0) {

                /* tick_num is the same in both header and index */
                if(md_header.tick_num == md_index.tick_num) {
                    HDmemcpy(&file->md_header, &md_header, sizeof(H5FD_vfd_swmr_md_header));
                    HDmemcpy(&file->md_index, &md_index, sizeof(H5FD_vfd_swmr_md_index));
                    break;
                }
                /* Error when tick_num in header is more than one greater that in the index */
                else if(md_header.tick_num > (md_index.tick_num + 1))
                    HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "tick number mis-match in header and index")

            } /* end if index ok */
        } /* end if header ok */

        /* Sleep and double the sleep time next time */
        H5_nanosleep(nanosec);
        nanosec *= 2;

    } while(--load_retries);

    /* Exhaust all retries for loading and decoding the md file header and index */
    if(load_retries == 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, "error in loading/decoding the metadata file header and index")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}  /* H5FD_vfd_swmr_load_hdr_and_idx() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_header_deserialize()
 *
 * Purpose:     To load and decode the header in the metadata file
 *              --Retry to get a file with size at least the size of the header
 *              --Retry on loading the valid magic and checksum for the header
 *              --Decode the header
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_header_deserialize(H5FD_t *_file, H5FD_vfd_swmr_md_header *md_header)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;   /* VFD SWMR file struct */
    struct stat stat_buf;                   /* Buffer for stat info */
    uint8_t image[H5FD_MD_HEADER_SIZE];     /* Buffer for element data */
    uint32_t stored_chksum;                 /* Stored metadata checksum value */
    uint32_t computed_chksum;               /* Computed metadata checksum value */
    uint64_t nanosec = 1;                   /* # of nanoseconds to sleep between retries */
    unsigned file_retries = H5FD_VFD_SWMR_MD_FILE_RETRY_MAX;        /* Retries for 'stat' the file */
    unsigned header_retries = H5FD_VFD_SWMR_MD_HEADER_RETRY_MAX;    /* Retries for loading header */
    uint8_t *p = NULL;                      /* Pointer to buffer */
    herr_t ret_value = SUCCEED;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Try to stat the metadata file till md header size */
    do {
        /* Retrieve the metadata file size */
        if(HDfstat(file->md_fd, &stat_buf))
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to fstat the md file")

        /* Verify file size is at least header size */
        if(stat_buf.st_size >= H5FD_MD_HEADER_SIZE)
            break;

        /* Sleep and double the sleep time next time */
        H5_nanosleep(nanosec);
        nanosec *= 2;               
    } while (--file_retries);

    /* Exhaust all retries for "stat" the md file */
    if(file_retries == 0)
        HGOTO_ERROR(H5E_VFL, H5E_OPENERROR, FAIL, "unable to the metadata file after all retry attempts")

    /* Try to get valid magic and checksum for header */
    p = image;
    do {
        /* Set file pointer to the beginning the file */
        if(HDlseek(file->md_fd, (HDoff_t)0, SEEK_SET) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")
        /* Read the header */
        if(HDread(file->md_fd, image, H5FD_MD_HEADER_SIZE) < H5FD_MD_HEADER_SIZE)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "error in reading the header in metadata file")

        /* Verify magic number */
        if(HDmemcmp(p, H5FD_MD_HEADER_MAGIC, (size_t)H5_SIZEOF_MAGIC) == 0) {

            /* Verify stored and computed checksums are equal */
            H5F_get_checksums(image, H5FD_MD_HEADER_SIZE, &stored_chksum, &computed_chksum);
            if(stored_chksum == computed_chksum)
                break;
        }
        /* Sleep and double the sleep time next time */
        H5_nanosleep(nanosec);
        nanosec *= 2;
    } while(--header_retries);

    /* Exhaust all retries for loading the header */
    if(header_retries == 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "incorrect checksum after after all read attempts")

    /* Header magic is already valid */
    p += H5_SIZEOF_MAGIC;

    /* Deserialize page size, tick number, index offset, index length */
    UINT32DECODE(p, md_header->fs_page_size);
    UINT64DECODE(p, md_header->tick_num);
    UINT64DECODE(p, md_header->index_offset);
    UINT64DECODE(p, md_header->index_length);

    /* Checksum is already valid */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)&image[0]) <= H5FD_MD_HEADER_SIZE);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_vfd_swmr_header_deserialize() */



/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_index_deserialize()
 *
 * Purpose:     Load and decode the index in the metadata file
 *              --Retry to get a file with size at least the size of the 
 *                (header+index)
 *              --Retry on loading the valid magic and checksum for the index
 *              --Decode the index
 *              --Decode the index entries if the tick number in the header and
 *                the index match
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_vfd_swmr_index_deserialize(H5FD_t *_file, H5FD_vfd_swmr_md_index *md_index, H5FD_vfd_swmr_md_header *md_header)
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;   /* VFD SWMR file struct */
    uint8_t *image;                 /* Buffer */
    uint8_t *p = NULL;              /* Pointer to buffer */
    struct stat stat_buf;           /* Buffer for stat info */
    uint32_t stored_chksum;         /* Stored metadata checksum value */
    uint32_t computed_chksum;       /* Computed metadata checksum value */
    uint64_t nanosec = 1;           /* # of nanoseconds to sleep between retries */
    unsigned i;                     /* Local index variable */
    unsigned file_retries = H5FD_VFD_SWMR_MD_FILE_RETRY_MAX;    /* Retries for 'stat' the file */
    unsigned index_retries = H5FD_VFD_SWMR_MD_INDEX_RETRY_MAX;  /* Retries for loading the index */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Try to stat the metadata file till at least md (header+index) size */
    do {
        /* Retrieve the metadata file size */
        if(HDfstat(file->md_fd, &stat_buf))
            HGOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "unable to fstat the md file")

        /* Verify file size is at least header size */
        if((uint64_t)stat_buf.st_size >= (H5FD_MD_HEADER_SIZE + md_header->index_length))
            break;

        /* Sleep and double the sleep time next time */
        H5_nanosleep(nanosec);
        nanosec *= 2;               
    } while (--file_retries);

    /* Allocate buffer for reading index */
    if(NULL == (image = (uint8_t *)H5MM_malloc(md_header->index_length)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "memory allocation failed for index's on disk image buffer")

    /* Verify magic and checksum for index */
    p = image;
    do {
        if(HDlseek(file->md_fd, (HDoff_t)md_header->index_offset, SEEK_SET) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_SEEKERROR, FAIL, "unable to seek in metadata file")
        if(HDread(file->md_fd, image, md_header->index_length) < (int64_t)md_header->index_length)
            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "error in reading the header in metadata file")

        /* Verify valid magic for index */
        if(HDmemcmp(p, H5FD_MD_INDEX_MAGIC, (size_t)H5_SIZEOF_MAGIC) == 0) {

            /* Verify stored and computed checksums are equal */
            H5F_get_checksums(image, md_header->index_length, &stored_chksum, &computed_chksum);
            if(stored_chksum == computed_chksum)
                break;
        }
        /* Double the sleep time next time */
        H5_nanosleep(nanosec);
        nanosec *= 2;               
    } while(--index_retries);

    /* Exhaust all retries for loading the index */
    if(index_retries == 0)
        HGOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "incorrect checksum after after all read attempts")

    /* Magic is already valid */
    p += H5_SIZEOF_MAGIC;

    /* Deserialize the index info: tick number, number of entries, entries, checksum */
    UINT64DECODE(p, md_index->tick_num);
    UINT32DECODE(p, md_index->num_entries);

    /* Read index entries */
    if(md_index->num_entries) {
        /* Allocate memory for index entries */
        if(NULL == (md_index->entries = H5FL_SEQ_MALLOC(H5FD_vfd_swmr_idx_entry_t, md_index->num_entries)))
            HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "memory allocation failed for index entries")

         /* Decode index entries */
         for(i = 0; i < md_index->num_entries; i++) {
            UINT32DECODE(p, md_index->entries[i].hdf5_page_offset);
            UINT32DECODE(p, md_index->entries[i].md_file_page_offset);
            UINT32DECODE(p, md_index->entries[i].length);
            UINT32DECODE(p, md_index->entries[i].chksum);
         } /* end for */

     } /* end if */

     /* Checksum is already valid */
     UINT32DECODE(p, stored_chksum);

     /* Sanity check */
     HDassert((size_t)(p - image) <= md_header->index_length);

done:
    if(image)
        image = (uint8_t *)H5MM_xfree(image);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD_vfd_swmr_index_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_vfd_swmr_get_tick_and_idx()
 *
 * Purpose:     Retrieve tick_num, num_entries and index from the metadata file
 *              --If the parameter "reload_hdr_and_index" is true, load and decode
 *                the header and index via H5FD_vfd_swmr_load_hdr_and_idx(), which 
 *                may replace the VFD's local copies of header and index with the
 *                latest info read.
 *              --Return tick_num, num_entries and index from the VFD's local copies.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_vfd_swmr_get_tick_and_idx(H5FD_t *_file, hbool_t reload_hdr_and_index,
    uint64_t *tick_ptr, uint32_t *num_entries_ptr, H5FD_vfd_swmr_idx_entry_t index[])
{
    H5FD_vfd_swmr_t *file = (H5FD_vfd_swmr_t *)_file;   /* VFD SWMR file struct */
    herr_t ret_value = SUCCEED;                         /* Return value  */

    FUNC_ENTER_NOAPI_NOINIT

    /* Load and decode the header and index as indicated */
    if(reload_hdr_and_index) {
        if(H5FD_vfd_swmr_load_hdr_and_idx(_file, FALSE) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTLOAD, FAIL, "unable to load/decode md header and index")
    }

    /* Return tick_num */
    if(tick_ptr != NULL)
        *tick_ptr = file->md_header.tick_num;

    if(num_entries_ptr != NULL) {
        if(*num_entries_ptr >= file->md_index.num_entries && index != NULL)
            HDmemcpy(index, file->md_index.entries, (file->md_index.num_entries * sizeof(H5FD_vfd_swmr_idx_entry_t)));

        *num_entries_ptr = file->md_index.num_entries;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}  /* H5FD_vfd_swmr_get_tick_and_idx() */
