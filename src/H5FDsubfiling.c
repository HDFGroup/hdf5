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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 29, 1999
 *
 * Purpose: The POSIX unbuffered file driver using only the HDF5 public
 *          API and with a few optimizations: the lseek() call is made
 *          only when the current file position is unknown or needs to be
 *          changed based on previous I/O through this driver (don't mix
 *          I/O from this driver with I/O from other parts of the
 *          application to the same file).
 */

#define H5S_FRIEND	        /*suppress error about including H5Spkg	  */
#include "H5FDdrvr_module.h" /* This source code file is part of the H5FD driver module */

#include "H5private.h"      /* Generic Functions        */
#include "H5Dprivate.h"		/* Dataset stuff            */
#include "H5Eprivate.h"     /* Error handling           */
#include "H5Fprivate.h"     /* File access              */
#include "H5CXprivate.h"    /* API contexts, etc.       */
#include "H5FDprivate.h"    /* File drivers             */
#include "H5FDsubfiling.h"  /* Subfiling file driver    */
#include "H5FLprivate.h"    /* Free Lists               */
#include "H5Iprivate.h"     /* IDs                      */
#include "H5MMprivate.h"    /* Memory management        */
#include "H5Pprivate.h"     /* Property lists           */
#include "H5Spkg.h"			/* For selections and creation of subfiling vectors */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_SUBFILING_g = 0;

/* These are used for the creation of read or write vectors */
static hssize_t sf_vlen = -1;
static hsize_t  *sf_offsets = NULL;
static hsize_t  *sf_sizes = NULL;
static void    **sf_bufs = NULL;


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
 *     Presents a system of subfiles as a file to the HDF5 library.
 *
 *
 *
 * `pub` (H5FD_t)
 *
 *     Instance of H5FD_t which contains all fields common to all VFDs.
 *     It must be the first item in this structure, since at higher levels,
 *     this structure will be treated as an instance of H5FD_t.
 *
 * `fa` (H5FD_subfiling_fapl_t)
 *
 *     Instance of `H5FD_subfiling_fapl_t` containing the subfiling  
 *     configuration data needed to "open" the HDF5 file.
 *
 *
 *  Document additional subfiling fields here.
 *
 *  Recall that the existing fields are inherited from the sec2 driver
 *  and should be kept or not as appropriate for the sub-filing VFD.
 *
 *
 * Programmer: Jacob Smith
 *
 ***************************************************************************/

typedef struct H5FD_subfiling_t {
    H5FD_t                pub;    /* public stuff, must be first      */
    H5FD_subfiling_fapl_t  fa;

    /* the following fields are inherrited from the sec2 VFD, and will
     * likely be deleted.
     */
    int             fd;     /* the filesystem file descriptor   */
    haddr_t         eoa;    /* end of allocated region          */
    haddr_t         eof;    /* end of file; current file size   */
    haddr_t         pos;    /* current file I/O position        */
    H5FD_file_op_t  op;     /* last operation                   */
    char            filename[H5FD_MAX_FILENAME_LEN];    /* Copy of file name from open operation */
	MPI_Info        info;
	MPI_Comm        comm;
	int             mpi_size;
	int             mpi_rank;

#ifndef H5_HAVE_WIN32_API
    /* On most systems the combination of device and i-node number uniquely
     * identify a file.  Note that Cygwin, MinGW and other Windows POSIX
     * environments have the stat function (which fakes inodes)
     * and will use the 'device + inodes' scheme as opposed to the
     * Windows code further below.
     */
    dev_t           device;     /* file device number   */
    ino_t           inode;      /* file i-node number   */
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
    DWORD           nFileIndexLow;
    DWORD           nFileIndexHigh;
    DWORD           dwVolumeSerialNumber;

    HANDLE          hFile;      /* Native windows file handle */
#endif  /* H5_HAVE_WIN32_API */

    /* Information from properties set by 'h5repart' tool
     *
     * Whether to eliminate the family driver info and convert this file to
     * a single file.
     */
    hbool_t         fam_to_single;

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
#define MAXADDR (((haddr_t)1<<(8*sizeof(HDoff_t)-1))-1)
#define ADDR_OVERFLOW(A)    (HADDR_UNDEF==(A) || ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z)    ((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)    (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||    \
                                 HADDR_UNDEF==(A)+(Z) ||                    \
                                (HDoff_t)((A)+(Z))<(HDoff_t)(A))

/* Prototypes */
static herr_t H5FD_subfiling_term(void);
static void   *H5FD_subfiling_fapl_get(H5FD_t *_file);
static void   *H5FD_subfiling_fapl_copy(const void *_old_fa);
static herr_t  H5FD_subfiling_fapl_free(void *_fa);
static H5FD_t *H5FD_subfiling_open(const char *name, unsigned flags, 
    hid_t fapl_id, haddr_t maxaddr);
static herr_t H5FD_subfiling_close(H5FD_t *_file);
static int H5FD_subfiling_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_subfiling_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_subfiling_get_eoa(const H5FD_t *_file, H5FD_mem_t type);
static herr_t H5FD_subfiling_set_eoa(H5FD_t *_file, H5FD_mem_t type, 
    haddr_t addr);
static haddr_t H5FD_subfiling_get_eof(const H5FD_t *_file, H5FD_mem_t type);
static herr_t  H5FD_subfiling_get_handle(H5FD_t *_file, hid_t fapl, 
    void** file_handle);
static herr_t H5FD_subfiling_read(H5FD_t *_file, H5FD_mem_t type, 
    hid_t fapl_id, haddr_t addr, size_t size, void *buf);
static herr_t H5FD_subfiling_write(H5FD_t *_file, H5FD_mem_t type, 
    hid_t fapl_id, haddr_t addr, size_t size, const void *buf);

static herr_t H5FD__subfiling_read_vector(H5FD_t *file, hid_t dxpl_id,
     uint32_t count, H5FD_mem_t types[], haddr_t addrs[], size_t sizes[], 
     void *bufs[] /* out */);
static herr_t H5FD__subfiling_write_vector(H5FD_t *file, hid_t dxpl_id,
    uint32_t count, H5FD_mem_t types[], haddr_t addrs[], size_t sizes[],
    void *bufs[] /* in */);

static herr_t H5FD_subfiling_truncate(H5FD_t *_file, hid_t dxpl_id, 
    hbool_t closing);
static herr_t H5FD_subfiling_lock(H5FD_t *_file, hbool_t rw);
static herr_t H5FD_subfiling_unlock(H5FD_t *_file);

static herr_t  H5FD_subfiling_validate_config(const H5FD_subfiling_fapl_t * fa);
static int  H5FD_subfiling_mpi_rank(const H5FD_t *_file);
static int H5FD_subfiling_mpi_size(const H5FD_t *_file);
static MPI_Comm H5FD_subfiling_communicator(const H5FD_t *_file);
static herr_t H5FD_subfiling_get_info(H5FD_t *_file, void **mpi_info);


static const H5FD_class_mpi_t H5FD_subfiling_g = {
	{
    "subfiling",                   /* name                 */
    MAXADDR,                       /* maxaddr              */
    H5F_CLOSE_WEAK,                /* fc_degree            */
    H5FD_subfiling_term,           /* terminate            */
    NULL,                          /* sb_size              */
    NULL,                          /* sb_encode            */
    NULL,                          /* sb_decode            */
    sizeof(H5FD_subfiling_fapl_t), /* fapl_size            */
    H5FD_subfiling_fapl_get,       /* fapl_get             */
    H5FD_subfiling_fapl_copy,      /* fapl_copy            */
    H5FD_subfiling_fapl_free,      /* fapl_free            */
    0,                             /* dxpl_size            */
    NULL,                          /* dxpl_copy            */
    NULL,                          /* dxpl_free            */
    H5FD_subfiling_open,           /* open                 */
    H5FD_subfiling_close,          /* close                */
    H5FD_subfiling_cmp,            /* cmp                  */
    H5FD_subfiling_query,          /* query                */
    NULL,                          /* get_type_map         */
    NULL,                          /* alloc                */
    NULL,                          /* free                 */
    H5FD_subfiling_get_eoa,        /* get_eoa              */
    H5FD_subfiling_set_eoa,        /* set_eoa              */
    H5FD_subfiling_get_eof,        /* get_eof              */
    H5FD_subfiling_get_handle,     /* get_handle           */
    H5FD_subfiling_read,           /* read                 */
    H5FD_subfiling_write,          /* write                */
    H5FD__subfiling_read_vector,   /* read_vector          */
    H5FD__subfiling_write_vector,  /* write_vector         */
    NULL,                          /* flush                */
    H5FD_subfiling_truncate,       /* truncate             */
    H5FD_subfiling_lock,           /* lock                 */
    H5FD_subfiling_unlock,         /* unlock               */
    H5FD_FLMAP_DICHOTOMY           /* fl_map               */
	},
	H5FD_subfiling_mpi_rank,
	H5FD_subfiling_mpi_size,
	H5FD_subfiling_communicator,
	H5FD_subfiling_get_info
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

    if(H5FD_subfiling_init() < 0)
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
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_subfiling_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;          /* Return value */

    FUNC_ENTER_NOAPI(H5I_INVALID_HID)

    if(H5I_VFL != H5I_get_type(H5FD_SUBFILING_g))
        H5FD_SUBFILING_g = H5FD_register(&H5FD_subfiling_g, sizeof(H5FD_class_t), FALSE);

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
H5FD_subfiling_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VFL ID */
    H5FD_SUBFILING_g = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_subfiling_term() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5Pset_fapl_subfiling
 *
 * Purpose:     Modify the file access property list to use the 
 *              H5FD_SUBFILING driver defined in this source file.  All 
 *              driver specfic properties are passed in as a pointer to 
 *              a suitably initialized instance of H5FD_subfiling_fapl_t
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
H5Pset_fapl_subfiling(hid_t                  fapl_id,
                      H5FD_subfiling_fapl_t *fa)
{
    H5P_genplist_t *plist     = NULL; /* Property list pointer */
    herr_t          ret_value = FAIL;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", fapl_id, fa);

    HDassert(fa != NULL);

    plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS);

    if (plist == NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, \
                    "not a file access property list")
    }

    if (FAIL == H5FD_subfiling_validate_config(fa)) {

        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid subfiling config")
    }

    ret_value = H5P_set_driver(plist, H5FD_SUBFILING, (void *)fa);

done:
    FUNC_LEAVE_API(ret_value)

} /* end H5Pset_fapl_subfiling() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_validate_config()
 *
 * Purpose:     Test to see if the supplied instance of 
 *              H5FD_subfiling_fapl_t contains internally consistant data.  
 *              Return SUCCEED if so, and FAIL otherwise.
 *
 *              Note the difference between internally consistant and
 *              correct.  As we will have to try to setup subfiling to
 *              determine whether the supplied data is correct,
 *              we will settle for internal consistancy at this point
 *
 * Return:      SUCCEED if instance of H5FD_subfiling_fapl_t contains 
 *              internally consistant data, FAIL otherwise.
 *
 * Programmer:  Jacob Smith
 *              9/10/17
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_validate_config(const H5FD_subfiling_fapl_t * fa)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(fa != NULL);

    if ( fa->version != H5FD_CURR_SUBFILING_FAPL_T_VERSION ) {
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                     "Unknown H5FD_subfiling_fapl_t version");
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
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_subfiling(hid_t                  fapl_id,
                      H5FD_subfiling_fapl_t *fa_out)
{
    const H5FD_subfiling_fapl_t *fa        = NULL;
    H5P_genplist_t              *plist     = NULL;
    herr_t                       ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", fapl_id, fa_out);

    if (fa_out == NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "fa_out is NULL")
    }

    plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS);

    if (plist == NULL) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access list")
    }

    if (H5FD_SUBFILING != H5P_peek_driver(plist)) {
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver")
    }

    fa = (const H5FD_subfiling_fapl_t *)H5P_peek_driver_info(plist);
    if (fa == NULL) {
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info")
    }

    /* Copy the subfiling fapl data out */
    HDmemcpy(fa_out, fa, sizeof(H5FD_subfiling_fapl_t));

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
H5FD_subfiling_fapl_get(H5FD_t *_file)
{
    H5FD_subfiling_t      *file      = (H5FD_subfiling_t*)_file;
    H5FD_subfiling_fapl_t *fa        = NULL;
    void                  *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    fa = (H5FD_subfiling_fapl_t *)H5MM_calloc(sizeof(H5FD_subfiling_fapl_t));

    if (fa == NULL) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                    "memory allocation failed")
    }

    /* Copy the fields of the structure */
    HDmemcpy(fa, &(file->fa), sizeof(H5FD_subfiling_fapl_t));

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
H5FD_subfiling_fapl_copy(const void *_old_fa)
{
    const H5FD_subfiling_fapl_t *old_fa = (const H5FD_subfiling_fapl_t*)_old_fa;
    H5FD_subfiling_fapl_t       *new_fa    = NULL;
    void                        *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT

    new_fa = (H5FD_subfiling_fapl_t *)H5MM_malloc(sizeof(H5FD_subfiling_fapl_t));
    if (new_fa == NULL) {
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                    "memory allocation failed");
    }

    HDmemcpy(new_fa, old_fa, sizeof(H5FD_subfiling_fapl_t));
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
 * Function:    H5FD_subfiling_fapl_free
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
H5FD_subfiling_fapl_free(void *_fa)
{
    H5FD_subfiling_fapl_t *fa = (H5FD_subfiling_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(fa != NULL); /* sanity check */

    H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5FD_subfiling_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_open
 *
 * Purpose:     Create and/or opens a file as an HDF5 file.
 *
 * Return:      Success:    A pointer to a new file data structure. The
 *                          public fields will be initialized by the
 *                          caller, which is always H5FD_open().
 *              Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_subfiling_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_subfiling_t     *file       = NULL;     /* subfiling VFD info        */
    int                   fd          = -1;       /* File descriptor          */
    int                   o_flags;                /* Flags for open() call    */
    int                   mpi_enabled = 0;
	int                   mpi_provides = -1;
    int                   my_rank;
#ifdef H5_HAVE_WIN32_API
    struct _BY_HANDLE_FILE_INFORMATION fileinfo;
#endif
    h5_stat_t             sb;
    H5FD_subfiling_fapl_t fa;
    H5FD_t               *ret_value = NULL;          /* Return value */
    char                 *dir_path = NULL;
    char                  file_prefix[H5FD_MAX_FILENAME_LEN];
    hid_t                 h5_file_id;
    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check on file offsets */
    HDcompile_assert(sizeof(HDoff_t) >= sizeof(size_t));

    /* Check arguments */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if(0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    if(ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr")

    /* Build the open flags */
    o_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;
    if(H5F_ACC_TRUNC & flags)
        o_flags |= O_TRUNC;
    if(H5F_ACC_CREAT & flags)
        o_flags |= O_CREAT;
    if(H5F_ACC_EXCL & flags)
        o_flags |= O_EXCL;

    if (FAIL == H5Pget_fapl_subfiling(fapl_id, &fa)) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "can't get property list")
    }

    if (MPI_Initialized(&mpi_enabled) == MPI_SUCCESS) {
		MPI_Query_thread(&mpi_provides);
		MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	}

    /* Open the file */
    if((fd = HDopen(name, o_flags, H5_POSIX_CREATE_MODE_RW)) < 0) {
        int myerrno = errno;
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
	    "unable to open file: name = '%s', errno = %d, error message = '%s', flags = %x, o_flags = %x",
                    name, myerrno, HDstrerror(myerrno), flags, (unsigned)o_flags);
    } /* end if */

	/* Avoid doing an addtional file stat on every MPI rank.
	 * By default the file open will stat the directory...
	 * We can be a bit more efficient by having rank 0 broadcast
	 * the stat buffer.
	 */
#if 0
	if (mpi_enabled && (my_rank == 0)) {
		int sb_size = sizeof(sb);

		MPI_Bcast(&sb, sb_size, MPI_BYTE, 0, MPI_COMM_WORLD);
	}				
#else
	if(HDfstat(fd, &sb) < 0)
		HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file")
#endif

    /* Create the new file struct */
    if(NULL == (file = H5FL_CALLOC(H5FD_subfiling_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "unable to allocate file struct")

    HDmemcpy(&(file->fa), &fa, sizeof(H5FD_subfiling_fapl_t));

    file->fd = fd;
    H5_CHECKED_ASSIGN(file->eof, haddr_t, sb.st_size, h5_stat_size_t);
    file->pos = HADDR_UNDEF;
    file->op = OP_UNKNOWN;
#ifdef H5_HAVE_WIN32_API
    file->hFile = (HANDLE)_get_osfhandle(fd);
    if(INVALID_HANDLE_VALUE == file->hFile)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to get Windows file handle")

    if(!GetFileInformationByHandle((HANDLE)file->hFile, &fileinfo))
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to get Windows file information")

    file->nFileIndexHigh = fileinfo.nFileIndexHigh;
    file->nFileIndexLow = fileinfo.nFileIndexLow;
    file->dwVolumeSerialNumber = fileinfo.dwVolumeSerialNumber;
#else /* H5_HAVE_WIN32_API */
    file->device = sb.st_dev;
    file->inode = sb.st_ino;
#endif /* H5_HAVE_WIN32_API */

    /* Retain a copy of the name used to open the file, for possible error reporting */
    HDstrncpy(file->filename, name, sizeof(file->filename));
    file->filename[sizeof(file->filename) - 1] = '\0';

    /* Check for non-default FAPL */
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        H5P_genplist_t      *plist;      /* Property list pointer */

        /* Get the FAPL */
        if(NULL == (plist = (H5P_genplist_t *)H5I_object(fapl_id)))
            HGOTO_ERROR(H5E_VFL, H5E_BADTYPE, NULL, "not a file access property list")

        /* This step is for h5repart tool only. If user wants to change file driver from
         * family to one that uses single files (sec2, etc.) while using h5repart, this
         * private property should be set so that in the later step, the library can ignore
         * the family driver information saved in the superblock.
         */
        if(H5P_exist_plist(plist, H5F_ACS_FAMILY_TO_SINGLE_NAME) > 0)
            if(H5P_get(plist, H5F_ACS_FAMILY_TO_SINGLE_NAME, &file->fam_to_single) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get property of changing family to single")
    } /* end if */

    /* Use a FILE_id which is consistent/constant across multiple MPI ranks */
    h5_file_id = (hid_t)file->inode;
    dir_path = strrchr(file->filename,'/');
    if (dir_path) {
        *dir_path = '\0';
        strcpy(file_prefix, file->filename);
        *dir_path = '/';
        dir_path = file_prefix;
    }

	/* Only open subfiling if we've enabled MPI */
	if (mpi_enabled &&
		(mpi_provides == MPI_THREAD_MULTIPLE) &&
		(sf_open_subfiles(h5_file_id, file->filename, dir_path, o_flags ) < 0)) {
	        HGOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't open subfiling files")
            MPI_Comm_dup(MPI_COMM_WORLD, &file->comm);
			MPI_Comm_rank(MPI_COMM_WORLD,&file->mpi_rank);
			MPI_Comm_size(MPI_COMM_WORLD,&file->mpi_size);
			file->info = MPI_INFO_NULL;
	}
	else {
            /* MPI isn't avail, so neither is subfiling...
             * In would be advantageous to replace subfiling parallel
             * subfiling with serial..
             */
    }
    /* Set return value */
    ret_value = (H5FD_t*)file;

done:
    if(NULL == ret_value) {
        if(fd >= 0)
            HDclose(fd);
        if(file)
            file = H5FL_FREE(H5FD_subfiling_t, file);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_open() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_close
 *
 * Purpose:     Closes an HDF5 file.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL, file not closed.
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_close(H5FD_t *_file)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;
    herr_t      ret_value = SUCCEED;                /* Return value */
    int         mpi_enabled = 0;

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(file);

    if (MPI_Initialized(&mpi_enabled) == MPI_SUCCESS) {
        /* Prepare to close the actual subfiles */
        hid_t h5_fid = (hid_t)file->inode;
        if (mpi_enabled && (sf_close_subfiles(h5_fid) < 0))
            HSYS_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "sf_close_subfiles returned and error")
    }
    /* Close the underlying HDF file */
    if(HDclose(file->fd) < 0)
        HSYS_GOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

    /* Release the file info */
    file = H5FL_FREE(H5FD_subfiling_t, file);

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
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_subfiling_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_subfiling_t   *f1 = (const H5FD_subfiling_t *)_f1;
    const H5FD_subfiling_t   *f2 = (const H5FD_subfiling_t *)_f2;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

#ifdef H5_HAVE_WIN32_API
    if(f1->dwVolumeSerialNumber < f2->dwVolumeSerialNumber) HGOTO_DONE(-1)
    if(f1->dwVolumeSerialNumber > f2->dwVolumeSerialNumber) HGOTO_DONE(1)

    if(f1->nFileIndexHigh < f2->nFileIndexHigh) HGOTO_DONE(-1)
    if(f1->nFileIndexHigh > f2->nFileIndexHigh) HGOTO_DONE(1)

    if(f1->nFileIndexLow < f2->nFileIndexLow) HGOTO_DONE(-1)
    if(f1->nFileIndexLow > f2->nFileIndexLow) HGOTO_DONE(1)
#else /* H5_HAVE_WIN32_API */
#ifdef H5_DEV_T_IS_SCALAR
    if(f1->device < f2->device) HGOTO_DONE(-1)
    if(f1->device > f2->device) HGOTO_DONE(1)
#else /* H5_DEV_T_IS_SCALAR */
    /* If dev_t isn't a scalar value on this system, just use memcmp to
     * determine if the values are the same or not.  The actual return value
     * shouldn't really matter...
     */
    if(HDmemcmp(&(f1->device),&(f2->device),sizeof(dev_t)) < 0) HGOTO_DONE(-1)
    if(HDmemcmp(&(f1->device),&(f2->device),sizeof(dev_t)) > 0) HGOTO_DONE(1)
#endif /* H5_DEV_T_IS_SCALAR */
    if(f1->inode < f2->inode) HGOTO_DONE(-1)
    if(f1->inode > f2->inode) HGOTO_DONE(1)
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
 * Return:      SUCCEED (Can't fail)
 *
 * Programmer:  Quincey Koziol
 *              Friday, August 25, 2000
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_query(const H5FD_t *_file, unsigned long *flags /* out */)
{
    const H5FD_subfiling_t  *file = (const H5FD_subfiling_t *)_file;    /* subfiling VFD info */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    /* Notice: the Mirror VFD Writer currently uses only the Sec2 driver as
     * the underying driver -- as such, the Mirror VFD implementation copies
     * these feature flags as its own. Any modifications made here must be
     * reflected in H5FDmirror.c
     * -- JOS 2020-01-13
     */
    if(flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;     /* OK to aggregate metadata allocations                             */
        *flags |= H5FD_FEAT_ACCUMULATE_METADATA;    /* OK to accumulate metadata for faster writes                      */
        *flags |= H5FD_FEAT_DATA_SIEVE;             /* OK to perform data sieving for faster raw data reads & writes    */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA;    /* OK to aggregate "small" raw data allocations                     */
        *flags |= H5FD_FEAT_POSIX_COMPAT_HANDLE;    /* get_handle callback returns a POSIX file descriptor              */
        *flags |= H5FD_FEAT_SUPPORTS_SWMR_IO;       /* VFD supports the single-writer/multiple-readers (SWMR) pattern   */
#if 0
		*flags |= H5FD_FEAT_HAS_MPI;				/* FIXME:: for experimentation only... */
#endif
        /* Check for flags that are set by h5repart */
        if(file && file->fam_to_single)
            *flags |= H5FD_FEAT_IGNORE_DRVRINFO; /* Ignore the driver info when file is opened (which eliminates it) */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_subfiling_query() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file. The EOA marker
 *              is the first address past the last byte allocated in the
 *              format address space.
 *
 * Return:      The end-of-address marker.
 *
 * Programmer:  Robb Matzke
 *              Monday, August  2, 1999
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_subfiling_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_subfiling_t  *file = (const H5FD_subfiling_t *)_file;

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
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_subfiling_t    *file = (H5FD_subfiling_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    file->eoa = addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_subfiling_set_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_get_eof
 *
 * Purpose:     Returns the end-of-file marker, which is the greater of
 *              either the filesystem end-of-file or the HDF5 end-of-address
 *              markers.
 *
 * Return:      End of file address, the first address past the end of the
 *              "file", either the filesystem file or the HDF5 file.
 *
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_subfiling_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_subfiling_t   *file = (const H5FD_subfiling_t *)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(file->eof)
} /* end H5FD_subfiling_get_eof() */


/*-------------------------------------------------------------------------
 * Function:       H5FD_subfiling_get_handle
 *
 * Purpose:        Returns the file handle of subfiling file driver.
 *
 * Returns:        SUCCEED/FAIL
 *
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, 
    void **file_handle)
{
    H5FD_subfiling_t         *file = (H5FD_subfiling_t *)_file;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(!file_handle)
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
 * Programmer:  Robb Matzke
 *              Thursday, July 29, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, 
    hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr, size_t size, void *buf /*out*/)
{
    H5FD_subfiling_t *file       = (H5FD_subfiling_t *)_file;
    herr_t           ret_value   = SUCCEED;                  /* Return value */
    hbool_t          addrs_cooked = FALSE;
    int              mpi_enabled = 0;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
    if(REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu", (unsigned long long)addr)

    addr += _file->base_addr;
    addrs_cooked = TRUE;        /* Follow the example of read_vector (see H5FDint.c) */

    if (MPI_Initialized(&mpi_enabled) == MPI_SUCCESS) {
        hid_t h5_fid = (hid_t)file->inode;
        if (mpi_enabled && (sf_read_independent(h5_fid, (int64_t)addr, (int64_t)size, 1, buf) < 0))
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "subfile read failed")
    }
    addr += (haddr_t)size;

    if ( addrs_cooked ) {
        addr -= _file->base_addr;
    }

    /* Update current position */
    file->pos = addr;
    file->op = OP_READ;

done:
    if(ret_value < 0) {
        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_read() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_subfiling_write
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
H5FD_subfiling_write(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, 
    hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr, size_t size, const void *buf)
{
    H5FD_subfiling_t *file       = (H5FD_subfiling_t *)_file;
    herr_t           ret_value   = SUCCEED;                  /* Return value */
    hbool_t          addrs_cooked = FALSE;
    int              mpi_enabled = 0;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file && file->pub.cls);
    HDassert(buf);

    /* Check for overflow conditions */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %llu", (unsigned long long)addr)
    if(REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %llu, size = %llu", (unsigned long long)addr, (unsigned long long)size)

    addr += _file->base_addr;
    addrs_cooked = TRUE;      /* Follow the example of read_vector (see H5FDint.c) */

    if (MPI_Initialized(&mpi_enabled) == MPI_SUCCESS) {
        hid_t h5_fid = (hid_t)file->inode;
        if (mpi_enabled && (sf_write_independent(h5_fid, (int64_t)addr, (int64_t)size, 1, buf) < 0)) 
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "subfile write failed")
    }

    addr += (haddr_t)size;    /* Point to the end of the current IO */

    if ( addrs_cooked ) {
        addr -= _file->base_addr;
    }

    /* Update current position and eof */
    file->pos = addr;
    file->op = OP_WRITE;
    if(file->pos > file->eof)
        file->eof = file->pos;

done:
    if(ret_value < 0) {
        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_write() */



/*-------------------------------------------------------------------------
 * Function:    H5FDsubfile__read_vector  (internal function)
 *
 * Purpose:     Perform count reads from the specified file at the offsets
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
 * Programmer:  JRM -- 6/10/20
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_read_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count,
                H5FD_mem_t types[], haddr_t addrs[], size_t sizes[],
                void *bufs[] /* out */)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;
    herr_t          ret_value = SUCCEED;    /* Return value             */
    hid_t           h5_fid;

    FUNC_ENTER_STATIC

    /* Check arguments
     * RAW - Do we really need to check arguments once again?
     * These have already been checked in H5FD_subfiling_read_vector (see below)!
     */
    if(!file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL")

    if((!types) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "types parameter can't be NULL if count is positive")

    if((!addrs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addrs parameter can't be NULL if count is positive")

    if((!sizes) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "sizes parameter can't be NULL if count is positive")

    if((!bufs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bufs parameter can't be NULL if count is positive")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    } else {
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")
    }

    /* Set DXPL for operation */
    H5CX_set_dxpl(dxpl_id);
    h5_fid = (hid_t)file->inode;
    if(sf_read_vector(h5_fid, count, (hsize_t *)addrs, (hsize_t *)sizes, bufs) != SUCCEED)
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file vector write request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FDsubfile__write_vector  (internal function)
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
 * Programmer:  JRM -- 6/10/20
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__subfiling_write_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count,
                 H5FD_mem_t types[], haddr_t addrs[], size_t sizes[],
                 void *bufs[] /* in */)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;
    herr_t          ret_value = SUCCEED;    /* Return value             */
    hid_t           h5_fid;

    FUNC_ENTER_STATIC

    HDassert(file != NULL); /* sanity check */
	
    /* Check arguments
     * RAW - Do we really need to check arguments once again?
     * These have already been checked in H5FD_subfiling_write_vector (see below)!
     */
    if(!file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL")

    if((!types) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "types parameter can't be NULL if count is positive")

    if((!addrs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addrs parameter can't be NULL if count is positive")

    if((!sizes) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "sizes parameter can't be NULL if count is positive")

    if((!bufs) && (count > 0))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bufs parameter can't be NULL if count is positive")

    /* Get the default dataset transfer property list if the user didn't provide one */
    if(H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    } else {
        if(TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list")
    }

    /* Set DXPL for operation */
    H5CX_set_dxpl(dxpl_id);
    h5_fid = (hid_t)file->inode;
    if(sf_write_vector(h5_fid, count, (hsize_t *)addrs, (hsize_t *)sizes, bufs) != SUCCEED)
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "file vector write request failed")

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
 * Programmer:  Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_subfiling_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id,
    hbool_t H5_ATTR_UNUSED closing)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;
    herr_t ret_value = SUCCEED;                 /* Return value */
    int mpi_enabled = 0;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    /* Extend the file to make sure it's large enough */
    if(!H5F_addr_eq(file->eoa, file->eof)) {
        if (MPI_Initialized(&mpi_enabled) == MPI_SUCCESS) {
        hid_t  h5_fid = (hid_t)file->inode;
        if (mpi_enabled && (sf_truncate(h5_fid, file->eof) < 0))
            HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to extend file properly")
    }
#ifdef H5_HAVE_WIN32_API
        LARGE_INTEGER   li;         /* 64-bit (union) integer for SetFilePointer() call */
        DWORD           dwPtrLow;   /* Low-order pointer bits from SetFilePointer()
                                     * Only used as an error code here.
                                     */
        DWORD           dwError;    /* DWORD error code from GetLastError() */
        BOOL            bError;     /* Boolean error flag */

        /* Windows uses this odd QuadPart union for 32/64-bit portability */
        li.QuadPart = (__int64)file->eoa;

        /* Extend the file to make sure it's large enough.
         *
         * Since INVALID_SET_FILE_POINTER can technically be a valid return value
         * from SetFilePointer(), we also need to check GetLastError().
         */
        dwPtrLow = SetFilePointer(file->hFile, li.LowPart, &li.HighPart, FILE_BEGIN);
        if(INVALID_SET_FILE_POINTER == dwPtrLow) {
            dwError = GetLastError();
            if(dwError != NO_ERROR )
                HGOTO_ERROR(H5E_FILE, H5E_FILEOPEN, FAIL, "unable to set file pointer")
        }

        bError = SetEndOfFile(file->hFile);
        if(0 == bError)
            HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to extend file properly")
#else /* H5_HAVE_WIN32_API */
        if(-1 == HDftruncate(file->fd, (HDoff_t)file->eoa))
            HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to extend file properly")
#endif /* H5_HAVE_WIN32_API */

        /* Update the eof value */
        file->eof = file->eoa;

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

done:
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
H5FD_subfiling_lock(H5FD_t *_file, hbool_t rw)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file; /* VFD file struct  */
    int lock_flags;                                   /* file locking flags */
    herr_t ret_value = SUCCEED;                       /* Return value       */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    /* Set exclusive or shared lock based on rw status */
    lock_flags = rw ? LOCK_EX : LOCK_SH;

    /* Place a non-blocking lock on the file */
    if(HDflock(file->fd, lock_flags | LOCK_NB) < 0) {
        if(ENOSYS == errno)
            HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "file locking disabled on this file system (use HDF5_USE_FILE_LOCKING environment variable to override)")
        else
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
H5FD_subfiling_unlock(H5FD_t *_file)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;   /* VFD file struct          */
    herr_t ret_value = SUCCEED;                 /* Return value             */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);

    if(HDflock(file->fd, LOCK_UN) < 0) {
        if(ENOSYS == errno)
            HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "file locking disabled on this file system (use HDF5_USE_FILE_LOCKING environment variable to override)")
        else
            HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to unlock file")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_subfiling_unlock() */

herr_t
H5FD__get_file_ino(const char *name, uint64_t *st_ino)
{
    herr_t ret_value = SUCCEED;  /* Return value */
    h5_stat_t  sb;

    FUNC_ENTER_PACKAGE

    if(HDstat(name, &sb) < 0)
        HSYS_GOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL, "unable to fstat file")
		
	*st_ino = sb.st_ino;
done:

    FUNC_LEAVE_NOAPI(ret_value)
}

static
herr_t create_simple_vector( hid_t file_space_id, void *memDataBuf, haddr_t addrBase, hssize_t elements, size_t type_extent, hssize_t *vlen, hsize_t **_offsets, hsize_t **_blocklens, void ***_bufs )
{
	int n_dims = H5Sget_simple_extent_ndims(file_space_id);
	hsize_t *offsets = *_offsets;
	hsize_t *blocklens = *_blocklens;
	void   **bufs = *_bufs;
	void *nextBuf = memDataBuf;

	assert(vlen);
	assert(_offsets);
	assert(_blocklens);
	assert(_bufs);

	if (n_dims > 0) {
		hsize_t simple_dims[n_dims];
		hsize_t stride[n_dims];
		if (H5Sget_simple_extent_dims(file_space_id, simple_dims, stride) < 0) {
			puts("H5Sget_simple_extent_dims returned an error");
			return -1;
		}

		if (*vlen < 0) {
			offsets = (hsize_t *)malloc((sizeof(haddr_t)));
			assert(offsets);

			blocklens = (hsize_t *)malloc((sizeof(hsize_t)));
			assert(blocklens);

			bufs = (void **)malloc((sizeof(void **)));
			assert(bufs);
		}
		bufs[0] = nextBuf;
		offsets[0] = addrBase;
		blocklens[0] = (hsize_t )((hsize_t)elements * type_extent);

		if (*vlen < 0) {
			*_offsets = offsets;
			*_blocklens = blocklens;
			*_bufs = bufs;
		}
		*vlen = 1;
		return 0;
	}
	return -1;
}


static
herr_t create_vector_from_hyperslab( hid_t file_space_id, void *memDataBuf, haddr_t addrBase, size_t type_extent, hssize_t *vlen, hsize_t **_offsets, hsize_t **_blocklens, void ***_bufs )
{
    herr_t ret_value = SUCCEED;
	hssize_t k, n_blocks = H5Sget_select_hyper_nblocks(file_space_id);

	void *nextBuf = memDataBuf;

	hsize_t stride[H5S_MAX_RANK];
	hsize_t count[H5S_MAX_RANK];

	hsize_t *strides = stride;
	hsize_t *counts = count;

	hsize_t *offsets = *_offsets;
	hsize_t *blocklens = *_blocklens;
	void   **bufs = *_bufs;

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
			offsets = (hsize_t *)realloc(offsets, ((size_t)n_blocks * sizeof(haddr_t)));
		} else {
			offsets = (hsize_t *)malloc(((size_t)n_blocks * sizeof(haddr_t)));
		}
		assert(offsets);
		if (blocklens) {
			blocklens = (hsize_t *)realloc(blocklens, ((size_t)n_blocks * sizeof(hsize_t)));
		} else {
			blocklens = (hsize_t *)malloc(((size_t)n_blocks * sizeof(hsize_t)));
		}
		assert(blocklens);
		if (bufs) {
			bufs = (void **)realloc(bufs, ((size_t)n_blocks * sizeof(void **)));
		} else {
			bufs = (void **)malloc(((size_t)n_blocks * sizeof(void **)));
		}
		assert(bufs);
		*vlen = n_blocks;
	}
	/* Fill vector elements */
	if ((ret_value = H5Sget_regular_hyperslab(file_space_id, offsets, strides, counts, blocklens)) < 0) {
		puts("H5Sget_regular_hyperslab failed");
		return -1;
	}

	for(k=0; k < n_blocks; k++) {
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

	*_offsets = offsets;
	*_blocklens = blocklens;
	*_bufs = bufs;

	return ret_value;
}


static
herr_t check_dims(int ndims, hsize_t *mem_dims, hsize_t *file_dims, int *diff_index)
{
	int i;
    herr_t ret_value = SUCCEED;
	for(i=0; i < ndims; i++) {
		if (mem_dims[i] != file_dims[i]) {
			*diff_index = i;
			return 0;
		}
	}
	/* ndims +1 == no differences */
	*diff_index = i;
	return ret_value;
}

static
haddr_t get_data_offset(int mpi_rank, int mpi_size, size_t dtype_extent, const H5S_t *mem_space, const H5S_t *file_space)
{
	haddr_t this_base = 0;
	return this_base;
}



static
haddr_t get_base_offset(int mpi_rank, int mpi_size, hid_t mem_space_id, hid_t file_space_id)
{
	haddr_t this_base = 0;
	int n_dims;
	int is_simple = H5Sis_simple(file_space_id);
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
			hsize_t total_size;
			if (H5Sget_simple_extent_dims(mem_space_id, mem_dims, mem_stride) < 0)
				puts("H5Sget_simple_extent_dims returned an error");
			if (H5Sget_simple_extent_dims(file_space_id, file_dims, file_stride) < 0)
				puts("H5Sget_simple_extent_dims returned an error");

			if (n_dims == 1) {
				if ((total_size = mem_dims[0] * (hsize_t)mpi_size) == file_dims[0]) {
					this_base = (mem_dims[0] * (hsize_t)mpi_rank);
				}
			}
			else {
				int diff_index = -1;
				if (check_dims(n_dims, mem_dims, file_dims, &diff_index) < 0)
					puts("check_dims returned an error");
				if ((total_size = mem_dims[diff_index] * (hsize_t)mpi_size) == file_dims[diff_index]) {
					this_base = (mem_dims[diff_index] * (hsize_t)mpi_rank);
				}
			}
		}
	}

	return this_base;
}



herr_t
H5FD__dataset_write_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
							   int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id,
							   hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf)
{
	H5D_t *dset = (H5D_t *)_dset;
    herr_t ret_value = SUCCEED;  /* Return value */
    hssize_t num_elem_file = -1, num_elem_mem = -1;
	H5S_sel_type sel_type;
	hsize_t mem_nelem, file_nelem;
	const H5S_t *mem_space;
	const H5S_t *file_space;

    FUNC_ENTER_PACKAGE

    if((num_elem_file = H5Sget_select_npoints(file_space_id)) < 0)
        puts("can't get number of points in file selection");
    if((num_elem_mem = H5Sget_select_npoints(mem_space_id)) < 0)
        puts("can't get number of points in memory selection");

    if(num_elem_file != num_elem_mem)
		puts("number of elements selected in file and memory dataspaces is different");
		
	if (H5S_get_validated_dataspace(mem_space_id, &mem_space) < 0) {
		puts("could not get a validated dataspace from mem_space_id");
	}
	else mem_nelem = mem_space->extent.nelem;
	if (H5S_get_validated_dataspace(file_space_id, &file_space) < 0) {
		puts("could not get a validated dataspace from file_space_id");
	}
	else file_nelem = file_space->extent.nelem;

	if (num_elem_file > 0) {
		sel_type = H5Sget_select_type(file_space_id);
		switch (sel_type) {
		case H5S_SEL_NONE:
			// printf("[%d] H5S_SEL_NONE\n", mpi_rank);
			break;
		case H5S_SEL_POINTS:
		{
			haddr_t rank_baseAddr;
			rank_baseAddr = get_base_offset(mpi_rank, mpi_size, mem_space_id, file_space_id);
			rank_baseAddr += dataset_baseAddr;
			// printf("[%d] H5S_SEL_POINTS - num_elem_file: %lld: UNSUPPORTED (for now)\n", mpi_rank, num_elem_file);
			ret_value = -1;
			goto done;

			break;
		}
		case H5S_SEL_HYPERSLABS:
		{
			int status;
			haddr_t rank_baseAddr;
#if 0
			rank_baseAddr = get_data_offset(mpi_rank, mpi_size, dtype_extent, mem_space, file_space);

#else		
			rank_baseAddr = get_base_offset(mpi_rank, mpi_size, mem_space_id, file_space_id);
			rank_baseAddr += dataset_baseAddr;
#endif
			// printf("[%d] H5S_SEL_HYPERSLABS, file_offset = %lld\n", mpi_rank, rank_baseAddr );
			if ((status = H5Sis_regular_hyperslab(file_space_id)) < 0) {
				puts("H5Sis_regular_hyperslab returned an error");
				ret_value = -1;
				goto done;
			}
			if (status > 0) {
				hssize_t previous_vlen = sf_vlen;
				if ((mem_space->extent.rank == 1)) {
					if (sf_offsets == NULL)
						sf_offsets = (hsize_t *)malloc(sizeof(hsize_t));
					if (sf_sizes == NULL)
						sf_sizes = (hsize_t *)malloc(sizeof(hsize_t));
					if (sf_bufs == NULL)
						sf_bufs = (void **)malloc(sizeof(void *));
					sf_vlen = 1;
					assert(sf_offsets);
					assert(sf_sizes);
					assert(sf_bufs);

					sf_offsets[0] = rank_baseAddr;
					sf_sizes[0] = num_elem_mem * dtype_extent;
					sf_bufs[0] = buf;
				}
				else if (create_vector_from_hyperslab(file_space_id, buf, rank_baseAddr, dtype_extent,
												 &sf_vlen, &sf_offsets, &sf_sizes, &sf_bufs) < 0) {
					puts("Unable to create vectors");
					ret_value = -1;
					goto done;
				}
				ret_value = sf_write_vector(h5_file_id, sf_vlen, sf_offsets, sf_sizes, sf_bufs);

				/* Possibly restore the sf_vlen value to accurately reflect the malloc sizes */
				if (sf_vlen < previous_vlen)
					sf_vlen = previous_vlen;
			}
			break;
		}
		case H5S_SEL_ALL:
		{
			int status;
			haddr_t rank_baseAddr;
			rank_baseAddr = get_base_offset(mpi_rank, mpi_size, mem_space_id, file_space_id);
			rank_baseAddr += dataset_baseAddr;
			// printf("[%d] H5S_SEL_ALL\n", mpi_rank);
			status = H5Sis_simple(file_space_id);
			if (status > 0) {
				if (create_simple_vector(file_space_id, buf, rank_baseAddr, num_elem_mem,
										 dtype_extent, &sf_vlen, &sf_offsets, &sf_sizes, &sf_bufs) < 0) {
					puts("Unable to create simple vectors");
					goto done;
				}
				ret_value = sf_write_vector(h5_file_id, sf_vlen, sf_offsets, sf_sizes, sf_bufs);
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
H5FD__dataset_read_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
							  int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id,
							  hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf)
{
	H5FD_t *dset = (H5FD_t *)_dset;	
    herr_t ret_value = SUCCEED;  /* Return value */
    hssize_t num_elem_file = -1, num_elem_mem = -1;
	H5S_sel_type sel_type;

    FUNC_ENTER_PACKAGE
    if((num_elem_file = H5Sget_select_npoints(file_space_id)) < 0)
        puts("can't get number of points in file selection");
    if((num_elem_mem = H5Sget_select_npoints(mem_space_id)) < 0)
        puts("can't get number of points in memory selection");

    if(num_elem_file != num_elem_mem)
		puts("number of elements selected in file and memory dataspaces is different");
		
	if (num_elem_file > 0) {
		sel_type = H5Sget_select_type(file_space_id);
		switch (sel_type) {
		case H5S_SEL_NONE:
			// printf("[%d] H5S_SEL_NONE\n", mpi_rank);
			break;
		case H5S_SEL_POINTS:
		{
			int status;
			haddr_t rank_baseAddr;
			rank_baseAddr = get_base_offset(mpi_rank, mpi_size, mem_space_id, file_space_id);
			rank_baseAddr += dataset_baseAddr;
			// printf("[%d] H5S_SEL_POINTS - num_elem_file: %lld: UNSUPPORTED (for now)\n", mpi_rank, num_elem_file);
			ret_value = -1;
			goto done;

			break;
		}
		case H5S_SEL_HYPERSLABS:
		{
			int status;
			haddr_t rank_baseAddr;
			const H5S_t *mem_space;
			const H5S_t *file_space;
			rank_baseAddr = get_base_offset(mpi_rank, mpi_size, mem_space_id, file_space_id);
			rank_baseAddr += dataset_baseAddr;
			if (H5S_get_validated_dataspace(mem_space_id, &mem_space) < 0) {
				puts("could not get a validated dataspace from mem_space_id");
			}
			if (H5S_get_validated_dataspace(file_space_id, &file_space) < 0) {
				puts("could not get a validated dataspace from file_space_id");
			}
			
			// printf("[%d] H5S_SEL_HYPERSLABS, file_offset = %lld\n", mpi_rank, rank_baseAddr );
			if ((status = H5Sis_regular_hyperslab(file_space_id)) < 0) {
				puts("H5Sis_regular_hyperslab returned an error");
				ret_value = -1;
				goto done;
			}
			if (status > 0) {
				hssize_t previous_vlen = sf_vlen;
				if (mem_space->extent.rank == 1) {
					if (sf_offsets == NULL)
						sf_offsets = (hsize_t *)malloc(sizeof(hsize_t));
					if (sf_sizes == NULL)
						sf_sizes = (hsize_t *)malloc(sizeof(hsize_t));
					if (sf_bufs == NULL)
						sf_bufs = (void **)malloc(sizeof(void *));
					sf_vlen = 1;
					assert(sf_offsets);
					assert(sf_sizes);
					assert(sf_bufs);

					sf_offsets[0] = rank_baseAddr;
					sf_sizes[0] = num_elem_mem * dtype_extent;
					sf_bufs[0] = buf;
				}
				else if (create_vector_from_hyperslab(file_space_id, buf, rank_baseAddr, dtype_extent,
												 &sf_vlen, &sf_offsets, &sf_sizes, &sf_bufs) < 0) {
					puts("Unable to create vectors");
					ret_value = -1;
					goto done;
				}
				ret_value = sf_read_vector(h5_file_id, sf_vlen, sf_offsets, sf_sizes, sf_bufs);

				/* Possibly restore the sf_vlen value to accurately reflect the malloc sizes */
				if (sf_vlen < previous_vlen)
					sf_vlen = previous_vlen;
			}
			break;
		}
		case H5S_SEL_ALL:
		{
			int status;
			haddr_t rank_baseAddr;
			rank_baseAddr = get_base_offset(mpi_rank, mpi_size, mem_space_id, file_space_id);
			rank_baseAddr += dataset_baseAddr;
			// printf("[%d] H5S_SEL_ALL\n", mpi_rank);
			status = H5Sis_simple(file_space_id);
			if (status > 0) {
				if (create_simple_vector(file_space_id, buf, rank_baseAddr, num_elem_mem,
										 dtype_extent, &sf_vlen, &sf_offsets, &sf_sizes, &sf_bufs) < 0) {
					puts("Unable to create simple vectors");
					goto done;
				}
				ret_value = sf_read_vector(h5_file_id, sf_vlen, sf_offsets, sf_sizes, sf_bufs);
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

static int
H5FD_subfiling_mpi_rank(const H5FD_t *_file)
{
    const H5FD_subfiling_t   *file = (const H5FD_subfiling_t*)_file;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(file);

    FUNC_LEAVE_NOAPI(file->mpi_rank)
} /* end H5FD__mpio_mpi_rank() */


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
static int
H5FD_subfiling_mpi_size(const H5FD_t *_file)
{
    const H5FD_subfiling_t   *file = (const H5FD_subfiling_t*)_file;

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
 * Programmer:  Robb Matzke
 *              Monday, August  9, 1999
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm
H5FD_subfiling_communicator(const H5FD_t *_file)
{
    const H5FD_subfiling_t   *file = (const H5FD_subfiling_t*)_file;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(file);

    FUNC_LEAVE_NOAPI(file->comm)
} /* end H5FD__subfiling_communicator() */


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
static herr_t
H5FD_subfiling_get_info(H5FD_t *_file, void **mpi_info)
{
    H5FD_subfiling_t *file = (H5FD_subfiling_t *)_file;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    if(!mpi_info)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mpi info not valid")

    *mpi_info = &(file->info);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FD__subfiling_get_info() */

