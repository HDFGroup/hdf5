/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.ed>
 *              Thursday, July 11, 2002
 *
 * Purpose:  This is a "combination" MPI-2 and posix I/O driver.
 *              It uses MPI for coordinating the actions of several processes
 *              and posix I/O calls to do the actual I/O to the disk.
 *
 *              This driver was derived from the H5FDmpio.c driver and may
 *              share bugs/quirks/etc.
 *
 * Limitations:
 *              There is no "collective" I/O mode with this driver.
 *
 *              This will almost certainly _not_ work correctly for files
 *              accessed on distributed parallel systems with the file located
 *              on a non-parallel filesystem.
 *
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC  H5FD_mpiposix_init_interface


#include "H5private.h"      /* Generic Functions            */
#include "H5ACprivate.h"    /* Metadata cache               */
#include "H5Eprivate.h"     /* Error handling               */
#include "H5Fprivate.h"     /* File access                  */
#include "H5FDprivate.h"    /* File drivers                 */
#include "H5FDmpi.h"        /* MPI-based file drivers       */
#include "H5Iprivate.h"     /* IDs                          */
#include "H5MMprivate.h"    /* Memory management            */
#include "H5Pprivate.h"     /* Property lists               */

/* Features:
 *   H5_HAVE_GPFS   -- issue gpfs_fcntl() calls to hopefully improve
 *                     performance when accessing files on a GPFS
 *                     file system.
 *
 *   REPORT_IO      -- if set then report all POSIX file calls to stderr.
 *
 */
/* #define REPORT_IO */

#ifdef H5_HAVE_GPFS
#   include <gpfs_fcntl.h>
#endif

#ifdef H5_HAVE_PARALLEL

/*
 * The driver identification number, initialized at runtime if H5_HAVE_PARALLEL
 * is defined. This allows applications to still have the H5FD_MPIPOSIX
 * "constants" in their source code (it also makes this file strictly ANSI
 * compliant when H5_HAVE_PARALLEL isn't defined)
 */
static hid_t H5FD_MPIPOSIX_g = 0;

/*
 * The description of a file belonging to this driver.
 * The EOF value is only used just after the file is opened in order for the
 * library to determine whether the file is empty, truncated, or okay. The
 * MPIPOSIX driver doesn't bother to keep it updated since it's an expensive
 * operation.
 */
typedef struct H5FD_mpiposix_t {
    H5FD_t          pub;            /* public stuff, must be first          */
    int             fd;             /* the unix file handle                 */
    MPI_Comm        comm;           /* communicator                         */
    int             mpi_rank;       /* This process's rank                  */
    int             mpi_size;       /* Total number of processes            */
    haddr_t         eof;            /* end-of-file marker                   */
    haddr_t         eoa;            /* end-of-address marker                */
    haddr_t         last_eoa;       /* Last known end-of-address marker     */
    haddr_t         pos;            /* Current file I/O position            */
    H5FD_file_op_t  op;             /* Last file I/O operation              */
    hsize_t         naccess;        /* Number of (write) accesses to file   */
#ifdef H5_HAVE_GPFS
    size_t          blksize;        /* Block size of file system            */
#endif
    hbool_t         use_gpfs;       /* Use GPFS to write things             */
#ifndef H5_HAVE_WIN32_API
    /* On most systems the combination of device and i-node number uniquely
     * identify a file.  Note that Cygwin, MinGW and other Windows POSIX
     * environments have the stat function (which fakes inodes)
     * and will use the 'device + inodes' scheme as opposed to the
     * Windows code further below.
     */
    dev_t           device;     /* file device number   */
#ifdef H5_VMS
    ino_t           inode[3];   /* file i-node number   */
#else
    ino_t           inode;      /* file i-node number   */
#endif /* H5_VMS */
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
} H5FD_mpiposix_t;

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
#define MAXADDR                 (((haddr_t)1 << (8*sizeof(HDoff_t) - 1)) - 1)
#define ADDR_OVERFLOW(A)        (HADDR_UNDEF == (A) || ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z)        ((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)    (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||    \
                                    HADDR_UNDEF == (A) + (Z) ||             \
                                    (HDoff_t)((A) + (Z)) < (HDoff_t)(A))

/* Callbacks */
static void *H5FD_mpiposix_fapl_get(H5FD_t *_file);
static void *H5FD_mpiposix_fapl_copy(const void *_old_fa);
static herr_t H5FD_mpiposix_fapl_free(void *_fa);
static H5FD_t *H5FD_mpiposix_open(const char *name, unsigned flags, hid_t fapl_id,
            haddr_t maxaddr);
static herr_t H5FD_mpiposix_close(H5FD_t *_file);
static int H5FD_mpiposix_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t H5FD_mpiposix_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_mpiposix_get_eoa(const H5FD_t *_file, H5FD_mem_t UNUSED type);
static herr_t H5FD_mpiposix_set_eoa(H5FD_t *_file, H5FD_mem_t type, haddr_t addr);
static haddr_t H5FD_mpiposix_get_eof(const H5FD_t *_file);
static herr_t  H5FD_mpiposix_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_mpiposix_read(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
        size_t size, void *buf);
static herr_t H5FD_mpiposix_write(H5FD_t *_file, H5FD_mem_t type, hid_t fapl_id, haddr_t addr,
        size_t size, const void *buf);
static herr_t H5FD_mpiposix_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static int H5FD_mpiposix_mpi_rank(const H5FD_t *_file);
static int H5FD_mpiposix_mpi_size(const H5FD_t *_file);
static MPI_Comm H5FD_mpiposix_communicator(const H5FD_t *_file);

/* MPIPOSIX-specific file access properties */
typedef struct H5FD_mpiposix_fapl_t {
    hbool_t     use_gpfs;   /* use GPFS hints       */
    MPI_Comm    comm;       /* communicator         */
} H5FD_mpiposix_fapl_t;

/* The MPIPOSIX file driver information */
static const H5FD_class_mpi_t H5FD_mpiposix_g = {
    {   /* Start of superclass information */
    "mpiposix",                     /* name             */
    MAXADDR,                        /* maxaddr          */
    H5F_CLOSE_SEMI,                 /* fc_degree        */
    NULL,                           /* sb_size          */
    NULL,                           /* sb_encode        */
    NULL,                           /* sb_decode        */
    sizeof(H5FD_mpiposix_fapl_t),   /* fapl_size        */
    H5FD_mpiposix_fapl_get,         /* fapl_get         */
    H5FD_mpiposix_fapl_copy,        /* fapl_copy        */
    H5FD_mpiposix_fapl_free,        /* fapl_free        */
    0,                              /* dxpl_size        */
    NULL,                           /* dxpl_copy        */
    NULL,                           /* dxpl_free        */
    H5FD_mpiposix_open,             /* open             */
    H5FD_mpiposix_close,            /* close            */
    H5FD_mpiposix_cmp,              /* cmp              */
    H5FD_mpiposix_query,            /* query            */
    NULL,                           /* get_type_map     */
    NULL,                           /* alloc            */
    NULL,                           /* free             */
    H5FD_mpiposix_get_eoa,          /* get_eoa          */
    H5FD_mpiposix_set_eoa,          /* set_eoa          */
    H5FD_mpiposix_get_eof,          /* get_eof          */
    H5FD_mpiposix_get_handle,       /* get_handle       */
    H5FD_mpiposix_read,             /* read             */
    H5FD_mpiposix_write,            /* write            */
    NULL,                           /* flush            */
    H5FD_mpiposix_truncate,         /* truncate         */
    NULL,                           /* lock             */
    NULL,                           /* unlock           */
    H5FD_FLMAP_DICHOTOMY            /* fl_map           */
    },  /* End of superclass information */
    H5FD_mpiposix_mpi_rank,         /* get_rank         */
    H5FD_mpiposix_mpi_size,         /* get_size         */
    H5FD_mpiposix_communicator      /* get_comm         */
};


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_init_interface
 *
 * Purpose:     Initializes any interface-specific data or routines. 
 *
 * Return:      Success:    The driver ID for the mpiposix driver.
 *              Failure:    Negative.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(H5FD_mpiposix_init())
} /* H5FD_mpiposix_init_interface() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_init
 *
 * Purpose:     Initialize this driver by registering the driver with the
 *              library.
 *
 * Return:      Success:    The driver ID for the mpiposix driver.
 *              Failure:    Negative.
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_mpiposix_init(void)
{
    hid_t ret_value = H5FD_MPIPOSIX_g;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(H5I_VFL != H5I_get_type(H5FD_MPIPOSIX_g))
        H5FD_MPIPOSIX_g = H5FD_register((const H5FD_class_t *)&H5FD_mpiposix_g, sizeof(H5FD_class_mpi_t), FALSE);

    /* Set return value */
    ret_value = H5FD_MPIPOSIX_g;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_init() */


/*---------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_term
 *
 * Purpose:     Shut down the VFD
 *
 * Returns:     SUCCEED (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              Friday, Jan 30, 2004
 *
 * Modification:
 *
 *---------------------------------------------------------------------------
 */
void
H5FD_mpiposix_term(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Reset VFL ID */
    H5FD_MPIPOSIX_g = 0;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5FD_mpiposix_term() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_mpiposix
 *
 * Purpose:     Store the user supplied MPI communicator COMM in
 *              the file access property list FAPL_ID which can then be used
 *              to create and/or open the file.  This function is available
 *              only in the parallel HDF5 library and is not collective.
 *
 *              comm is the MPI communicator to be used for file open as
 *              defined in MPI_FILE_OPEN of MPI-2. This function makes a
 *              duplicate of comm. Any modification to comm after this function
 *              call returns has no effect on the access property list.
 *
 *              If fapl_id has previously set comm value, it will be replaced
 *              and the old communicator is freed.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_mpiposix(hid_t fapl_id, MPI_Comm comm, hbool_t use_gpfs)
{
    H5FD_mpiposix_fapl_t    fa;
    H5P_genplist_t          *plist;      /* Property list pointer */
    herr_t                  ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "iMcb", fapl_id, comm, use_gpfs);

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")
    if (MPI_COMM_NULL == comm)
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a valid communicator")

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.use_gpfs = use_gpfs;

    /* duplication is done during driver setting. */
    ret_value = H5P_set_driver(plist, H5FD_MPIPOSIX, &fa);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_mpiposix() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_mpiposix
 *
 * Purpose:     If the file access property list is set to the H5FD_MPIPOSIX
 *              driver then this function returns a duplicate of the MPI
 *              communicator through the comm pointer. It is the responsibility
 *              of the application to free the returned communicator.
 *
 * Return:      Success:    Non-negative with the communicator and
 *                          information returned through the COMM
 *                          argument if non-null.  Since it is a duplicate
 *                          of the stored object, future modifications to
 *                          the access property list do not affect it and
 *                          it is the responsibility of the application to
 *                          free it.
 *              Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_mpiposix(hid_t fapl_id, MPI_Comm *comm/*out*/, hbool_t *use_gpfs/*out*/)
{
    H5FD_mpiposix_fapl_t    *fa;
    H5P_genplist_t          *plist;                 /* Property list pointer    */
    int                     mpi_code;               /* mpi return code          */
    herr_t                  ret_value = SUCCEED;    /* Return value             */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ixx", fapl_id, comm, use_gpfs);

    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list")
    if (H5FD_MPIPOSIX != H5P_get_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver")
    if (NULL == (fa = H5P_get_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info")

    /* Get MPI Communicator */
    if (comm){
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(fa->comm, comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code)
    }

    if (use_gpfs)
        *use_gpfs = fa->use_gpfs;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_fapl_mpiposix() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_fapl_get
 *
 * Purpose:     Returns a file access property list which could be used to
 *              create another file the same as this one.
 *
 * Return:      Success:    Ptr to new file access property list with all
 *                          fields copied from the file pointer.
 *              Failure:    NULL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_mpiposix_fapl_get(H5FD_t *_file)
{
    H5FD_mpiposix_t         *file = (H5FD_mpiposix_t*)_file;
    H5FD_mpiposix_fapl_t    *fa = NULL;
    int                     mpi_code;       /* MPI return code */
    void                    *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    if (NULL == (fa = H5MM_calloc(sizeof(H5FD_mpiposix_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Duplicate the communicator. */
    if (MPI_SUCCESS != (mpi_code=MPI_Comm_dup(file->comm, &fa->comm)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_dup failed", mpi_code)

    fa->use_gpfs = file->use_gpfs;

    /* Set return value */
    ret_value=fa;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_fapl_get() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_fapl_copy
 *
 * Purpose:     Copies the mpiposix-specific file access properties.
 *
 * Return:      Success:    Ptr to a new property list
 *              Failure:    NULL
 *
 * Programmer:  Albert Cheng
 *              Apr 24, 2003
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_mpiposix_fapl_copy(const void *_old_fa)
{
    void    *ret_value = NULL;
    const H5FD_mpiposix_fapl_t *old_fa = (const H5FD_mpiposix_fapl_t*)_old_fa;
    H5FD_mpiposix_fapl_t  *new_fa = NULL;
    int    mpi_code;  /* MPI return code */

    FUNC_ENTER_NOAPI_NOINIT

    if (NULL == (new_fa = H5MM_malloc(sizeof(H5FD_mpiposix_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the general information */
    HDmemcpy(new_fa, old_fa, sizeof(H5FD_mpiposix_fapl_t));

    /* Duplicate communicator. */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(old_fa->comm, &new_fa->comm)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_dup failed", mpi_code)

    new_fa->use_gpfs = old_fa->use_gpfs;
    ret_value = new_fa;

done:
    if (NULL == ret_value){
        /* cleanup */
        if (new_fa)
            H5MM_xfree(new_fa);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_fapl_free
 *
 * Purpose:     Frees the mpiposix-specific file access properties.
 *
 * Return:      SUCCEED (can't fail)
 *
 * Programmer:  Albert Cheng
 *              Apr 24, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_fapl_free(void *_fa)
{
    H5FD_mpiposix_fapl_t  *fa = (H5FD_mpiposix_fapl_t*)_fa;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(fa);

    /* Free the internal communicator */
    HDassert(MPI_COMM_NULL != fa->comm);
    MPI_Comm_free(&fa->comm);
    H5MM_xfree(fa);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_mpiposix_fapl_free() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_open
 *
 * Purpose:     Opens a file with name NAME.  The FLAGS are a bit field with
 *              purpose similar to the second argument of open(2) and which
 *              are defined in H5Fpublic.h. The file access property list
 *              FAPL_ID contains the properties driver properties and MAXADDR
 *              is the largest address which this file will be expected to
 *              access.  This is collective.
 *
 * Return:      Success:    A new file pointer.
 *              Failure:    NULL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_mpiposix_open(const char *name, unsigned flags, hid_t fapl_id,
         haddr_t maxaddr)
{
    H5FD_mpiposix_t *file = NULL;   /* New MPIPOSIX file struct         */
    int             o_flags;        /* Flags for file open call         */
    int             fd = -1;        /* File handle for file opened      */
    int             mpi_rank;       /* MPI rank of this process         */
    int             mpi_size;       /* Total number of MPI processes    */
    int             mpi_code;       /* mpi return code                  */
    const H5FD_mpiposix_fapl_t  *fa = NULL;     /* MPIPOSIX file access property list information                   */
    H5FD_mpiposix_fapl_t        _fa;            /* Private copy of default file access property list information    */
    H5P_genplist_t  *plist;         /* Property list pointer            */
    h5_stat_t       sb;             /* Portable 'stat' struct           */
#ifdef H5_HAVE_WIN32_API
    struct _BY_HANDLE_FILE_INFORMATION fileinfo;
#endif
    H5FD_t          *ret_value = NULL; /* Return value */
    MPI_Comm        comm_dup = MPI_COMM_NULL;

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name")
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr")
    if (ADDR_OVERFLOW(maxaddr))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr")

    /* Obtain a pointer to mpiposix-specific file access properties */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list")
    if (H5P_FILE_ACCESS_DEFAULT == fapl_id || H5FD_MPIPOSIX != H5P_get_driver(plist)) {
        _fa.comm = MPI_COMM_SELF; /*default*/
        _fa.use_gpfs = FALSE;
        fa = &_fa;
    } /* end if */
    else {
        if(NULL == (fa = (const H5FD_mpiposix_fapl_t *)H5P_get_driver_info(plist)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "bad VFL driver info")
    } /* end else */

    /* Duplicate the communicator for use by this file. */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(fa->comm, &comm_dup)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_dup failed", mpi_code)

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank (comm_dup, &mpi_rank)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code)
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size (comm_dup, &mpi_size)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code)

    /* Build the open flags */
    o_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;

    /* Only set the creation flag(s) for process 0 */
    if(0 == mpi_rank) {
        if (H5F_ACC_TRUNC & flags)
            o_flags |= O_TRUNC;
        if (H5F_ACC_CREAT & flags)
            o_flags |= O_CREAT;
        if (H5F_ACC_EXCL & flags)
            o_flags |= O_EXCL;
    } /* end if */

    /* Process 0 opens (or creates) the file while the rest of the
     * processes wait.  Then process 0 signals the other processes and they
     * open (never create) the file and all processes proceed.
     */
    /* Process 0 opens (or creates) file and broadcasts result to other processes */
    if(0 == mpi_rank) {
        /* Open the file */
        fd = HDopen(name, o_flags, 0666);
    } /* end if */

    /* Broadcast the results of the open() from process 0
     *
     * This is necessary because of the "tentative open" code in H5F_open()
     * where the file is attempted to be opened with different flags from the
     * user's, in order to check for the file's existence, etc.  Here, process 0
     * gets different flags from the other processes (since it is in charge of
     * creating the file, if necessary) and can fail in situations where the
     * other process's file opens would succeed, so allow the other processes
     * to check for that situation and bail out now also. - QAK
     */
    if (MPI_SUCCESS != (mpi_code= MPI_Bcast(&fd, sizeof(int), MPI_BYTE, 0, comm_dup)))
        HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)

    /* If the file open on process 0 failed, bail out on all processes now */
    if(fd < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")

    /* Other processes (non 0) wait for broadcast result from process 0 and then open file */
    if(mpi_rank != 0) {
        /* Open the file */
        if ((fd = HDopen(name, o_flags, 0666)) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open file")
    } /* end if */

    /* Process 0 fstat()s the file and broadcasts the results to the other processes */
    if(0 == mpi_rank) {
        /* Get the stat information */
        if (HDfstat(fd, &sb) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADFILE, NULL, "unable to fstat file")
    } /* end if */

    /* Broadcast the results of the fstat() from process 0 */
    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sb, sizeof(h5_stat_t), MPI_BYTE, 0, comm_dup)))
        HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code)

#ifdef H5_HAVE_GPFS
    if (fa->use_gpfs) {
        /*
         * Free all byte range tokens. This is a good thing to do if raw data
         * is aligned on 256kB boundaries (a GPFS page is 256kB). Care should
         * be taken that there aren't too many sub-page writes, or the mmfsd
         * may become overwhelmed.  This should probably eventually be passed
         * down here as a property. The gpfs_fcntl() will most likely fail if
         * 'fd' isn't on a GPFS file system. */
        struct {
            gpfsFcntlHeader_t   hdr;
            gpfsFreeRange_t     fr;
        } hint;
        HDmemset(&hint, 0, sizeof hint);
        hint.hdr.totalLength = sizeof hint;
        hint.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        hint.fr.structLen = sizeof hint.fr;
        hint.fr.structType = GPFS_FREE_RANGE;
        hint.fr.start = 0;
        hint.fr.length = 0;

        if (gpfs_fcntl(fd, &hint) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_FCNTL, NULL, "failed to send hints to GPFS")
    }
#endif  /* H5_HAVE_GPFS */

    /* Build the file struct and initialize it */
    if (NULL == (file=H5MM_calloc(sizeof(H5FD_mpiposix_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

#ifdef REPORT_IO
    HDfprintf(stderr, "open:  rank=%d name=%s file=0x%08lx\n", mpi_rank, name, (unsigned long)file);
#endif

    /* Set the general file information */
    file->fd = fd;
    file->eof = sb.st_size;

    /* for H5_HAVE_WIN32_API support. H5_HAVE_WIN32_API 'stat' does not have
     * st_blksize and st_blksize is only used for the H5_HAVE_GPFS case.
     */
#ifdef H5_HAVE_GPFS
    file->blksize = sb.st_blksize;
#endif

    /* Set this field in the H5FD_mpiposix_t struct for later use */
    file->use_gpfs = fa->use_gpfs;

    /* Set the MPI information */
    file->comm = comm_dup;
    file->mpi_rank = mpi_rank;
    file->mpi_size = mpi_size;

    /* Reset the last file I/O operation */
    file->pos = HADDR_UNDEF;
    file->op = OP_UNKNOWN;

    /* Set the information for the file's device and inode */
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
#ifdef H5_VMS
    file->inode[0] = sb.st_ino[0];
    file->inode[1] = sb.st_ino[1];
    file->inode[2] = sb.st_ino[2];
#else /* H5_VMS */
    file->inode = sb.st_ino;
#endif /* H5_VMS */
#endif /* H5_HAVE_WIN32_API */

    /* Indicate success */
    ret_value = (H5FD_t *)file;

done:
    /* Error cleanup */
    if(NULL == ret_value) {
        /* Close the file if it was left open */
        if(fd != -1)
            HDclose(fd);
        if (MPI_COMM_NULL != comm_dup)
            MPI_Comm_free(&comm_dup);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_open() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_close
 *
 * Purpose:     Closes a file.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_close(H5FD_t *_file)
{
    H5FD_mpiposix_t     *file = (H5FD_mpiposix_t*)_file;
    herr_t              ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    /* Close the unix file */
    if(HDclose(file->fd) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTCLOSEFILE, FAIL, "unable to close file")

    /* make sure all processes have closed the file before returning. */
    MPI_Barrier(file->comm);
    /* Clean up other stuff */
    MPI_Comm_free(&file->comm);
    H5MM_xfree(file);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_close() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_cmp
 *
 * Purpose:     Compares two files belonging to this driver using an
 *              arbitrary (but consistent) ordering.
 *
 * Return:      Success:    A value like strcmp()
 *              Failure:    never fails (arguments were checked by the caller).
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mpiposix_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_mpiposix_t  *f1 = (const H5FD_mpiposix_t*)_f1;
    const H5FD_mpiposix_t  *f2 = (const H5FD_mpiposix_t*)_f2;
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
#ifdef H5_VMS
    if(HDmemcmp(&(f1->inode), &(f2->inode), 3 * sizeof(ino_t)) < 0) HGOTO_DONE(-1)
    if(HDmemcmp(&(f1->inode), &(f2->inode), 3 * sizeof(ino_t)) > 0) HGOTO_DONE(1)
#else /* H5_VMS */
    if(f1->inode < f2->inode) HGOTO_DONE(-1)
    if(f1->inode > f2->inode) HGOTO_DONE(1)
#endif /* H5_VMS */
#endif /* H5_HAVE_WIN32_API */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_cmp() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      SUCCEED (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_query(const H5FD_t UNUSED *_file, unsigned long *flags /* out */)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags=0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_mpiposix_query() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_get_eoa
 *
 * Purpose:     Gets the end-of-address marker for the file. The EOA marker
 *              is the first address past the last byte allocated in the
 *              format address space.
 *
 * Return:      Success:    The end-of-address marker.
 *              Failure:    HADDR_UNDEF
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mpiposix_get_eoa(const H5FD_t *_file, H5FD_mem_t UNUSED type)
{
    const H5FD_mpiposix_t *file = (const H5FD_mpiposix_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->eoa)
} /* end H5FD_mpiposix_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file. This function is
 *              called shortly after an existing HDF5 file is opened in order
 *              to tell the driver where the end of the HDF5 data is located.
 *
 * Return:      SUCCEED (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_set_eoa(H5FD_t *_file, H5FD_mem_t UNUSED type, haddr_t addr)
{
    H5FD_mpiposix_t  *file = (H5FD_mpiposix_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    file->eoa = addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FD_mpi_posix_set_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_get_eof
 *
 * Purpose:     Gets the end-of-file marker for the file. The EOF marker
 *              is the real size of the file.
 *
 *              The MPIPOSIX driver doesn't bother keeping this field updated
 *              since that's a relatively expensive operation. Fortunately
 *              the library only needs the EOF just after the file is opened
 *              in order to determine whether the file is empty, truncated,
 *              or okay.
 *
 * Return:      Success:    The end-of-address marker.
 *              Failure:    HADDR_UNDEF
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mpiposix_get_eof(const H5FD_t *_file)
{
    const H5FD_mpiposix_t  *file = (const H5FD_mpiposix_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    FUNC_LEAVE_NOAPI(MAX(file->eof, file->eoa))
} /* end H5FD_mpiposix_get_eof() */


/*-------------------------------------------------------------------------
 * Function:       H5FD_mpiposix_get_handle
 *
 * Purpose:        Returns the file handle of MPI-POSIX file driver.
 *
 * Returns:        SUCCEED/FAIL
 *
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_get_handle(H5FD_t *_file, hid_t UNUSED fapl, void** file_handle)
{
    H5FD_mpiposix_t       *file = (H5FD_mpiposix_t *)_file;
    herr_t                ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid")

    *file_handle = &(file->fd);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_read
 *
 * Purpose:     Reads SIZE bytes of data from FILE beginning at address ADDR
 *              into buffer BUF according to data transfer properties in
 *              DXPL_ID using potentially complex file and buffer types to
 *              effect the transfer.
 *
 *              Reading past the end of the file returns zeros instead of
 *              failing.
 *
 * Return:      Success:    Non-negative. Result is stored in caller-supplied
 *                          buffer BUF.
 *              Failure:    Negative, Contents of buffer BUF are undefined.
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_read(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t UNUSED dxpl_id,
        haddr_t addr, size_t size, void *buf/*out*/)
{
    H5FD_mpiposix_t *file = (H5FD_mpiposix_t*)_file;
    ssize_t         nbytes;         /* Number of bytes read each I/O call */
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);
    HDassert(buf);

    /* Check for overflow conditions */
    if (HADDR_UNDEF == addr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined")
    if (REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow")
    if((addr + size) > file->eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow")

#ifdef REPORT_IO
    {
        int commrank;
        MPI_Comm_rank(MPI_COMM_WORLD, &commrank);
        HDfprintf(stderr, "read:  rank=%d file=0x%08lx type=%d, addr=%a size=%Zu\n",
                commrank, (unsigned long)file, (int)type, addr, size);
    }
#endif

    /* Seek to the correct location */
    if(addr != file->pos || OP_READ != file->op) {
        if(HDlseek(file->fd, (HDoff_t)addr, SEEK_SET) < 0)
            HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to seek to proper position")
    } /* end if */

    /* Read data, being careful of interrupted system calls, partial results,
     * and the end of the file.
     */
    while(size > 0) {

        h5_posix_io_t           bytes_in    = 0;    /* # of bytes to read       */
        h5_posix_io_ret_t       bytes_read  = -1;   /* # of bytes actually read */ 

        /* Trying to read more bytes than the return type can handle is
         * undefined behavior in POSIX.
         */
        if(size > H5_POSIX_MAX_IO_BYTES)
            bytes_in = H5_POSIX_MAX_IO_BYTES;
        else
            bytes_in = (h5_posix_io_t)size;

        do {
            bytes_read = HDread(file->fd, buf, bytes_in);
        } while(-1 == bytes_read && EINTR == errno);
        
        if(-1 == bytes_read) { /* error */
            int myerrno = errno;
            time_t mytime = HDtime(NULL);
            HDoff_t myoffset = HDlseek(file->fd, (HDoff_t)0, SEEK_CUR);

            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file read failed: time = %s, file descriptor = %d, errno = %d, error message = '%s', buf = %p, total read size = %llu, bytes this sub-read = %llu, bytes actually read = %llu, offset = %llu", HDctime(&mytime), file->fd, myerrno, HDstrerror(myerrno), buf, (unsigned long long)size, (unsigned long long)bytes_in, (unsigned long long)bytes_read, (unsigned long long)myoffset);
        } /* end if */
        
        if(0 == bytes_read) {
            /* end of file but not end of format address space */
            HDmemset(buf, 0, size);
            break;
        } /* end if */
        
        HDassert(bytes_read >= 0);
        HDassert((size_t)bytes_read <= size);
        
        size -= (size_t)bytes_read;
        addr += (haddr_t)bytes_read;
        buf = (char *)buf + bytes_read;
    } /* end while */

    /* Update current position */
    file->pos = addr;
    file->op = OP_READ;

done:
    /* Check for error */
    if(ret_value < 0) {
        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_read() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_write
 *
 * Purpose:     Writes SIZE bytes of data to FILE beginning at address ADDR
 *              from buffer BUF according to data transfer properties in
 *              DXPL_ID using potentially complex file and buffer types to
 *              effect the transfer.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
    size_t size, const void *buf)
{
    H5FD_mpiposix_t *file = (H5FD_mpiposix_t*)_file;
#if 0 /* JRM */
    int      mpi_code;  /* MPI return code */
#endif /* JRM */
    ssize_t         nbytes;                 /* Number of bytes written each I/O call    */
    H5P_genplist_t  *plist;                 /* Property list pointer                    */
    herr_t          ret_value = SUCCEED;    /* Return value                             */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);
    HDassert(H5I_GENPROP_LST == H5I_get_type(dxpl_id));
    HDassert(TRUE == H5P_isa_class(dxpl_id,H5P_DATASET_XFER));
    HDassert(buf);

    /* Check for overflow conditions */
    if (HADDR_UNDEF == addr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined")
    if (REGION_OVERFLOW(addr, size))
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow")
    if ((addr + size) > file->eoa)
        HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow")

    /* Obtain the data transfer properties */
    if(NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    /* Metadata specific actions */
    /* All metadata is now written from process 0 -- thus this function
     * needs to be re-written to reflect this.  For now I have simply
     * commented out the code that attempts to synchronize metadata
     * writes between processes, but we should really just flag an error
     * whenever any process other than process 0 attempts to write
     * metadata.
     *             -- JRM 9/1/05
     */
    if(type != H5FD_MEM_DRAW) {
        unsigned    block_before_meta_write = 0;    /* Whether to block before a metadata write */

        /* Check if we need to synchronize all processes before attempting
         * metadata write (Prevents race condition where the process writing
         * the metadata goes ahead and writes the metadata to the file before
         * all the processes have read the data, "transmitting" data from the
         * "future" to the reading process. -QAK )
         *
         * The only time we don't want to block before a metadata write is when
         * we are flushing out a bunch of metadata.  Then, we block before the
         * first write and don't block for further writes in the sequence.
         */
        if(H5P_exist_plist(plist,H5AC_BLOCK_BEFORE_META_WRITE_NAME) > 0)
            if(H5P_get(plist,H5AC_BLOCK_BEFORE_META_WRITE_NAME,&block_before_meta_write) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get H5AC property")

#if 0 /* JRM */
        if(block_before_meta_write)
            if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file->comm)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)
#endif /* JRM */

#if 0 /* JRM -- 3/23/10 */ /* this is no longer always the case */
        /* Only one process will do the actual write if all procs in comm write same metadata */
        if (file->mpi_rank != H5_PAR_META_WRITE)
            HGOTO_DONE(SUCCEED) /* skip the actual write */
#endif /* JRM */
    } /* end if */

#ifdef REPORT_IO
    {
        int commrank;
        MPI_Comm_rank(MPI_COMM_WORLD, &commrank);
        HDfprintf(stderr, "write: rank=%d file=0x%08lx type=%d, addr=%a size=%Zu %s\n",
                commrank, (unsigned long)file, (int)type, addr, size,
                0 == file->naccess ? "(FIRST ACCESS)" : "");
    }
#endif

    if (0 == file->naccess++) {
        /* First write access to this file */
#ifdef H5_HAVE_GPFS
        if (file->use_gpfs) {
            struct {
                gpfsFcntlHeader_t           hdr;
                gpfsMultipleAccessRange_t   mar;
            } hint;
            HDmemset(&hint, 0, sizeof hint);
            hint.hdr.totalLength = sizeof hint;
            hint.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
            hint.mar.structLen = sizeof hint.mar;
            hint.mar.structType = GPFS_MULTIPLE_ACCESS_RANGE;
            hint.mar.accRangeCnt = 1;
            hint.mar.accRangeArray[0].blockNumber = addr / file->blksize;
            hint.mar.accRangeArray[0].start = addr % file->blksize;
            hint.mar.accRangeArray[0].length = MIN(file->blksize-hint.mar.accRangeArray[0].start, size);
            hint.mar.accRangeArray[0].isWrite = 1;
            if (gpfs_fcntl(file->fd, &hint)<0)
                HGOTO_ERROR(H5E_FILE, H5E_FCNTL, NULL, "failed to send hints to GPFS")
        }
#endif  /* H5_HAVE_GPFS */
    }

    /* Seek to the correct location */
    if(addr != file->pos || OP_WRITE != file->op) {
        if(HDlseek(file->fd, (HDoff_t)addr, SEEK_SET) < 0)
            HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to seek to proper position")
    } /* end if */

    /* Write data, being careful of interrupted system calls, partial results,
     * and the end of the file.
     */
    while(size > 0) {

        h5_posix_io_t           bytes_in    = 0;    /* # of bytes to write          */
        h5_posix_io_ret_t       bytes_wrote = -1;   /* # of bytes actually written  */ 

        /* Trying to write more bytes than the return type can handle is
         * undefined behavior in POSIX.
         */
        if(size > H5_POSIX_MAX_IO_BYTES)
            bytes_in = H5_POSIX_MAX_IO_BYTES;
        else
            bytes_in = (h5_posix_io_t)size;

        do {
            bytes_wrote = HDwrite(file->fd, buf, bytes_in);
        } while(-1 == bytes_wrote && EINTR == errno);
        
        if(-1 == bytes_wrote) { /* error */
            int myerrno = errno;
            time_t mytime = HDtime(NULL);
            HDoff_t myoffset = HDlseek(file->fd, (HDoff_t)0, SEEK_CUR);

            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file write failed: time = %s, file descriptor = %d, errno = %d, error message = '%s', buf = %p, total write size = %llu, bytes this sub-write = %llu, bytes actually written = %llu, offset = %llu", HDctime(&mytime), file->fd, myerrno, HDstrerror(myerrno), buf, (unsigned long long)size, (unsigned long long)bytes_in, (unsigned long long)bytes_wrote, (unsigned long long)myoffset);
        } /* end if */
        
        if(0 == bytes_wrote) {
            /* end of file but not end of format address space */
            HDmemset(buf, 0, size);
            break;
        } /* end if */
        
        HDassert(bytes_wrote >= 0);
        HDassert((size_t)bytes_wrote <= size);
        
        size -= (size_t)bytes_wrote;
        addr += (haddr_t)bytes_wrote;
        buf = (char *)buf + bytes_wrote;
    } /* end while */

    /* Update current position */
    file->pos = addr;
    file->op = OP_WRITE;

done:
    /* Check for error */
    if(ret_value < 0) {
        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */
#if 0 /* JRM */
    /* Since metadata writes are now done by process 0 only, this broadcast
     * is no longer needed.  I leave it in and commented out to remind us
     * that we need to re-work this function to reflect this reallity.
     *
     *                                          -- JRM 9/1/05
     */

    /* Guard against getting into metadata broadcast in failure cases */
    else {
        /* when only one process writes, need to broadcast the ret_value to other processes */
        if (type != H5FD_MEM_DRAW) {
            if (MPI_SUCCESS != (mpi_code= MPI_Bcast(&ret_value, sizeof(ret_value), MPI_BYTE, H5_PAR_META_WRITE, file->comm)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code)
        } /* end if */
    } /* end else */
#endif /* JRM */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_write() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_truncate
 *
 * Purpose:     Makes sure that the true file size is the same (or larger)
 *              than the end-of-address.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpiposix_truncate(H5FD_t *_file, hid_t UNUSED dxpl_id, hbool_t UNUSED closing)
{
    H5FD_mpiposix_t  *file = (H5FD_mpiposix_t*)_file;
    herr_t          ret_value = SUCCEED;                /* Return value     */
    int             mpi_code;                           /* MPI return code  */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    /* Extend the file to make sure it's large enough */
    if(file->eoa > file->last_eoa) {
        /* Use the round-robin process to truncate (extend) the file */
        if(file->mpi_rank == H5_PAR_META_WRITE) {

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
#ifdef H5_VMS
            /* Reset seek offset to the beginning of the file, so that the file isn't
             * re-extended later.  This may happen on Open VMS. */
            if(-1 == HDlseek(file->fd, (HDoff_t)0, SEEK_SET))
                HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to seek to proper position")
#endif /* H5_VMS */
            if(-1 == HDftruncate(file->fd, (HDoff_t)file->eoa))
                HSYS_GOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "unable to extend file properly")
#endif /* H5_HAVE_WIN32_API */
        } /* end if */

        /* Don't let any proc return until all have extended the file.
         * (Prevents race condition where some processes go ahead and write
         * more data to the file before all the processes have finished making
         * it the shorter length, potentially truncating the file and dropping
         * the new data written)
         */
        if(MPI_SUCCESS != (mpi_code = MPI_Barrier(file->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code)

        /* Update the 'last' eoa and eof values */
        file->last_eoa = file->eoa;
        file->eof = file->eoa;

        /* Reset last file I/O information */
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_mpiposix_truncate() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_mpi_rank
 *
 * Purpose:     Returns the MPI rank for a process
 *
 * Return:      MPI rank.  Cannot report failure.
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mpiposix_mpi_rank(const H5FD_t *_file)
{
    const H5FD_mpiposix_t *file = (const H5FD_mpiposix_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->mpi_rank)
} /* end H5FD_mpiposix_mpi_rank() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_mpi_size
 *
 * Purpose:     Returns the number of MPI processes
 *
 * Return:      The number of MPI processes.  Cannot report failure.
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_mpiposix_mpi_size(const H5FD_t *_file)
{
    const H5FD_mpiposix_t *file = (const H5FD_mpiposix_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->mpi_size)
} /* end H5FD_mpiposix_mpi_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpiposix_communicator
 *
 * Purpose:     Returns the MPI communicator for the file.
 *
 * Return:      The MPI communicator.  Cannot report failure.
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 11, 2002
 *
 *-------------------------------------------------------------------------
 */
static MPI_Comm
H5FD_mpiposix_communicator(const H5FD_t *_file)
{
    const H5FD_mpiposix_t *file = (const H5FD_mpiposix_t*)_file;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file);
    HDassert(H5FD_MPIPOSIX == file->pub.driver_id);

    FUNC_LEAVE_NOAPI(file->comm)
} /* end H5FD_mpi_posix_communicator() */

#endif /*H5_HAVE_PARALLEL*/
