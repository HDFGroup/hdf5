/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*
* MODIFICATIONS
*       Robb Matzke, 30 Aug 1997
*       Added `ERRORS' fields to function prologues.
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*
   FILE
   hdf5file.c
   HDF5 file I/O routines

   EXPORTED ROUTINES
   H5Fcreate    -- Create an HDF5 file
   H5Fclose     -- Close an open HDF5 file

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
   H5F_init_interface    -- initialize the H5F interface
 */

/* Packages needed by this file... */
#include <H5private.h>          /*library functions                       */
#include <H5Aprivate.h>         /*atoms                                   */
#include <H5ACprivate.h>        /*cache                                   */
#include <H5Cprivate.h>         /*templates                               */
#include <H5Eprivate.h>         /*error handling                  */
#include <H5Gprivate.h>         /*symbol tables                           */
#include <H5Mprivate.h>         /*meta data                               */
#include <H5MMprivate.h>        /*core memory management          */

#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * Define the following if you want H5F_block_read() and H5F_block_write() to
 * keep track of the file position and attempt to minimize calls to the file
 * seek method.
 */
/* #define H5F_OPT_SEEK */

#define PABLO_MASK      H5F_mask

/*--------------------- Locally scoped variables -----------------------------*/

/*
 * Define the default file creation template.
 */
const H5F_create_t      H5F_create_dflt =
{
    0,                          /* Default user-block size */
    4,                          /* Default 1/2 rank for symtab leaf nodes */
    {                           /* Default 1/2 rank for btree intern nodes */
        16,                     /* Symbol table internal nodes          */
        32,                     /* Indexed storage internal nodes       */
        0,                      /* unused                               */
        0,                      /* unused                               */
        0,                      /* unused                               */
        0,                      /* unused                               */
        0,                      /* unused                               */
        0,                      /* unused                               */
    },
    4,                          /* Default offset size */
    4,                          /* Default length size */
    HDF5_BOOTBLOCK_VERSION,     /* Current Boot-Block version # */
    HDF5_SMALLOBJECT_VERSION,   /* Current Small-Object heap version # */
    HDF5_FREESPACE_VERSION,     /* Current Free-Space info version # */
    HDF5_OBJECTDIR_VERSION,     /* Current Object Directory info version # */
    HDF5_SHAREDHEADER_VERSION,  /* Current Shared-Header format version # */
};

/* Interface initialization */
static intn             interface_initialize_g = FALSE;
#define INTERFACE_INIT H5F_init_interface
static herr_t           H5F_init_interface(void);
static void             H5F_term_interface(void);

/* PRIVATE PROTOTYPES */
static H5F_t           *H5F_new(H5F_file_t *shared);
static H5F_t           *H5F_dest(H5F_t *f);
static herr_t           H5F_flush(H5F_t *f, hbool_t invalidate);
static herr_t           H5F_locate_signature(H5F_low_t *f_handle, haddr_t *addr /*out */ );

/*--------------------------------------------------------------------------
NAME
   H5F_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5F_init_interface()
   
RETURNS
   SUCCEED/FAIL
DESCRIPTION
    Initializes any interface-specific data or routines.

ERRORS

Modifications:
    Robb Matzke, 4 Aug 1997
    Changed pablo mask from H5_mask to H5F_mask for the FUNC_LEAVE call.
    It was already H5F_mask for the PABLO_TRACE_ON call.

--------------------------------------------------------------------------*/
static herr_t 
H5F_init_interface(void)
{
    herr_t                  ret_value = SUCCEED;
    FUNC_ENTER(H5F_init_interface, FAIL);

    /* Initialize the atom group for the file IDs */
    if ((ret_value = H5Ainit_group(H5_FILE, H5A_FILEID_HASHSIZE, 0, NULL)) != FAIL)
        ret_value = H5_add_exit(&H5F_term_interface);

    FUNC_LEAVE(ret_value);
}                               /* H5F_init_interface */

/*--------------------------------------------------------------------------
 NAME
    H5F_term_interface
 PURPOSE
    Terminate various H5F objects
 USAGE
    void H5F_term_interface()
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static void
H5F_term_interface(void)
{
    H5Adestroy_group(H5_FILE);
}

/*--------------------------------------------------------------------------
 NAME
       H5F_encode_length_unusual -- encode an unusual length size
 USAGE
       void H5F_encode_length_unusual(f, p, l)
       const H5F_t *f;             IN: pointer to the file record
       uint8 **p;               IN: pointer to buffer pointer to encode length in
       uint8 *l;                IN: pointer to length to encode

 ERRORS

 RETURNS
    none
 DESCRIPTION
    Encode non-standard (i.e. not 2, 4 or 8-byte) lengths in file meta-data.
--------------------------------------------------------------------------*/
void 
H5F_encode_length_unusual(const H5F_t *f, uint8 **p, uint8 *l)
{
    intn                    i = H5F_SIZEOF_SIZE(f);

#ifdef WORDS_BIGENDIAN
    /*
     * For non-little-endian platforms, encode each byte in memory backwards.
     */
    for (; i >= 0; i--, (*p)++)
        *(*p) = *(l + i);
#else
    /* platform has little-endian integers */
    for (; i >= 0; i--, (*p)++)
        *(*p) = *l;
#endif

#ifdef LATER
  done:
    if (ret_value == FALSE) {   /* Error condition cleanup */

    }                           /* end if */
#endif                       /* LATER */

    /* Normal function cleanup */

}                               /* H5F_encode_length_unusual */

/*--------------------------------------------------------------------------
 NAME
    H5Fget_create_template

 PURPOSE
    Get an atom for a copy of the file-creation template for this file

 USAGE
    hid_t H5Fget_create_template(fid)
        hid_t fid;    IN: File ID

 ERRORS
    ATOM      BADATOM       Can't get file struct. 
    FUNC      CANTCREATE    Can't create template. 
    FUNC      CANTINIT      Can't init template. 

 RETURNS
    Returns template ID on success, FAIL on failure

 DESCRIPTION
        This function returns an atom with a copy of the template parameters
    used to create a file.
--------------------------------------------------------------------------*/
hid_t
H5Fget_create_template(hid_t fid)
{
    H5F_t                  *file = NULL;
    hid_t                   ret_value = FAIL;
    H5F_create_t           *tmpl = NULL;

    FUNC_ENTER(H5Fget_create_template, FAIL);
    H5ECLEAR;

    /* check args */
    if (H5_FILE != H5Aatom_group(fid)) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a file");
    }
    if (NULL == (file = H5Aatom_object(fid))) {
        HRETURN_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't get file struct");
    }
    /* Create the template object to return */
    tmpl = H5MM_xmalloc(sizeof(H5F_create_t));
    *tmpl = file->shared->create_parms;
    if ((ret_value = H5C_create(H5C_FILE_CREATE, tmpl)) < 0) {
        HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
                      "can't register template");
    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
       H5F_compare_files -- compare file objects for the atom API
 USAGE
       intn HPcompare_filename(obj, key)
       const VOIDP obj;             IN: pointer to the file record
       const VOIDP key;             IN: pointer to the search key

 ERRORS

 RETURNS
       TRUE if the key matches the obj, FALSE otherwise
 DESCRIPTION
       Look inside the file record for the atom API and compare the the
       keys.
--------------------------------------------------------------------------*/
static intn
H5F_compare_files(const VOIDP _obj, const VOIDP _key)
{
    const H5F_t            *obj = (const H5F_t *) _obj;
    const H5F_search_t     *key = (const H5F_search_t *) _key;
    int                     ret_value = FALSE;

    FUNC_ENTER(H5F_compare_files, FALSE);

    ret_value = (obj->shared->key.dev == key->dev &&
                 obj->shared->key.ino == key->ino);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_locate_signature
 *
 * Purpose:     Finds the HDF5 boot block signature in a file.  The signature
 *              can appear at address 0, or any power of two beginning with
 *              512.
 *
 * Return:      Success:        SUCCEED.  The address of the signature is
 *                              returned through the ADDR argument.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_locate_signature(H5F_low_t *f_handle, haddr_t *addr /*out */ )
{
    haddr_t                 max_addr;
    uint8                   buf[H5F_SIGNATURE_LEN];
    uintn                   n = 9;

    FUNC_ENTER(H5F_locate_signature, FAIL);

    H5F_low_size(f_handle, &max_addr);
    H5F_addr_reset(addr);
    while (H5F_addr_lt(addr, &max_addr)) {
        if (H5F_low_read(f_handle, addr, H5F_SIGNATURE_LEN, buf) < 0) {
            HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "can't read file");
        }
        if (!HDmemcmp(buf, H5F_SIGNATURE, H5F_SIGNATURE_LEN))
            break;
        H5F_addr_pow2(n++, addr);
    }

    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5Fis_hdf5

 PURPOSE
    Check the file signature to detect an HDF5 file.

 USAGE
    hbool_t H5Fis_hdf5(filename)
        const char *filename;   IN: Name of the file to check
 ERRORS
    ARGS      BADRANGE      No filename specified. 
    FILE      BADFILE       Low-level file open failure. 
    IO        READERROR     Read error. 
    IO        READERROR     Seek error. 
    IO        SEEKERROR     Unable to determine length of file due to seek
                            failure. 

 RETURNS
    TRUE/FALSE/FAIL

 DESCRIPTION
    This function determines if a file is an HDF5 format file.
--------------------------------------------------------------------------*/
hbool_t 
H5Fis_hdf5(const char *filename)
{
    H5F_low_t              *f_handle = NULL;    /* file handle */
    haddr_t                 addr;       /* Address of file signature & header */
    hbool_t                 ret_value = BFALSE;

    FUNC_ENTER(H5Fis_hdf5, BFAIL);

    /* Clear errors and check args and all the boring stuff. */
    H5ECLEAR;
    if (filename == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, BFAIL, "no filename specified");

    /* Open the file */
    if (NULL == (f_handle = H5F_low_open(H5F_LOW_DFLT, filename, 0, NULL))) {
        HGOTO_ERROR(H5E_FILE, H5E_BADFILE, BFAIL,
                    "low-level file open failure");
    }
    if (H5F_locate_signature(f_handle, &addr) >= 0) {
        ret_value = BTRUE;
    }
  done:
    H5F_low_close(f_handle);    /* close the file we opened */
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_new
 *
 * Purpose:     Creates a new file object and initializes it.  The
 *              H5Fopen and H5Fcreate functions then fill in various
 *              fields.  If SHARED is a non-null pointer then the shared info
 *              to which it points has the reference count incremented.
 *              Otherwise a new, empty shared info struct is created.
 *
 * Errors:
 *
 * Return:      Success:        Ptr to a new file struct.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5F_t           *
H5F_new(H5F_file_t *shared)
{
    H5F_t                  *f = NULL;
    FUNC_ENTER(H5F_new, NULL);

    f = H5MM_xcalloc(1, sizeof(H5F_t));
    f->shared = shared;

    if (!f->shared) {
        f->shared = H5MM_xcalloc(1, sizeof(H5F_file_t));
        H5F_addr_undef(&(f->shared->boot_addr));
        H5F_addr_undef(&(f->shared->base_addr));
        H5F_addr_undef(&(f->shared->smallobj_addr));
        H5F_addr_undef(&(f->shared->freespace_addr));
        H5F_addr_undef(&(f->shared->hdf5_eof));

        /* Create a main cache */
        H5AC_create(f, H5AC_NSLOTS);
    }
    f->shared->nrefs++;

    FUNC_LEAVE(f);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_dest
 *
 * Purpose:     Destroys a file structure.  This function does not flush
 *              the cache or anything else; it only frees memory associated
 *              with the file struct.  The shared info for the file is freed
 *              only when its reference count reaches zero.
 *
 * Errors:
 *
 * Return:      Success:        NULL
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5F_t           *
H5F_dest(H5F_t *f)
{
    FUNC_ENTER(H5F_dest, NULL);

    if (f) {
        if (0 == --(f->shared->nrefs)) {
            H5AC_dest(f);
            f->shared->root_ent = H5MM_xfree(f->shared->root_ent);
            f->shared = H5MM_xfree(f->shared);
        }
        f->name = H5MM_xfree(f->name);
        H5MM_xfree(f);
    }
    FUNC_LEAVE(NULL);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_open
 *
 * Purpose:     Opens (or creates) a file.  This function understands the
 *              following flags which are similar in nature to the Posix
 *              open(2) flags.
 *
 *              H5F_ACC_WRITE:  Open with read/write access. If the file is
 *                              currently open for read-only access then it
 *                              will be reopened. Absence of this flag
 *                              implies read-only access.
 *
 *              H5F_ACC_CREAT:  Create a new file if it doesn't exist yet.
 *                              The permissions are 0666 bit-wise AND with
 *                              the current umask.  H5F_ACC_WRITE must also
 *                              be specified.
 *
 *              H5F_ACC_EXCL:   This flag causes H5F_open() to fail if the
 *                              file already exists.
 *
 *              H5F_ACC_TRUNC:  The file is truncated and a new HDF5 boot
 *                              block is written.  This operation will fail
 *                              if the file is already open.
 *
 *              Unlinking the file name from the group directed graph while
 *              the file is opened causes the file to continue to exist but
 *              one will not be able to upgrade the file from read-only
 *              access to read-write access by reopening it. Disk resources
 *              for the file are released when all handles to the file are
 *              closed. NOTE: This paragraph probably only applies to Unix;
 *              deleting the file name in other OS's has undefined results.
 *
 *              The CREATE_PARMS argument is optional.  A null pointer will
 *              cause the default file creation parameters to be used.
 *
 *              The TYPE argument determins the low-level type of file that
 *              is opened.  The special value H5F_LOW_DFLT uses the default
 *              method which is defined at compile time.
 *
 * Errors:
 *              ATOM      BADATOM       Can't unatomize default template
 *                                      id. 
 *              FILE      BADVALUE      Can't create file without write
 *                                      intent. 
 *              FILE      BADVALUE      Can't truncate without write intent. 
 *              FILE      CANTCREATE    Can't create file. 
 *              FILE      CANTCREATE    Can't truncate file. 
 *              FILE      CANTINIT      Can't get default file create template
 *                                      id. 
 *              FILE      CANTINIT      Can't write file boot block. 
 *              FILE      CANTOPENFILE  Bad address size. 
 *              FILE      CANTOPENFILE  Bad boot block version number. 
 *              FILE      CANTOPENFILE  Bad free space version number. 
 *              FILE      CANTOPENFILE  Bad length size. 
 *              FILE      CANTOPENFILE  Bad object dir version number. 
 *              FILE      CANTOPENFILE  Bad shared header version number. 
 *              FILE      CANTOPENFILE  Bad small object heap version number. 
 *              FILE      CANTOPENFILE  Bad symbol table internal node 1/2
 *                                      rank. 
 *              FILE      CANTOPENFILE  Bad symbol table leaf node 1/2 rank. 
 *              FILE      CANTOPENFILE  Can't read root symbol entry. 
 *              FILE      CANTOPENFILE  Cannot open existing file. 
 *              FILE      CANTOPENFILE  File cannot be reopened with write
 *                                      access. 
 *              FILE      CANTOPENFILE  File does not exist. 
 *              FILE      CANTOPENFILE  Invalid file family name. 
 *              FILE      FILEEXISTS    File already exists - CREAT EXCL
 *                                      failed. 
 *              FILE      FILEOPEN      File already open - TRUNC failed. 
 *              FILE      NOTHDF5       Can't find signature. 
 *              FILE      NOTHDF5       Can't read boot block. 
 *              FILE      READERROR     File is not readable. 
 *              FILE      TRUNCATED     Truncated file? 
 *              FILE      WRITEERROR    File is not writable. 
 *              IO        READERROR     Can't read boot block. 
 *
 * Return:      Success:        Ptr to the file pointer.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, September 23, 1997
 *
 * Modifications:
 *
 *      Robb Matzke, 11 Nov 1997
 *      If the name contains the pattern /[^%]%\d*[duxX]/ then the file is
 *      assumed to be a family of files.  The TYPE argument is ignored and
 *      H5F_LOW_FAM is used instead.
 *
 *-------------------------------------------------------------------------
 */
H5F_t                  *
H5F_open(const H5F_low_class_t *type, const char *name, uintn flags,
         const H5F_create_t *create_parms)
{
    H5F_t                  *f = NULL;   /*return value                  */
    H5F_t                  *ret_value = NULL;   /*a copy of `f'                 */
    H5F_t                  *old = NULL;         /*a file already opened         */
    H5F_search_t            search;     /*file search key               */
    H5F_low_t              *fd = NULL;  /*low level file desc           */
    hbool_t                 empty_file = FALSE;         /*is file empty?                */
    hbool_t                 file_exists = FALSE;        /*file already exists           */
    uint8                   buf[256];   /*I/O buffer..                  */
    const uint8            *p = NULL;   /*         ..and pointer into it */
    size_t                  fixed_size = 24;    /*size of fixed part of boot blk */
    size_t                  variable_size;      /*variable part of boot block   */
    H5F_create_t           *cp = NULL;  /*file creation parameters      */
    haddr_t                 addr1, addr2;       /*temporary address             */
    H5G_entry_t             root_ent;   /*root symbol table entry       */
    const char             *s = name;

    FUNC_ENTER(H5F_open, NULL);

    assert(name && *name);

    /*
     * Does the name look like a family name?  A family name always has a
     * percent (not preceded by a percent) followed by an optional plus and/or
     * minus, followed by optional digits, followed by the letter `d', `u',
     * `x', or `X'. This is a printf() format for an integer.
     */
    while (*s) {
        if ('%' != *s++)
            continue;
        if ('%' == *s) {
            s++;
            continue;
        }
        while (*s && isdigit(*s))
            s++;
        if ('d' != *s && 'u' != *s && 'x' != *s && 'X' != *s)
            continue;
        break;
    }
    if (*s) {
#ifdef H5F_DEBUG
        if (type != H5F_LOW_FAM) {
            fprintf(stderr, "HDF5-DIAG: opening a file family\n");
        }
#endif
        type = H5F_LOW_FAM;
    } else if (type == H5F_LOW_FAM) {
        HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                      "invalid file family name");
    }
    /*
     * If the name ends with `.h5' and there's another file that ends with
     * `.raw' then open the file as a split file.
     */
    {
        if (H5F_LOW_SPLIT != type && (s = strstr(name, ".h5")) && !s[3]) {
            char                    fullname[4096];
            strncpy(fullname, name, s - name);
            strcpy(fullname + (s - name), ".raw");
            if (H5F_low_access(H5F_LOW_DFLT, fullname, F_OK, NULL)) {
#ifdef H5F_DEBUG
                fprintf(stderr, "HDF5-DIAG: opening a split file\n");
#endif
                fullname[s - name] = '\0';
                f = H5F_open(H5F_LOW_SPLIT, fullname, flags, create_parms);
                HRETURN(f);
            }
        }
    }

    /*
     * If no file creation parameters are supplied then use defaults.
     */
    if (!create_parms)
        create_parms = &H5F_create_dflt;

    /*
     * Does the file exist?  If so, get the device and i-node values so we can
     * compare them with other files already open.  On Unix (and other systems
     * with hard or soft links) it doesn't work to compare files based only on
     * their full path name.
     */
    file_exists = H5F_low_access(type, name, F_OK, &search);

    /*
     * Open the low-level file (if necessary) and create an H5F_t struct that
     * points to an H5F_file_t struct.
     */
    if (file_exists) {
        if (flags & H5F_ACC_EXCL) {
            HRETURN_ERROR(H5E_FILE, H5E_FILEEXISTS, NULL,
                          "file already exists - CREAT EXCL failed");
        }
        if (!H5F_low_access(type, name, R_OK, NULL)) {
            HRETURN_ERROR(H5E_FILE, H5E_READERROR, NULL,
                          "file is not readable");
        }
        if ((flags & H5F_ACC_WRITE) &&
            !H5F_low_access(type, name, W_OK, NULL)) {
            HRETURN_ERROR(H5E_FILE, H5E_WRITEERROR, NULL,
                          "file is not writable");
        }
        if ((old = H5Asearch_atom(H5_FILE, H5F_compare_files, &search))) {
            if (flags & H5F_ACC_TRUNC) {
                HRETURN_ERROR(H5E_FILE, H5E_FILEOPEN, NULL,
                              "file already open - TRUNC failed");
            }
            if ((flags & H5F_ACC_WRITE) &&
                0 == (old->shared->flags & H5F_ACC_WRITE)) {
                if (NULL == (fd = H5F_low_open(type, name, H5F_ACC_WRITE, NULL))) {
                    HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                               "file cannot be reopened with write access");
                }
                H5F_low_close(old->shared->lf);
                old->shared->lf = fd;
                old->shared->flags |= H5F_ACC_WRITE;
                fd = NULL;      /*so we don't close it during error */
            }
            f = H5F_new(old->shared);

        } else if (flags & H5F_ACC_TRUNC) {
            /* Truncate existing file */
            if (0 == (flags & H5F_ACC_WRITE)) {
                HRETURN_ERROR(H5E_FILE, H5E_BADVALUE, NULL,
                              "can't truncate without write intent");
            }
            fd = H5F_low_open(type, name, H5F_ACC_WRITE | H5F_ACC_TRUNC, NULL);
            if (!fd) {
                HRETURN_ERROR(H5E_FILE, H5E_CANTCREATE, NULL,
                              "can't truncate file");
            }
            f = H5F_new(NULL);
            f->shared->key = search;
            f->shared->flags = flags;
            f->shared->lf = fd;
            empty_file = TRUE;

        } else {
            fd = H5F_low_open(type, name, (flags & H5F_ACC_WRITE), NULL);
            if (!fd) {
                HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                              "cannot open existing file");
            }
            f = H5F_new(NULL);
            f->shared->key = search;
            f->shared->flags = flags;
            f->shared->lf = fd;
        }

    } else if (flags & H5F_ACC_CREAT) {
        if (0 == (flags & H5F_ACC_WRITE)) {
            HRETURN_ERROR(H5E_FILE, H5E_BADVALUE, NULL,
                          "can't create file without write intent");
        }
        fd = H5F_low_open(type, name,
                          H5F_ACC_WRITE | H5F_ACC_CREAT | H5F_ACC_EXCL,
                          &search);
        if (!fd) {
            HRETURN_ERROR(H5E_FILE, H5E_CANTCREATE, NULL,
                          "can't create file");
        }
        f = H5F_new(NULL);
        f->shared->key = search;
        f->shared->flags = flags;
        f->shared->lf = fd;
        empty_file = TRUE;

    } else {
        HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                      "file does not exist");
    }
    assert(f);

    /*
     * The intent at the top level file struct are not necessarily the same as
     * the flags at the bottom.  The top level describes how the file can be
     * accessed through the HDF5 library.  The bottom level describes how the
     * file can be accessed through the C library.
     */
    f->intent = flags;
    f->name = H5MM_xstrdup(name);

    /*
     * Update the file creation parameters with default values if this is the
     * first time this file is opened.
     */
    if (1 == f->shared->nrefs) {
        f->shared->create_parms = *create_parms;
    }
    cp = &(f->shared->create_parms);

    /*
     * Read or write the file boot block.
     */
    if (empty_file) {
        /*
         * For new files we must write the boot block.  The boot block starts
         * immediately after the user-defined header, which we have already
         * insured is a proper size.  The base address is set to the same thing
         * as the boot block.
         */
        H5F_addr_reset(&(f->shared->boot_addr));
        H5F_addr_inc(&(f->shared->boot_addr),
                     f->shared->create_parms.userblock_size);
        f->shared->base_addr = f->shared->boot_addr;

        f->shared->consist_flags = 0x03;
        if (H5F_flush(f, FALSE) < 0) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL,
                        "can't write file boot block");
        }
    } else if (1 == f->shared->nrefs) {
        /* For existing files we must read the boot block. */
        if (H5F_locate_signature(f->shared->lf, &(f->shared->boot_addr)) < 0) {
            HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, NULL, "can't find signature");
        }
        if (H5F_low_read(f->shared->lf, &(f->shared->boot_addr),
                         fixed_size, buf) < 0) {
            HGOTO_ERROR(H5E_IO, H5E_READERROR, NULL, "can't read boot block");
        }
        /*
         * Decode the fixed size part of the boot block.  For each of the
         * version parameters, check that the library is able to handle that
         * version.
         */
        p = buf + H5F_SIGNATURE_LEN;    /*already checked */

        cp->bootblock_ver = *p++;
        if (cp->bootblock_ver != HDF5_BOOTBLOCK_VERSION) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad boot block version number");
        }
        cp->smallobject_ver = *p++;
        if (cp->smallobject_ver != HDF5_SMALLOBJECT_VERSION) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad small object heap version number");
        }
        cp->freespace_ver = *p++;
        if (cp->freespace_ver != HDF5_FREESPACE_VERSION) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad free space version number");
        }
        cp->objectdir_ver = *p++;
        if (cp->objectdir_ver != HDF5_OBJECTDIR_VERSION) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad object dir version number");
        }
        cp->sharedheader_ver = *p++;
        if (cp->sharedheader_ver != HDF5_SHAREDHEADER_VERSION) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad shared header version number");
        }
        cp->sizeof_addr = *p++;
        if (cp->sizeof_addr != 2 &&
            cp->sizeof_addr != 4 &&
            cp->sizeof_addr != 8 &&
            cp->sizeof_addr != 16 &&
            cp->sizeof_addr != 32) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad address size");
        }
        cp->sizeof_size = *p++;
        if (cp->sizeof_size != 2 &&
            cp->sizeof_size != 4 &&
            cp->sizeof_size != 8 &&
            cp->sizeof_size != 16 &&
            cp->sizeof_size != 32) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad length size");
        }
        /* Reserved byte */
        p++;

        UINT16DECODE(p, cp->sym_leaf_k);
        if (cp->sym_leaf_k < 1) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad symbol table leaf node 1/2 rank");
        }
        UINT16DECODE(p, cp->btree_k[H5B_SNODE_ID]);
        if (cp->btree_k[H5B_SNODE_ID] < 1) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "bad symbol table internal node 1/2 rank");
        }
        UINT32DECODE(p, f->shared->consist_flags);
        /* nothing to check for consistency flags */

        assert(p - buf == fixed_size);

        /* Read the variable length part of the boot block... */
        variable_size = H5F_SIZEOF_ADDR(f) +    /*base address */
            H5F_SIZEOF_ADDR(f) +        /*global small obj heap */
            H5F_SIZEOF_ADDR(f) +        /*global free list addr */
            H5F_SIZEOF_ADDR(f) +        /*logical file size */
            H5G_SIZEOF_ENTRY(f);
        assert(variable_size <= sizeof buf);
        addr1 = f->shared->boot_addr;
        H5F_addr_inc(&addr1, fixed_size);
        if (H5F_low_read(f->shared->lf, &addr1, variable_size, buf) < 0) {
            HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, NULL,
                        "can't read boot block");
        }
        p = buf;
        H5F_addr_decode(f, &p, &(f->shared->base_addr));
        H5F_addr_decode(f, &p, &(f->shared->smallobj_addr));
        H5F_addr_decode(f, &p, &(f->shared->freespace_addr));
        H5F_addr_decode(f, &p, &(f->shared->hdf5_eof));
        if (H5G_ent_decode(f, &p, &root_ent) < 0) {
            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                        "can't read root symbol entry");
        }
        if (H5F_addr_defined(&(root_ent.header))) {
            f->shared->root_ent = H5MM_xmalloc(sizeof(H5G_entry_t));
            *(f->shared->root_ent) = root_ent;
        }
        /*
         * The userdefined data is the area of the file before the base
         * address.
         */
        f->shared->create_parms.userblock_size = f->shared->base_addr.offset;
    }
    /*
     * What is the current size of the file? The max_addr field is a relative
     * address while H5F_low_size() returns an absolute address.
     */
    H5F_low_size(f->shared->lf, &addr1);
    addr2 = f->shared->hdf5_eof;
    H5F_addr_add(&addr2, &(f->shared->base_addr));
    if (H5F_addr_lt(&addr1, &addr2)) {
        /*
         * Truncated file? This might happen if one tries to open the first
         * member of a file family.
         */
        HGOTO_ERROR(H5E_FILE, H5E_TRUNCATED, NULL, "truncated file");
    } else if (H5F_addr_gt(&addr1, &addr2)) {
        /*
         * The file is larger than the hdf5 data.  It either has extra junk at
         * the end, or a wrapper.  In either case, make the file think it's
         * shorter so when we allocate memory from the file for hdf5 it's
         * allocated immediately after the end of the previous hdf5 data.  This
         * will cause internal wrappers to be overwritten if they follow the
         * hdf5 data.
         */
#ifdef H5F_DEBUG
        fprintf(stderr, "HDF5-DIAG: resetting EOF from ");
        H5F_addr_print(stderr, &addr1);
        fprintf(stderr, " to ");
        H5F_addr_print(stderr, &addr2);
        fprintf(stderr, " (abs)\n");
#endif
        H5F_low_seteof(f->shared->lf, &addr2);
    }
    /* Success! */
    ret_value = f;

  done:
    if (!ret_value) {
        if (f)
            H5F_dest(f);
        H5F_low_close(fd);
    }
    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Fcreate

 PURPOSE
    Create a new HDF5 file.

 USAGE
    int32 H5Fcreate(filename, flags, create_temp, access_temp)
        const char *filename;   IN: Name of the file to create
        uintn flags;            IN: Flags to indicate various options.
        hid_t create_temp;    IN: File-creation template ID
        hid_t access_temp;    IN: File-access template ID

 ERRORS
    ARGS      BADVALUE      Invalid file name. 
    ARGS      BADVALUE      Invalid flags. 
    ATOM      BADATOM       Can't unatomize template. 
    ATOM      CANTREGISTER  Can't atomize file. 
    FILE      CANTOPENFILE  Can't create file. 

 RETURNS
    Returns file ID on success, FAIL on failure

 DESCRIPTION
        This is the primary function for creating HDF5 files . The flags
    parameter determines whether an existing file will be overwritten or not.
    All newly created files are opened for both reading and writing.  All flags
    may be combined with the "||" (logical OR operator) to change the behavior
    of the file open call.
        The flags currently defined:
            H5ACC_OVERWRITE - Truncate file, if it already exists. The file
                will be truncated, erasing all data previously stored in the
                file.
        The more complex behaviors of a file's creation and access are
    controlled through the file-creation and file-access templates.  The value
    of 0 for a template value indicates that the library should use the default
    values for the appropriate template.  (Documented in the template module).
    [Access templates are currently unused in this routine, although they will
    be implemented in the future]

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct creation and destruction is through H5F_new() H5F_dest().
    Writing the root symbol table entry is done with H5G_encode().

    Robb Matzke, 29 Aug 1997
    Moved creation of the boot block to H5F_flush().
 
    Robb Matzke, 23 Sep 1997
    Most of the work is now done by H5F_open() since H5Fcreate() and H5Fopen()
    originally contained almost identical code.
--------------------------------------------------------------------------*/
hid_t 
H5Fcreate(const char *filename, uintn flags, hid_t create_temp,
          hid_t access_temp)
{
    H5F_t                  *new_file = NULL;    /* file struct for new file */
    const H5F_create_t     *create_parms;       /* pointer to the parameters to
                                                   * use when creating the file
                                                 */
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5Fcreate, FAIL);
    H5ECLEAR;

    /* Check/fix arguments */
    if (!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name");
    if (flags & ~H5ACC_OVERWRITE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");
    flags = (H5F_ACC_WRITE | H5F_ACC_CREAT) |
        (H5ACC_OVERWRITE == flags ? H5F_ACC_TRUNC : H5F_ACC_EXCL);

    if (create_temp <= 0) {
        create_parms = &H5F_create_dflt;
    } else if (NULL == (create_parms = H5Aatom_object(create_temp))) {
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't unatomize template");
    }
#ifdef LATER
    if (access_temp <= 0) {
        access_parms = &H5F_access_dflt;
    } else if (NULL == (access_parms = H5Aatom_object(access_temp))) {
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);       /*can't unatomize template */
    }
#endif

    /*
     * Create a new file or truncate an existing file.
     */
    if (NULL == (new_file = H5F_open(H5F_LOW_DFLT, filename, flags,
                                     create_parms))) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "can't create file");
    }
    /* Get an atom for the file */
    if ((ret_value = H5Aregister_atom(H5_FILE, new_file)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "can't atomize file");

  done:
    if (ret_value < 0 && new_file) {
        /* Error condition cleanup */
        H5F_close(new_file);
    }
    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
}

/*--------------------------------------------------------------------------
 NAME
    H5Fopen

 PURPOSE
    Open an existing HDF5 file.

 USAGE
    hid_t H5Fopen(filename, flags, access_temp)
        const char *filename;   IN: Name of the file to create
        uintn flags;            IN: Flags to indicate various options.
        hid_t access_temp;    IN: File-access template

 ERRORS
    ARGS      BADRANGE      Invalid file name. 
    ATOM      BADATOM       Can't unatomize template. 
    ATOM      CANTREGISTER  Can't atomize file. 
    FILE      CANTOPENFILE  Cant open file. 

 RETURNS
    Returns file ID on success, FAIL on failure

 DESCRIPTION
        This is the primary function for accessing existing HDF5 files. The
    flags parameter determines whether writing to an existing file will be
    allowed or not.  All flags may be combined with the "||" (logical OR
    operator) to change the behavior of the file open call.
        The flags currently defined:
            H5ACC_WRITE - Allow writing to the file.
        The more complex behaviors of a file's access are controlled through
    the file-access template.

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct creation and destruction is through H5F_new() H5F_dest().
    Reading the root symbol table entry is done with H5G_decode().

    Robb Matzke, 23 Sep 1997
    Most of the work is now done by H5F_open() since H5Fcreate() and H5Fopen()
    originally contained almost identical code.
--------------------------------------------------------------------------*/
hid_t 
H5Fopen(const char *filename, uintn flags, hid_t access_temp)
{
    H5F_t                  *new_file = NULL;    /* file struct for new file */
    hid_t                   ret_value = FAIL;

    FUNC_ENTER(H5Fopen, FAIL);
    H5ECLEAR;

    /* Check/fix arguments. */
    if (!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "invalid file name");
    flags = flags & H5ACC_WRITE ? H5F_ACC_WRITE : 0;

#ifdef LATER
    if (access_temp <= 0)
        access_temp = H5CPget_default_atom(H5_TEMPLATE);
    if (NULL == (f_access_parms = H5Aatom_object(access_temp)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL);       /*can't unatomize template */
#endif

    /* Open the file */
    if (NULL == (new_file = H5F_open(H5F_LOW_DFLT, filename, flags, NULL))) {
        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "cant open file");
    }
    /* Get an atom for the file */
    if ((ret_value = H5Aregister_atom(H5_FILE, new_file)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "can't atomize file");

  done:
    if (ret_value < 0 && new_file) {
        H5F_close(new_file);
    }
    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_flush
 *
 * Purpose:     Flushes (and optionally invalidates) cached data plus the
 *              file boot block.  If the logical file size field is zero
 *              then it is updated to be the length of the boot block.
 *
 * Errors:
 *              CACHE     CANTFLUSH     Can't flush cache. 
 *              IO        WRITEERROR    Can't write header. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug 29 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_flush(H5F_t *f, hbool_t invalidate)
{
    uint8                   buf[2048], *p = buf;

    FUNC_ENTER(H5F_flush, FAIL);

    /*
     * Nothing to do if the file is read only.  This determination is made at
     * the shared open(2) flags level, implying that opening a file twice,
     * once for read-only and once for read-write, and then calling
     * H5F_flush() with the read-only handle, still causes data to be flushed.
     */
    if (0 == (H5F_ACC_WRITE & f->shared->flags))
        HRETURN(SUCCEED);

    /* flush (and invalidate) the entire cache */
    if (H5AC_flush(f, NULL, 0, invalidate) < 0) {
        HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "can't flush cache");
    }
    /* encode the file boot block */
    HDmemcpy(p, H5F_SIGNATURE, H5F_SIGNATURE_LEN);
    p += H5F_SIGNATURE_LEN;

    *p++ = f->shared->create_parms.bootblock_ver;
    *p++ = f->shared->create_parms.smallobject_ver;
    *p++ = f->shared->create_parms.freespace_ver;
    *p++ = f->shared->create_parms.objectdir_ver;
    *p++ = f->shared->create_parms.sharedheader_ver;
    *p++ = H5F_SIZEOF_ADDR(f);
    *p++ = H5F_SIZEOF_SIZE(f);
    *p++ = 0;                   /*reserved */
    UINT16ENCODE(p, f->shared->create_parms.sym_leaf_k);
    UINT16ENCODE(p, f->shared->create_parms.btree_k[H5B_SNODE_ID]);
    UINT32ENCODE(p, f->shared->consist_flags);
    H5F_addr_encode(f, &p, &(f->shared->base_addr));
    H5F_addr_encode(f, &p, &(f->shared->smallobj_addr));
    H5F_addr_encode(f, &p, &(f->shared->freespace_addr));
    H5F_addr_encode(f, &p, &(f->shared->hdf5_eof));
    H5G_ent_encode(f, &p, f->shared->root_ent);

    /* update file length if necessary */
    if (!H5F_addr_defined(&(f->shared->hdf5_eof))) {
        H5F_addr_reset(&(f->shared->hdf5_eof));
        H5F_addr_inc(&(f->shared->hdf5_eof), p - buf);
        H5F_low_seteof(f->shared->lf, &(f->shared->hdf5_eof));
    }
    /* write the boot block to disk */
    if (H5F_low_write(f->shared->lf, &(f->shared->boot_addr), p - buf, buf) < 0) {
        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't write header");
    }
    /* Flush file buffers to disk */
    if (H5F_low_flush(f->shared->lf) < 0) {
        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "low level flush failed");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_close
 *
 * Purpose:     Closes an open HDF5 file.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, September 23, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_close(H5F_t *f)
{
    FUNC_ENTER(H5F_close, FAIL);

    /* Close all current working groups */
    while (H5G_pop(f) >= 0)     /*void */
        ;

    /* Flush the boot block and caches */
    if (H5F_flush(f, TRUE) < 0) {
        HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "can't flush cache");
    }
    /*
     * If object headers are still open then delay deletion of resources until
     * they have all been closed.  The file is in a consistent state now, so
     * forgetting to close everything is not a major problem.
     */
    if (f->nopen > 0) {
#ifndef NDEBUG
        fprintf(stderr, "HDF5-DIAG: H5F_close: %d object header%s still "
                "open (file close will complete when %s closed)\n",
                f->nopen,
                1 == f->nopen ? " is" : "s are",
                1 == f->nopen ? "that header is" : "those headers are");
#endif
        f->close_pending = TRUE;
        HRETURN(SUCCEED);
    } else if (f->close_pending) {
#ifndef NDEBUG
        fprintf(stderr, "HDF5-DIAG: H5F_close: operation completed\n");
#endif
    }
    /* Dump debugging info */
    if (f->intent & H5F_ACC_DEBUG)
        H5AC_debug(f);

    /* Close files and release resources */
    H5F_low_close(f->shared->lf);
    f = H5F_dest(f);

    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5Fclose

 PURPOSE
    Close an open HDF5 file.

 USAGE
    int32 H5Fclose(fid)
        int32 fid;      IN: File ID of file to close

 ERRORS
    ARGS      BADTYPE       Not a file atom. 
    ATOM      BADATOM       Can't remove atom. 
    ATOM      BADATOM       Can't unatomize file. 
    CACHE     CANTFLUSH     Can't flush cache. 

 RETURNS
    SUCCEED/FAIL

 DESCRIPTION
        This function terminates access to an HDF5 file.  If this is the last
    file ID open for a file and if access IDs are still in use, this function
    will fail.

 MODIFICATIONS:
    Robb Matzke, 18 Jul 1997
    File struct destruction is through H5F_dest().

    Robb Matzke, 29 Aug 1997
    The file boot block is flushed to disk since it's contents may have
    changed.
--------------------------------------------------------------------------*/
herr_t 
H5Fclose(hid_t fid)
{
    H5F_t                  *file = NULL;        /* file struct for file to close */
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5Fclose, FAIL);
    H5ECLEAR;

    /* Check/fix arguments. */
    if (H5_FILE != H5Aatom_group(fid))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file atom");
    if (NULL == (file = H5Aatom_object(fid)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't unatomize file");

    /* Close the file */
    ret_value = H5F_close(file);

    /* Remove the file atom */
    if (NULL == H5Aremove_atom(fid)) {
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't remove atom");
    }
  done:
    FUNC_LEAVE(ret_value < 0 ? FAIL : SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_block_read
 *
 * Purpose:     Reads some data from a file/server/etc into a buffer.
 *              The data is contiguous.  The address is relative to the base
 *              address for the file.
 *
 * Errors:
 *              IO        READERROR     Low-level read failed. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_read(H5F_t *f, const haddr_t *addr, size_t size, void *buf)
{
    haddr_t                 abs_addr;

    FUNC_ENTER(H5F_block_read, FAIL);

    if (0 == size)
        return 0;

    /* convert the relative address to an absolute address */
    abs_addr = f->shared->base_addr;
    H5F_addr_add(&abs_addr, addr);

    /* Read the data */
    if (H5F_low_read(f->shared->lf, &abs_addr, size, buf) < 0) {
        HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "low-level read failed");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_block_write
 *
 * Purpose:     Writes some data from memory to a file/server/etc.  The
 *              data is contiguous.  The address is relative to the base
 *              address.
 *
 * Errors:
 *              IO        WRITEERROR    Low-level write failed. 
 *              IO        WRITEERROR    No write intent. 
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 10 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_write(H5F_t *f, const haddr_t *addr, size_t size, const void *buf)
{
    haddr_t                 abs_addr;

    FUNC_ENTER(H5F_block_write, FAIL);

    if (0 == size)
        return 0;

    if (0 == (f->intent & H5F_ACC_WRITE)) {
        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "no write intent");
    }
    /* Convert the relative address to an absolute address */
    abs_addr = f->shared->base_addr;
    H5F_addr_add(&abs_addr, addr);

    /* Write the data */
    if (H5F_low_write(f->shared->lf, &abs_addr, size, buf)) {
        HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "low-level write failed");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_debug
 *
 * Purpose:     Prints a file header to the specified stream.  Each line
 *              is indented and the field name occupies the specified width
 *              number of characters.
 *
 * Errors:
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_debug(H5F_t *f, const haddr_t *addr, FILE * stream, intn indent,
          intn fwidth)
{
    FUNC_ENTER(H5F_debug, FAIL);

    /* check args */
    assert(f);
    assert(addr && H5F_addr_defined(addr));
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    /* debug */
    fprintf(stream, "%*sFile Boot Block...\n", indent, "");

    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
            "File name:",
            f->name);
    fprintf(stream, "%*s%-*s 0x%08x\n", indent, "", fwidth,
            "Flags",
            (unsigned) (f->shared->flags));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Reference count:",
            (unsigned) (f->shared->nrefs));
    fprintf(stream, "%*s%-*s 0x%08lx\n", indent, "", fwidth,
            "Consistency flags:",
            (unsigned long) (f->shared->consist_flags));

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            "Address of boot block:");
    H5F_addr_print(stream, &(f->shared->boot_addr));
    fprintf(stream, " (abs)\n");

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            "Base address:");
    H5F_addr_print(stream, &(f->shared->base_addr));
    fprintf(stream, " (abs)\n");

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            "Small object heap address:");
    H5F_addr_print(stream, &(f->shared->smallobj_addr));
    fprintf(stream, " (rel)\n");

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            "Free list address:");
    H5F_addr_print(stream, &(f->shared->freespace_addr));
    fprintf(stream, " (rel)\n");

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            "Total size of hdf5 data:");
    H5F_addr_print(stream, &(f->shared->hdf5_eof));
    fprintf(stream, " bytes\n");

    fprintf(stream, "%*s%-*s %lu bytes\n", indent, "", fwidth,
            "Size of user block:",
            (unsigned long) (f->shared->create_parms.userblock_size));
    fprintf(stream, "%*s%-*s %u bytes\n", indent, "", fwidth,
            "Size of file size_t type:",
            (unsigned) (f->shared->create_parms.sizeof_size));
    fprintf(stream, "%*s%-*s %u bytes\n", indent, "", fwidth,
            "Size of file haddr_t type:",
            (unsigned) (f->shared->create_parms.sizeof_addr));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Symbol table leaf node 1/2 rank:",
            (unsigned) (f->shared->create_parms.sym_leaf_k));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Symbol table internal node 1/2 rank:",
            (unsigned) (f->shared->create_parms.btree_k[H5B_SNODE_ID]));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Boot block version number:",
            (unsigned) (f->shared->create_parms.bootblock_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Small object heap version number:",
            (unsigned) (f->shared->create_parms.smallobject_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Free list version number:",
            (unsigned) (f->shared->create_parms.freespace_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Object directory version number:",
            (unsigned) (f->shared->create_parms.objectdir_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
            "Shared header version number:",
            (unsigned) (f->shared->create_parms.sharedheader_ver));

    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
            "Root symbol table entry:",
            f->shared->root_ent ? "" : "(none)");
    if (f->shared->root_ent) {
        H5G_ent_debug(f, f->shared->root_ent, stream,
                      indent + 3, MAX(0, fwidth - 3));
    }
    FUNC_LEAVE(SUCCEED);
}
