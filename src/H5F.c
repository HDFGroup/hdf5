/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*
* MODIFICATIONS
*	Robb Matzke, 30 Aug 1997
*	Added `ERRORS' fields to function prologues.
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

/*LINTLIBRARY */
/*
   FILE
   hdf5file.c
   HDF5 file I/O routines

   EXPORTED ROUTINES
   H5Fcreate	-- Create an HDF5 file
   H5Fclose	-- Close an open HDF5 file

   LIBRARY-SCOPED ROUTINES

   LOCAL ROUTINES
   H5F_init_interface	 -- initialize the H5F interface
 */

/* Packages needed by this file... */
#include <H5private.h>		/*library functions			  */
#include <H5Aprivate.h>		/*attributes				  */
#include <H5Dprivate.h>		/*datasets				  */
#include <H5Iprivate.h>		/*object IDs				  */
#include <H5ACprivate.h>	/*cache					  */
#include <H5Eprivate.h>		/*error handling			  */
#include <H5Fprivate.h>         /*file access                             */
#include <H5Gprivate.h>		/*symbol tables				  */
#include <H5MMprivate.h>	/*core memory management		  */
#include <H5Pprivate.h>		/*property lists			  */
#include <H5Tprivate.h>		/*data types				  */

/*
 * Define the following if you want H5F_block_read() and H5F_block_write() to
 * keep track of the file position and attempt to minimize calls to the file
 * seek method.
 */
/* #define H5F_OPT_SEEK */

#define PABLO_MASK	H5F_mask

/*-------------------- Locally scoped variables -----------------------------*/

/*
 * Define the default file creation property list.
 */
const H5F_create_t	H5F_create_dflt = {
    0,				/* Default user-block size */
    4,				/* Default 1/2 rank for symtab leaf nodes */
    {				/* Default 1/2 rank for btree intern nodes */
	16,			/* Symbol table internal nodes		   */
	32,			/* Indexed storage internal nodes	   */
	0,			/* unused				   */
	0,			/* unused				   */
	0,			/* unused				   */
	0,			/* unused				   */
	0,			/* unused				   */
	0,			/* unused				   */
    },
    sizeof(hsize_t),		/* Default offset size 			   */
    sizeof(hsize_t),		/* Default length size 			   */
    HDF5_BOOTBLOCK_VERSION,	/* Current Boot-Block version #		   */
    HDF5_FREESPACE_VERSION,	/* Current Free-Space info version #	   */
    HDF5_OBJECTDIR_VERSION,	/* Current Object Directory info version # */
    HDF5_SHAREDHEADER_VERSION,	/* Current Shared-Header format version #  */
};

/*
 * Define the default file access property list.  The template is initialized
 * by H5F_init_interface().
 */
H5F_access_t H5F_access_dflt;

/*
 * Define the default mount property list.
 */
const H5F_mprop_t	H5F_mount_dflt = {
    FALSE,			/* Absolute symlinks are wrt mount root	   */
};

/* Interface initialization */
static intn interface_initialize_g = 0;
#define INTERFACE_INIT H5F_init_interface
static herr_t H5F_init_interface(void);

/* PRIVATE PROTOTYPES */
static H5F_t *H5F_new(H5F_file_t *shared, const H5F_create_t *fcpl,
		      const H5F_access_t *fapl);
static herr_t H5F_dest(H5F_t *f);
static herr_t H5F_flush(H5F_t *f, H5F_scope_t scope, hbool_t invalidate);
static herr_t H5F_locate_signature(H5F_low_t *f_handle,
				   const H5F_access_t *access_parms,
				   haddr_t *addr/*out*/);


/*-------------------------------------------------------------------------
 * Function:	H5F_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_init(void)
{
    FUNC_ENTER(H5F_init, FAIL);
    /* FUNC_ENTER() does all the work */
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_init_interface
 *
 * Purpose:	Initialize interface-specific information.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
 * 	Robb Matzke, 4 Aug 1997
 *	Changed pablo mask from H5_mask to H5F_mask for the FUNC_LEAVE call.
 *	It was already H5F_mask for the PABLO_TRACE_ON call.
 *
 *  	rky 980816
 *	Added .disp, .btype, .ftype to H5F_access_t.
 *-------------------------------------------------------------------------
 */
static herr_t 
H5F_init_interface(void)
{
    herr_t	ret_value = SUCCEED;
    
    FUNC_ENTER(H5F_init_interface, FAIL);

#ifdef HAVE_PARALLEL
    {
        /* Allow MPI buf-and-file-type optimizations? */
        const char *s = HDgetenv ("HDF5_MPI_1_METAWRITE");
        if (s && HDisdigit(*s)) {
            H5_mpi_1_metawrite_g = (int)HDstrtol (s, NULL, 0);
        }
    }
#endif

    /* Initialize the atom group for the file IDs */
    if (H5I_init_group(H5I_FILE, H5I_FILEID_HASHSIZE, 0,
		       (herr_t (*)(void*))H5F_close)<0) {
	HRETURN_ERROR (H5E_FILE, H5E_CANTINIT, FAIL,
		       "unable to initialize interface");
    }

    /* Initialize the default file access property list */
    H5F_access_dflt.mdc_nelmts = H5AC_NSLOTS;
    H5F_access_dflt.rdcc_nelmts = 521;
    H5F_access_dflt.rdcc_nbytes = 1024*1024; /*1MB*/
    H5F_access_dflt.rdcc_w0 = 0.75; /*preempt fully read chunks*/
    H5F_access_dflt.threshold = 1; /*alignment applies to everything*/
    H5F_access_dflt.alignment = 1; /*no alignment*/
    H5F_access_dflt.gc_ref = 0;     /* Don't garbage-collect references unless user chooses to */
    H5F_access_dflt.driver = H5F_LOW_DFLT;
#if (H5F_LOW_DFLT == H5F_LOW_SEC2)
    /* Nothing to initialize */
#elif (H5F_LOW_DFLT == H5F_LOW_STDIO)
    /* Nothing to initialize */
#elif (H5F_LOW_DFLT == H5F_LOW_CORE)
    H5F_access_dflt.u.core.increment = 10*1024;
#elif (H5F_LOW_DFLT == H5F_LOW_MPIO)
    H5F_access_dflt.u.mpio.comm = MPI_COMM_SELF;
    H5F_access_dflt.u.mpio.info = MPI_INFO_NULL;
    H5F_access_dflt.u.mpio.btype = MPI_DATATYPE_NULL;
    H5F_access_dflt.u.mpio.ftype = MPI_DATATYPE_NULL;
    H5F_addr_reset( &(H5F_access_dflt.u.mpio.disp) );
    H5F_access_dflt.u.mpio.use_types = 0;
    H5F_access_dflt.u.mpio.old_use_types = 0;
#elif (H5F_LOW_DFLT == H5F_LOW_SPLIT)
#   error "H5F_LOW_SPLIT cannot be a default file driver"
#elif (H5F_LOW_DFLT == H5F_LOW_FAMILY)
#   error "H5F_LOW_FAMILY cannot be a default file driver"
#else
#   error "Unknown default file driver"
#endif

    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5F_term_interface
 PURPOSE
    Terminate various H5F objects
 USAGE
    void H5F_term_interface()
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
void
H5F_term_interface(intn status)
{
    if (interface_initialize_g>0) {
	H5I_destroy_group(H5I_FILE);
    }
    interface_initialize_g = status;
}


/*--------------------------------------------------------------------------
 NAME
       H5F_encode_length_unusual -- encode an unusual length size
 USAGE
       void H5F_encode_length_unusual(f, p, l)
       const H5F_t *f;		   IN: pointer to the file record
       uint8_t **p;		IN: pointer to buffer pointer to encode length in
       uint8_t *l;		IN: pointer to length to encode

 ERRORS

 RETURNS
    none
 DESCRIPTION
    Encode non-standard (i.e. not 2, 4 or 8-byte) lengths in file meta-data.
--------------------------------------------------------------------------*/
void 
H5F_encode_length_unusual(const H5F_t *f, uint8_t **p, uint8_t *l)
{
    intn		    i = (intn)H5F_SIZEOF_SIZE(f)-1;

#ifdef WORDS_BIGENDIAN
    /*
     * For non-little-endian platforms, encode each byte in memory backwards.
     */
    for (/*void*/; i>=0; i--, (*p)++)*(*p) = *(l+i);
#else
    /* platform has little-endian integers */
    HDmemcpy(*p,l,(size_t)(i+1));
    *p+=(i+1);
#endif

}


/*-------------------------------------------------------------------------
 * Function:	H5Fget_create_plist
 *
 * Purpose:	Get an atom for a copy of the file-creation property list for
 *		this file. This function returns an atom with a copy of the
 *		properties used to create a file.
 *
 * Return:	Success:	template ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Calls H5P_copy() to copy the property list and H5P_close() to free
 *	that property list if an error occurs.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_create_plist(hid_t file_id)
{
    H5F_t		*file = NULL;
    hid_t		ret_value = FAIL;
    H5F_create_t	*plist = NULL;

    FUNC_ENTER(H5Fget_create_plist, FAIL);
    H5TRACE1("i","i",file_id);

    /* check args */
    if (H5I_FILE!=H5I_get_type(file_id) || NULL==(file=H5I_object(file_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    
    /* Create the property list object to return */
    if (NULL==(plist=H5P_copy(H5P_FILE_CREATE, file->shared->create_parms))) {
	HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
		      "unable to copy file creation properties");
    }

    /* Create an atom */
    if ((ret_value = H5P_create(H5P_FILE_CREATE, plist)) < 0) {
	H5P_close(H5P_FILE_CREATE, plist);
	HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		      "unable to register property list");
    }
    
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Fget_access_plist
 *
 * Purpose:	Returns a copy of the file access property list of the
 *		specified file.
 *
 * Return:	Success:	Object ID for a copy of the file access
 *				property list.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, February 18, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_access_plist(hid_t file_id)
{
    H5F_t		*f = NULL;
    H5F_access_t	*plist = NULL;
    hid_t		ret_value = FAIL;
    
    FUNC_ENTER(H5Fget_access_plist, FAIL);
    H5TRACE1("i","i",file_id);

    /* Check args */
    if (H5I_FILE!=H5I_get_type(file_id) || NULL==(f=H5I_object(file_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }

    /* Create the property list object to return */
    if (NULL==(plist=H5P_copy(H5P_FILE_ACCESS, f->shared->access_parms))) {
	HRETURN_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL,
		      "unable to copy file access properties");
    }

    /* Create an atom */
    if ((ret_value = H5P_create(H5P_FILE_ACCESS, plist))<0) {
	H5P_close(H5P_FILE_ACCESS, plist);
	HRETURN_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		      "unable to register property list");
    }

    FUNC_LEAVE(ret_value);
}
	    

/*--------------------------------------------------------------------------
 NAME
       H5F_compare_files -- compare file objects for the atom API
 USAGE
       intn HPcompare_filename(obj, key)
       const void * obj;	     IN: pointer to the file record
       const void * key;	     IN: pointer to the search key

 ERRORS

 RETURNS
       TRUE if the key matches the obj, FALSE otherwise
 DESCRIPTION
       Look inside the file record for the atom API and compare the the
       keys.
--------------------------------------------------------------------------*/
static intn
H5F_compare_files(void * _obj, const void * _key)
{
    const H5F_t		*obj = (const H5F_t *) _obj;
    const H5F_search_t	*key = (const H5F_search_t *) _key;
    int			ret_value = FALSE;

    FUNC_ENTER(H5F_compare_files, FALSE);

    ret_value = (obj->shared->key.dev == key->dev &&
		 obj->shared->key.ino == key->ino);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_locate_signature
 *
 * Purpose:	Finds the HDF5 boot block signature in a file.	The signature
 *		can appear at address 0, or any power of two beginning with
 *		512.
 *
 * Return:	Success:	SUCCEED.  The address of the signature is
 *				returned through the ADDR argument.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_locate_signature(H5F_low_t *f_handle, const H5F_access_t *access_parms,
		     haddr_t *addr/*out*/)
{
    herr_t          ret_value=FAIL;
    haddr_t	    max_addr;
    uint8_t	    buf[H5F_SIGNATURE_LEN];
    uintn	    n = 9;

    FUNC_ENTER(H5F_locate_signature, FAIL);

    H5F_low_size(f_handle, &max_addr);
    H5F_addr_reset(addr);
    while (H5F_addr_lt(addr, &max_addr)) {
        if (H5F_low_read(f_handle, access_parms, H5D_XFER_DFLT, addr,
                 H5F_SIGNATURE_LEN, buf) < 0) {
            HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "unable to read file");
        }
        if (!HDmemcmp(buf, H5F_SIGNATURE, H5F_SIGNATURE_LEN)) {
            ret_value=SUCCEED;
            break;
        }
        H5F_addr_pow2(n++, addr);
    }

    FUNC_LEAVE(ret_value);
}


/*--------------------------------------------------------------------------
 NAME
    H5Fis_hdf5

 PURPOSE
    Check the file signature to detect an HDF5 file.

 USAGE
    htri_t H5Fis_hdf5(filename)
	const char *filename;	IN: Name of the file to check
 ERRORS
    ARGS      BADRANGE	    No filename specified. 
    FILE      BADFILE	    Low-level file open failure. 
    IO	      READERROR	    Read error. 
    IO	      READERROR	    Seek error. 
    IO	      SEEKERROR	    Unable to determine length of file due to seek
			    failure. 

 RETURNS
    TRUE/FALSE/FAIL

 DESCRIPTION
    This function determines if a file is an HDF5 format file.
--------------------------------------------------------------------------*/
htri_t
H5Fis_hdf5(const char *filename)
{
    H5F_low_t	*f_handle = NULL;	/* file handle */
    haddr_t	addr;		       /* Address of file signature & header */
    hbool_t	ret_value = FALSE;
    const H5F_low_class_t *type = NULL;

    FUNC_ENTER(H5Fis_hdf5, FAIL);
    H5TRACE1("b","s",filename);

    /* Check args and all the boring stuff. */
    if (filename == NULL) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "no filename specified");
    }

    /* Open the file at the low level driver */
    type = H5F_low_class (H5F_access_dflt.driver);
    assert (type);
    if (NULL == (f_handle = H5F_low_open(type, filename, &H5F_access_dflt,
					 0, NULL))) {
	HGOTO_ERROR(H5E_FILE, H5E_BADFILE, FAIL,
		    "low-level file open failure");
    }
    if (H5F_locate_signature(f_handle, &H5F_access_dflt, &addr) >= 0) {
	ret_value = TRUE;
    }
    
  done:
    if (f_handle) {
	H5F_low_close(f_handle, &H5F_access_dflt); /*close the file we opened*/
    }
    
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_new
 *
 * Purpose:	Creates a new file object and initializes it.  The
 *		H5Fopen and H5Fcreate functions then fill in various
 *		fields.	 If SHARED is a non-null pointer then the shared info
 *		to which it points has the reference count incremented.
 *		Otherwise a new, empty shared info struct is created and
 *		initialized with the specified file access property list.
 *
 * Errors:
 *
 * Return:	Success:	Ptr to a new file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5F_t *
H5F_new(H5F_file_t *shared, const H5F_create_t *fcpl, const H5F_access_t *fapl)
{
    H5F_t		*f=NULL, *ret_value=NULL;
    intn		n;
    
    FUNC_ENTER(H5F_new, NULL);

    if (NULL==(f = H5MM_calloc(sizeof(H5F_t)))) {
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed");
    }

    if (shared) {
	f->shared = shared;
    } else {
	f->shared = H5MM_calloc(sizeof(H5F_file_t));
	H5F_addr_undef(&(f->shared->boot_addr));
	H5F_addr_undef(&(f->shared->base_addr));
	H5F_addr_undef(&(f->shared->freespace_addr));
	H5F_addr_undef(&(f->shared->hdf5_eof));
    
	/*
	 * Deep-copy the file creation and file access property lists into the
	 * new file handle.  We do this early because some values might need
	 * to change as the file is being opened.
	 */
	if (NULL==(f->shared->create_parms=H5P_copy(H5P_FILE_CREATE, fcpl))) {
	    HRETURN_ERROR (H5E_FILE, H5E_CANTINIT, NULL,
			   "unable to copy file creation property list");
	}
	if (NULL==(f->shared->access_parms=H5P_copy(H5P_FILE_ACCESS, fapl))) {
	    HRETURN_ERROR (H5E_FILE, H5E_CANTINIT, NULL,
			   "unable to copy file access property list");
	}

#ifdef HAVE_PARALLEL
	/*
	 * Disable cache if file is open using MPIO driver.  Parallel
	 * does not permit caching.  (maybe able to relax it for
	 * read only open.)
	 */
	if (f->shared->access_parms->driver==H5F_LOW_MPIO){
	    f->shared->access_parms->rdcc_nbytes = 0;
	    f->shared->access_parms->mdc_nelmts = 0;
	}
#endif

	/*
	 * Create a meta data cache with the specified number of elements.
	 * The cache might be created with a different number of elements and
	 * the access property list should be updated to reflect that.
	 */
	if ((n=H5AC_create(f, f->shared->access_parms->mdc_nelmts))<0) {
	    HRETURN_ERROR (H5E_FILE, H5E_CANTINIT, NULL,
			   "unable to create meta data cache");
	}
	f->shared->access_parms->mdc_nelmts = n;
	
	/* Create the chunk cache */
	H5F_istore_init (f);
    }
    f->shared->nrefs++;
    f->nrefs = 1;
    ret_value = f;

 done:
    if (!ret_value && f) {
	if (!shared) H5MM_xfree (f->shared);
	H5MM_xfree (f);
    }
    
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_dest
 *
 * Purpose:	Destroys a file structure.  This function flushes the cache
 *		but doesn't do any other cleanup other than freeing memory
 *		for the file struct.  The shared info for the file is freed
 *		only when its reference count reaches zero.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 18 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 1998-10-14
 *	Nothing happens unless the reference count for the H5F_t goes to
 *	zero.  The reference counts are decremented here.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_dest(H5F_t *f)
{
    herr_t	ret_value = SUCCEED;
    
    FUNC_ENTER(H5F_dest, FAIL);

    if (f && 0 == --f->nrefs) {
	if (0 == --f->shared->nrefs) {
	    /*
	     * Do not close the root group since we didn't count it, but free
	     * the memory associated with it.
	     */
	    H5MM_xfree (f->shared->root_grp);
	    f->shared->root_grp=NULL;
	    if (H5AC_dest(f)) {
		HERROR (H5E_FILE, H5E_CANTINIT, "problems closing file");
		ret_value = FAIL; /*but keep going*/
	    }
	    if (H5F_istore_dest (f)<0) {
		HERROR (H5E_FILE, H5E_CANTINIT, "problems closing file");
		ret_value = FAIL; /*but keep going*/
	    }
	    f->shared->cwfs = H5MM_xfree (f->shared->cwfs);
	    H5P_close (H5P_FILE_CREATE, f->shared->create_parms);
	    H5P_close (H5P_FILE_ACCESS, f->shared->access_parms);
	    f->shared = H5MM_xfree(f->shared);
	}
	f->name = H5MM_xfree(f->name);
	f->mtab.child = H5MM_xfree(f->mtab.child);
	f->mtab.nalloc = 0;
	H5MM_xfree(f);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_open
 *
 * Purpose:	Opens (or creates) a file.  This function understands the
 *		following flags which are similar in nature to the Posix
 *		open(2) flags.
 *
 *		H5F_ACC_RDWR:	Open with read/write access. If the file is
 *				currently open for read-only access then it
 *				will be reopened. Absence of this flag
 *				implies read-only access.
 *
 *		H5F_ACC_CREAT:	Create a new file if it doesn't exist yet.
 *				The permissions are 0666 bit-wise AND with
 *				the current umask.  H5F_ACC_WRITE must also
 *				be specified.
 *
 *		H5F_ACC_EXCL:	This flag causes H5F_open() to fail if the
 *				file already exists.
 *
 *		H5F_ACC_TRUNC:	The file is truncated and a new HDF5 boot
 *				block is written.  This operation will fail
 *				if the file is already open.
 *
 *		Unlinking the file name from the group directed graph while
 *		the file is opened causes the file to continue to exist but
 *		one will not be able to upgrade the file from read-only
 *		access to read-write access by reopening it. Disk resources
 *		for the file are released when all handles to the file are
 *		closed. NOTE: This paragraph probably only applies to Unix;
 *		deleting the file name in other OS's has undefined results.
 *
 *		The CREATE_PARMS argument is optional.	A null pointer will
 *		cause the default file creation parameters to be used.
 *
 *		The ACCESS_PARMS argument is optional.  A null pointer will
 *		cause the default file access parameters to be used.
 *
 * Errors:
 *		ATOM	  BADATOM	Can't unatomize default template
 *					id. 
 *		FILE	  BADVALUE	Can't create file without write
 *					intent. 
 *		FILE	  BADVALUE	Can't truncate without write intent. 
 *		FILE	  CANTCREATE	Can't create file. 
 *		FILE	  CANTCREATE	Can't truncate file. 
 *		FILE	  CANTINIT	Can't get default file create template
 *					id. 
 *		FILE	  CANTINIT	Can't write file boot block. 
 *		FILE	  CANTOPENFILE	Bad address size. 
 *		FILE	  CANTOPENFILE	Bad boot block version number. 
 *		FILE	  CANTOPENFILE	Bad free space version number. 
 *		FILE	  CANTOPENFILE	Bad length size. 
 *		FILE	  CANTOPENFILE	Bad object dir version number. 
 *		FILE	  CANTOPENFILE	Bad shared header version number. 
 *		FILE	  CANTOPENFILE	Bad small object heap version number. 
 *		FILE	  CANTOPENFILE	Bad symbol table internal node 1/2
 *					rank. 
 *		FILE	  CANTOPENFILE	Bad symbol table leaf node 1/2 rank. 
 *		FILE	  CANTOPENFILE	Can't read root symbol entry. 
 *		FILE	  CANTOPENFILE	Cannot open existing file. 
 *		FILE	  CANTOPENFILE	File cannot be reopened with write
 *					access. 
 *		FILE	  CANTOPENFILE	File does not exist. 
 *		FILE	  CANTOPENFILE	Invalid file family name. 
 *		FILE	  FILEEXISTS	File already exists - CREAT EXCL
 *					failed. 
 *		FILE	  FILEOPEN	File already open - TRUNC failed. 
 *		FILE	  NOTHDF5	Can't find signature. 
 *		FILE	  NOTHDF5	Can't read boot block. 
 *		FILE	  READERROR	File is not readable. 
 *		FILE	  TRUNCATED	Truncated file? 
 *		FILE	  WRITEERROR	File is not writable. 
 *		IO	  READERROR	Can't read boot block. 
 *
 * Return:	Success:	Ptr to the file pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, September 23, 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 11 Nov 1997
 *	If the name contains the pattern /[^%]%\d*[duxX]/ then the file is
 *	assumed to be a family of files.  The TYPE argument is ignored and
 *	H5F_LOW_FAM is used instead.
 *
 *	Albert Cheng, 5 Feb 1998
 *	Added the access_parms argument to pass down access template
 *	information.
 *
 * 	Robb Matzke, 18 Feb 1998
 *	The H5F_access_t changed to allow more generality.  The low level
 *	driver is part of the file access template so the TYPE argument has
 *	been removed.
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5F_open(const char *name, uintn flags,
	 const H5F_create_t *create_parms, const H5F_access_t *access_parms)
{
    H5F_t		*f = NULL;	/*return value			*/
    H5F_t		*ret_value = NULL; /*a copy of `f'		*/
    H5F_t		*old = NULL;	/*a file already opened		*/
    H5F_search_t	search;		/*file search key		*/
    H5F_low_t		*fd = NULL;	/*low level file desc		*/
    hbool_t		empty_file = FALSE; /*is file empty?		*/
    hbool_t		file_exists = FALSE; /*file already exists	*/
    uint8_t		buf[256];	/*I/O buffer..			*/
    const uint8_t	*p = NULL;	/*        ..and pointer into it */
    size_t		fixed_size = 24; /*size of fixed part of boot blk*/
    size_t		variable_size;	/*variable part of boot block	*/
    H5F_create_t	*cp = NULL;	/*file creation parameters	*/
    haddr_t		addr1, addr2;	/*temporary address		*/
    H5G_entry_t		root_ent;	/*root symbol table entry	*/
    const H5F_low_class_t *type = NULL;	/*low-level file driver		*/
    haddr_t		reserved_addr;	/*reserved address		*/

    FUNC_ENTER(H5F_open, NULL);

    assert(name && *name);

    /*
     * If no file creation parameters or file access parameters are supplied
     * then use defaults.
     */
    if (!create_parms) create_parms = &H5F_create_dflt;
    if (!access_parms) access_parms = &H5F_access_dflt;

    /*
     * Does the file exist?  If so, get the device and i-node values so we can
     * compare them with other files already open.  On Unix (and other systems
     * with hard or soft links) it doesn't work to compare files based only on
     * their full path name.
     */
    type = H5F_low_class (access_parms->driver);
    assert (type);
    file_exists = H5F_low_access(type, name, access_parms, F_OK, &search);

    /*
     * Open the low-level file (if necessary) and create an H5F_t struct that
     * points to an H5F_file_t struct.
     */
    if (file_exists) {
	if (flags & H5F_ACC_EXCL) {
	    HRETURN_ERROR(H5E_FILE, H5E_FILEEXISTS, NULL,
			  "file already exists - CREAT EXCL failed");
	}
	if (!H5F_low_access(type, name, access_parms, R_OK, NULL)) {
	    HRETURN_ERROR(H5E_FILE, H5E_READERROR, NULL,
			  "file is not readable");
	}
	if ((flags & H5F_ACC_RDWR) &&
	    !H5F_low_access(type, name, access_parms, W_OK, NULL)) {
	    HRETURN_ERROR(H5E_FILE, H5E_WRITEERROR, NULL,
			  "file is not writable");
	}
	if ((old = H5I_search(H5I_FILE, H5F_compare_files, &search))) {
	    if (flags & H5F_ACC_TRUNC) {
		HRETURN_ERROR(H5E_FILE, H5E_FILEOPEN, NULL,
			      "file already open - TRUNC failed");
	    }
	    if ((flags & H5F_ACC_RDWR) &&
		0 == (old->shared->flags & H5F_ACC_RDWR)) {
		if (NULL==(fd=H5F_low_open(type, name, access_parms,
					   H5F_ACC_RDWR, NULL))) {
		    HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
				  "file cannot be reopened with write access");
		}
		H5F_low_close(old->shared->lf, access_parms);
		old->shared->lf = fd;
		old->shared->flags |= H5F_ACC_RDWR;
		fd = NULL;	/*so we don't close it during error */
	    }
	    f = H5F_new(old->shared, NULL, NULL);

	} else if (flags & H5F_ACC_TRUNC) {
	    /* Truncate existing file */
	    if (0 == (flags & H5F_ACC_RDWR)) {
		HRETURN_ERROR(H5E_FILE, H5E_BADVALUE, NULL,
			      "unable to truncate without write intent");
	    }
	    fd = H5F_low_open(type, name, access_parms,
			      H5F_ACC_RDWR | H5F_ACC_TRUNC, NULL);
	    if (!fd) {
		HRETURN_ERROR(H5E_FILE, H5E_CANTCREATE, NULL,
			      "unable to truncate file");
	    }
	    f = H5F_new(NULL, create_parms, access_parms);
	    f->shared->key = search;
	    f->shared->flags = flags;
	    f->shared->lf = fd;
	    empty_file = TRUE;

	} else {
	    fd = H5F_low_open(type, name, access_parms,
			      (flags & H5F_ACC_RDWR), NULL);
	    if (!fd) {
		HRETURN_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
			      "cannot open existing file");
	    }
	    f = H5F_new(NULL, create_parms, access_parms);
	    f->shared->key = search;
	    f->shared->flags = flags;
	    f->shared->lf = fd;
	}

    } else if (flags & H5F_ACC_CREAT) {
	if (0 == (flags & H5F_ACC_RDWR)) {
	    HRETURN_ERROR(H5E_FILE, H5E_BADVALUE, NULL,
			  "unable to create file without write intent");
	}
#ifdef HAVE_PARALLEL
	/*
	 * ROMIO cannot handle file-open with EXCL Create due to racing
	 * problem.  The first process creates the file which then fails all
	 * other processes.  Turn on TRUNC bit here.  It does not matter since
	 * the file does not exist at this point.
	 */
	fd = H5F_low_open(type, name, access_parms,
			  H5F_ACC_RDWR | H5F_ACC_CREAT |
			  (flags & H5F_ACC_TRUNC),
			  &search);
#else
	fd = H5F_low_open(type, name, access_parms,
			  H5F_ACC_RDWR | H5F_ACC_CREAT |
			  (flags & H5F_ACC_EXCL) | (flags & H5F_ACC_TRUNC),
			  &search);
#endif /*HAVE_PARALLEL*/
	if (!fd) {
	    HRETURN_ERROR(H5E_FILE, H5E_CANTCREATE, NULL,
			  "unable to create file");
	}
	f = H5F_new(NULL, create_parms, access_parms);
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
     * the flags at the bottom.	 The top level describes how the file can be
     * accessed through the HDF5 library.  The bottom level describes how the
     * file can be accessed through the C library.
     */
    f->intent = flags;
    f->name = H5MM_xstrdup(name);

    /*
     * Some of the properties may need to be updated. We would like to
     * eventually get rid of this step by not having redundant data!
     */
    if (1 == f->shared->nrefs) {
	if (H5F_LOW_FAMILY==f->shared->access_parms->driver) {
	    haddr_t x = f->shared->lf->u.fam.memb_size;
	    f->shared->access_parms->u.fam.memb_size = x;
	}
    }
    cp = f->shared->create_parms;

    /*
     * Read or write the file boot block.
     */
    if (empty_file) {
	/*
	 * For new files we must write the boot block.	The boot block starts
	 * immediately after the user-defined header, which we have already
	 * insured is a proper size.  The base address is set to the same thing
	 * as the boot block.
	 */
	H5F_addr_reset(&(f->shared->boot_addr));
	H5F_addr_inc(&(f->shared->boot_addr),
		     f->shared->create_parms->userblock_size);
	f->shared->base_addr = f->shared->boot_addr;

	f->shared->consist_flags = 0x03;
	if (H5F_flush(f, H5F_SCOPE_LOCAL, FALSE) < 0) {
	    HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL,
			"unable to write file boot block");
	}

    } else if (1 == f->shared->nrefs) {
	/* For existing files we must read the boot block. */
	if (H5F_locate_signature(f->shared->lf,
				 f->shared->access_parms,
				 &(f->shared->boot_addr)) < 0) {
	    HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, NULL,
			"unable to find signature");
	}
	if (H5F_low_read(f->shared->lf, access_parms, H5D_XFER_DFLT,
			 &(f->shared->boot_addr), fixed_size, buf) < 0) {
	    HGOTO_ERROR(H5E_IO, H5E_READERROR, NULL,
			"unable to read boot block");
	}
	
	/*
	 * Decode the fixed size part of the boot block.  For each of the
	 * version parameters, check that the library is able to handle that
	 * version.
	 */
	p = buf + H5F_SIGNATURE_LEN;	/*already checked */

	cp->bootblock_ver = *p++;
	if (cp->bootblock_ver != HDF5_BOOTBLOCK_VERSION) {
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
			"bad boot block version number");
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
	p++; /*reserved*/
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

	assert((size_t)(p-buf) == fixed_size);

	/* Read the variable length part of the boot block... */
	variable_size = H5F_SIZEOF_ADDR(f) +	/*base address */
			H5F_SIZEOF_ADDR(f) +	/*global free list addr */
			H5F_SIZEOF_ADDR(f) +	/*logical file size */
			H5F_SIZEOF_ADDR(f) +	/*reserved address*/
			H5G_SIZEOF_ENTRY(f);
	assert(variable_size <= sizeof buf);
	addr1 = f->shared->boot_addr;
	H5F_addr_inc(&addr1, (hsize_t)fixed_size);
	if (H5F_low_read(f->shared->lf, access_parms, H5D_XFER_DFLT,
			 &addr1, variable_size, buf) < 0) {
	    HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, NULL,
			"unable to read boot block");
	}
	p = buf;
	H5F_addr_decode(f, &p, &(f->shared->base_addr));
	H5F_addr_decode(f, &p, &(f->shared->freespace_addr));
	H5F_addr_decode(f, &p, &(f->shared->hdf5_eof));
	H5F_addr_decode(f, &p, &reserved_addr);
	if (H5G_ent_decode(f, &p, &root_ent) < 0) {
	    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
			"unable to read root symbol entry");
	}
	if (H5G_mkroot (f, &root_ent)<0) {
	    HGOTO_ERROR (H5E_FILE, H5E_CANTOPENFILE, NULL,
			 "unable to read root group");
	}

	/*
	 * The userdefined data is the area of the file before the base
	 * address.
	 */
	f->shared->create_parms->userblock_size = f->shared->base_addr.offset;
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
	if (H5DEBUG(F)) {
	    HDfprintf(H5DEBUG(F), "H5F: resetting EOF from %a to %a (abs)\n",
		      &addr1, &addr2);
	}
#endif
	H5F_low_seteof(f->shared->lf, &addr2);
    }

    /* Create and/or open the root group if we haven't already done so */
    if (H5G_mkroot (f, NULL)<0) {
	HGOTO_ERROR (H5E_FILE, H5E_CANTINIT, NULL,
		     "unable to create/open root group");
    }
    
    /* Success! */
    ret_value = f;

  done:
    if (!ret_value) {
	if (f) H5F_dest(f);
	H5F_low_close(fd, access_parms);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Fcreate
 *
 * Purpose:	This is the primary function for creating HDF5 files . The
 *		flags parameter determines whether an existing file will be
 *		overwritten or not.  All newly created files are opened for
 *		both reading and writing.  All flags may be combined with the
 *		bit-wise OR operator (`|') to change the behavior of the file
 *		create call.
 *
 *		The more complex behaviors of a file's creation and access
 *		are controlled through the file-creation and file-access
 *		property lists.  The value of H5P_DEFAULT for a template
 *		value indicates that the library should use the default
 *		values for the appropriate template.
 *
 * See also:	H5Fpublic.h for the list of supported flags. H5Ppublic.h for
 * 		the list of file creation and file access properties.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 * 
 * 	Robb Matzke, 18 Jul 1997
 *	File struct creation and destruction is through H5F_new() and
 *	H5F_dest(). Writing the root symbol table entry is done with
 *	H5G_encode().
 *	
 *  	Robb Matzke, 29 Aug 1997
 *	Moved creation of the boot block to H5F_flush().
 *	
 *  	Robb Matzke, 23 Sep 1997
 *	Most of the work is now done by H5F_open() since H5Fcreate() and
 *	H5Fopen() originally contained almost identical code.
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Better error checking for the creation and access property lists. It
 *	used to be possible to swap the two and core the library.  Also, zero
 *	is no longer valid as a default property list; one must use
 *	H5P_DEFAULT instead.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate(const char *filename, unsigned flags, hid_t create_id,
	  hid_t access_id)
{
    
    H5F_t		*new_file = NULL;	/* file struct for new file */
    const H5F_create_t	*create_parms;		/* pointer to the parameters to
						 * use when creating the file
						 */
    const H5F_access_t	*access_parms;		/* pointer to the file access
						 * parameters to use when
						 * creating the file
						 */
    hid_t		  ret_value = FAIL;

    FUNC_ENTER(H5Fcreate, FAIL);
    H5TRACE4("i","sIuii",filename,flags,create_id,access_id);

    /* Check/fix arguments */
    if (!filename || !*filename) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name");
    }
    if (flags & ~(H5F_ACC_EXCL|H5F_ACC_TRUNC|H5F_ACC_DEBUG)) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags");
    }
    if ((flags & H5F_ACC_EXCL) && (flags & H5F_ACC_TRUNC)) {
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL,
		     "mutually exclusive flags for file creation");
    }
    if (H5P_DEFAULT==create_id) {
	create_parms = &H5F_create_dflt;
    } else if (H5P_FILE_CREATE!=H5P_get_class (create_id) ||
	       NULL == (create_parms = H5I_object(create_id))) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		    "not a file creation property list");
    }
    if (H5P_DEFAULT==access_id) {
	access_parms = &H5F_access_dflt;
    } else if (H5P_FILE_ACCESS!=H5P_get_class (access_id) ||
	       NULL == (access_parms = H5I_object(access_id))) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		    "not a file access property list");
    }

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if (0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC))) {
	flags |= H5F_ACC_EXCL;	 /*default*/
    }
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /*
     * Create a new file or truncate an existing file.
     */
    if (NULL == (new_file = H5F_open(filename, flags, create_parms,
				     access_parms))) {
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file");
    }
    
    /* Get an atom for the file */
    if ((ret_value = H5I_register(H5I_FILE, new_file)) < 0) {
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		    "unable to atomize file");
    }

  done:
    if (ret_value < 0 && new_file) {
	/* Error condition cleanup */
	H5F_close(new_file);
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Fopen
 *
 * Purpose:	This is the primary function for accessing existing HDF5
 *		files.  The FLAGS argument determines whether writing to an
 *		existing file will be allowed or not.  All flags may be
 *		combined with the bit-wise OR operator (`|') to change the
 *		behavior of the file open call.  The more complex behaviors
 *		of a file's access are controlled through the file-access
 *		property list.
 *
 * See Also:	H5Fpublic.h for a list of possible values for FLAGS.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 * 
 *  	Robb Matzke, 18 Jul 1997
 *	File struct creation and destruction is through H5F_new() and
 *	H5F_dest(). Reading the root symbol table entry is done with
 *	H5G_decode().
 *	
 *  	Robb Matzke, 23 Sep 1997
 *	Most of the work is now done by H5F_open() since H5Fcreate() and
 *	H5Fopen() originally contained almost identical code.
 *
 * 	Robb Matzke, 18 Feb 1998
 *	Added better error checking for the flags and the file access
 *	property list.  It used to be possible to make the library dump core
 *	by passing an object ID that was not a file access property list.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen(const char *filename, unsigned flags, hid_t access_id)
{
    H5F_t		*new_file = NULL;	/* file struct for new file */
    const H5F_access_t	*access_parms;		/* pointer to the file access
						 * parameters to use when
						 * creating the file
						 */
    hid_t		  ret_value = FAIL;

    FUNC_ENTER(H5Fopen, FAIL);
    H5TRACE3("i","sIui",filename,flags,access_id);

    /* Check/fix arguments. */
    if (!filename || !*filename) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name");
    }
    if ((flags & ~H5F_ACC_PUBLIC_FLAGS) ||
	(flags & H5F_ACC_TRUNC) || (flags & H5F_ACC_EXCL)) {
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file open flags");
    }
    if (H5P_DEFAULT==access_id) {
	access_parms = &H5F_access_dflt;
    } else if (H5P_FILE_ACCESS!=H5P_get_class (access_id) ||
	       NULL == (access_parms = H5I_object(access_id))) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		    "not a file access property list");
    }

    /* Open the file */
    if (NULL==(new_file=H5F_open(filename, flags, NULL, access_parms))) {
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to open file");
    }

    /* Get an atom for the file */
    if ((ret_value = H5I_register(H5I_FILE, new_file)) < 0) {
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		    "unable to atomize file handle");
    }

 done:
    if (ret_value < 0 && new_file) {
	H5F_close(new_file);
    }
    /* Normal function cleanup */

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Fflush
 *
 * Purpose:	Flushes all outstanding buffers of a file to disk but does
 *		not remove them from the cache.  The OBJECT_ID can be a file,
 *		dataset, group, attribute, or named data type.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 * Modifications:
 *
 * 		Robb Matzke, 1998-10-16
 *		Added the `scope' argument.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fflush(hid_t object_id, H5F_scope_t scope)
{
    H5F_t	*f = NULL;
    H5G_t	*grp = NULL;
    H5T_t	*type = NULL;
    H5D_t	*dset = NULL;
    H5A_t	*attr = NULL;
    H5G_entry_t	*ent = NULL;
    
    FUNC_ENTER(H5Fflush, FAIL);
    H5TRACE2("e","iFs",object_id,scope);

    switch (H5I_get_type(object_id)) {
    case H5I_FILE:
	if (NULL==(f=H5I_object(object_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "invalid file identifier");
	}
	break;

    case H5I_GROUP:
	if (NULL==(grp=H5I_object(object_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "invalid group identifier");
	}
	ent = H5G_entof(grp);
	break;

    case H5I_DATATYPE:
	if (NULL==(type=H5I_object(object_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "invalid type identifier");
	}
	ent = H5T_entof(type);
	break;

    case H5I_DATASET:
	if (NULL==(dset=H5I_object(object_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "invalid dataset identifier");
	}
	ent = H5D_entof(dset);
	break;

    case H5I_ATTR:
	if (NULL==(attr=H5I_object(object_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "invalid attribute identifier");
	}
	ent = H5A_entof(attr);
	break;

    default:
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a file or file object");
    }

    if (!f) {
	if (!ent) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
			  "object is not assocated with a file");
	}
	f = ent->file;
    }
    if (!f) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "object is not associated with a file");
    }

    /* Flush the file */
    if (H5F_flush(f, scope, FALSE)<0) {
	HRETURN_ERROR(H5E_FILE, H5E_CANTINIT, FAIL,
		      "flush failed");
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_flush
 *
 * Purpose:	Flushes (and optionally invalidates) cached data plus the
 *		file boot block.  If the logical file size field is zero
 *		then it is updated to be the length of the boot block.
 *
 * Errors:
 *		CACHE	  CANTFLUSH	Can't flush cache. 
 *		IO	  WRITEERROR	Can't write header. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 29 1997
 *
 * Modifications:
 *              rky 980828 Only p0 writes metadata to disk.
 *
 * 		Robb Matzke, 1998-10-16
 *		Added the `scope' argument to indicate what should be
 *		flushed. If the value is H5F_SCOPE_GLOBAL then the entire
 *		virtual file is flushed; a value of H5F_SCOPE_LOCAL means
 *		that only the specified file is flushed.  A value of
 *		H5F_SCOPE_DOWN means flush the specified file and all
 *		children.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_flush(H5F_t *f, H5F_scope_t scope, hbool_t invalidate)
{
    uint8_t		buf[2048], *p = buf;
    haddr_t		reserved_addr;
    uintn		nerrors=0, i;
    
    FUNC_ENTER(H5F_flush, FAIL);
	
    /*
     * Nothing to do if the file is read only.	This determination is made at
     * the shared open(2) flags level, implying that opening a file twice,
     * once for read-only and once for read-write, and then calling
     * H5F_flush() with the read-only handle, still causes data to be flushed.
     */
    if (0 == (H5F_ACC_RDWR & f->shared->flags)) {
	HRETURN(SUCCEED);
    }

    /* Flush other stuff depending on scope */
    if (H5F_SCOPE_GLOBAL==scope) {
	while (f->mtab.parent) f = f->mtab.parent;
	scope = H5F_SCOPE_DOWN;
    }
    if (H5F_SCOPE_DOWN==scope) {
	for (i=0; i<f->mtab.nmounts; i++) {
	    if (H5F_flush(f->mtab.child[i].file, scope, invalidate)<0) {
		nerrors++;
	    }
	}
    }

    /* flush the entire raw data cache */
    if (H5F_istore_flush (f, invalidate)<0) {
	HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL,
		      "unable to flush raw data cache");
    }
    
    /* flush (and invalidate) the entire meta data cache */
    if (H5AC_flush(f, NULL, 0, invalidate) < 0) {
	HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL,
		      "unable to flush meta data cache");
    }

    /* encode the file boot block */
    HDmemcpy(p, H5F_SIGNATURE, H5F_SIGNATURE_LEN);
    p += H5F_SIGNATURE_LEN;

    *p++ = f->shared->create_parms->bootblock_ver;
    *p++ = f->shared->create_parms->freespace_ver;
    *p++ = f->shared->create_parms->objectdir_ver;
    *p++ = 0;			/*reserved*/
    *p++ = f->shared->create_parms->sharedheader_ver;
    assert (H5F_SIZEOF_ADDR(f)<=255);
    *p++ = (uint8_t)H5F_SIZEOF_ADDR(f);
    assert (H5F_SIZEOF_SIZE(f)<=255);
    *p++ = (uint8_t)H5F_SIZEOF_SIZE(f);
    *p++ = 0;			/*reserved */
    UINT16ENCODE(p, f->shared->create_parms->sym_leaf_k);
    UINT16ENCODE(p, f->shared->create_parms->btree_k[H5B_SNODE_ID]);
    UINT32ENCODE(p, f->shared->consist_flags);
    H5F_addr_encode(f, &p, &(f->shared->base_addr));
    H5F_addr_encode(f, &p, &(f->shared->freespace_addr));
    H5F_addr_encode(f, &p, &(f->shared->hdf5_eof));
    H5F_addr_undef(&reserved_addr);
    H5F_addr_encode(f, &p, &reserved_addr);
    H5G_ent_encode(f, &p, H5G_entof(f->shared->root_grp));

    /* update file length if necessary */
    if (!H5F_addr_defined(&(f->shared->hdf5_eof))) {
	H5F_addr_reset(&(f->shared->hdf5_eof));
	H5F_addr_inc(&(f->shared->hdf5_eof), (hsize_t)(p-buf));
	H5F_low_seteof(f->shared->lf, &(f->shared->hdf5_eof));
    }
    
    /* write the boot block to disk */
#ifdef HAVE_PARALLEL
    H5F_mpio_tas_allsame(f->shared->lf, TRUE);	/* only p0 will write */
#endif
    if (H5F_low_write(f->shared->lf, f->shared->access_parms,
    		      H5D_XFER_DFLT,
		      &(f->shared->boot_addr), (size_t)(p-buf), buf)<0) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "unable to write header");
    }
    
    /* Flush file buffers to disk */
    if (H5F_low_flush(f->shared->lf, f->shared->access_parms) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "low level flush failed");
    }

    /* Check flush errors for children - errors are already on the stack */
    if (nerrors) HRETURN(FAIL);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_close
 *
 * Purpose:	Closes an open HDF5 file.  From the API this function gets
 *		called when a file hid_t reference count gets to zero as a
 *		result of calling H5Fclose().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, September 23, 1997
 *
 * Modifications:
 *
 * 	Robb Matzke, 1998-10-14
 *	Nothing happens unless the H5F_t reference count is one (the
 *	file is flushed anyway).  The reference count is decremented by
 *	H5F_dest().
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_close(H5F_t *f)
{
    uintn	i;

    FUNC_ENTER(H5F_close, FAIL);

    /*
     * If the reference count is positive then just decrement the count and
     * flush the file.
     */
    if (f->nrefs>1) {
	if (H5F_flush(f, H5F_SCOPE_LOCAL, FALSE)<0) {
	    HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL,
			  "unable to flush cache");
	}
	H5F_dest(f); /*decrement reference counts*/
	HRETURN(SUCCEED);
    }
    
    /*
     * Unmount and close each child before closing the current file.
     */
    assert(NULL==f->mtab.parent);
    for (i=0; i<f->mtab.nmounts; i++) {
	f->mtab.child[i].file->mtab.parent = NULL;
	H5G_close(f->mtab.child[i].group);
	H5F_close(f->mtab.child[i].file);
    }
    f->mtab.nmounts = 0;

    /*
     * If object headers are still open then delay deletion of resources until
     * they have all been closed.  Flush all caches and update the object
     * header anyway so that failing to close all objects isn't a major
     * problem.
     */
    if (f->nopen_objs>0) {
	if (H5F_flush(f, H5F_SCOPE_LOCAL, FALSE)<0) {
	    HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL,
			  "unable to flush cache");
	}
#ifdef H5F_DEBUG
	if (H5DEBUG(F)) {
	    fprintf(H5DEBUG(F), "H5F: H5F_close(%s): %u object header%s still "
		    "open (file close will complete when %s closed)\n",
		    f->name,
		    f->nopen_objs,
		    1 == f->nopen_objs?" is":"s are",
		    1 == f->nopen_objs?"that header is":"those headers are");
	}
#endif
	f->close_pending = TRUE;
	HRETURN(SUCCEED);
    } else if (f->close_pending) {
#ifdef H5F_DEBUG
	if (H5DEBUG(F)) {
	    fprintf(H5DEBUG(F), "H5F: H5F_close: operation completing\n");
	}
#endif
    }

    /*
     * If this is the last reference to the shared part of the file then
     * close it also.
     */
    if (1==f->nrefs && 1==f->shared->nrefs) {
	/* Flush and destroy all caches */
	if (H5F_flush (f, H5F_SCOPE_LOCAL, TRUE)<0) {
	    HRETURN_ERROR (H5E_CACHE, H5E_CANTFLUSH, FAIL,
			   "unable to flush cache");
	}

	/* Dump debugging info */
	H5AC_debug(f);
	H5F_istore_stats (f, FALSE);

	/* Close files and release resources */
	H5F_low_close(f->shared->lf, f->shared->access_parms);
    } else {
	/*
	 * Flush all caches but do not destroy. As long as all handles for
	 * this file are closed the flush isn't really necessary, but lets
	 * just be safe.
	 */
	if (H5F_flush(f, H5F_SCOPE_LOCAL, TRUE)<0) {
	    HRETURN_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL,
			  "unable to flush cache");
	}
    }
    
    if (H5F_dest(f)<0) {
	HRETURN_ERROR (H5E_FILE, H5E_CANTINIT, FAIL,
		       "problems closing file");
    }
    FUNC_LEAVE(SUCCEED);
}


/*--------------------------------------------------------------------------
 NAME
    H5Fclose

 PURPOSE
    Close an open HDF5 file.

 USAGE
    herr_t H5Fclose(file_id)
	int32_t file_id;	IN: File ID of file to close

 ERRORS
    ARGS      BADTYPE	    Not a file atom. 
    ATOM      BADATOM	    Can't remove atom. 
    ATOM      BADATOM	    Can't unatomize file. 
    CACHE     CANTFLUSH	    Can't flush cache. 

 RETURNS
    Non-negative on success/Negative on failure

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
H5Fclose(hid_t file_id)
{
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER(H5Fclose, FAIL);
    H5TRACE1("e","i",file_id);

    /* Check/fix arguments. */
    if (H5I_FILE != H5I_get_type(file_id)) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file atom");
    }
    if (NULL == H5I_object(file_id)) {
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "unable to unatomize file");
    }

    /*
     * Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if (H5I_dec_ref (file_id)<0) {
	HGOTO_ERROR (H5E_ATOM, H5E_CANTINIT, FAIL, "problems closing file");
    }
    
 done:
    FUNC_LEAVE(ret_value < 0 ? FAIL : SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_mount
 *
 * Purpose:	Mount file CHILD onto the group specified by LOC and NAME,
 *		using mount properties in PLIST.  CHILD must not already be
 *		mouted and must not be a mount ancestor of the mount-point.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October  6, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 1998-10-14
 *	The reference count for the mounted H5F_t is incremented.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_mount(H5G_entry_t *loc, const char *name, H5F_t *child,
	  const H5F_mprop_t __unused__ *plist)
{
    H5G_t	*mount_point = NULL;	/*mount point group		*/
    H5G_entry_t	*mp_ent = NULL;		/*mount point symbol table entry*/
    H5F_t	*ancestor = NULL;	/*ancestor files		*/
    H5F_t	*parent = NULL;		/*file containing mount point	*/
    intn	lt, rt, md, cmp;	/*binary search indices		*/
    H5G_entry_t	*ent = NULL;		/*temporary symbol table entry	*/
    herr_t	ret_value = FAIL;	/*return value			*/
    
    FUNC_ENTER(H5F_mount, FAIL);
    assert(loc);
    assert(name && *name);
    assert(child);
    assert(plist);

    /*
     * Check that the child isn't mounted, that the mount point exists, and
     * that the mount wouldn't introduce a cycle in the mount tree.
     */
    if (child->mtab.parent) {
	HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "file is already mounted");
    }
    if (NULL==(mount_point=H5G_open(loc, name))) {
	HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "mount point not found");
    }
    parent = H5G_fileof(mount_point);
    mp_ent = H5G_entof(mount_point);
    for (ancestor=parent; ancestor; ancestor=ancestor->mtab.parent) {
	if (ancestor==child) {
	    HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL,
			"mount would introduce a cycle");
	}
    }
    
    /*
     * Use a binary search to locate the position that the child should be
     * inserted into the parent mount table.  At the end of this paragraph
     * `md' will be the index where the child should be inserted.
     */
    lt = md = 0;
    rt = parent->mtab.nmounts;
    cmp = -1;
    while (lt<rt && cmp) {
	md = (lt+rt)/2;
	ent = H5G_entof(parent->mtab.child[md].group);
	cmp = H5F_addr_cmp(&(mp_ent->header), &(ent->header));
	if (cmp<0) {
	    rt = md;
	} else if (cmp>0) {
	    lt = md+1;
	}
    }
    if (cmp>0) md++;
    if (!cmp) {
	HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL,
		    "mount point is already in use");
    }
    
    /* Make room in the table */
    if (parent->mtab.nmounts>=parent->mtab.nalloc) {
	uintn n = MAX(16, 2*parent->mtab.nalloc);
	H5F_mount_t *x = H5MM_realloc(parent->mtab.child,
				      n*sizeof(parent->mtab.child[0]));
	if (!x) {
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			"memory allocation failed for mount table");
	}
	parent->mtab.child = x;
	parent->mtab.nalloc = n;
    }

    /* Insert into table */
    HDmemmove(parent->mtab.child+md+1,
	      parent->mtab.child+md,
	      (parent->mtab.nmounts-md)*sizeof(parent->mtab.child[0]));
    parent->mtab.nmounts++;
    parent->mtab.child[md].group = mount_point;
    parent->mtab.child[md].file = child;
    child->mtab.parent = parent;
    child->nrefs++;
    ret_value = SUCCEED;

 done:
    if (ret_value<0 && mount_point) {
	H5G_close(mount_point);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_unmount
 *
 * Purpose:	Unmount the child which is mounted at the group specified by
 *		LOC and NAME or fail if nothing is mounted there.  Neither
 *		file is closed.
 *
 *		Because the mount point is specified by name and opened as a
 *		group, the H5G_namei() will resolve it to the root of the
 *		mounted file, not the group where the file is mounted.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October  6, 1998
 *
 * Modifications:
 *
 * 	Robb Matzke, 1998-10-14
 *	The ref count for the child is decremented by calling H5F_close().
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_unmount(H5G_entry_t *loc, const char *name)
{
    H5G_t	*mounted = NULL;	/*mount point group		*/
    H5G_entry_t	*mnt_ent = NULL;	/*mounted symbol table entry	*/
    H5F_t	*child = NULL;		/*mounted file			*/
    H5F_t	*parent = NULL;		/*file where mounted		*/
    H5G_entry_t	*ent = NULL;		/*temporary symbol table entry	*/
    herr_t	ret_value = FAIL;	/*return value			*/
    uintn	i;			/*coutners			*/
    intn	lt, rt, md=(-1), cmp;	/*binary search indices		*/
    
    FUNC_ENTER(H5F_unmount, FAIL);
    assert(loc);
    assert(name && *name);

    /*
     * Get the mount point, or more precisely the root of the mounted file.
     * If we get the root group and the file has a parent in the mount tree,
     * then we must have found the mount point.
     */
    if (NULL==(mounted=H5G_open(loc, name))) {
	HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "mount point not found");
    }
    child = H5G_fileof(mounted);
    mnt_ent = H5G_entof(mounted);
    ent = H5G_entof(child->shared->root_grp);

    if (child->mtab.parent &&
	H5F_addr_eq(&(mnt_ent->header), &(ent->header))) {
	/*
	 * We've been given the root group of the child.  We do a reverse
	 * lookup in the parent's mount table to find the correct entry.
	 */
	parent = child->mtab.parent;
	for (i=0; i<parent->mtab.nmounts; i++) {
	    if (parent->mtab.child[i].file==child) {
		/* Unmount the child */
		parent->mtab.nmounts -= 1;
		H5G_close(parent->mtab.child[i].group);
		child->mtab.parent = NULL;
		H5F_close(child);
		HDmemmove(parent->mtab.child+i,
			  parent->mtab.child+i+1,
			  ((parent->mtab.nmounts-i)*
			   sizeof(parent->mtab.child[0])));
		ret_value = SUCCEED;
	    }
	}
	assert(ret_value>=0);
	
    } else {
	/*
	 * We've been given the mount point in the parent.  We use a binary
	 * search in the parent to locate the mounted file, if any.
	 */
	parent = child; /*we guessed wrong*/
	lt = 0;
	rt = parent->mtab.nmounts;
	cmp = -1;
	while (lt<rt && cmp) {
	    md = (lt+rt)/2;
	    ent = H5G_entof(parent->mtab.child[md].group);
	    cmp = H5F_addr_cmp(&(mnt_ent->header), &(ent->header));
	    if (cmp<0) {
		rt = md;
	    } else {
		lt = md+1;
	    }
	}
	if (cmp) {
	    HGOTO_ERROR(H5E_FILE, H5E_MOUNT, FAIL, "not a mount point");
	}

	/* Unmount the child */
	parent->mtab.nmounts -= 1;
	H5G_close(parent->mtab.child[md].group);
	parent->mtab.child[md].file->mtab.parent = NULL;
	H5F_close(parent->mtab.child[md].file);
	HDmemmove(parent->mtab.child+md,
		  parent->mtab.child+md+1,
		  (parent->mtab.nmounts-md)*sizeof(parent->mtab.child[0]));
	ret_value = SUCCEED;
    }
    
 done:
    if (mounted) H5G_close(mounted);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_mountpoint
 *
 * Purpose:	If ENT is a mount point then copy the entry for the root
 *		group of the mounted file into ENT.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_mountpoint(H5G_entry_t *find/*in,out*/)
{
    H5F_t	*parent = find->file;
    intn	lt, rt, md=(-1), cmp;
    H5G_entry_t	*ent = NULL;
    
    FUNC_ENTER(H5F_mountpoint, FAIL);
    assert(find);

    /*
     * The loop is necessary because we might have file1 mounted at the root
     * of file2, which is mounted somewhere in file3.
     */
    do {
	/*
	 * Use a binary search to find the potential mount point in the mount
	 * table for the parent
	 */
	lt = 0;
	rt = parent->mtab.nmounts;
	cmp = -1;
	while (lt<rt && cmp) {
	    md = (lt+rt)/2;
	    ent = H5G_entof(parent->mtab.child[md].group);
	    cmp = H5F_addr_cmp(&(find->header), &(ent->header));
	    if (cmp<0) {
		rt = md;
	    } else {
		lt = md+1;
	    }
	}

	/* Copy root info over to ENT */
	if (0==cmp) {
	    ent = H5G_entof(parent->mtab.child[md].file->shared->root_grp);
	    *find = *ent;
	    parent = ent->file;
	}
    } while (!cmp);
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Fmount
 *
 * Purpose:	Mount file CHILD_ID onto the group specified by LOC_ID and
 *		NAME using mount properties PLIST_ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fmount(hid_t loc_id, const char *name, hid_t child_id, hid_t plist_id)
{
    H5G_entry_t		*loc = NULL;
    const H5F_mprop_t	*plist = NULL;
    H5F_t		*child = NULL;
    
    FUNC_ENTER(H5Fmount, FAIL);
    H5TRACE4("e","isii",loc_id,name,child_id,plist_id);

    /* Check arguments */
    if (NULL==(loc=H5G_loc(loc_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }
    if (H5I_FILE!=H5I_get_type(child_id) ||
	NULL==(child=H5I_object(child_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (H5P_DEFAULT==plist_id) {
	plist = &H5F_mount_dflt;
    } else if (H5P_MOUNT!=H5P_get_class(plist_id) ||
	       NULL==(plist=H5I_object(plist_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
		      "not a mount property list");
    }

    /* Do the mount */
    if (H5F_mount(loc, name, child, plist)<0) {
	HRETURN_ERROR(H5E_FILE, H5E_MOUNT, FAIL,
		      "unable to mount file");
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Funmount
 *
 * Purpose:	Given a mount point, dissassociate the mount point's file
 *		from the file mounted there.  Do not close either file.
 *
 *		The mount point can either be the group in the parent or the
 *		root group of the mounted file (both groups have the same
 *		name).  If the mount point was opened before the mount then
 *		it's the group in the parent, but if it was opened after the
 *		mount then it's the root group of the child.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Funmount(hid_t loc_id, const char *name)
{
    H5G_entry_t		*loc = NULL;
    
    FUNC_ENTER(H5Funmount, FAIL);
    H5TRACE2("e","is",loc_id,name);

    /* Check args */
    if (NULL==(loc=H5G_loc(loc_id))) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    }
    if (!name || !*name) {
	HRETURN_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    }

    /* Unmount */
    if (H5F_unmount(loc, name)<0) {
	HRETURN_ERROR(H5E_FILE, H5E_MOUNT, FAIL,
		      "unable to unmount file");
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Freopen
 *
 * Purpose:	Reopen a file.  The new file handle which is returned points
 *		to the same file as the specified file handle.  Both handles
 *		share caches and other information.  The only difference
 *		between the handles is that the new handle is not mounted
 *		anywhere and no files are mounted on it.
 *
 * Return:	Success:	New file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Freopen(hid_t file_id)
{
    H5F_t	*old_file=NULL;
    H5F_t	*new_file=NULL;
    hid_t	ret_value = -1;
    

    FUNC_ENTER(H5Freopen, FAIL);
    H5TRACE1("i","i",file_id);

    if (H5I_FILE!=H5I_get_type(file_id) ||
	NULL==(old_file=H5I_object(file_id))) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file");
    }
    if (NULL==(new_file=H5F_new(old_file->shared, NULL, NULL))) {
	HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file");
    }
    if ((ret_value=H5I_register(H5I_FILE, new_file))<0) {
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL,
		    "unable to atomize file handle");
    }

 done:
    if (ret_value<0 && new_file) {
	H5F_close(new_file);
    }
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_block_read
 *
 * Purpose:	Reads some data from a file/server/etc into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Errors:
 *		IO	  READERROR	Low-level read failed. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *		June 2, 1998	Albert Cheng
 *		Added xfer_mode argument
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_read(H5F_t *f, const haddr_t *addr, hsize_t size,
	       const H5D_transfer_t xfer_mode, void *buf)
{
    haddr_t		    abs_addr;

    FUNC_ENTER(H5F_block_read, FAIL);

    assert (size < SIZET_MAX);

    /* convert the relative address to an absolute address */
    abs_addr = f->shared->base_addr;
    H5F_addr_add(&abs_addr, addr);

    /* Read the data */
    if (H5F_low_read(f->shared->lf, f->shared->access_parms, xfer_mode,
		     &abs_addr, (size_t)size, buf) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "low-level read failed");
    }
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_block_write
 *
 * Purpose:	Writes some data from memory to a file/server/etc.  The
 *		data is contiguous.  The address is relative to the base
 *		address.
 *
 * Errors:
 *		IO	  WRITEERROR	Low-level write failed. 
 *		IO	  WRITEERROR	No write intent. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 * Modifications:
 *		June 2, 1998	Albert Cheng
 *		Added xfer_mode argument
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_block_write(H5F_t *f, const haddr_t *addr, hsize_t size,
		const H5D_transfer_t xfer_mode, const void *buf)
{
    haddr_t		    abs_addr;

    FUNC_ENTER(H5F_block_write, FAIL);

    assert (size < SIZET_MAX);

    if (0 == (f->intent & H5F_ACC_RDWR)) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "no write intent");
    }

    /* Convert the relative address to an absolute address */
    abs_addr = f->shared->base_addr;
    H5F_addr_add(&abs_addr, addr);

    /* Write the data */
    if (H5F_low_write(f->shared->lf, f->shared->access_parms, xfer_mode,
		      &abs_addr, (size_t)size, buf)) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "low-level write failed");
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_debug
 *
 * Purpose:	Prints a file header to the specified stream.  Each line
 *		is indented and the field name occupies the specified width
 *		number of characters.
 *
 * Errors:
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug  1 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_debug(H5F_t *f, const haddr_t __unused__ *addr, FILE * stream,
	  intn indent, intn fwidth)
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
	    "Free list address:");
    H5F_addr_print(stream, &(f->shared->freespace_addr));
    fprintf(stream, " (rel)\n");

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
	    "Total size of hdf5 data:");
    H5F_addr_print(stream, &(f->shared->hdf5_eof));
    fprintf(stream, " bytes\n");

    fprintf(stream, "%*s%-*s %lu bytes\n", indent, "", fwidth,
	    "Size of user block:",
	    (unsigned long) (f->shared->create_parms->userblock_size));
    fprintf(stream, "%*s%-*s %u bytes\n", indent, "", fwidth,
	    "Size of file size_t type:",
	    (unsigned) (f->shared->create_parms->sizeof_size));
    fprintf(stream, "%*s%-*s %u bytes\n", indent, "", fwidth,
	    "Size of file haddr_t type:",
	    (unsigned) (f->shared->create_parms->sizeof_addr));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Symbol table leaf node 1/2 rank:",
	    (unsigned) (f->shared->create_parms->sym_leaf_k));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Symbol table internal node 1/2 rank:",
	    (unsigned) (f->shared->create_parms->btree_k[H5B_SNODE_ID]));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Boot block version number:",
	    (unsigned) (f->shared->create_parms->bootblock_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Free list version number:",
	    (unsigned) (f->shared->create_parms->freespace_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Object directory version number:",
	    (unsigned) (f->shared->create_parms->objectdir_ver));
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Shared header version number:",
	    (unsigned) (f->shared->create_parms->sharedheader_ver));

    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	    "Root group symbol table entry:",
	    f->shared->root_grp ? "" : "(none)");
    if (f->shared->root_grp) {
	H5G_ent_debug(f, H5G_entof (f->shared->root_grp), stream,
		      indent+3, MAX(0, fwidth-3), NULL);
    }
    FUNC_LEAVE(SUCCEED);
}
