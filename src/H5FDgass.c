/*
 * Copyright © 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Saurabh Bagchi (bagchi@uiuc.edu)
 *              Thursday, August 12 -Tuesday, August 17, 1999
 *
 * Purpose:	This is the GASS I/O driver.
 *
 */
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

/* Packages needed by this file.*/
#include <H5Eprivate.h>

#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))

/* The driver identification number, initialized at runtime */
static hid_t H5FD_GASS_g = 0;

#ifdef HAVE_GASS

/* File operations */
#define OP_UNKNOWN	0
#define OP_READ		1
#define OP_WRITE	2

/*
 * The description of a file belonging to this driver. The `eoa' and `eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying Unix file). The `pos'
 * value is used to eliminate file position updates when they would be a
 * no-op. Unfortunately we've found systems that use separate file position
 * indicators for reading and writing so the lseek can only be eliminated if
 * the current operation is the same as the previous operation.  When opening
 * a file the `eof' will be set to the current file size, `eoa' will be set
 * to zero, `pos' will be set to H5F_ADDR_UNDEF (as it is when an error
 * occurs), and `op' will be set to H5F_OP_UNKNOWN.
 */
typedef struct H5FD_gass_t {
    H5FD_t	pub;			/*public stuff, must be first	*/
    int		fd;			/*the unix file			*/
    GASS_Info   info;                   /*file information */
    haddr_t	eoa;			/*end of allocated region	*/
    haddr_t	eof;			/*end of file; current file size*/
    haddr_t	pos;			/*current file I/O position	*/
    int		op;			/*last operation		*/
    
  
} H5FD_gass_t;

/*
 * This driver supports systems that have the lseek64() function by defining
 * some macros here so we don't have to have conditional compilations later
 * throughout the code.
 *
 * file_offset_t:	The datatype for file offsets, the second argument of
 *			the lseek() or lseek64() call.
 *
 * file_seek:		The function which adjusts the current file position,
 *			either lseek() or lseek64().
 */
#ifdef HAVE_LSEEK64
#   define file_offset_t	off64_t
#   define file_seek		lseek64
#else
#   define file_offset_t	off_t
#   define file_seek		lseek
#endif

/*
 * These macros check for overflow of various quantities.  These macros
 * assume that file_offset_t is signed and haddr_t and size_t are unsigned.
 * 
 * ADDR_OVERFLOW:	Checks whether a file address of type `haddr_t'
 *			is too large to be represented by the second argument
 *			of the file seek function.
 *
 * SIZE_OVERFLOW:	Checks whether a buffer size of type `hsize_t' is too
 *			large to be represented by the `size_t' type.
 *
 * REGION_OVERFLOW:	Checks whether an address and size pair describe data
 *			which can be addressed entirely by the second
 *			argument of the file seek function.
 */
#define MAXADDR (((haddr_t)1<<(8*sizeof(file_offset_t)-1))-1)
#define ADDR_OVERFLOW(A)	(HADDR_UNDEF==(A) ||			      \
				 ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z)	((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)	(ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||      \
				 sizeof(file_offset_t)<sizeof(size_t) ||      \
                                 HADDR_UNDEF==(A)+(Z) ||		      \
				 (file_offset_t)((A)+(Z))<(file_offset_t)(A))

/* Prototypes */
static H5FD_t *H5FD_gass_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_gass_close(H5FD_t *_file);
static haddr_t H5FD_gass_get_eoa(H5FD_t *_file);
static herr_t H5FD_gass_set_eoa(H5FD_t *_file, haddr_t addr);
static haddr_t H5FD_gass_get_eof(H5FD_t *_file);
static herr_t H5FD_gass_read(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			     hsize_t size, void *buf);
static herr_t H5FD_gass_write(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			      hsize_t size, const void *buf);

/* GASS I/O-specific file access properties */
typedef struct H5FD_gass_fapl_t {
  GASS_Info            info;      /* access property parameters */
} H5FD_gass_fapl_t;

/* The GASS IO driver information */
static const H5FD_class_t H5FD_gass_g = {
    "gass",					/*name			*/
    MAXADDR,					/*maxaddr		*/
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    sizeof(H5FD_gass_fapl_t), 			/*fapl_size		*/
    NULL,					/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    0,						/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_gass_open,				/*open			*/
    H5FD_gass_close,				/*close			*/
    NULL,				        /*cmp			*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_gass_get_eoa,				/*get_eoa		*/
    H5FD_gass_set_eoa, 				/*set_eoa		*/
    H5FD_gass_get_eof,				/*get_eof		*/
    H5FD_gass_read,				/*read			*/
    H5FD_gass_write,				/*write			*/
    NULL,				        /*flush			*/
    H5FD_FLMAP_SINGLE,				/*fl_map		*/
};


/*-------------------------------------------------------------------------
 * Function:	H5FD_gass_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the gass driver.
 *
 *		Failure:	Negative.
 *
 * Programmer:	Saurabh Bagchi
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_gass_init(void)
{
    if (!H5FD_GASS_g) {
	H5FD_GASS_g = H5FDregister(&H5FD_gass_g);
    }
    globus_module_activate (GLOBUS_COMMON_MODULE);
    globus_module_activate (GLOBUS_GASS_FILE_MODULE);

    return H5FD_GASS_g;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_gass
 *
 * Purpose:	Store the user supplied GASS INFO in
 *		the file access property list FAPL_ID which can then be used
 *		to create and/or open the file. 
 *
 *		GASS info object to be used for file open using GASS.
 *              Any modification to info after
 *		this function call returns may have undetermined effect
 *		to the access property list.  Users should call this
 *		function again to setup the property list.
 *
 *
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Saurabh Bagchi
 *		Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_gass(hid_t fapl_id, GASS_Info info)
{
    H5FD_gass_fapl_t	fa;
    
    /*NO TRACE*/

    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
#ifdef LATER
#warning "We need to verify that INFO contain sensible information."
#endif

    /* Initialize driver specific properties */
    fa.info = info;
    return H5Pset_driver(fapl_id, H5FD_GASS, &fa);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_gass
 *
 * Purpose:	If the file access property list is set to the H5FD_GASS
 *		driver then this function returns the GASS info object
 *		through the INFO pointer.
 *
 * Return:	Success:	Non-negative with the info object returned 
 *                              through the INFO arguments if non-null. 
 *				The information is copied and it is therefore
 *				valid only until the file access property
 *				list is modified or closed.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Saurabh Bagchi
 *		Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_gass(hid_t fapl_id, GASS_Info *info/*out*/)
{
    H5FD_gass_fapl_t	*fa;
    
    /*NO TRACE*/

    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (H5FD_GASS!=H5Pget_driver(fapl_id)) return -1;
    if (NULL==(fa=H5Pget_driver_info(fapl_id))) return -1;

    if (info) *info = fa->info;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_gass_open
 *
 * Purpose:	Opens a file with name NAME.  The FLAGS are a bit field with
 *		purpose similar to the second argument of open(2) and which
 *		are defined in H5Fpublic.h. The file access property list
 *		FAPL_ID contains the driver properties and MAXADDR
 *		is the largest address which this file will be expected to
 *		access.
 *
 * Return:	Success:	A pointer to a new file data structure. The
 *				public fields will be initialized by the
 *				caller, which is always H5FD_open().
 *
 *		Failure:	NULL
 *
 * Programmer:	Saurabh Bagchi
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_gass_open(const char *name, unsigned flags, hid_t fapl_id,
	       haddr_t maxaddr)
{
    int		fd;
    struct stat	sb;
    H5FD_gass_t	*file=NULL;
    const H5FD_gass_fapl_t	*fa=NULL;
    H5FD_gass_fapl_t		_fa;
    char *filename = (char *) malloc(80 * sizeof(char));
    
    /* fprintf(stdout, "Entering H5FD_gass_open name=%s flags=0x%x\n", name, flags); */

    strcpy (filename, name);
    
    /* Obtain a pointer to gass-specific file access properties */
    if (H5P_DEFAULT==fapl_id || H5FD_GASS!=H5Pget_driver(fapl_id)) {
      GASS_INFO_NULL (_fa.info);
      /* _fa.info = GASS_INFO_NULL; */
      /* _fa.info = {0,0}; */ /*default*/
	fa = &_fa;
    } else {
	fa = H5Pget_driver_info(fapl_id);
	assert(fa);
    }
    
    /* Check arguments */
    if (!name || !*name) return NULL;
    if (0==maxaddr || HADDR_UNDEF==maxaddr) return NULL;
    if (ADDR_OVERFLOW(maxaddr)) return NULL;

    /* When I come down here, the possible flag values and the correct
       responses are given here :-

       1. H5F_ACC_CREAT | H5F_ACC_RDWR | H5F_ACC_EXCL : The file is 
          a new one. Go ahead and open it in O_RDWR. 

       2. H5F_ACC_CREAT | H5F_ACC_RDWR | H5F_ACC_TRUNC : Use
          O_RDWR | O_TRUNC with gass_open.

       3. H5F_ACC_RDWR | H5F_ACC_TRUNC : File already exists. Truncate it.
	  
       4. H5F_ACC_RDWR : Use O_RDWR with gass_open

       5. H5F_ACC_RDWR is not set : Use O_RDONLY with gass_open

       One reason why we cannot simply pass on the flags to gass_open
       is that gass_open does not support many of them (e.g., O_CREAT)
    */

    /* fprintf (stderr, "flags=0x%x\n", flags); */
    
    if ((flags & H5F_ACC_CREAT) && (flags & H5F_ACC_RDWR) && (flags & H5F_ACC_EXCL)) {
      if ((fd = globus_gass_open (filename, O_RDWR|O_TRUNC)) < 0) {
	/* HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, FAIL, "open failed"); */
	fprintf (stderr, "Can't open file. \n");
	return NULL;
      }
      
    }
    else if ((flags & H5F_ACC_CREAT) && (flags & H5F_ACC_RDWR) && (flags & H5F_ACC_TRUNC)) {
      if ((fd = globus_gass_open (filename, O_RDWR|O_TRUNC)) < 0) {
	/* HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "open failed"); */
	fprintf (stderr, "Can't open file. \n");
	return NULL;
      }
      
    }
    else if ((flags & H5F_ACC_RDWR) && (flags & H5F_ACC_TRUNC)) {
      if ((fd = globus_gass_open (filename, O_RDWR|O_TRUNC)) < 0) {
	/* HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "open failed"); */
	fprintf (stderr, "Can't open file. \n");
	return NULL;
      }
    
    }
    else if (flags & H5F_ACC_RDWR) {
      printf ("I'm here in H5FDgass_open going to call globus_gass_open with O_RDWR. \n");
      printf ("Name of URL =%s \n", filename);
      if ((fd = globus_gass_open (filename, O_RDWR)) < 0) {
	/* HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "open failed"); */
	fprintf (stderr, "Can't open file. \n");
	return NULL;
      }
      
    }
    else { /* This is case where H5F_ACC_RDWR is not set */
      if ((fd = globus_gass_open (filename, O_RDONLY)) < 0) {
	/* HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "open failed");*/
	fprintf (stderr, "Can't open file. \n");
	return NULL;
      }
      
    }
   
     if (fstat(fd, &sb)<0) {
	close(fd);
	return NULL;
    }

    /* Create the new file struct */
    file = calloc(1, sizeof(H5FD_gass_t));
    file->fd = fd;
    file->eof = sb.st_size;
    file->pos = HADDR_UNDEF;
    file->op = OP_UNKNOWN;
    file->info = fa->info;
    
    return (H5FD_t*)file;
}

/*-------------------------------------------------------------------------
 * Function:	H5FD_gass_close
 *
 * Purpose:	Closes a GASS file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1, file not closed.
 *
 * Programmer:	Saurabh Bagchi
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_gass_close (H5FD_t *_file)
{
	int gasserr;
        H5FD_gass_t *file = (H5FD_gass_t *)_file;

	if (file == NULL) {
	  /* HRETURN_ERROR(H5E_IO, H5E_CANTCLOSEFILE, NULL, "close failed"); */
	  fprintf (stderr, "Can't close file. \n");
	  return -1;
	}
	
	gasserr = globus_gass_close (file->fd);
	if (gasserr == -1)
		fprintf (stderr, "GASS close failed. \n");

	return gasserr;
}

	
/*-------------------------------------------------------------------------
 * Function:	H5FD_gass_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The EOA marker
 *              is the first address past the last byte allocated in the
 *              format address space.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:  Saurabh Bagchi
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_gass_get_eoa(H5FD_t *_file)
{
	H5FD_gass_t *file = (H5FD_gass_t *)_file;
	return file->eoa;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_gass_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file. This function is
 *              called shortly after an existing HDF5 file is opened in order
 *              to tell the driver where the end of the HDF5 data is located.
 *
 * Return:      Success:        0
 *
 * Programmer:  Saurabh Bagchi
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_gass_set_eoa(H5FD_t *_file, haddr_t addr)
{
    H5FD_gass_t *file = (H5FD_gass_t *)_file;
    file->eoa = addr;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_gass_get_eof
 *
 * Purpose:     Returns the end-of-file marker, which is the greater of
 *              either the Unix end-of-file or the HDF5 end-of-address
 *              markers.
 *
 * Return:      Success:        End of file address, the first address past
 *                              the end of the "file", either the Unix file
 *                              or the HDF5 file.
 *
 *              Failure:        HADDR_UNDEF
 *
 * Programmer:  Saurabh Bagchi
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_gass_get_eof(H5FD_t *_file)
{
    H5FD_gass_t *file = (H5FD_gass_t*)_file;
    return MAX(file->eof, file->eoa);
}

/*-------------------------------------------------------------------------
 * Function:    H5FD_gass_read
 *
 * Purpose:     Reads SIZE bytes of data from FILE beginning at address ADDR
 *              into buffer BUF.
 *
 * Return:      Success:        Zero. Result is stored in caller-supplied
 *                              buffer BUF.
 *
 *              Failure:        -1, Contents of buffer BUF are undefined.
 *
 * Programmer:  Saurabh Bagchi
 *              Monday, August 16, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_gass_read(H5FD_t *_file, hid_t dxpl_id/*unused*/, haddr_t addr,
               hsize_t size, void *buf/*out*/)
{
    H5FD_gass_t         *file = (H5FD_gass_t*)_file;
    ssize_t             nbytes;

    assert(file && file->pub.cls);
    assert(buf);

    /* Check for overflow conditions */
    if (HADDR_UNDEF==addr) return -1;
    if (REGION_OVERFLOW(addr, size)) return -1;
    if (addr+size>file->eoa) return -1;

    /* Seek to the correct location */
    if ((addr!=file->pos || OP_READ!=file->op) &&
        file_seek(file->fd, (file_offset_t)addr, SEEK_SET)<0) {
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
        return -1;
    }

    /*
     * Read data, being careful of interrupted system calls, partial results,
     * and the end of the file.
     */
    while (size>0) {
do nbytes = read(file->fd, buf, size);
        while (-1==nbytes && EINTR==errno);
        if (-1==nbytes) {
            /* error */
            file->pos = HADDR_UNDEF;
            file->op = OP_UNKNOWN;
            return -1;
        }
        if (0==nbytes) {
            /* end of file but not end of format address space */
            memset(buf, 0, size);
            size = 0;
        }
        assert(nbytes>=0);
        assert((hsize_t)nbytes<=size);
        size -= (hsize_t)nbytes;
        addr += (haddr_t)nbytes;
        buf = (char*)buf + nbytes;
    }

    /* Update current position */
    file->pos = addr;
    file->op = OP_READ;
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD_gass_write
 *
 * Purpose:     Writes SIZE bytes of data to FILE beginning at address ADDR
 *              from buffer BUF.
 *
 * Return:      Success:        Zero
 *
 *              Failure:        -1
 *
 * Programmer:  Saurabh Bagchi
 *              Tuesday, August 17, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_gass_write(H5FD_t *_file, hid_t dxpl_id/*unused*/, haddr_t addr,
                hsize_t size, const void *buf)
{
    H5FD_gass_t         *file = (H5FD_gass_t*)_file;
    ssize_t             nbytes;

    assert(file && file->pub.cls);
    assert(buf);

    /* Check for overflow conditions */
    if (HADDR_UNDEF==addr) return -1;
    if (REGION_OVERFLOW(addr, size)) return -1;
    if (addr+size>file->eoa) return -1;

    /* Seek to the correct location */
    if ((addr!=file->pos || OP_WRITE!=file->op) &&
        file_seek(file->fd, (file_offset_t)addr, SEEK_SET)<0) {
        file->pos = HADDR_UNDEF;
        file->op = OP_UNKNOWN;
        return -1;
    }

    /*
     * Write the data, being careful of interrupted system calls and partial
     * results
     */
    while (size>0) {
        do nbytes = write(file->fd, buf, size);
        while (-1==nbytes && EINTR==errno);
if (-1==nbytes) {
            /* error */
            file->pos = HADDR_UNDEF;
            file->op = OP_UNKNOWN;
            return -1;
        }
        assert(nbytes>0);
        assert((hsize_t)nbytes<=size);
        size -= (hsize_t)nbytes;
        addr += (haddr_t)nbytes;
        buf = (const char*)buf + nbytes;
    }

    /* Update current position and eof */
    file->pos = addr;
    file->op = OP_WRITE;
    if (file->pos>file->eof) file->eof = file->pos;

    return 0;
}

#endif
