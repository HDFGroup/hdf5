/*
 * Copyright © 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 29, 1999
 *
 * Purpose:	The POSIX unbuffered file driver using only the HDF5 public
 *		API and with a few optimizations: the lseek() call is made
 *		only when the current file position is unknown or needs to be
 *		changed based on previous I/O through this driver (don't mix
 *		I/O from this driver with I/O from other parts of the
 *		application to the same file).
 */
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <hdf5.h>
#include <stdlib.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef WIN32
#include <windows.h>
#endif



#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))

/* The driver identification number, initialized at runtime */
static hid_t H5FD_SEC2_g = 0;

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
typedef struct H5FD_sec2_t {
    H5FD_t	pub;			/*public stuff, must be first	*/
    int		fd;			/*the unix file			*/
    haddr_t	eoa;			/*end of allocated region	*/
    haddr_t	eof;			/*end of file; current file size*/
    haddr_t	pos;			/*current file I/O position	*/
    int		op;			/*last operation		*/
#ifndef WIN32
    /*
     * On most systems the combination of device and i-node number uniquely
     * identify a file.
     */
    dev_t	device;			/*file device number		*/
    ino_t	inode;			/*file i-node number		*/
#else
    /*
     * On WIN32 the low-order word of a unique identifier associated with the
     * file and the volume serial number uniquely identify a file. This number
     * (which, both? -rpm) may change when the system is restarted or when the
     * file is opened. After a process opens a file, the identifier is
     * constant until the file is closed. An application can use this
     * identifier and the volume serial number to determine whether two
     * handles refer to the same file.
     */
    int fileindexlo;
    int fileindexhi;
#endif
} H5FD_sec2_t;

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
static H5FD_t *H5FD_sec2_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_sec2_close(H5FD_t *_file);
static int H5FD_sec2_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static haddr_t H5FD_sec2_get_eoa(H5FD_t *_file);
static herr_t H5FD_sec2_set_eoa(H5FD_t *_file, haddr_t addr);
static haddr_t H5FD_sec2_get_eof(H5FD_t *_file);
static herr_t H5FD_sec2_read(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			     hsize_t size, void *buf);
static herr_t H5FD_sec2_write(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			      hsize_t size, const void *buf);
static herr_t H5FD_sec2_flush(H5FD_t *_file);

static const H5FD_class_t H5FD_sec2_g = {
    "sec2",					/*name			*/
    MAXADDR,					/*maxaddr		*/
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    0, 						/*fapl_size		*/
    NULL,					/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    0,						/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_sec2_open,				/*open			*/
    H5FD_sec2_close,				/*close			*/
    H5FD_sec2_cmp,				/*cmp			*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_sec2_get_eoa,				/*get_eoa		*/
    H5FD_sec2_set_eoa, 				/*set_eoa		*/
    H5FD_sec2_get_eof,				/*get_eof		*/
    H5FD_sec2_read,				/*read			*/
    H5FD_sec2_write,				/*write			*/
    H5FD_sec2_flush,				/*flush			*/
    H5FD_FLMAP_SINGLE,				/*fl_map		*/
};


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the sec2 driver.
 *
 *		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_sec2_init(void)
{
    if (H5I_VFL!=H5Iget_type(H5FD_SEC2_g)) {
	H5FD_SEC2_g = H5FDregister(&H5FD_sec2_g);
    }
    return H5FD_SEC2_g;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_sec2
 *
 * Purpose:	Modify the file access property list to use the H5FD_SEC2
 *		driver defined in this source file.  There are no driver
 *		specific properties.
 *		
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_sec2(hid_t fapl_id)
{
    /*NO TRACE*/
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    return H5Pset_driver(fapl_id, H5FD_SEC2, NULL);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_open
 *
 * Purpose:	Create and/or opens a Unix file as an HDF5 file.
 *
 * Return:	Success:	A pointer to a new file data structure. The
 *				public fields will be initialized by the
 *				caller, which is always H5FD_open().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_sec2_open(const char *name, unsigned flags, hid_t fapl_id/*unused*/,
	       haddr_t maxaddr)
{
    unsigned	o_flags;
    int		fd;
    struct stat	sb;
    H5FD_sec2_t	*file=NULL;
#ifdef WIN32
	HFILE filehandle;
	struct _BY_HANDLE_FILE_INFORMATION fileinfo;
	int results;   
#endif
    /* Check arguments */
    if (!name || !*name) return NULL;
    if (0==maxaddr || HADDR_UNDEF==maxaddr) return NULL;
    if (ADDR_OVERFLOW(maxaddr)) return NULL;

    /* Build the open flags */
    o_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;
    if (H5F_ACC_TRUNC & flags) o_flags |= O_TRUNC;
    if (H5F_ACC_CREAT & flags) o_flags |= O_CREAT;
    if (H5F_ACC_EXCL & flags) o_flags |= O_EXCL;
#ifdef WIN32
	/*this is included since we aren't using HDopen which would
	 normally include this flag for us*/
	o_flags |= O_BINARY;
#endif
    /* Open the file */

    if ((fd=open(name, o_flags, 0666))<0) return NULL;
    if (fstat(fd, &sb)<0) {
	close(fd);
	return NULL;
    }

    /* Create the new file struct */
    file = calloc(1, sizeof(H5FD_sec2_t));
    file->fd = fd;
    file->eof = sb.st_size;
    file->pos = HADDR_UNDEF;
    file->op = OP_UNKNOWN;
#ifdef WIN32
	filehandle = _get_osfhandle(fd);
	results = GetFileInformationByHandle(filehandle, &fileinfo);
	file->fileindexhi = fileinfo.nFileIndexHigh;
	file->fileindexlo = fileinfo.nFileIndexLow;
#else
    file->device = sb.st_dev;
    file->inode = sb.st_ino;
#endif
    return (H5FD_t*)file;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_close
 *
 * Purpose:	Closes a Unix file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1, file not closed.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_close(H5FD_t *_file)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t*)_file;

    if (H5FD_sec2_flush(_file)<0) return -1;
    if (close(file->fd)<0) return -1;
    free(file);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_cmp
 *
 * Purpose:	Compares two files belonging to this driver using an
 *		arbitrary (but consistent) ordering.
 *
 * Return:	Success:	A value like strcmp()
 *
 *		Failure:	never fails (arguments were checked by the
 *				caller).
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5FD_sec2_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_sec2_t	*f1 = (const H5FD_sec2_t*)_f1;
    const H5FD_sec2_t	*f2 = (const H5FD_sec2_t*)_f2;
#ifdef WIN32
    if (f1->fileindexhi < f2->fileindexhi) return -1;
    if (f1->fileindexhi > f2->fileindexhi) return 1;

    if (f1->fileindexlo < f2->fileindexlo) return -1;
    if (f1->fileindexlo > f2->fileindexlo) return 1;

#else
    if (f1->device < f2->device) return -1;
    if (f1->device > f2->device) return 1;

    if (f1->inode < f2->inode) return -1;
    if (f1->inode > f2->inode) return 1;
#endif
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The EOA marker
 *		is the first address past the last byte allocated in the
 *		format address space.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Monday, August  2, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_sec2_get_eoa(H5FD_t *_file)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t*)_file;
    return file->eoa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file. This function is
 *		called shortly after an existing HDF5 file is opened in order
 *		to tell the driver where the end of the HDF5 data is located.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_set_eoa(H5FD_t *_file, haddr_t addr)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t*)_file;
    file->eoa = addr;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_get_eof
 *
 * Purpose:	Returns the end-of-file marker, which is the greater of
 *		either the Unix end-of-file or the HDF5 end-of-address
 *		markers.
 *
 * Return:	Success:	End of file address, the first address past
 *				the end of the "file", either the Unix file
 *				or the HDF5 file.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_sec2_get_eof(H5FD_t *_file)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t*)_file;
    return MAX(file->eof, file->eoa);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_read
 *
 * Purpose:	Reads SIZE bytes of data from FILE beginning at address ADDR
 *		into buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *
 *		Failure:	-1, Contents of buffer BUF are undefined.
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_read(H5FD_t *_file, hid_t dxpl_id/*unused*/, haddr_t addr,
	       hsize_t size, void *buf/*out*/)
{
    H5FD_sec2_t		*file = (H5FD_sec2_t*)_file;
    ssize_t		nbytes;
    
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
 * Function:	H5FD_sec2_write
 *
 * Purpose:	Writes SIZE bytes of data to FILE beginning at address ADDR
 *		from buffer BUF according to data transfer properties in
 *		DXPL_ID.
 *
 * Return:	Success:	Zero
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_write(H5FD_t *_file, hid_t dxpl_id/*unused*/, haddr_t addr,
		hsize_t size, const void *buf)
{
    H5FD_sec2_t		*file = (H5FD_sec2_t*)_file;
    ssize_t		nbytes;
    
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


/*-------------------------------------------------------------------------
 * Function:	H5FD_sec2_flush
 *
 * Purpose:	Makes sure that the true file size is the same (or larger)
 *		than the end-of-address.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_sec2_flush(H5FD_t *_file)
{
    H5FD_sec2_t	*file = (H5FD_sec2_t*)_file;

    if (file->eoa>file->eof) {
	if (-1==file_seek(file->fd, file->eoa-1, SEEK_SET)) return -1;
	if (write(file->fd, "", 1)!=1) return -1;
	file->eof = file->eoa;
	file->pos = file->eoa;
	file->op = OP_WRITE;
    }

    return 0;
}
