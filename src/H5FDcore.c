/*
 * Copyright © 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, August 10, 1999
 *
 * Purpose:	A driver which stores the HDF5 data in main memory  using
 *		only the HDF5 public API. This driver is useful for fast
 *		access to small, temporary hdf5 files.
 */
#include <H5private.h>
#include <H5Fprivate.h>
#include <H5FDcore.h>
#include <H5Pprivate.h>

#undef MAX
#define MAX(X,Y)	((X)>(Y)?(X):(Y))

#undef MIN
#define MIN(X,Y)	((X)<(Y)?(X):(Y))

/* The driver identification number, initialized at runtime */
static hid_t H5FD_CORE_g = 0;

/*
 * The description of a file belonging to this driver. The `eoa' and `eof'
 * determine the amount of hdf5 address space in use and the high-water mark
 * of the file (the current size of the underlying memory).
 */
typedef struct H5FD_core_t {
    H5FD_t	pub;			/*public stuff, must be first	*/
    char	*name;			/*for equivalence testing	*/
    unsigned char *mem;			/*the underlying memory		*/
    haddr_t	eoa;			/*end of allocated region	*/
    haddr_t	eof;			/*current allocated size	*/
    size_t	increment;		/*multiples for mem allocation	*/
    int		fd;			/*backing store file descriptor	*/
    hbool_t	dirty;			/*changes not saved?		*/
} H5FD_core_t;

/* Driver-specific file access properties */
typedef struct H5FD_core_fapl_t {
    size_t	increment;		/*how much to grow memory	*/
    hbool_t	backing_store;		/*write to file name on flush	*/
} H5FD_core_fapl_t;

/* Allocate memory in multiples of this size by default */
#define H5FD_CORE_INCREMENT		8192

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
 *			which can be addressed entirely in memory.
 */
#define MAXADDR 		((haddr_t)~(size_t)0)
#define ADDR_OVERFLOW(A)	(HADDR_UNDEF==(A) ||			      \
				 ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z)	((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A,Z)	(ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) ||      \
                                 HADDR_UNDEF==(A)+(Z) ||		      \
				 (size_t)((A)+(Z))<(size_t)(A))

/* Prototypes */
static void *H5FD_core_fapl_get(H5FD_t *_file);
static H5FD_t *H5FD_core_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_core_flush(H5FD_t *_file);
static herr_t H5FD_core_close(H5FD_t *_file);
static int H5FD_core_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static haddr_t H5FD_core_get_eoa(H5FD_t *_file);
static herr_t H5FD_core_set_eoa(H5FD_t *_file, haddr_t addr);
static haddr_t H5FD_core_get_eof(H5FD_t *_file);
static herr_t H5FD_core_read(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			     hsize_t size, void *buf);
static herr_t H5FD_core_write(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			      hsize_t size, const void *buf);

static const H5FD_class_t H5FD_core_g = {
    "core",					/*name			*/
    MAXADDR,					/*maxaddr		*/
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    sizeof(H5FD_core_fapl_t),			/*fapl_size		*/
    H5FD_core_fapl_get,				/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    0,						/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_core_open,				/*open			*/
    H5FD_core_close,				/*close			*/
    H5FD_core_cmp,				/*cmp			*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_core_get_eoa,				/*get_eoa		*/
    H5FD_core_set_eoa, 				/*set_eoa		*/
    H5FD_core_get_eof,				/*get_eof		*/
    H5FD_core_read,				/*read			*/
    H5FD_core_write,				/*write			*/
    H5FD_core_flush,				/*flush			*/
    H5FD_FLMAP_SINGLE,				/*fl_map		*/
};


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the core driver.
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
H5FD_core_init(void)
{
    if (H5I_VFL!=H5Iget_type(H5FD_CORE_g)) {
	H5FD_CORE_g = H5FDregister(&H5FD_core_g);
    }
    return H5FD_CORE_g;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_core
 *
 * Purpose:	Modify the file access property list to use the H5FD_CORE
 *		driver defined in this source file.  The INCREMENT specifies
 *		how much to grow the memory each time we need more.
 *		
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 19, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-10-19
 *		Added the BACKING_STORE argument. If set then the entire file
 *		contents are flushed to a file with the same name as this
 *		core file.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_core(hid_t fapl_id, size_t increment, hbool_t backing_store)
{
    H5FD_core_fapl_t	fa;

    /* NO TRACE */
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    fa.increment = increment;
    fa.backing_store = backing_store;
    return H5Pset_driver(fapl_id, H5FD_CORE, &fa);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_core
 *
 * Purpose:	Queries properties set by the H5Pset_fapl_core() function.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, August 10, 1999
 *
 * Modifications:
 *		Robb Matzke, 1999-10-19
 *		Added the BACKING_STORE argument.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_core(hid_t fapl_id, size_t *increment/*out*/,
		 hbool_t *backing_store/*out*/)
{
    H5FD_core_fapl_t	*fa;

    /* NO TRACE */
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (H5FD_CORE!=H5Pget_driver(fapl_id)) return -1;
    if (NULL==(fa=H5Pget_driver_info(fapl_id))) return -1;
    
    if (increment) *increment = fa->increment;
    if (backing_store) *backing_store = fa->backing_store;
    
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_fapl_get
 *
 * Purpose:	Returns a copy of the file access properties.
 *
 * Return:	Success:	Ptr to new file access properties.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_core_fapl_get(H5FD_t *_file)
{
    H5FD_core_t		*file = (H5FD_core_t*)_file;
    H5FD_core_fapl_t	*fa = calloc(1, sizeof(H5FD_core_fapl_t));

    fa->increment = file->increment;
    fa->backing_store = (file->fd>=0);
    return fa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_open
 *
 * Purpose:	Create memory as an HDF5 file.
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
 *		Robb Matzke, 1999-10-19
 *		The backing store file is created and opened if specified.
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_core_open(const char *name, unsigned UNUSED flags, hid_t fapl_id,
	       haddr_t maxaddr)
{
    H5FD_core_t		*file=NULL;
    H5FD_core_fapl_t	*fa=NULL;
    int			fd=-1;
    
    /* Check arguments */
    if (0==maxaddr || HADDR_UNDEF==maxaddr) return NULL;
    if (ADDR_OVERFLOW(maxaddr)) return NULL;
    if (H5P_DEFAULT!=fapl_id) fa = H5Pget_driver_info(fapl_id);

    /* Open backing store */
    if (fa && fa->backing_store && name &&
	(fd=open(name, O_CREAT|O_TRUNC|O_RDWR, 0666))<0) {
	return NULL;
    }

    /* Create the new file struct */
    file = calloc(1, sizeof(H5FD_core_t));
    file->fd = fd;
    if (name && *name) {
	file->name = malloc(strlen(name)+1);
	strcpy(file->name, name);
    }

    /*
     * The increment comes from either the file access property list or the
     * default value. But if the file access property list was zero then use
     * the default value instead.
     */
    file->increment = (fa && fa->increment>0) ?
		      fa->increment : H5FD_CORE_INCREMENT;

    return (H5FD_t*)file;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_flush
 *
 * Purpose:	Flushes the file to backing store if there is any and if the
 *		dirty flag is set.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, October 15, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_core_flush(H5FD_t *_file)
{
    H5FD_core_t	*file = (H5FD_core_t*)_file;
    
    /* Write to backing store */
    if (file->dirty && file->fd>=0) {
	haddr_t size = file->eof;
	unsigned char *ptr = file->mem;
	if (0!=lseek(file->fd, 0, SEEK_SET)) return -1;

	while (size) {
	    ssize_t n = write(file->fd, ptr, size);
	    if (n<0 && EINTR==errno) continue;
	    if (n<0) return -1;
	    ptr += (size_t)n;
	    size -= (size_t)n;
	}
	file->dirty = FALSE;
    }
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_close
 *
 * Purpose:	Closes the file.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 29, 1999
 *
 * Modifications:
 * 		Robb Matzke, 1999-10-19
 *		The contents of memory are written to the backing store if
 *		one is open.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_core_close(H5FD_t *_file)
{
    H5FD_core_t	*file = (H5FD_core_t*)_file;

    /* Flush */
    if (H5FD_core_flush(_file)<0) return -1;

    /* Release resources */
    if (file->fd>=0) close(file->fd);
    if (file->name) free(file->name);
    if (file->mem) free(file->mem);
    memset(file, 0, sizeof(H5FD_core_t));
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_cmp
 *
 * Purpose:	Compares two files belonging to this driver by name. If one
 *		file doesn't have a name then it is less than the other file.
 *		If neither file has a name then the comparison is by file
 *		address.
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
H5FD_core_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_core_t	*f1 = (const H5FD_core_t*)_f1;
    const H5FD_core_t	*f2 = (const H5FD_core_t*)_f2;

    if (NULL==f1->name && NULL==f2->name) {
	if (f1<f2) return -1;
	if (f1>f2) return 1;
	return 0;
    }
    
    if (NULL==f1->name) return -1;
    if (NULL==f2->name) return 1;

    return strcmp(f1->name, f2->name);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_get_eoa
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
H5FD_core_get_eoa(H5FD_t *_file)
{
    H5FD_core_t	*file = (H5FD_core_t*)_file;
    return file->eoa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_set_eoa
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
H5FD_core_set_eoa(H5FD_t *_file, haddr_t addr)
{
    H5FD_core_t	*file = (H5FD_core_t*)_file;
    if (ADDR_OVERFLOW(addr)) return -1;
    file->eoa = addr;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_get_eof
 *
 * Purpose:	Returns the end-of-file marker, which is the greater of
 *		either the size of the underlying memory or the HDF5
 *		end-of-address markers.
 *
 * Return:	Success:	End of file address, the first address past
 *				the end of the "file", either the memory
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
H5FD_core_get_eof(H5FD_t *_file)
{
    H5FD_core_t	*file = (H5FD_core_t*)_file;
    return MAX(file->eof, file->eoa);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_read
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
H5FD_core_read(H5FD_t *_file, hid_t UNUSED dxpl_id, haddr_t addr,
	       hsize_t size, void *buf/*out*/)
{
    H5FD_core_t		*file = (H5FD_core_t*)_file;
    ssize_t		nbytes;
    
    assert(file && file->pub.cls);
    assert(buf);

    /* Check for overflow conditions */
    if (HADDR_UNDEF==addr) return -1;
    if (REGION_OVERFLOW(addr, size)) return -1;
    if (addr+size>file->eoa) return -1;

    /* Read the part which is before the EOF marker */
    if (addr<file->eof) {
	nbytes = MIN(size, file->eof-addr);
	memcpy(buf, file->mem+addr, nbytes);
	size -= nbytes;
	addr += nbytes;
	buf = (char*)buf + nbytes;
    }

    /* Read zeros for the part which is after the EOF markers */
    if (size>0) {
	memset(buf, 0, size);
    }
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_core_write
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
H5FD_core_write(H5FD_t *_file, hid_t UNUSED dxpl_id, haddr_t addr,
		hsize_t size, const void *buf)
{
    H5FD_core_t		*file = (H5FD_core_t*)_file;
    
    assert(file && file->pub.cls);
    assert(buf);

    /* Check for overflow conditions */
    if (REGION_OVERFLOW(addr, size)) return -1;
    if (addr+size>file->eoa) return -1;

    /*
     * Allocate more memory if necessary, careful of overflow. Also, if the
     * allocation fails then the file should remain in a usable state.  Be
     * careful of non-Posix realloc() that doesn't understand what to do when
     * the first argument is null.
     */
    if (addr+size>file->eof) {
	unsigned char *x;
	size_t new_eof = file->increment * ((addr+size)/file->increment);
	if ((addr+size) % file->increment) new_eof += file->increment;
	if (NULL==file->mem) x = malloc(new_eof);
	else x = realloc(file->mem, new_eof);
	if (!x) return -1;
	file->mem = x;
	file->eof = new_eof;
    }

    /* Write from BUF to memory */
    memcpy(file->mem+addr, buf, size);
    file->dirty = TRUE;
    return 0;
}
