/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@viper.llnl.gov>
 *	       Wednesday, October 22, 1997
 *
 * Purpose:	This file contains virtual functions for the H5F_low
 *		class.	These are functions that operate on various kinds
 *		of files at a level where the file is just a one-dimensional
 *		array of bytes.
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#define addr_defined(X) (((uint64_t)(-1)!=(X)->offset) ? TRUE : FALSE)

#define PABLO_MASK	H5Flow_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT NULL


/*-------------------------------------------------------------------------
 * Function:	H5F_low_class
 *
 * Purpose:	Given a driver identifier return the class pointer for that
 *		low-level driver.
 *
 * Return:	Success:	A low-level driver class pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, February 18, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const H5F_low_class_t *
H5F_low_class (H5F_driver_t driver)
{
    const H5F_low_class_t	*type = NULL;
    
    FUNC_ENTER (H5F_low_class, NULL);

    switch (driver) {
    case H5F_LOW_STDIO:
	type = H5F_LOW_STDIO_g;
	break;

    case H5F_LOW_SEC2:
	type = H5F_LOW_SEC2_g;
	break;

    case H5F_LOW_CORE:
	type = H5F_LOW_CORE_g;
	break;

#ifdef HAVE_PARALLEL
    case H5F_LOW_MPIO:
	type = H5F_LOW_MPIO_g;
	break;
#endif

    case H5F_LOW_SPLIT:
	type = H5F_LOW_SPLIT_g;
	break;

    case H5F_LOW_FAMILY:
	type = H5F_LOW_FAMILY_g;
	break;

    default:
	HRETURN_ERROR (H5E_IO, H5E_UNSUPPORTED, NULL,
		       "unknown low-level driver");
    }

    FUNC_LEAVE (type);
}
	

/*-------------------------------------------------------------------------
 * Function:	H5F_low_open
 *
 * Purpose:	Opens a file of type TYPE with name NAME according to the
 *		field of bit flags FLAGS which are:
 *		
 *		H5F_ACC_WRITE:	The file is open for read/write access.
 *				Without this bit set, the file would be open
 *				for read-only access.
 *
 *		H5F_ACC_CREAT:	The file is created if it doesn't already
 *				exist.	On unix, the file permissions are set
 *				to 0666 modified by the umask.
 *
 *		H5F_ACC_EXCL:	This function will fail if the file already
 *				exists.
 *
 *		H5F_ACC_TRUNC:	Truncate the file to a zero-length file as it
 *				is opened.  This allows existing files to be
 *				overwritten.
 *
 *		The KEY argument is initialized with data which is unique to
 *		this file.  Opening the same file (even by a different name)
 *		should return the same key.
 *
 *		This is a virtual function only; the actual open operation is
 *		performed by the subclass.  This function will fail if the
 *		subclass hasn't defined an open method.
 *
 * Errors:
 *		IO	  CANTOPENFILE	Open failed. 
 *
 * Return:	Success:	Pointer to the new file descriptor.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_low_t *
H5F_low_open(const H5F_low_class_t *type, const char *name,
	     const H5F_access_t *access_parms, uintn flags,
	     H5F_search_t *key/*out*/)
{
    H5F_low_t		   *lf = NULL;

    FUNC_ENTER(H5F_low_open, NULL);

    assert(type && type->open);
    assert(name && *name);

    if (NULL == (lf = (type->open) (name, access_parms, flags, key))) {
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "open failed");
    }
    lf->type = type;

    FUNC_LEAVE(lf);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_low_close
 *
 * Purpose:	Closes a low-level file. The subclass should free all
 *		resources used by the file descriptor but should not free the
 *		file descriptor itself.	 The close method in the subclass is
 *		optional; lack of a close method results in only the file
 *		descriptor being freed.
 *
 *		It is safe to call this function with a null pointer for the
 *		file descriptor.  This function returns a null pointer that
 *		the caller can assign to the file descriptor pointer as it's
 *		closed like `desc=H5F_low_close(desc)'.
 *
 * Errors:
 *		IO	  CLOSEERROR	Close failed. 
 *
 * Return:	Success:	NULL
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_low_t *
H5F_low_close(H5F_low_t *lf, const H5F_access_t *access_parms)
{
    FUNC_ENTER(H5F_low_close, NULL);

    if (lf) {
	if ((lf->type->close) (lf, access_parms) < 0) {
	    H5MM_xfree(lf);
	    HRETURN_ERROR(H5E_IO, H5E_CLOSEERROR, NULL, "close failed");
	}
	H5MM_xfree(lf);
    }
    FUNC_LEAVE(NULL);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_low_read
 *
 * Purpose:	Reads SIZE bytes of data beginning at address ADDR of the
 *		file LF and puts the result in BUF.  Behavior when reading
 *		past the logical or physical end of file is to return zeros
 *		for that part of the request.
 *
 *		This is only a virtual function; the subclass must define a
 *		read method or this function will fail.
 *
 * Errors:
 *		IO	  READERROR	Read failed. 
 *		IO	  UNSUPPORTED	No read method. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *		June 2, 1998	Albert Cheng
 *		Added xfer_mode argument
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_read(H5F_low_t *lf, const H5F_access_t *access_parms,
	     const H5D_transfer_t xfer_mode,
	     const haddr_t *addr, size_t size, uint8_t *buf/*out*/)
{
    herr_t		    ret_value = FAIL;

    FUNC_ENTER(H5F_low_read, FAIL);

    assert(lf && lf->type);
    assert(addr && addr_defined(addr));
    assert(buf);

    if (lf->type->read) {
	if ((ret_value = (lf->type->read) (lf, access_parms, xfer_mode,
					   addr, size, buf)) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_READERROR, ret_value, "read failed");
	}
    } else {
	HRETURN_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "no read method");
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_low_write
 *
 * Purpose:	Writes SIZE bytes of data from BUF into the file LF beginning
 *		at address ADDR of the file. Writing past the logical or
 *		physical end of file causes the file to be extended.
 *
 *		This is a virtual function only; if the subclass doesn't
 *		define a write method then this function will fail.
 *
 * Errors:
 *		IO	  UNSUPPORTED	No write method. 
 *		IO	  WRITEERROR	Write failed. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *		June 2, 1998	Albert Cheng
 *		Added xfer_mode argument
 *
 *		rky 980816
 *		Accommodate fancy MPI derived datatype writes.
 *
 *		rky 980902
 *		For non-block parallel writes, don't change value of lf->eof.
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_write(H5F_low_t *lf, const H5F_access_t *access_parms,
	      const H5D_transfer_t xfer_mode,
	      const haddr_t *addr, size_t size, const uint8_t *buf)
{
    herr_t		ret_value = FAIL;
    haddr_t		tmp_addr;

    FUNC_ENTER(H5F_low_write, FAIL);

    assert(lf && lf->type);
    assert(addr && addr_defined(addr));
    assert(buf);

    /* check for writing past the end of file marker */
#ifdef HAVE_PARALLEL
    if (H5F_LOW_MPIO==access_parms->driver &&
	access_parms->u.mpio.use_types) {
	/* rky 090902 KLUGE
	 * In the case of fancy use of MPI datatypes, the addr and size
	 * parameters have a very peculiar interpretation.
	 * It is logically possible, but quite complex, to calculate
	 * the physical offset that the last byte to be written will have
	 * (assuming the write doesn't fail partway thru, which it may).
	 * I don't yet fully understand the relationship between
	 * the lf->eof processor-local variable and the file's true eof.
	 * But presumably lf->eof has the correct value at this point,
	 * and we should _not_ change it,
	 * even if the file's true eof differs from the value of lf->eof.
	 * So for now we DO NOTHING!
	 * (Eventually, perhaps we should at least calculate the address
	 * of the last byte of this write, and compare it to lf->eof.) */
    } else {
#endif /* HAVE_PARALLEL */
	/* writing a simple block of bytes; can check for writing beyond eof */
	tmp_addr = *addr;
	H5F_addr_inc(&tmp_addr, (hsize_t)size);
	if (H5F_addr_gt(&tmp_addr, &(lf->eof))) {
#ifdef H5F_DEBUG
	    if (H5DEBUG(F)) {
		fprintf(H5DEBUG(F), "H5F: extending file w/o allocation\n");
	    }
#endif
	    lf->eof = tmp_addr;
	}
#ifdef HAVE_PARALLEL
    } /* end else */
#endif /* HAVE_PARALLEL */
    
    /* Write the data */
    if (lf->type->write) {
	if ((ret_value = (lf->type->write) (lf, access_parms, xfer_mode,
					    addr, size, buf)) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, ret_value, "write failed");
	}
    } else {
	HRETURN_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "no write method");
    }

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_flush
 *
 * Purpose:	Flushes file buffers to disk.  For instance, the stdio.h
 *		driver would call fflush().  Flushing also insures that the
 *		file exists to the current logical EOF (the library maintains
 *		a notion of EOF which is independent of the physical EOF) by
 *		reading and writing the last byte.  On some systems, this
 *		allocates a single block at the end of the file while on
 *		other systems it allocates all blocks up to the end of the
 *		file.  Extending the physical file is necessary because
 *		H5F_open() checks for truncated files.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, November 10, 1997
 *
 * Modifications:
 *              rky 980828 Only p0 writes metadata to disk.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_flush(H5F_low_t *lf, const H5F_access_t *access_parms)
{
    haddr_t		    last_byte;
    uint8_t		    buf[1];

    FUNC_ENTER(H5F_low_flush, FAIL);

    assert(lf && lf->type);

    /* Make sure the last block of the file has been allocated on disk */
    /* rky 980828 NOTE
     * Is this really necessary? Could this be eliminated for MPI-IO files? */
    H5F_addr_reset(&last_byte);
    if (addr_defined(&(lf->eof)) && H5F_addr_gt(&(lf->eof), &last_byte)) {
	last_byte = lf->eof;
	last_byte.offset -= 1;
	if (H5F_low_read(lf, access_parms, H5D_XFER_DFLT, &last_byte,
			 1, buf) >= 0) {
#ifdef HAVE_PARALLEL
	    H5F_mpio_tas_allsame( lf, TRUE );	/* only p0 will write */
#endif /* HAVE_PARALLEL */
	    H5F_low_write(lf, access_parms, H5D_XFER_DFLT, &last_byte, 1, buf);
	}
    }
    /* Invoke the subclass the flush method */
    if (lf->type->flush) {
	if ((lf->type->flush) (lf, access_parms) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL,
			  "low level flush failed");
	}
    }
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_size
 *
 * Purpose:	Returns the current logical size of the file in bytes.	This
 *		may differ from the physical size of the file (most
 *		subclasses extend the physical file size during the write
 *		operation instead of the alloc operation).
 *		
 *		The next absolute file address is returned through the
 *		EOF argument.  This is the address of the logical end of
 *		file (that is, the address of the first byte past the last
 *		byte which is logically in the file).
 *
 * Warning:	The return value type may not be large enough to represent
 *		the true size of the file.  In such cases, the maximum
 *		possible size is returned.  It is better to look at the EOF
 *		output argument to determine the total size.
 *
 * Errors:
 *		IO	  UNSUPPORTED	No size method. 
 *
 * Return:	Success:	Current size of file
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5F_low_size(H5F_low_t *lf, haddr_t *eof/*out*/)
{
    hsize_t	size = (hsize_t)(-1);	      /*max possible size */

    FUNC_ENTER(H5F_low_size, 0);

    assert(lf && lf->type);
    assert(eof);

    *eof = lf->eof;
    if (eof->offset < size) size = eof->offset;

    FUNC_LEAVE(size);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_access
 *
 * Purpose:	Determines if a file can be accessed in a particular way by a
 *		particular subclass.  The access modes for a file are the
 *		same as those of access(2), namely
 *
 *		F_OK:	determines if the file (or all parts of a multi-part
 *			file) exists.
 *
 *		R_OK:	determines if the file (or all parts of a multi-part
 *			file) are readable.
 *
 *		W_OK:	determines if the file (or all parts of a multi-part
 *			file) are writable.
 *
 *		If a subclass doesn't define an access method, then we treat
 *		the name as if it were a local Unix file and test
 *		accessibility with the access(2) function.  The KEY is
 *		returned as a device number and i-node pair.
 *
 * Return:	Success:	TRUE or FALSE.	If TRUE, then KEY is
 *				initialized with data that makes this file
 *				unique (same value as H5F_low_open).
 *
 *		Failure:	FAIL, KEY is undefined.
 *
 * Programmer:	Robb Matzke
 *		Friday, October 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5F_low_access(const H5F_low_class_t *type, const char *name,
	       const H5F_access_t *access_parms, int mode,
	       H5F_search_t *key/*out*/)
{
    htri_t		ret_value;
    struct stat		sb;
	
    FUNC_ENTER(H5F_low_size, FAIL);
    assert(type);

    if (type->access) {
	ret_value = (type->access) (name, access_parms, mode, key /*out*/);
    } else {
	ret_value = (0 == HDaccess(name, mode) ? TRUE : FALSE);
	if (key) {

#ifdef WIN32
	    /*
	     * Some windows specific types. the LPSTR is just a char*  
	     */
	    LPSTR pathbuf = NULL; 
	    LPSTR *namebuf = NULL; 
	    int bufsize = 0;


	    /*
	     * Gets the full path of the file name.  the if statement below
	     * is to try to distinguish if we have the ablosute path already
	     */
	    if ((*(name+1) != ':') && (*(name+2)!= '\\')){ 
		/*
		 * if the size of the buffer is too small it will return
		 * the appropriate size of the buffer not including the null
		 */
		bufsize = GetFullPathName(name,bufsize,pathbuf,namebuf);
		if (bufsize != 0){
		    pathbuf = malloc(sizeof(char) * (bufsize + 1));
		    namebuf = malloc(sizeof(char) * (bufsize + 1));
		    bufsize++;
		    GetFullPathName(name,bufsize,pathbuf,namebuf); 
		} else {
		    pathbuf = NULL;
		}
	    } else {
		pathbuf = malloc(strlen(name));
		strcpy(pathbuf,name);
	    }

	    key->path = pathbuf;
	    key->dev = 0;
	    key->ino = 0;
#else
	    HDstat(name, &sb);
	    key->path = NULL;
	    key->dev = sb.st_dev;
	    key->ino = sb.st_ino;
#endif
	}
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_low_extend
 *
 * Purpose:	Increases the logical size of a file by moving the logical
 *		end of file marker.  A subclass can override this function by
 *		providing its own allocation method.
 *
 * Return:	Success:	Non-negative.  The address of the old
 *				end-of-file is returned through the ADDR
 *				argument and the logical size of the file has
 *				been extended by SIZE bytes.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *		Thursday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_extend(H5F_low_t *lf, const H5F_access_t *access_parms, intn op,
	       hsize_t size, haddr_t *addr/*out*/)
{
    FUNC_ENTER(H5F_low_alloc, FAIL);

    assert(lf);
    assert(size > 0);
    assert(addr);

    if (lf->type->extend) {
	if ((lf->type->extend) (lf, access_parms, op, size, addr/*out*/) < 0) {
	    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			  "unable to extend file");
	}
    } else {
	*addr = lf->eof;
	H5F_addr_inc(&(lf->eof), size);
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_alloc
 *
 * Purpose:	Determines if a free block BLK can satisfy a request for SIZE
 *		bytes of memory from file F.  If SIZE >= THRESH then the
 *		memory must be aligned on an ALIGN-byte boundary.  Alignment
 *		is wrt the relative file addresses (that is, the size of the
 *		user-defined block at the beginning of the file is subtracted
 *		from the addresses before aligning them).
 *
 * Return:	Success:	Positive if the free block exactly satisfies
 *				the request; zero if the free block
 *				over-satisfies the request.  In either case,
 *				ADDR will be the address within the free
 *				block where the request can be satisfied.
 *
 *		Failure:	Negative with the output value of ADDR undefined.
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5F_low_alloc (H5F_low_t *lf, intn op, hsize_t alignment, hsize_t threshold,
	       hsize_t size, H5MF_free_t *blk, haddr_t *addr/*out*/)
{
    intn	ret_value = FAIL;
    hsize_t	wasted;
    
    FUNC_ENTER (H5F_low_alloc, FAIL);
    assert (lf);
    assert (alignment>0);
    assert (size>0);
    assert (blk);
    assert (addr);

    if (lf->type->alloc) {
	ret_value = (lf->type->alloc)(lf, op, alignment, threshold, size, blk,
				      addr/*out*/);
    } else {
	if (size>=threshold) {
	    wasted = blk->addr.offset % alignment;
	} else {
	    wasted = 0;
	}
	if (0==wasted && size==blk->size) {
	    /* exact match */
	    *addr = blk->addr;
	    ret_value = 1;
	} else if (blk->size>wasted && blk->size-wasted>=size) {
	    /* over-satisfied */
	    *addr = blk->addr;
	    H5F_addr_inc (addr, wasted);
	    ret_value = 0;
	}
    }
    
    FUNC_LEAVE (ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_low_seteof
 *
 * Purpose:	Sets the logical end-of-file to the specified address.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_low_seteof(H5F_low_t *lf, const haddr_t *addr)
{
    FUNC_ENTER(H5F_low_seteof, FAIL);

    assert(lf);
    assert(addr && addr_defined(addr));

    lf->eof = *addr;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_cmp
 *
 * Purpose:	Compares two addresses.
 *
 * Return:	Success:	<0 if A1<A2
 *				=0 if A1=A2
 *				>0 if A1>A2
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
intn
H5F_addr_cmp(const haddr_t *a1, const haddr_t *a2)
{
    FUNC_ENTER(H5F_addr_cmp, FAIL);

    assert(a1 && addr_defined(a1));
    assert(a2 && addr_defined(a2));

    if (a1->offset < a2->offset)
	HRETURN(-1);
    if (a1->offset > a2->offset)
	HRETURN(1);

    FUNC_LEAVE(0);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_undef
 *
 * Purpose:	Cause an address to become undefined.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_undef(haddr_t *addr/*out*/)
{
    assert(addr);

    addr->offset = (uint64_t)(-1);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_defined
 *
 * Purpose:	Determines if an address has a defined value.
 *
 * Return:	Success:	TRUE or FALSE
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
htri_t
H5F_addr_defined(const haddr_t *addr)
{
    FUNC_ENTER(H5F_addr_defined, FAIL);
    FUNC_LEAVE(addr_defined(addr));
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_reset
 *
 * Purpose:	Reset the address to zero.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_reset(haddr_t *addr/*out*/)
{
    assert(addr);
    addr->offset = 0;
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_zerop
 *
 * Purpose:	Determines if an address is zero.
 *
 * Return:	Success:	TRUE or FALSE
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
htri_t
H5F_addr_zerop(const haddr_t *addr)
{
    FUNC_ENTER(H5F_addr_zerop, FAIL);
    FUNC_LEAVE(0 == addr->offset ? TRUE : FALSE);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_encode
 *
 * Purpose:	Encodes an address into the buffer pointed to by *PP and
 *		then increments the pointer to the first byte after the
 *		address.  An undefined value is stored as all 1's.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_encode(H5F_t *f, uint8_t **pp, const haddr_t *addr)
{
    uintn		    i;
    haddr_t		    tmp;

    assert(f);
    assert(pp && *pp);
    assert(addr);

    if (addr_defined(addr)) {
	tmp = *addr;
	for (i=0; i<H5F_SIZEOF_ADDR(f); i++) {
	    *(*pp)++ = (uint8_t)(tmp.offset & 0xff);
	    tmp.offset >>= 8;
	}
	assert("overflow" && 0 == tmp.offset);

    } else {
	for (i=0; i<H5F_SIZEOF_ADDR(f); i++) {
	    *(*pp)++ = 0xff;
	}
    }
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_decode
 *
 * Purpose:	Decodes an address from the buffer pointed to by *PP and
 *		updates the pointer to point to the next byte after the
 *		address.
 *
 *		If the value read is all 1's then the address is returned
 *		with an undefined value.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_decode(H5F_t *f, const uint8_t **pp, haddr_t *addr/*out*/)
{
    uintn		    i;
    haddr_t		    tmp;
    uint8_t		    c;
    hbool_t		    all_zero = TRUE;

    assert(f);
    assert(pp && *pp);
    assert(addr);

    addr->offset = 0;

    for (i=0; i<H5F_SIZEOF_ADDR(f); i++) {
	c = *(*pp)++;
	if (c != 0xff) all_zero = FALSE;

	if (i<sizeof(addr->offset)) {
	    tmp.offset = c;
	    tmp.offset <<= i * 8;	/*use tmp to get casting right */
	    addr->offset |= tmp.offset;
	} else if (!all_zero) {
	    assert(0 == **pp);	/*overflow */
	}
    }
    if (all_zero) H5F_addr_undef(addr);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_print
 *
 * Purpose:	Print an address
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_print(FILE *stream, const haddr_t *addr)
{
    assert(stream);
    assert(addr);

    HDfprintf(stream, "%a", addr);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_pow2
 *
 * Purpose:	Returns an address which is a power of two.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_pow2(uintn n, haddr_t *addr /*out */ )
{
    assert(addr);
    assert(n < 8 * sizeof(addr->offset));

    addr->offset = 1;
    addr->offset <<= n;
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_inc
 *
 * Purpose:	Increments an address by some number of bytes.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_inc(haddr_t *addr/*in,out*/, hsize_t inc)
{
    assert(addr && addr_defined(addr));
    assert(addr->offset <= addr->offset + inc);
    
    addr->offset += inc;
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_adj
 *
 * Purpose:	Adjusts an address by adding or subtracting some amount.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Monday, April  6, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_adj(haddr_t *addr/*in,out */, hssize_t adj)
{
#ifndef NDEBUG
    assert(addr && addr_defined(addr));
    if (adj>=0) {
	assert(addr->offset <= addr->offset + adj);
    } else {
	assert (addr->offset > addr->offset + adj);
    }
#endif
    
    addr->offset += adj;
}


/*-------------------------------------------------------------------------
 * Function:	H5F_addr_add
 *
 * Purpose:	Adds two addresses and puts the result in the first argument.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5F_addr_add(haddr_t *a1 /*in,out */ , const haddr_t *a2)
{
    assert(a1 && addr_defined(a1));
    assert(a2 && addr_defined(a2));
    a1->offset += a2->offset;
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_hash
 *
 * Purpose:	Computes a hash value of an address between 0 and MOD-1,
 *		inclusive.
 *
 * Return:	Success:	The hash value
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		Friday, November  7, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
uintn
H5F_addr_hash(const haddr_t *addr, uintn mod)
{
    assert(addr && addr_defined(addr));
    assert(mod > 0);

    return (unsigned)(addr->offset % mod);  /*ignore file number */
}

/*-------------------------------------------------------------------------
 * Function:	H5F_addr_pack
 *
 * Purpose:	Converts a long[2] array (usually returned from H5G_get_objinfo)
 *      back into a haddr_t
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, October  23, 1998
 *
 * Modifications:
 *	Albert Cheng, Feb 18, 1999
 *	Changed objno to unsigned long type to be consistent with
 *      addr->offset and how it is being called.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_addr_pack(H5F_t UNUSED *f, haddr_t *addr, const unsigned long objno[2])
{
    assert(f);
    assert(objno);
    assert(addr);

    addr->offset = objno[0];
#if SIZEOF_LONG<SIZEOF_UINT64_T
    addr->offset |= ((uint64_t)objno[1]) << (8*sizeof(long));
#endif
    
    return(SUCCEED);
}
