/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Wednesday, October 22, 1997
 *
 * Purpose:    This is the Posix stdio.h I/O subclass of H5Flow.
 */
#include <H5private.h>
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#define PABLO_MASK	H5Fstdio_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT	NULL

static H5F_low_t *H5F_stdio_open(const char *name,
				 const H5F_access_t *access_parms, uintn flags,
				 H5F_search_t *key/*out*/);
static herr_t H5F_stdio_close(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_stdio_read(H5F_low_t *lf, const H5F_access_t *access_parms,
			     const H5D_transfer_t xfer_mode,
			     const haddr_t *addr, size_t size,
			     uint8_t *buf/*out*/);
static herr_t H5F_stdio_write(H5F_low_t *lf, const H5F_access_t *access_parms,
			      const H5D_transfer_t xfer_mode,
			      const haddr_t *addr, size_t size,
			      const uint8_t *buf);
static herr_t H5F_stdio_flush(H5F_low_t *lf, const H5F_access_t *access_parms);

const H5F_low_class_t H5F_LOW_STDIO_g[1] = {{
    NULL,			/* use default access(2) func		*/
    H5F_stdio_open,		/* open method				*/
    H5F_stdio_close,		/* close method				*/
    H5F_stdio_read,		/* read method				*/
    H5F_stdio_write,		/* write method				*/
    H5F_stdio_flush,		/* flush method				*/
    NULL,			/* extend method			*/
    NULL,			/* alloc method				*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_open
 *
 * Purpose:	Opens a file with name NAME.  The FLAGS are a bit field with
 *		the possible values defined in H5F_low_open().
 *
 * Bugs:	H5F_ACC_EXCL has a race condition.
 *
 * Errors:
 *		IO	  CANTOPENFILE	File doesn't exist and CREAT wasn't
 *					specified. 
 *		IO	  CANTOPENFILE	Fopen failed. 
 *		IO	  FILEEXISTS	File exists but CREAT and EXCL were
 *					specified. 
 *
 * Return:	Success:	Low-level file pointer
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
static H5F_low_t *
H5F_stdio_open(const char *name, const H5F_access_t __unused__ *access_parms,
	       uintn flags, H5F_search_t *key/*out*/)
{
    H5F_low_t		   *lf = NULL;
    FILE		   *f = NULL;
    struct stat		    sb;

    FUNC_ENTER(H5F_stdio_open, NULL);

    if (HDaccess(name, F_OK) < 0) {
	if ((flags & H5F_ACC_CREAT) && (flags & H5F_ACC_RDWR)) {
	    f = HDfopen(name, "wb+");
	} else {
	    HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL,
			  "file doesn't exist and CREAT wasn't specified");
	}

    } else if ((flags & H5F_ACC_CREAT) && (flags & H5F_ACC_EXCL)) {
	HRETURN_ERROR(H5E_IO, H5E_FILEEXISTS, NULL,
		      "file exists but CREAT and EXCL were specified");

    } else if (flags & H5F_ACC_RDWR) {
	if (flags & H5F_ACC_TRUNC)
	    f = HDfopen(name, "wb+");
	else
	    f = HDfopen(name, "rb+");

    } else {
	f = HDfopen(name, "rb");
    }
    if (!f)
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "fopen failed");

    /* Build the return value */
    if (NULL==(lf = H5MM_calloc(sizeof(H5F_low_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }
    lf->u.stdio.f = f;
    lf->u.stdio.op = H5F_OP_SEEK;
    lf->u.stdio.cur = 0;
    H5F_addr_reset(&(lf->eof));
    if (HDfseek(lf->u.stdio.f, 0, SEEK_END) < 0) {
	lf->u.stdio.op = H5F_OP_UNKNOWN;
    } else {
	hssize_t x = HDftell (lf->u.stdio.f);
	assert (x>=0);
	H5F_addr_inc(&(lf->eof), (hsize_t)x);
    }

    /* The unique key */
    if (key) {
	HDfstat(fileno(f), &sb);
	key->dev = sb.st_dev;
	key->ino = sb.st_ino;
    }
    FUNC_LEAVE(lf);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_close
 *
 * Purpose:	Closes a file.
 *
 * Errors:
 *		IO	  CLOSEERROR	Fclose failed. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_stdio_close(H5F_low_t *lf, const H5F_access_t __unused__ *access_parms)
{
    FUNC_ENTER(H5F_stdio_close, FAIL);

    if (HDfclose(lf->u.stdio.f) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "fclose failed");
    }
    lf->u.stdio.f = NULL;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_read
 *
 * Purpose:	Reads SIZE bytes beginning at address ADDR in file LF and
 *		places them in buffer BUF.  Reading past the logical or
 *		physical end of file returns zeros instead of failing.
 *
 * Errors:
 *		IO	  READERROR	Fread failed. 
 *		IO	  SEEKERROR	Fseek failed. 
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
static herr_t
H5F_stdio_read(H5F_low_t *lf, const H5F_access_t __unused__ *access_parms,
	       const H5D_transfer_t __unused__ xfer_mode,
	       const haddr_t *addr, size_t size, uint8_t *buf/*out*/)
{
    size_t		n;
    uint64_t		mask;
#ifdef HAVE_FSEEK64
    int64_t		offset;
#else
    long		offset;
#endif

    FUNC_ENTER(H5F_stdio_read, FAIL);

    /* Check for overflow */
    mask = (uint64_t)1 << (8*sizeof(offset)-1);
    if (addr->offset >= mask ||
	addr->offset + size < addr->offset ||
	addr->offset+size >= mask) {
	HRETURN_ERROR (H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
    }
#ifdef HAVE_FSEEK64
    offset = (int64_t)(addr->offset); /*checked for overflow*/
#else
    offset = (long)(addr->offset); /*checked for overflow*/
#endif

    /* Check easy cases */
    if (0 == size) HRETURN(SUCCEED);
    if ((uint64_t)offset >= lf->eof.offset) {
	HDmemset(buf, 0, size);
	HRETURN(SUCCEED);
    }

    /*
     * Seek to the correct file position.
     */
    if (!H5F_OPT_SEEK ||
	lf->u.stdio.op != H5F_OP_READ ||
	lf->u.stdio.cur != offset) {
#ifdef HAVE_FSEEK64
	if (fseek64 (lf->u.stdio.f, offset, SEEK_SET)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL, "fseek64 failed");
	}
#else
	if (HDfseek(lf->u.stdio.f, offset, SEEK_SET) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "fseek failed");
	}
#endif
	lf->u.stdio.cur = offset;
    }

    /*
     * Read zeros past the logical end of file (physical is handled below)
     */
    if ((size_t) offset + size > lf->eof.offset) {
	size_t nbytes = (size_t) offset + size - lf->eof.offset;
	HDmemset(buf + size - nbytes, 0, nbytes);
	size -= nbytes;
    }
    
    /*
     * Read the data.  Since we're reading single-byte values, a partial read
     * will advance the file position by N.  If N is negative or an error
     * occurs then the file position is undefined.
     */
    n = HDfread(buf, 1, size, lf->u.stdio.f);
    if (n <= 0 && HDferror(lf->u.stdio.f)) {
	lf->u.stdio.op = H5F_OP_UNKNOWN;
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "fread failed");
    } else if (n < size) {
	HDmemset(buf + n, 0, size - n);
    }
    
    /*
     * Update the file position data.
     */
    lf->u.stdio.op = H5F_OP_READ;
#ifdef HAVE_FSEEK64
    lf->u.stdio.cur = (int64_t)(offset+n); /*checked for overflow above*/
#else
    lf->u.stdio.cur = (off_t)(offset+n); /*checked for overflow above*/
#endif
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_write
 *
 * Purpose:	Writes SIZE bytes from the beginning of BUF into file LF at
 *		file address ADDR.
 *
 * Errors:
 *		IO	  SEEKERROR	Fseek failed. 
 *		IO	  WRITEERROR	Fwrite failed. 
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
static herr_t
H5F_stdio_write(H5F_low_t *lf, const H5F_access_t __unused__ *access_parms,
	        const H5D_transfer_t __unused__ xfer_mode,
		const haddr_t *addr, size_t size, const uint8_t *buf)
{
    uint64_t		mask;
#ifdef HAVE_FSEEK64
    int64_t		offset;
    uint64_t		n;
#else
    long		offset;
    size_t		n;
#endif

    FUNC_ENTER(H5F_stdio_write, FAIL);

    /* Check for overflow */
    mask = (uint64_t)1 << (8*sizeof(offset)-1);
    if (addr->offset >= mask ||
	addr->offset+size < addr->offset ||
	addr->offset+size >= mask) {
	HRETURN_ERROR (H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
    }
#ifdef HAVE_FSEEK64
    offset = (int64_t)(addr->offset); /*checked for overflow*/
    n = size; /*checked for overflow*/
#else
    offset = (long)(addr->offset); /*checked for overflow*/
    n = size; /*checked for overflow*/
#endif

    /*
     * Seek to the correct file position.
     */
    if (!H5F_OPT_SEEK ||
	lf->u.stdio.op != H5F_OP_WRITE ||
	lf->u.stdio.cur != offset) {
#ifdef HAVE_FSEEK64
	if (fseek64 (lf->u.stdio.f, offset, SEEK_SET)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL, "fseek64 failed");
	}
#else
	if (HDfseek(lf->u.stdio.f, offset, SEEK_SET) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "fseek failed");
	}
#endif
	lf->u.stdio.cur = offset;
    }
    
    /*
     * Write the buffer.  On successful return, the file position will be
     * advanced by the number of bytes read.  Otherwise nobody knows where it
     * is.
     */
    if (n != HDfwrite(buf, 1, size, lf->u.stdio.f)) {
	lf->u.stdio.op = H5F_OP_UNKNOWN;
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "fwrite failed");
    }
    
    /*
     * Update seek optimizing data.
     */
    lf->u.stdio.op = H5F_OP_WRITE;
    lf->u.stdio.cur = offset + (int64_t)n;
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_stdio_flush
 *
 * Purpose:	Makes sure that all data is on disk.
 *
 * Errors:
 *		IO	  WRITEERROR	Fflush failed. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_stdio_flush(H5F_low_t *lf, const H5F_access_t __unused__ *access_parms)
{
    FUNC_ENTER(H5F_stdio_flush, FAIL);

    /*
     * What happens to the file position?  Is it guaranteed to be the same
     * after the fflush() as it was before?
     */
    lf->u.stdio.op = H5F_OP_UNKNOWN;

    /*
     * Flush
     */
    if (HDfflush(lf->u.stdio.f) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "fflush failed");
    }
    FUNC_LEAVE(SUCCEED);
}
