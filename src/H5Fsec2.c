/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Wednesday, October 22, 1997
 *
 * Purpose:    This is the Posix section-2 I/O subclass of H5Flow.
 *
 * Notes:      This driver keeps track of its own file position in order to
 *	       minimize the number of calls to lseek().	 We assume that
 *	       opening a file sets the current file position to the beginning
 *	       and that read() and write() modify the file position as
 *	       expected when they return successfully (unsuccessful return
 *	       leaves the file position undefined).
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#define PABLO_MASK	H5Fsec2_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT	NULL

static H5F_low_t *H5F_sec2_open(const char *name,
				const H5F_access_t *access_parms, uintn flags,
				H5F_search_t *key/*out*/);
static herr_t H5F_sec2_close(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_sec2_read(H5F_low_t *lf, const H5F_access_t *access_parms,
			    const H5F_xfer_t *xfer_parms, haddr_t addr,
			    size_t size, uint8_t *buf/*out*/);
static herr_t H5F_sec2_write(H5F_low_t *lf, const H5F_access_t *access_parms,
			     const H5F_xfer_t *xfer_parms, haddr_t addr,
			     size_t size, const uint8_t *buf);

const H5F_low_class_t	H5F_LOW_SEC2_g[1] = {{
    NULL,			/* access method			*/
    H5F_sec2_open,		/* open method				*/
    H5F_sec2_close,		/* close method				*/
    H5F_sec2_read,		/* read method				*/
    H5F_sec2_write,		/* write method				*/
    NULL,			/* flush method				*/
    NULL,			/* extend method			*/
    NULL,			/* alloc method				*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_open
 *
 * Purpose:	Opens a file with name NAME.  The FLAGS are a bit field with
 *		the possible values defined in H5F_low_open().
 *
 * Errors:
 *		IO	  CANTOPENFILE	Open failed. 
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
H5F_sec2_open(const char *name, const H5F_access_t UNUSED *access_parms,
	      uintn flags, H5F_search_t *key/*out*/)
{
    intn		    oflags;
    H5F_low_t		   *lf = NULL;
    int			    fd;
    struct stat		    sb;

    FUNC_ENTER(H5F_sec2_open, NULL);

    oflags = (flags & H5F_ACC_RDWR) ? O_RDWR : O_RDONLY;
    oflags |= (flags & H5F_ACC_CREAT) ? O_CREAT : 0;
    oflags |= (flags & H5F_ACC_EXCL) ? O_EXCL : 0;
    oflags |= (flags & H5F_ACC_TRUNC) ? O_TRUNC : 0;

    if ((fd = HDopen(name, oflags, 0666)) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "open failed");
    }
    if (NULL==(lf = H5MM_calloc(sizeof(H5F_low_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }
    lf->u.sec2.fd = fd;
    lf->u.sec2.op = H5F_OP_SEEK;
    lf->u.sec2.cur = 0;
    HDfstat(fd, &sb);
    lf->eof = sb.st_size;

    if (key) {
#if WIN32

	int fd;
	HFILE filehandle;
	struct _BY_HANDLE_FILE_INFORMATION fileinfo;
	int results;


	fd = HDopen(name,_O_RDONLY,0);
	filehandle = _get_osfhandle(fd);
	results = GetFileInformationByHandle(filehandle, &fileinfo);

	/*returns a 0 on failure*/
	
	if (!results) {			
		lf = NULL;		
	}	
	
	else {		
		HDstat(name,&sb);		
		key->dev = sb.st_dev;		
		key->ino = 0;		
		key->fileindexhi = fileinfo.nFileIndexHigh;		
		key->fileindexlo = fileinfo.nFileIndexLow;		
	}
	
	HDclose(fd);	

#else
	key->dev = sb.st_dev;
	key->ino = sb.st_ino;
#endif
    }
    FUNC_LEAVE(lf);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_close
 *
 * Purpose:	Closes a file.
 *
 * Errors:
 *		IO	  CLOSEERROR	Close failed. 
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
H5F_sec2_close(H5F_low_t *lf, const H5F_access_t UNUSED *access_parms)
{
    FUNC_ENTER(H5F_sec2_close, FAIL);

    if (HDclose(lf->u.sec2.fd) < 0) {
	HRETURN_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "close failed");
    }
    lf->u.sec2.fd = -1;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_read
 *
 * Purpose:	Reads SIZE bytes beginning at address ADDR in file LF and
 *		places them in buffer BUF.  Reading past the logical or
 *		physical end of file returns zeros instead of failing.
 *
 * Errors:
 *		IO	  READERROR	Read failed. 
 *		IO	  SEEKERROR	Lseek failed. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *		Albert Cheng, 1998-06-02
 *		Added XFER_MODE argument.
 *
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_sec2_read(H5F_low_t *lf, const H5F_access_t UNUSED *access_parms,
	      const H5F_xfer_t UNUSED *xfer_parms, haddr_t addr, size_t size,
	      uint8_t *buf)
{
    ssize_t		n;
    uint64_t		mask;
#ifdef HAVE_LSEEK64
    off64_t		offset;
#else
    off_t		offset;
#endif

    FUNC_ENTER(H5F_sec2_read, FAIL);

    /* Check for overflow */
    mask = (uint64_t)1 << (8*sizeof(offset)-1);
    if (addr >= mask ||
	addr+size < addr ||
	addr+size >= mask) {
	HRETURN_ERROR (H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
    }
#ifdef HAVE_LSEEK64
    offset = (off64_t)(addr); /*checked for overflow above*/
#else
    offset = (off_t)(addr); /*checked for overflow above*/
#endif

    /* Check easy cases */
    if (0 == size) HRETURN(SUCCEED);
    if ((uint64_t)offset >= lf->eof) {
	HDmemset(buf, 0, size);
	HRETURN(SUCCEED);
    }

    /*
     * Optimize seeking.  If that optimization is disabled then always call
     * lseek().
     */
    if (!H5F_OPT_SEEK ||
	lf->u.sec2.op == H5F_OP_UNKNOWN ||
	lf->u.sec2.cur != offset) {
#ifdef HAVE_LSEEK64
	if (lseek64 (lf->u.sec2.fd, offset, SEEK_SET)<0) {
	    HRETURN_ERROR (H5E_IO, H5E_SEEKERROR, FAIL, "lseek64 failed");
	}
#else
	if (HDlseek(lf->u.sec2.fd, offset, SEEK_SET) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "lseek failed");
	}
#endif
	lf->u.sec2.cur = offset;
    }
    
    /*
     * Read zeros past the logical end of file (physical is handled below)
     */
    if ((size_t) offset + size > lf->eof) {
	size_t nbytes = (size_t)offset + size - lf->eof;
	HDmemset(buf + size - nbytes, 0, nbytes);
	size -= nbytes;
    }
    
    /*
     * Read the data.  If a read error occurs then set the last file operation
     * to UNKNOWN because the file position isn't guaranteed by Posix.
     */
    if ((n = HDread(lf->u.sec2.fd, buf, size)) < 0) {
	lf->u.sec2.op = H5F_OP_UNKNOWN;
	HRETURN_ERROR(H5E_IO, H5E_READERROR, FAIL, "read failed");
    } else if ((size_t)n < size) {
	HDmemset(buf + n, 0, size - n);
    }

    /*
     * Update the file position with the number of bytes actually read.	 This
     * might be different than the number requested.
     */
    lf->u.sec2.op = H5F_OP_READ;
    lf->u.sec2.cur = offset + n;

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5F_sec2_write
 *
 * Purpose:	Writes SIZE bytes from the beginning of BUF into file LF at
 *		file address ADDR.
 *
 * Errors:
 *		IO	  SEEKERROR	Lseek failed. 
 *		IO	  WRITEERROR	Write failed. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *		Albert Cheng, 1998-06-02
 *		Added XFER_MODE argument.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_sec2_write(H5F_low_t *lf, const H5F_access_t UNUSED *access_parms,
	       const H5F_xfer_t UNUSED *xfer_parms, haddr_t addr, size_t size,
	       const uint8_t *buf)
{
    uint64_t	mask;
    ssize_t	n;
#ifdef HAVE_LSEEK64
    off64_t	offset;
#else
    off_t	offset;
#endif

    FUNC_ENTER(H5F_sec2_write, FAIL);

    /* Check for overflow */
    mask = (uint64_t)1 << (8*sizeof(offset)-1);
    if (addr >= mask ||
	addr+size < addr ||
	addr+size >= mask) {
	HRETURN_ERROR (H5E_IO, H5E_OVERFLOW, FAIL, "file address overflowed");
    }
#ifdef HAVE_LSEEK64
    offset = (off64_t)(addr); /*checked for overflow*/
    n = (off64_t)size; /*checked for overflow*/
#else
    offset = (off_t)(addr); /*checked for overflow*/
    n = (off_t)size; /*checked for overflow*/
#endif

    /*
     * Optimize seeking. If that optimization is disabled then always call
     * lseek().
     */
    if (!H5F_OPT_SEEK ||
	lf->u.sec2.op == H5F_OP_UNKNOWN ||
	lf->u.sec2.cur != offset) {
	if (HDlseek(lf->u.sec2.fd, offset, SEEK_SET) < 0) {
	    HRETURN_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "lseek failed");
	}
	lf->u.sec2.cur = offset;
    }

    /*
     * Write the data to the file.  If the write failed then set the
     * operation back to UNKNOWN since Posix doesn't gurantee its value.
     */
    if (n != HDwrite(lf->u.sec2.fd, buf, size)) {
	lf->u.sec2.op = H5F_OP_UNKNOWN;
	HRETURN_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "write failed");
    }

    /*
     * Update the file position.
     */
    lf->u.sec2.op = H5F_OP_WRITE;
    lf->u.sec2.cur = offset + n;

    FUNC_LEAVE(SUCCEED);
}
