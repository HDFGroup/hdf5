/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *	       Wednesday, October 22, 1997
 *
 * Purpose:    This file implements an in-core temporary file.	It's intended
 *	       for storing small temporary files such as wrappers generated
 *	       on the fly.
 *
 * Note:       This is mostly an exercise to help clean up parts of the H5F
 *	       package since this driver is quite different than the other
 *	       low level drivers we have so far.
 *
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MMprivate.h>

#define H5F_CORE_DEV	0xffff	/*pseudo dev for core until we fix things */

#define PABLO_MASK	H5Fcore_mask
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT NULL

static htri_t H5F_core_access(const char *name,
			      const H5F_access_t *access_parms, int mode,
			      H5F_search_t *key/*out*/);
static H5F_low_t *H5F_core_open(const char *name,
				const H5F_access_t *access_parms, uintn flags,
				H5F_search_t *key/*out*/);
static herr_t H5F_core_close(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_core_read(H5F_low_t *lf, const H5F_access_t *access_parms,
			    const H5F_xfer_t *xfer_parms, haddr_t addr,
			    size_t size, uint8_t *buf);
static herr_t H5F_core_write(H5F_low_t *lf, const H5F_access_t *access_parms,
			     const H5F_xfer_t *xfer_parms, haddr_t addr,
			     size_t size, const uint8_t *buf);

const H5F_low_class_t	H5F_LOW_CORE_g[1] = {{
    H5F_core_access,		/*access method				*/
    H5F_core_open,		/*open method				*/
    H5F_core_close,		/*close method				*/
    H5F_core_read,		/*read method				*/
    H5F_core_write,		/*write method				*/
    NULL,			/*flush method				*/
    NULL,			/*extend method				*/
    NULL,			/*alloc method				*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5F_core_access
 *
 * Purpose:	Determines if the specified file already exists.  This driver
 *		doesn't use names, so every call to H5F_core_open() would
 *		create a new file.  Therefore, this function always returns
 *		false and KEY is never initialized.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, October 24, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5F_core_access(const char UNUSED*name,
		const H5F_access_t UNUSED *access_parms,
		int UNUSED mode, H5F_search_t UNUSED *key/*out*/)
{
    FUNC_ENTER(H5F_core_access, FAIL);
    FUNC_LEAVE(FALSE);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_open
 *
 * Purpose:	Opens a temporary file which will exist only in memory.	 The
 *		NAME argument is unused.  The FLAGS are a bit field with
 *		the possible values defined in H5F_low_open().
 *
 * Errors:
 *		IO	  CANTOPENFILE	Must creat file with write access. 
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
H5F_core_open(const char UNUSED *name,
	      const H5F_access_t UNUSED *access_parms,
	      uintn flags, H5F_search_t *key/*out*/)
{
    H5F_low_t		   *lf = NULL;
    static ino_t	    ino = 0;

    FUNC_ENTER(H5F_core_open, NULL);

    if (0 == (flags & H5F_ACC_RDWR) || 0 == (flags & H5F_ACC_CREAT)) {
	HRETURN_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL,
		      "must creat file with write access");
    }
    
    if (NULL==(lf = H5MM_calloc(sizeof(H5F_low_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }
    lf->u.core.mem = NULL;
    lf->u.core.alloc = 0;
    lf->u.core.size = 0;
    H5F_addr_reset(&(lf->eof));

    if (key) {
	key->dev = H5F_CORE_DEV;
	key->ino = ino++;
    }
    
    FUNC_LEAVE(lf);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_close
 *
 * Purpose:	Closes a file.
 *
 * Errors:
 *
 * Return:  	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, October 22, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_core_close(H5F_low_t *lf, const H5F_access_t UNUSED *access_parms)
{
    FUNC_ENTER(H5F_core_close, FAIL);

    lf->u.core.mem = H5MM_xfree(lf->u.core.mem);
    lf->u.core.size = 0;
    lf->u.core.alloc = 0;

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_read
 *
 * Purpose:	Reads SIZE bytes beginning at address ADDR in file LF and
 *		places them in buffer BUF.  Reading past the logical or
 *		physical end of the file returns zeros instead of failing.
 *
 * Errors:
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
H5F_core_read(H5F_low_t *lf, const H5F_access_t UNUSED *access_parms,
	      const H5F_xfer_t UNUSED *xfer_parms, haddr_t addr,
	      size_t size, uint8_t *buf)
{
    size_t		n;
    size_t		eof;

    FUNC_ENTER(H5F_core_read, FAIL);

    assert(lf);
    assert(H5F_addr_defined(addr));
    assert(buf);

    eof = MIN(lf->eof, lf->u.core.size);

    if (addr >= eof) {
	HDmemset(buf, 0, size);
    } else {
	n = MIN(size, eof-addr);
	HDmemcpy(buf, lf->u.core.mem + addr, n);
	HDmemset(buf+n, 0, size-n);
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_core_write
 *
 * Purpose:	Writes SIZE bytes from the beginning of BUF into file LF at
 *		file address ADDR.  The file is extended as necessary to
 *		accommodate the new data.
 *
 * Errors:
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
H5F_core_write(H5F_low_t *lf, const H5F_access_t *access_parms,
	       const H5F_xfer_t UNUSED *xfer_parms, haddr_t addr,
	       size_t size, const uint8_t *buf)
{
    size_t		need_more, na;
    size_t		increment = 1;
    uint8_t		*x = NULL;

    FUNC_ENTER(H5F_core_write, FAIL);

    assert(lf);
    assert(H5F_addr_defined(addr));
    assert(buf);
    assert (!access_parms || H5F_LOW_CORE==access_parms->driver);

    /*
     * Allocate more space.  We always allocate a multiple of the increment
     * size, which is either defined in the file access property list or
     * which defaults to one.
     */
    if (addr + size > lf->u.core.alloc) {
	if (access_parms) increment = access_parms->u.core.increment;
	need_more = addr+size - lf->u.core.alloc;
	need_more = increment*((need_more+increment-1)/increment);

	na = lf->u.core.alloc + need_more;
	if (NULL==(x = H5MM_realloc (lf->u.core.mem, na))) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}
	lf->u.core.alloc = na;
	lf->u.core.mem = x;
    }
    
    /* Move the physical EOF marker */
    if (addr + size > lf->u.core.size) {
	lf->u.core.size = addr + size;
    }
    
    /* Copy data */
    HDmemcpy(lf->u.core.mem+addr, buf, size);

    FUNC_LEAVE(SUCCEED);
}
