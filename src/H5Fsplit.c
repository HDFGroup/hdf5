/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Thursday, November 13, 1997
 *
 * Purpose:    A driver that splits the meta data and raw data into two
 *             separate files.  The high-order bit of the file address
 *             determines whether the address refers to the meta data file
 *             (high order bit is clear) or the raw data file (high order bit
 *             is set).
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>
#include <H5MMprivate.h>

/* Default file name extensions */
#define H5F_SPLIT_META_EXT	".meta"
#define H5F_SPLIT_RAW_EXT	".raw"

#define PABLO_MASK H5F_split
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL

static hbool_t H5F_split_access(const char *name,
				const H5F_access_t *access_parms, int mode,
				H5F_search_t *key/*out*/);
static H5F_low_t *H5F_split_open(const char *name,
				 const H5F_access_t *access_parms, uintn flags,
				 H5F_search_t *key/*out*/);
static herr_t H5F_split_close(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_split_read(H5F_low_t *lf, const H5F_access_t *access_parms,
			     const H5D_transfer_t xfer_mode,
			     const haddr_t *addr, size_t size,
			     uint8 *buf/*out*/);
static herr_t H5F_split_write(H5F_low_t *lf, const H5F_access_t *access_parms,
			      const H5D_transfer_t xfer_mode,
			      const haddr_t *addr, size_t size,
			      const uint8 *buf);
static herr_t H5F_split_flush(H5F_low_t *lf, const H5F_access_t *access_parms);
static herr_t H5F_split_extend(H5F_low_t *lf, const H5F_access_t *access_parms,
			       intn op, hsize_t size, haddr_t *addr/*out*/);
static intn H5F_split_alloc (H5F_low_t *lf, intn op, hsize_t alignment,
			     hsize_t threshold, hsize_t size, H5MF_free_t *blk,
			     haddr_t *addr/*out*/);

const H5F_low_class_t	H5F_LOW_SPLIT_g[1] = {{
    H5F_split_access,		/*access method				*/
    H5F_split_open,		/*open method				*/
    H5F_split_close,		/*close method				*/
    H5F_split_read,		/*read method				*/
    H5F_split_write,		/*write method				*/
    H5F_split_flush,		/*flush method				*/
    H5F_split_extend,		/*extend method				*/
    H5F_split_alloc,		/*alloc method				*/
}};

/*
 * This is the bit that determines whether the address is part of the meta
 * data file or part of the raw data file.  Eventually we'll want to pass
 * this kind of thing down to this function from above...
 */
#define H5F_SPLIT_MASK          0x80000000


/*-------------------------------------------------------------------------
 * Function:    H5F_split_open
 *
 * Purpose:     Opens a split meta data/raw data family with the specified
 *              base name.  The name of the meta data file will be created by
 *              appending `.h5' while the name of the raw data file will be
 *              created by appending `.raw'.
 *
 * Return:      Success:        Low-level file pointer
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Monday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5F_low_t *
H5F_split_open(const char *name, const H5F_access_t *access_parms,
	       uintn flags, H5F_search_t *key/*out*/)
{
    H5F_low_t              	*lf = NULL, *ret_value = NULL;
    char                    	fullname[4096];
    const char			*ext; /*file name extension*/
    const H5F_low_class_t	*meta_type = NULL;
    const H5F_low_class_t	*raw_type = NULL;

    FUNC_ENTER(H5F_split_open, NULL);

    assert(name && *name);
    assert (access_parms);
    assert (H5F_LOW_SPLIT==access_parms->driver);
    assert (access_parms->u.split.meta_access);
    assert (access_parms->u.split.raw_access);

    /* Get member types */
    meta_type = H5F_low_class (access_parms->u.split.meta_access->driver);
    raw_type = H5F_low_class (access_parms->u.split.raw_access->driver);
    
    /* Create the file descriptor */
    if (NULL==(lf = H5MM_calloc(sizeof(H5F_low_t)))) {
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		     "memory allocation failed");
    }
    lf->u.split.name = H5MM_xstrdup(name);
    lf->u.split.mask = H5F_SPLIT_MASK;

    /* Open the meta data file */
    ext = access_parms->u.split.meta_ext ?
	  access_parms->u.split.meta_ext : H5F_SPLIT_META_EXT;
    if (HDstrlen(name)+HDstrlen(ext) >= sizeof fullname) {
	HGOTO_ERROR (H5E_IO, H5E_CANTINIT, NULL, "file name is too long");
    }
    HDstrcpy(fullname, name);
    HDstrcat(fullname, ext);

    lf->u.split.meta = H5F_low_open(meta_type, fullname,
				    access_parms->u.split.meta_access,
				    flags, key/*out*/);
    if (NULL == lf->u.split.meta) {
        HGOTO_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "can't open meta file");
    }
    
    /* Open the raw data file */
    ext = access_parms->u.split.raw_ext ?
	  access_parms->u.split.raw_ext : H5F_SPLIT_RAW_EXT;
    if (HDstrlen(name)+HDstrlen(ext) >= sizeof fullname) {
	HGOTO_ERROR (H5E_IO, H5E_CANTINIT, NULL, "file name is too long");
    }
    HDstrcpy(fullname, name);
    HDstrcat(fullname, ext);

    lf->u.split.raw = H5F_low_open(raw_type, fullname,
				   access_parms->u.split.raw_access,
				   flags, NULL);
    if (NULL == lf->u.split.raw) {
        HGOTO_ERROR(H5E_IO, H5E_CANTOPENFILE, NULL, "can't open raw file");
    }
    
    /* Initialize the file size */
    H5F_low_size(lf->u.split.raw, &(lf->eof));
    lf->eof.offset |= lf->u.split.mask;

    HRETURN(lf);

  done:
    if (!ret_value) {
        if (lf) {
            H5F_split_close(lf, access_parms);
            H5MM_xfree(lf);
        }
    }
    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_split_close
 *
 * Purpose:     Closes a split file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Monday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_split_close(H5F_low_t *lf, const H5F_access_t *access_parms)
{
    FUNC_ENTER(H5F_split_close, FAIL);

    assert(lf);

    H5F_low_close(lf->u.split.meta, access_parms->u.split.meta_access);
    H5F_low_close(lf->u.split.raw, access_parms->u.split.raw_access);
    H5MM_xfree(lf->u.split.name);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_split_read
 *
 * Purpose:     Reads a chunk of contiguous data from a split file.  We
 *              assume that the data being read never crosses the meta
 *              data/raw data boundary. Reading past the end of a file
 *              returns zeros instead of failing.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Monday, November 13, 1997
 *
 * Modifications:
 *		June 2, 1998	Albert Cheng
 *		Added xfer_mode argument
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_split_read(H5F_low_t *lf, const H5F_access_t *access_parms,
	       const H5D_transfer_t xfer_mode,
	       const haddr_t *addr, size_t size, uint8 *buf/*out*/)
{
    haddr_t             tmp_addr;
    H5F_low_t           *sub = NULL;
    herr_t		status;
    const H5F_access_t	*sub_parms = NULL;

    FUNC_ENTER(H5F_split_read, FAIL);

    assert(lf);
    assert(addr && H5F_addr_defined(addr));
    assert(buf);
    assert(xfer_mode != H5D_XFER_COLLECTIVE);	/* no collective support */

    /* Which file to we actually read from? */
    if (addr->offset & lf->u.split.mask) {
        sub = lf->u.split.raw;
	sub_parms = access_parms->u.split.raw_access;
        tmp_addr.offset = addr->offset & (lf->u.split.mask - 1);
    } else {
        sub = lf->u.split.meta;
	sub_parms = access_parms->u.split.meta_access;
        tmp_addr = *addr;
    }

    /* Read the data */
    status = H5F_low_read(sub, sub_parms, xfer_mode, &tmp_addr, size, buf/*out*/);
    FUNC_LEAVE(status);
}


/*-------------------------------------------------------------------------
 * Function:    H5F_split_write
 *
 * Purpose:     Writes BUF to either the meta data file or the raw data file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Monday, November 13, 1997
 *
 * Modifications:
 *		June 2, 1998	Albert Cheng
 *		Added xfer_mode argument
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_split_write(H5F_low_t *lf, const H5F_access_t *access_parms,
	        const H5D_transfer_t xfer_mode,
		const haddr_t *addr, size_t size, const uint8 *buf)
{
    haddr_t             tmp_addr;
    H5F_low_t           *sub = NULL;
    herr_t		status;
    const H5F_access_t	*sub_parms = NULL;

    FUNC_ENTER(H5F_split_write, FAIL);

    assert(lf);
    assert(addr && H5F_addr_defined(addr));
    assert(buf);
    assert(xfer_mode != H5D_XFER_COLLECTIVE);	/* no collective support */

    /* Which file to we actually write to? */
    if (addr->offset & lf->u.split.mask) {
        sub = lf->u.split.raw;
	sub_parms = access_parms->u.split.raw_access;
        tmp_addr.offset = addr->offset & (lf->u.split.mask - 1);
    } else {
        sub = lf->u.split.meta;
	sub_parms = access_parms->u.split.meta_access;
        tmp_addr = *addr;
    }

    /* Write the data */
    status = H5F_low_write(sub, sub_parms, xfer_mode, &tmp_addr, size, buf);
    FUNC_LEAVE(status);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_split_flush
 *
 * Purpose:     Flushes all data to disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Monday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_split_flush(H5F_low_t *lf, const H5F_access_t *access_parms)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER(H5F_split_flush, FAIL);

    assert(lf);

    ret_value = (H5F_low_flush(lf->u.split.meta,
			       access_parms->u.split.meta_access) >= 0 &&
                 H5F_low_flush(lf->u.split.raw,
			       access_parms->u.split.raw_access) >= 0);

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_split_access
 *
 * Purpose:     Determines if both members of the split data file family can
 *              be accessed and returns the key for the first member of the
 *              family.
 *
 * Return:      Success:        TRUE or FALSE
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Monday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5F_split_access(const char *name, const H5F_access_t *access_parms,
		 int mode, H5F_search_t *key/*out*/)
{
    char                   	 fullname[4096];
    hbool_t                 	status;
    const char			*ext; /*file extension*/
    const H5F_low_class_t	*meta_type = NULL;
    const H5F_low_class_t	*raw_type = NULL;

    FUNC_ENTER(H5F_split_access, FAIL);

    assert(name && *name);
    assert (access_parms);
    assert (H5F_LOW_SPLIT==access_parms->driver);
    assert (access_parms->u.split.meta_access);
    assert (access_parms->u.split.raw_access);

    /* The meta data member */
    meta_type = H5F_low_class (access_parms->u.split.meta_access->driver);
    ext = access_parms->u.split.meta_ext ?
	  access_parms->u.split.meta_ext : H5F_SPLIT_META_EXT;
    if (HDstrlen(name)+HDstrlen(ext) >= sizeof fullname) {
	HRETURN_ERROR (H5E_IO, H5E_CANTINIT, FAIL, "file name is too long");
    }
    HDstrcpy(fullname, name);
    HDstrcat(fullname, ext);

    status = H5F_low_access(meta_type, fullname,
			    access_parms->u.split.meta_access,
			    mode, key/*out*/);
    if (status < 0) {
        HRETURN_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                      "access call failed for meta data member");
    }
    if (!status) HRETURN(FALSE);

    /* The raw data member */
    raw_type = H5F_low_class (access_parms->u.split.raw_access->driver);
    ext = access_parms->u.split.raw_ext ?
	  access_parms->u.split.raw_ext : H5F_SPLIT_RAW_EXT;
    if (HDstrlen(name)+HDstrlen(ext) >= sizeof fullname) {
	HRETURN_ERROR (H5E_IO, H5E_CANTINIT, FAIL, "file name is too long");
    }
    HDstrcpy(fullname, name);
    HDstrcat(fullname, ext);

    status = H5F_low_access(raw_type, fullname,
			    access_parms->u.split.raw_access,
			    mode, NULL/*out*/);
    if (status < 0) {
        HRETURN_ERROR(H5E_IO, H5E_CANTINIT, FAIL,
                      "access call failed for raw data member");
    }
    FUNC_LEAVE(status);
}

/*-------------------------------------------------------------------------
 * Function:    H5F_split_extend
 *
 * Purpose:     Allocates memory from the end of the meta data file or raw
 *              data file.
 *
 * Return:      Non-negative on success (with the address of the allocated
 *                      memory returned through the ADDR argument.) /Negative
 *                      on failure
 *
 * Programmer:  Robb Matzke
 *              Thursday, November 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F_split_extend(H5F_low_t *lf, const H5F_access_t *access_parms, intn op,
		 hsize_t size, haddr_t *addr/*out*/)
{
    FUNC_ENTER(H5F_split_extend, FAIL);

    assert(lf);
    assert(H5MF_META == op || H5MF_RAW == op);
    assert(size > 0);
    assert(addr);

    if (H5MF_META == op) {
        if (H5F_low_extend(lf->u.split.meta, access_parms->u.split.meta_access,
			   op, size, addr/*out*/)<0) {
            HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                          "meta data allocation failed");
        }
        if (addr->offset + size > lf->eof.offset) {
            lf->eof.offset = addr->offset + size;
        }
    } else {
        if (H5F_low_extend(lf->u.split.raw, access_parms->u.split.raw_access,
			   op, size, addr/*out*/)<0) {
            HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                          "raw data allocation failed");
        }
        addr->offset |= lf->u.split.mask;
        lf->eof = lf->u.split.raw->eof;
        lf->eof.offset |= lf->u.split.mask;
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5F_split_alloc
 *
 * Purpose:	Determines if free block BLK in file LF can be used to
 *		satisfy the request for SIZE bytes.  This function is
 *		actually the same as H5F_low_alloc() except it returns
 *		failure if the OP is not compatible with the block address,
 *		insuring that meta data is allocated from one half of the
 *		address space and raw data from the other half.
 *
 * Return:	Success:	Positive if the free block satisfies the
 *				request exactly, zero if the free block
 *				over-satisfies the request.  The ADDR will
 *				contain the address within the free block
 *				where the request starts.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static intn
H5F_split_alloc (H5F_low_t *lf, intn op, hsize_t alignment, hsize_t threshold,
		 hsize_t size, H5MF_free_t *blk, haddr_t *addr/*out*/)
{
    intn	ret_value = FAIL;
    hsize_t	wasted;

    FUNC_ENTER (H5F_split_alloc, FAIL);
    assert (lf);
    assert (alignment>0);
    assert (size>0);
    assert (blk);
    assert (addr);

    switch (op) {
    case H5MF_META:
	if (blk->addr.offset & lf->u.split.mask) HRETURN(FAIL);
	break;
    case H5MF_RAW:
	if (0==(blk->addr.offset & lf->u.split.mask)) HRETURN(FAIL);
	break;
    }

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
    
    FUNC_LEAVE (ret_value);
}
