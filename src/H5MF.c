/*-------------------------------------------------------------------------
 * Copyright (C) 1997   National Center for Supercomputing Applications.
 *                      All rights reserved.
 *
 *-------------------------------------------------------------------------
 *
 * Created:             H5MF.c
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             File memory management functions.
 *
 * Modifications:
 *      Robb Matzke, 5 Aug 1997
 *      Added calls to H5E.
 *
 * 	Robb Matzke, 8 Jun 1998
 *	Implemented a very simple free list which is not persistent and which
 *	is lossy.
 *
 *-------------------------------------------------------------------------
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Fprivate.h>
#include <H5MFprivate.h>

#define PABLO_MASK      H5MF_mask

/* Is the interface initialized? */
static intn             interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL


/*-------------------------------------------------------------------------
 * Function:    H5MF_alloc
 *
 * Purpose:     Allocate at least SIZE bytes of file memory and return
 *              the address where that contiguous chunk of file memory
 *              exists. The allocation operation should be either H5MF_META or
 *              H5MF_RAW depending on the purpose for which the storage is
 *              being requested.
 *
 * Return:      Success:        SUCCEED.  The file address of new chunk is
 *                              returned through the ADDR argument.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_alloc(H5F_t *f, intn op, hsize_t size, haddr_t *addr/*out*/)
{
    haddr_t             tmp_addr;
    intn		i, found, status;
    hsize_t		n;
    H5MF_free_t		blk;
    hsize_t		thresh = f->shared->access_parms->threshold;
    hsize_t		align = f->shared->access_parms->alignment;

    FUNC_ENTER(H5MF_alloc, FAIL);

    /* check arguments */
    assert(f);
    assert(H5MF_META == op || H5MF_RAW == op);
    assert(size > 0);
    assert(addr);
    
    /* Fail if we don't have write access */
    if (0==(f->intent & H5F_ACC_RDWR)) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_CANTINIT, FAIL, "file is read-only");
    }

    /*
     * Try to satisfy the request from the free list. We prefer exact matches
     * to partial matches, so if we find an exact match then we break out of
     * the loop immediately, otherwise we keep looking for an exact match.
     */
    for (i=0, found=-1; i<f->shared->fl_nfree; i++) {
	if ((status=H5F_low_alloc(f->shared->lf, op, align, thresh, size,
				  f->shared->fl_free+i, addr/*out*/))>0) {
	    /* Exact match found */
	    found = i;
	    break;
	} else if (0==status) {
	    /* Partial match */
	    found = i;
	}
    }

    if (found>=0 &&
	(status=H5F_low_alloc (f->shared->lf, op, align, thresh, size, 
			       f->shared->fl_free+found, addr/*out*/))>0) {
	/*
	 * We found an exact match.  Remove that block from the free list and
	 * use it to satisfy the request.
	 */
	--(f->shared->fl_nfree);
	HDmemmove (f->shared->fl_free+found, f->shared->fl_free+found+1,
		   (f->shared->fl_nfree-found) * sizeof(H5MF_free_t));
	
    } else if (found>=0 && status==0) {
	/*
	 * We found a free block which is larger than the requested size.
	 * Return the unused parts of the free block to the free list.
	 */
	blk = f->shared->fl_free[found];
	--f->shared->fl_nfree;
	HDmemmove (f->shared->fl_free+found, f->shared->fl_free+found+1,
		   (f->shared->fl_nfree-found) * sizeof(H5MF_free_t));
	if (H5F_addr_gt (addr, &(blk.addr))) {
	    /* Free the first part of the free block */
	    n = addr->offset - blk.addr.offset;
	    H5MF_xfree (f, &(blk.addr), n);
	    blk.addr = *addr;
	    blk.size -= n;
	}
	
	if (blk.size > size) {
	    /* Free the second part of the free block */
	    H5F_addr_inc (&(blk.addr), size);
	    blk.size -= size;
	    H5MF_xfree (f, &(blk.addr), blk.size);
	}
	
    } else {
	/*
	 * No suitable free block was found.  Allocate space from the end of
	 * the file.  We don't know about alignment at this point, so we
	 * allocate enough space to align the data also.
	 */
	if (size>=thresh) {
	    blk.size = size + align - 1;
	} else {
	    blk.size = size;
	}
	if (H5F_low_extend(f->shared->lf, f->shared->access_parms, op,
			   blk.size, &(blk.addr)/*out*/) < 0) {
	    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			  "low level mem management failed");
	}
	
	/* Convert from absolute to relative */
	blk.addr.offset -= f->shared->base_addr.offset;

	/* Did we extend the size of the hdf5 data? */
	tmp_addr = blk.addr;
	H5F_addr_inc(&tmp_addr, blk.size);
	if (H5F_addr_gt(&tmp_addr, &(f->shared->hdf5_eof))) {
	    f->shared->hdf5_eof = tmp_addr;
	}

	if ((status=H5F_low_alloc (f->shared->lf, op, align, thresh, size,
				   &blk, addr/*out*/))>0) {
	    /* Exact match */
	} else if (0==status) {
	    /* Partial match */
	    if (H5F_addr_gt (addr, &(blk.addr))) {
		n = addr->offset - blk.addr.offset;
		H5MF_xfree (f, &(blk.addr), n);
		blk.addr = *addr;
		blk.size -= n;
	    }
	    if (blk.size > size) {
		H5F_addr_inc (&(blk.addr), size);
		blk.size -= size;
		H5MF_xfree (f, &(blk.addr), blk.size);
	    }
	}
    }
    
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5MF_xfree
 *
 * Purpose:     Frees part of a file, making that part of the file
 *              available for reuse.
 *
 * Note:        This version of the function doesn't do anything.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 17 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_xfree(H5F_t *f, const haddr_t *addr, hsize_t size)
{
    int		i;
    
    FUNC_ENTER(H5MF_xfree, FAIL);

    /* check arguments */
    assert(f);
    if (!addr || !H5F_addr_defined(addr) || 0 == size) {
        HRETURN(SUCCEED);
    }
    assert(!H5F_addr_zerop(addr));

    /*
     * Insert this free block into the free list without attempting to
     * combine it with other free blocks.  If the list is overfull then
     * remove the smallest free block.
     */
    if (f->shared->fl_nfree>=H5MF_NFREE) {
	for (i=0; i<H5MF_NFREE; i++) {
	    if (f->shared->fl_free[i].size<size) {
#ifdef H5MF_DEBUG
		if (H5DEBUG(MF)) {
		    fprintf(H5DEBUG(MF),
			    "H5MF_free: lost %lu bytes of file storage\n",
			    (unsigned long) f->shared->fl_free[i].size);
		}
#endif
		f->shared->fl_free[i].addr = *addr;
		f->shared->fl_free[i].size = size;
		break;
	    }
	}
    } else {
	i = f->shared->fl_nfree++;
	f->shared->fl_free[i].addr = *addr;
	f->shared->fl_free[i].size = size;
    }
    
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5MF_realloc
 *
 * Purpose:	Changes the size of an allocated chunk, possibly moving it to
 *		a new address.  The chunk to change is at address ORIG_ADDR
 *		and is exactly ORIG_SIZE bytes (if these are zero and undef
 *		then this function acts like H5MF_alloc).  The new size will
 *		be NEW_SIZE and its address is returned though NEW_ADDR (if
 *		NEW_SIZE is zero then this function acts like H5MF_free and
 *		an undefined address is returned for NEW_ADDR).
 *
 *		If the new size is less than the old size then the new
 *		address will be the same as the old address (except for the
 *		special case where the new size is zero).
 *
 *		If the new size is more than the old size then most likely a
 *		new address will be returned.  However, under certain
 *		circumstances the library may return the same address.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_realloc (H5F_t *f, intn op, hsize_t orig_size, const haddr_t *orig_addr,
	      hsize_t new_size, haddr_t *new_addr/*out*/)
{
    FUNC_ENTER (H5MF_realloc, FAIL);

    if (0==orig_size) {
	/* Degenerate to H5MF_alloc() */
	assert (!H5F_addr_defined (orig_addr));
	if (new_size>0) {
	    if (H5MF_alloc (f, op, new_size, new_addr/*out*/)<0) {
		HRETURN_ERROR (H5E_RESOURCE, H5E_CANTINIT, FAIL,
			       "unable to allocate new file memory");
	    }
	} else {
	    H5F_addr_undef (new_addr);
	}
	
    } else if (0==new_size) {
	/* Degenerate to H5MF_free() */
	assert (H5F_addr_defined (orig_addr));
	if (H5MF_xfree (f, orig_addr, orig_size)<0) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_CANTINIT, FAIL,
			   "unable to free old file memory");
	}
	H5F_addr_undef (new_addr);
	
    } else if (new_size > orig_size) {
	/* Size is getting larger */
	if (H5MF_alloc (f, op, new_size, new_addr/*out*/)<0) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_CANTINIT, FAIL,
			   "unable to allocate new file memory");
	}
	if (H5MF_xfree (f, orig_addr, orig_size)<0) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_CANTINIT, FAIL,
			   "unable to free old file memory");
	}

    } else {
	/* New size is not larger */
#ifdef H5MF_DEBUG
	if (H5DEBUG(MF) && new_size<orig_size) {
	    HDfprintf (H5DEBUG(MF), "H5MF: realloc lost %Hd bytes\n",
		       orig_size-new_size);
	}
#endif
	*new_addr = *orig_addr;
    }

    FUNC_LEAVE (SUCCEED);
}

	    
	    
