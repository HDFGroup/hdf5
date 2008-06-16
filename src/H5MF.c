/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5MF.c
 *                      Jul 11 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             File memory management functions.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5MFprivate.h"	/* File memory management		*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static hbool_t H5MF_alloc_overflow(const H5F_t *f, hsize_t size);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:    H5MF_alloc
 *
 * Purpose:     Allocate SIZE bytes of file memory and return the relative
 *		address where that contiguous chunk of file memory exists.
 *		The TYPE argument describes the purpose for which the storage
 *		is being requested.
 *
 * Return:      Success:        The file address of new chunk.
 *              Failure:        HADDR_UNDEF
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 11 1997
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5MF_alloc(const H5F_t *f, H5FD_mem_t type, hid_t dxpl_id, hsize_t size)
{
    haddr_t	ret_value;

    FUNC_ENTER_NOAPI(H5MF_alloc, HADDR_UNDEF)

    /* check arguments */
    HDassert(f);
    HDassert(size > 0);

    /* Fail if we don't have write access */
    if(0 == (f->intent & H5F_ACC_RDWR))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, HADDR_UNDEF, "file is read-only")

    /* Allocate space from the virtual file layer */
    if(HADDR_UNDEF == (ret_value = H5FD_alloc(f->shared->lf, type, dxpl_id, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "file allocation failed")

    /* Convert absolute file address to relative file address */
    HDassert(ret_value >= f->shared->base_addr);

    /* Set return value */
    ret_value -= f->shared->base_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MF_xfree
 *
 * Purpose:     Frees part of a file, making that part of the file
 *              available for reuse.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_xfree(const H5F_t *f, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOFUNC(H5MF_xfree)

    /* check arguments */
    HDassert(f);
    if(!H5F_addr_defined(addr) || 0 == size)
        HGOTO_DONE(SUCCEED);
    HDassert(addr != 0);

    /* Convert relative address to absolute address */
    addr += f->shared->base_addr;

    /* Allow virtual file layer to free block */
    if(H5FD_free(f->shared->lf, type, dxpl_id, addr, size) < 0) {
#ifdef H5MF_DEBUG
	if(H5DEBUG(MF))
	    fprintf(H5DEBUG(MF), "H5MF_free: lost %lu bytes of file storage\n", (unsigned long)size);
#endif
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_xfree() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_realloc
 *
 * Purpose:	Changes the size of an allocated chunk, possibly moving it to
 *		a new address.  The chunk to change is at address OLD_ADDR
 *		and is exactly OLD_SIZE bytes (if these are H5F_ADDR_UNDEF
 *		and zero then this function acts like H5MF_alloc).  The new
 *		size will be NEW_SIZE and its address is the return value (if
 *		NEW_SIZE is zero then this function acts like H5MF_free and
 *		an undefined address is returned).
 *
 *		If the new size is less than the old size then the new
 *		address will be the same as the old address (except for the
 *		special case where the new size is zero).
 *
 *		If the new size is more than the old size then most likely a
 *		new address will be returned.  However, under certain
 *		circumstances the library may return the same address.
 *
 * Return:	Success:	The relative file address of the new block.
 * 		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5MF_realloc(H5F_t *f, H5FD_mem_t type, hid_t dxpl_id, haddr_t old_addr, hsize_t old_size,
	     hsize_t new_size)
{
    haddr_t	ret_value;

    FUNC_ENTER_NOAPI(H5MF_realloc, HADDR_UNDEF)

    /* Convert old relative address to absolute address */
    old_addr += f->shared->base_addr;

    /* Reallocate memory from the virtual file layer */
    ret_value = H5FD_realloc(f->shared->lf, type, dxpl_id, old_addr, old_size, new_size);
    if(HADDR_UNDEF == ret_value)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, HADDR_UNDEF, "unable to allocate new file memory")

    /* Convert return value to relative address */
    HDassert(ret_value >= f->shared->base_addr);

    /* Set return value */
    ret_value -= f->shared->base_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_realloc() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_alloc_overflow
 *
 * Purpose:	Checks if an allocation of file space would cause an overflow.
 *          F is the file whose space is being allocated, SIZE is the amount
 *          of space needed.
 *
 * Return:	FALSE if no overflow would result
 *          TRUE if overflow would result (the allocation should not be allowed)
 *
 * Programmer:	James Laird
 *		Nat Furrer
 *              Tuesday, June 1, 2004
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5MF_alloc_overflow(const H5F_t *f, hsize_t size)
{
    haddr_t eoa;                /* End-of-allocation in the file */
    haddr_t space_avail;        /* Unallocated space still available in file */
    hbool_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5MF_alloc_overflow)

    /* Start with the current end of the file's address. */
    eoa = H5F_get_eoa(f);
    HDassert(H5F_addr_defined(eoa));

    /* Subtract EOA from the file's maximum address to get the actual amount of
     * addressable space left in the file.
     */
    HDassert(f->shared->maxaddr >= eoa);
    space_avail = (hsize_t)(f->shared->maxaddr - eoa);

    /* Ensure that there's enough room left in the file for something of this size */
    if(size > space_avail)
        ret_value = TRUE;
    else
        ret_value = FALSE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_alloc_overflow() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_can_extend
 *
 * Purpose:	Check if a block in the file can be extended.
 *
 * Return:	Success:	TRUE(1)/FALSE(0)
 * 		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *              Friday, June 11, 2004
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MF_can_extend(H5F_t *f, H5FD_mem_t type, haddr_t addr, hsize_t size, hsize_t extra_requested)
{
    htri_t	ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5MF_can_extend, FAIL)

    /* Convert old relative address to absolute address */
    addr += H5F_BASE_ADDR(f);

    /* Pass the request down to the virtual file layer */
    if((ret_value = H5FD_can_extend(f->shared->lf, type, addr, size, extra_requested)) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate new file memory");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_can_extend() */


/*-------------------------------------------------------------------------
 * Function:	H5MF_extend
 *
 * Purpose:	Extend a block in the file.
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, June 12, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MF_extend(H5F_t *f, H5FD_mem_t type, haddr_t addr, hsize_t size, hsize_t extra_requested)
{
    herr_t	ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5MF_extend, FAIL)

    /* Convert relative address to absolute address */
    addr += H5F_BASE_ADDR(f);

    /* Pass the request down to the virtual file layer */
    if((ret_value = H5FD_extend(f->shared->lf, type, addr, size, extra_requested)) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate new file memory")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MF_extend() */

