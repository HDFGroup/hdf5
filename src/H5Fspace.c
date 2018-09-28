/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Fspace.c
 *			Dec 30 2013
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Space allocation routines for the file.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h"          /* This source code file is part of the H5F module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"         	/* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/


/****************/
/* Local Macros */
/****************/

/* Define this to display debugging information for VFD SWMR */
/* #define H5F_VFD_SWMR_DEBUG */



/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static haddr_t H5F__extend_md(H5F_t *f, hsize_t size);


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
 * Function:    H5F__alloc
 *
 * Purpose:     Wrapper for H5FD_alloc, to make certain EOA changes are
 *		reflected in superblock.
 *
 * Note:	When the metadata cache routines are updated to allow
 *		marking an entry dirty without a H5F_t*, this routine should
 *		be changed to take a H5F_super_t* directly.
 *
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Quincey Koziol
 *              Monday, December 30, 2013
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5F__alloc(H5F_t *f, H5F_mem_t type, hsize_t size, haddr_t *frag_addr, hsize_t *frag_size)
{
    haddr_t     ret_value = 0;          /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

    /* Check whether the file can use temporary addresses */
    if(f->shared->use_tmp_space) {
        haddr_t	eoa;            /* Current EOA for the file */

        /* Get the EOA for the file */
        if(HADDR_UNDEF == (eoa = H5F_get_eoa(f, type)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, HADDR_UNDEF, "Unable to get eoa")

        /* Check for overlapping into file's temporary allocation space */
        if(H5F_addr_gt((eoa + size), f->shared->tmp_addr))
            HGOTO_ERROR(H5E_FILE, H5E_BADRANGE, HADDR_UNDEF, "'normal' file space allocation request will overlap into 'temporary' file space")
    } /* end if */

    /* Call the file driver 'alloc' routine */
    ret_value = H5FD_alloc(f->shared->lf, type, f, size, frag_addr, frag_size);
    if(!H5F_addr_defined(ret_value))
        HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, HADDR_UNDEF, "file driver 'alloc' request failed")

    /* Mark EOA dirty */
    if(H5F_eoa_dirty(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, HADDR_UNDEF, "unable to mark EOA as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5F__free
 *
 * Purpose:     Wrapper for H5FD_free, to make certain EOA changes are
 *		reflected in superblock.
 *
 * Note:	When the metadata cache routines are updated to allow
 *		marking an entry dirty without a H5F_t*, this routine should
 *		be changed to take a H5F_super_t* directly.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Monday, December 30, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__free(H5F_t *f, H5FD_mem_t type, haddr_t addr, hsize_t size)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

    /* Call the file driver 'free' routine */
    if(H5FD_free(f->shared->lf, type, f, addr, size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "file driver 'free' request failed")

    /* Mark EOA dirty */
    if(H5F_eoa_dirty(f) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTMARKDIRTY, FAIL, "unable to mark EOA as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__free() */


/*-------------------------------------------------------------------------
 * Function:    H5F__try_extend
 *
 * Purpose:	Extend a block at the end of the file, if possible.
 *
 * Note:	When the metadata cache routines are updated to allow
 *		marking an entry dirty without a H5F_t*, this routine should
 *		be changed to take a H5F_super_t* directly.
 *
 * Return:	Success:	TRUE(1)  - Block was extended
 *                              FALSE(0) - Block could not be extended
 * 		Failure:	FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, 30 December, 2013
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5F__try_extend(H5F_t *f, H5FD_mem_t type, haddr_t blk_end, hsize_t extra_requested)
{
    htri_t ret_value = FALSE;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(extra_requested > 0);

    /* Extend the object by extending the underlying file */
    if((ret_value = H5FD_try_extend(f->shared->lf, type, f, blk_end, extra_requested)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTEXTEND, FAIL, "driver try extend request failed")

    /* H5FD_try_extend() updates driver message and marks the superblock
     * dirty, so no need to do it again here.
     */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__try_extend() */


/*
 * VFD SWMR
 */

/*-------------------------------------------------------------------------
 * Function:    H5F__free_md
 *
 * Purpose:     Release space at the end of the metadata file's allocated space
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F__free_md(H5F_t *f, haddr_t addr, hsize_t size)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(size > 0);

     /* Sanity checking */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid file offset")

    if(addr > f->shared->maxaddr || H5F_addr_overflow(addr, size) || (addr + size) > f->shared->maxaddr)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid file free space region to free")

    /* Check if this free block is at the end of file allocated space.
     * Truncate it if this is true.
     */
    if(f->shared->vfd_swmr_md_eoa == (addr + size))
        f->shared->vfd_swmr_md_eoa = addr;
    else {
        /* leak memory */
#ifdef H5F_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: LEAKED MEMORY!!! addr = %a, size = %Hu\n", FUNC, addr, size);
#endif
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__free_md() */


/*-------------------------------------------------------------------------
 * Function:    H5F__alloc_md
 *
 * Purpose:     Allocate space at the end of the metadata file 
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5F__alloc_md(H5F_t *f, hsize_t size)
{
    haddr_t ret_value = HADDR_UNDEF;    /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(size > 0);

    /* Extend the EOA space of the metadata file */
    ret_value = H5F__extend_md(f, size);

    if(!H5F_addr_defined(ret_value))
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "driver eoa update request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5F__alloc_md() */

/*-------------------------------------------------------------------------
 * Function:    H5F__try_extend_md
 *
 * Purpose:     Try to extend a block at the end of the metadata file, if possible.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5F__try_extend_md(H5F_t *f, haddr_t blk_end, hsize_t extra_requested)
{
    htri_t ret_value = FALSE;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(f);
    HDassert(f->shared);
    HDassert(extra_requested > 0);

    /* Check if the block is exactly at the end of the file */
    if(H5F_addr_eq(blk_end, f->shared->vfd_swmr_md_eoa)) {

         /* Extend the EOA space of the metadata file */
        if(HADDR_UNDEF == H5F__extend_md(f, extra_requested))
            HGOTO_ERROR(H5E_FILE, H5E_CANTEXTEND, FAIL, "driver extend request failed")

        /* Indicate success */
        ret_value = TRUE;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__try_extend_md() */

/*-------------------------------------------------------------------------
 * Function:    H5F__extend_md
 *
 * Purpose:     Extend the EOA space of the metadata file.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static haddr_t 
H5F__extend_md(H5F_t *f, hsize_t size)
{
    haddr_t eoa;
    haddr_t ret_value = HADDR_UNDEF;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get current end-of-allocated space address */
    eoa = f->shared->vfd_swmr_md_eoa;

    /* Check for overflow when extending */
    if(H5F_addr_overflow(eoa, size) || (eoa + size) > f->shared->maxaddr)
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

    /* Set the address to return */
    ret_value = eoa;

    /* Extend the end-of-allocated space address */
    f->shared->vfd_swmr_md_eoa += size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}
