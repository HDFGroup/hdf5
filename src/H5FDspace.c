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
 * Created:		H5FDspace.c
 *			Jan  3 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Space allocation routines for the file.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5FD_PACKAGE		/*suppress error about including H5FDpkg  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_space_init_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"         /* File access				*/
#include "H5FDpkg.h"		/* File Drivers				*/
#include "H5FDmulti.h"		/* Usage-partitioned file family	*/
#include "H5MMprivate.h"	/* Memory management			*/


/****************/
/* Local Macros */
/****************/

/* Define this to display information about file allocations */
/* #define H5FD_ALLOC_DEBUG */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5FD_free_t struct */
H5FL_DEFINE(H5FD_free_t);



/*--------------------------------------------------------------------------
NAME
   H5FD_space_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_space_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_init_iterface currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_space_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_space_init_interface)

    FUNC_LEAVE_NOAPI(H5FD_init())
} /* H5FD_space_init_interface() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_extend
 *
 * Purpose:     Extend the EOA space of a file.
 *
 * NOTE:        Returns absolute file offset
 *
 * Return:      Success:    The address of the previous EOA.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Bill Wendling
 *              Wednesday, 04. December, 2002
 *
 * Modifications:
 *	Vailin Choi, 29th July 2008
 *	  Add 2 more parameters for handling alignment: address and size of the fragment
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_extend(H5FD_t *file, H5FD_mem_t type, hbool_t new_block, hsize_t size, haddr_t *frag_addr, hsize_t *frag_size)
{
    haddr_t eoa;                /* Address of end-of-allocated space */
    hsize_t extra;        	/* Extra space to allocate, to align request */
    haddr_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FD_extend)

    /* check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

    /* Get current end-of-allocated space address */
    eoa = file->cls->get_eoa(file, type);

    /* Compute extra space to allocate, if this is a new block and should be aligned */
    extra = 0;
    if(new_block && file->alignment > 1 && size >= file->threshold) {
        hsize_t mis_align;              /* Amount EOA is misaligned */

        /* Check for EOA already aligned */
        if((mis_align = (eoa % file->alignment)) > 0) {
            extra = file->alignment - mis_align;
	    if (frag_addr) *frag_addr = eoa;
	    if (frag_size) *frag_size = extra;
	}
    } /* end if */

    /* Add in extra allocation amount */
    size += extra;

    /* Check for overflow when extending */
    if(H5F_addr_overflow(eoa, size) || (eoa + size) > file->maxaddr)
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

    /* Set the [possibly aligned] address to return */
    ret_value = eoa + extra;

    /* Extend the end-of-allocated space address */
    eoa += size;
    if(file->cls->set_eoa(file, type, eoa) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "file allocation request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_extend() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_alloc
 *
 * Purpose:     Private version of H5FDalloc().
 *
 * Return:      Success:    The format address of the new file memory.
 *              Failure:    The undefined address HADDR_UNDEF
 *
 * Programmer:  Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modification:
 *	Vailin Choi, 29th July 2008
 *	  Add two more parameters () for handling alignment: address & size of the fragment
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_alloc(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type, hsize_t size, haddr_t *frag_addr, hsize_t *frag_size)
{
    haddr_t     ret_value = HADDR_UNDEF;

    FUNC_ENTER_NOAPI(H5FD_alloc, HADDR_UNDEF)
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, size = %Hu\n", FUNC, (unsigned)type, size);
#endif /* H5FD_ALLOC_DEBUG */

    /* check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

    /* Dispatch to driver `alloc' callback or extend the end-of-address marker */
    if(file->cls->alloc) {
        if((ret_value = (file->cls->alloc)(file, type, dxpl_id, size)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "driver allocation request failed")
    } /* end if */
    else {
        if((ret_value = H5FD_extend(file, type, TRUE, size, frag_addr, frag_size)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_VFL, H5E_NOSPACE, HADDR_UNDEF, "driver eoa update request failed")
    } /* end else */

    /* Convert absolute file offset to relative address */
    ret_value -= file->base_addr;

done:
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: ret_value = %a\n", FUNC, ret_value);
#endif /* H5FD_ALLOC_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_alloc() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_free
 *
 * Purpose:     Private version of H5FDfree()
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_free(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type, haddr_t addr, hsize_t size)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_free, FAIL)

    /* Check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(size > 0);

#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)type, addr, size);
#endif /* H5FD_ALLOC_DEBUG */

    /* Sanity checking */
    if(!H5F_addr_defined(addr))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file offset")

    /* Convert address to absolute file offset */
    addr += file->base_addr;

    /* More sanity checking */
    if(addr > file->maxaddr || H5F_addr_overflow(addr, size) || (addr + size) > file->maxaddr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file free space region to free")

    /* Check for file driver 'free' callback and call it if available */
    if(file->cls->free) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Letting VFD free space\n", FUNC);
#endif /* H5FD_ALLOC_DEBUG */
        if((file->cls->free)(file, type, dxpl_id, addr, size) < 0)
            HGOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "driver free request failed")
    } /* end if */
    /* Check if this free block is at the end of file allocated space.
     * Truncate it if this is true.
     */
    else if(file->cls->get_eoa) {
        haddr_t     eoa;

        eoa = file->cls->get_eoa(file, type);
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: eoa = %a\n", FUNC, eoa);
#endif /* H5FD_ALLOC_DEBUG */
        if(eoa == (addr + size)) {
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: Reducing file size to = %a\n", FUNC, addr);
#endif /* H5FD_ALLOC_DEBUG */
            if(file->cls->set_eoa(file, type, addr) < 0)
                HGOTO_ERROR(H5E_VFL, H5E_CANTSET, FAIL, "set end of space allocation request failed")
        } /* end if */
    } /* end else-if */
    else {
        /* leak memory */
#ifdef H5FD_ALLOC_DEBUG
HDfprintf(stderr, "%s: LEAKED MEMORY!!! type = %u, addr = %a, size = %Hu\n", FUNC, (unsigned)type, addr, size);
#endif /* H5FD_ALLOC_DEBUG */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_free() */


/*-------------------------------------------------------------------------
 * Function:    H5FD_try_extend
 *
 * Purpose:	Extend a block at the end of the file, if possible.
 *
 * Return:	Success:	TRUE(1)  - Block was extended
 *                              FALSE(0) - Block could not be extended
 * 		Failure:	FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, 17. January, 2008
 *
 * Modification:
 *	Vailin Choi, 29th July 2008
 *	  Two more parameters were added to FD_extend() for handling alignment
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FD_try_extend(H5FD_t *file, H5FD_mem_t type, haddr_t blk_end, hsize_t extra_requested)
{
    haddr_t eoa;                /* End of allocated space in file */
    htri_t ret_value = FALSE;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_try_extend, FAIL)

    /* check args */
    HDassert(file);
    HDassert(file->cls);
    HDassert(type >= H5FD_MEM_DEFAULT && type < H5FD_MEM_NTYPES);
    HDassert(extra_requested > 0);

    /* Retrieve the end of the address space */
    if(HADDR_UNDEF == (eoa = file->cls->get_eoa(file, type)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "driver get_eoa request failed")

    /* Adjust block end by base address of the file, to create absolute address */
    blk_end += file->base_addr;

    /* Check if the block is exactly at the end of the file */
    if(H5F_addr_eq(blk_end, eoa)) {
        /* Extend the object by extending the underlying file */
        if(HADDR_UNDEF == H5FD_extend(file, type, FALSE, extra_requested, NULL, NULL))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "driver extend request failed")

        /* Indicate success */
        HGOTO_DONE(TRUE)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_try_extend() */

