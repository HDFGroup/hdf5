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
 * Created:             H5MV.c
 *
 * Purpose:             Free-space manager for VFD SWMR's metadata file
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_FRIEND		    /*suppress error about including H5Fpkg	  */
#define H5FS_FRIEND		    /*suppress error about including H5Fpkg	  */
#include "H5MVmodule.h"     /* This source code file is part of the H5MV module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"         /* File access				*/
#include "H5FSpkg.h"        /* File free space                      */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MVpkg.h"		/* File memory management		*/
#include "H5VMprivate.h"	/* Vectors and arrays 			*/


/****************/
/* Local Macros */
/****************/

#define H5MV_FSPACE_SHRINK      80              /* Percent of "normal" size to shrink serialized free space size */
#define H5MV_FSPACE_EXPAND      120             /* Percent of "normal" size to expand serialized free space size */

/******************/
/* Local Typedefs */
/******************/

/* User data for section info iterator callback for iterating over free space sections */
typedef struct {
    H5F_sect_info_t *sects;     /* section info to be retrieved */
    size_t sect_count;          /* # of sections requested */
    size_t sect_idx;            /* the current count of sections */
} H5MV_sect_iter_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5MV__create()
 *
 * Purpose:	    Create free space manager for the metadata file by creating
 *              a free-space structure
 *
 * Return:	    Success:	non-negative
 *		        Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MV__create(H5F_t *f)
{
    /* Free space section classes implemented for file */
    const H5FS_section_class_t *classes[] = { H5MV_FSPACE_SECT_CLS_SIMPLE };
    H5FS_create_t fs_create;    /* Free space creation parameters */
    herr_t ret_value = SUCCEED;             /* Return value */

    FUNC_ENTER_PACKAGE

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->fs_state_md == H5F_FS_STATE_CLOSED);

    /* Set the free space creation parameters */
    fs_create.client = H5FS_CLIENT_MD_VFD_ID;
    fs_create.shrink_percent = H5MV_FSPACE_SHRINK;
    fs_create.expand_percent = H5MV_FSPACE_EXPAND;
    fs_create.max_sect_addr = 1 + H5VM_log2_gen((uint64_t)f->shared->maxaddr);
    fs_create.max_sect_size = f->shared->maxaddr;

    if(NULL == (f->shared->fs_man_md = H5FS_create(f, NULL, &fs_create, NELMTS(classes), classes, f, f->shared->fs_page_size, f->shared->fs_page_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space info")

    /* Set the state for the free space manager to "open", if it is now */
    if(f->shared->fs_man_md)
        f->shared->fs_state_md = H5F_FS_STATE_OPEN;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV__create() */


/*-------------------------------------------------------------------------
 * Function:    H5MV_close
 *
 * Purpose:     Close the free space manager for the metadata file
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MV_close(H5F_t *f)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(f->shared);
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Trying to close free space manager\n", FUNC);
#endif

    /* Close an existing free space structure for the file */
    if(f->shared->fs_man_md) {
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Going to close free space manager\n", FUNC);
#endif
        HDassert(f->shared->fs_state_md != H5F_FS_STATE_CLOSED);

        if(H5FS_close(f, f->shared->fs_man_md) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't release free space info")
    }

    f->shared->fs_man_md = NULL;
    f->shared->fs_state_md = H5F_FS_STATE_CLOSED;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV_close() */



/*-------------------------------------------------------------------------
 * Function:    H5MV__find_sect
 *
 * Purpose:	    To find a section from the specified free-space manager 
 *              to fulfill the request.
 *		        If found, re-add the left-over space back to the manager.
 *
 * Return:	    TRUE if a section is found to fulfill the request
 *		        FALSE if not
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MV__find_sect(H5F_t *f, hsize_t size, H5FS_t *fspace, haddr_t *addr)
{
    H5MV_free_section_t *node;              /* Free space section pointer */
    htri_t ret_value = FAIL;      	    /* Whether an existing free list node was found */

    FUNC_ENTER_PACKAGE

    HDassert(f);
    HDassert(fspace);

    /* Try to get a section from the free space manager */
    if((ret_value = H5FS_sect_find(f, fspace, size, (H5FS_section_info_t **)&node)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "error locating free space in file")

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: section found = %t\n", FUNC, ret_value);
#endif

    /* Check for actually finding section */
    if(ret_value) {
        /* Sanity check */
        HDassert(node);

        /* Retrieve return value */
        if(addr)
            *addr = node->sect_info.addr;

        /* Check for eliminating the section */
        if(node->sect_info.size == size) {
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: freeing node\n", FUNC);
#endif

            /* Free section node */
            if(H5MV__sect_free(&node->sect_info) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")
        } /* end if */
        else {
            /* Adjust information for section */
            node->sect_info.addr += size;
            node->sect_info.size -= size;

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: adding node, node->sect_info.addr = %a, node->sect_info.size = %Hu\n", FUNC, node->sect_info.addr, node->sect_info.size);
#endif

            /* Re-add the section to the free-space manager */
            if(H5FS_sect_add(f, fspace, &node->sect_info, H5FS_ADD_RETURNED_SPACE, f) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, FAIL, "can't re-add section to file free space")
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV__find_sect() */


/*-------------------------------------------------------------------------
 * Function:    H5MV_alloc
 *
 * Purpose:     Allocate SIZE bytes of file memory and return the relative
 *		        address where that contiguous chunk of file memory exists.
 *
 * Return:      Success:        The file address of new chunk.
 *              Failure:        HADDR_UNDEF
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5MV_alloc(H5F_t *f, hsize_t size)
{
    haddr_t eoa;                        /* EOA for the file */
    hsize_t frag_size = 0;              /* Fragment size */
    hsize_t misalign_size = 0;          /* Mis-aligned size */
    H5MV_free_section_t *node = NULL;   /* Free space section pointer */
    haddr_t ret_value = HADDR_UNDEF;    /* Return value */

    FUNC_ENTER_NOAPI(HADDR_UNDEF)
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: size = %Hu\n", FUNC, size);
#endif

    /* check arguments */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->vfd_swmr_md_fd >= 0);
    HDassert(size > 0);

    /* Search for large enough space in the free space manager */
    if(f->shared->fs_man_md != NULL) {
        if(H5MV__find_sect(f, size, f->shared->fs_man_md, &ret_value) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, HADDR_UNDEF, "error locating a node")
    }

    /* If no space is found from the free-space manager or no free-space manager, extend md's EOF */
    if(!H5F_addr_defined(ret_value)) {

        /* Get the EOA for the metadata file */
        if(HADDR_UNDEF == (eoa = H5F_get_vfd_swmr_md_eoa(f)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, HADDR_UNDEF, "Unable to get eoa for VFD SWMR metadata file")

        /* If EOA is mis-aligned, calculate the fragment size */
        if(H5F_addr_gt(eoa, 0) && (misalign_size = eoa % f->shared->fs_page_size))
            frag_size = f->shared->fs_page_size - misalign_size;

        /* Allocate from end of file */
        if(HADDR_UNDEF == (ret_value = H5F__alloc_md(f, size + frag_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, HADDR_UNDEF, "allocation failed")

        /* If there is a mis-aligned fragment at EOA */
        if(frag_size) {
            /* Start up the free-space manager if not so */
            if(f->shared->fs_man_md == NULL) {
                if(H5MV__create(f) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, HADDR_UNDEF, "can't initialize free space manager")
            }
            HDassert(f->shared->fs_man_md);

            /* Create the free-space section for the fragment */
            if(NULL == (node = H5MV__sect_new(eoa, frag_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, HADDR_UNDEF, "can't initialize free space section")

            /* Add the section */
            if(H5FS_sect_add(f, f->shared->fs_man_md, &node->sect_info, H5FS_ADD_RETURNED_SPACE, f) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, HADDR_UNDEF, "can't re-add section to file free space")

            node = NULL;
        }
        ret_value += frag_size;

    } /* end if */

    HDassert(H5F_addr_defined(ret_value));

done:
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Leaving: ret_value = %a, size = %Hu\n", FUNC, ret_value, size);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV_alloc() */



/*-------------------------------------------------------------------------
 * Function:    H5MV_free
 *
 * Purpose:     Frees part of a file, making that part of the file
 *              available for reuse.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MV_free(H5F_t *f, haddr_t addr, hsize_t size)
{
    H5MV_free_section_t *node = NULL;   /* Free space section pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Entering - addr = %a, size = %Hu\n", FUNC, addr, size);
#endif

    /* check arguments */
    HDassert(f);
    if(!H5F_addr_defined(addr) || 0 == size)
        HGOTO_DONE(SUCCEED)
    HDassert(addr != 0);


    /* Check if the free space manager for the file has been initialized */
    if(f->shared->fs_man_md == NULL) {
        /* If there's no free space manager for objects of this type,
         *  see if we can avoid creating one by checking if the freed
         *  space is at the end of the file
         */
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: fs_addr = %a\n", FUNC, f->shared->fs_man_md);
#endif
        htri_t status;          /* "can absorb" status for section into */

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Trying to avoid starting up free space manager\n", FUNC);
#endif
         /* Try to shrink the file */
         if((status = H5MV_try_shrink(f, addr, size)) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTMERGE, FAIL, "can't check for absorbing block")
         else if(status > 0)
            /* Indicate success */
            HGOTO_DONE(SUCCEED)

        /* If we are deleting the free space manager, leave now, to avoid
         *  [re-]starting it: dropping free space section on the floor.
         */
        if(f->shared->fs_state_md == H5F_FS_STATE_DELETING) {
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: dropping addr = %a, size = %Hu, on the floor!\n", FUNC, addr, size);
#endif
            HGOTO_DONE(SUCCEED)
        } /* end if */

        /* There's either already a free space manager, or the freed
         *  space isn't at the end of the file, so start up (or create)
         *  the file space manager
         */
        if(H5MV__create(f) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space manager")
    } /* end if */

    /* Create the free-space section for the freed section */
    if(NULL == (node = H5MV__sect_new(addr, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space section")

	HDassert(f->shared->fs_man_md);

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Before H5FS_sect_add(): addr=%a, size=%Hu\n", FUNC, addr, size);
#endif

     /* Add the section */
    if(H5FS_sect_add(f, f->shared->fs_man_md, &node->sect_info, H5FS_ADD_RETURNED_SPACE, f) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINSERT, FAIL, "can't re-add section to file free space")

    node = NULL;

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: After H5FS_sect_add()\n", FUNC);
#endif

done:
    /* Release section node, if allocated and not added to section list or merged */
    if(node)
        if(H5MV__sect_free(&node->sect_info) < 0)
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV_free() */


/*-------------------------------------------------------------------------
 * Function:	H5MV_try_extend
 *
 * Purpose:	    Extend a block at EOA in the file if possible.
 *
 * Return:	    Success:	TRUE(1)  -  Block was extended
 *                          FALSE(0) -  Block could not be extended
 * 		        Failure:	FAIL
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MV_try_extend(H5F_t *f, haddr_t addr, hsize_t size, hsize_t extra_requested)
{
    haddr_t end;                    /* End of block to extend */
    htri_t ret_value = FALSE;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Entering: addr = %a, size = %Hu, extra_requested = %Hu\n", FUNC, addr, size, extra_requested);
#endif

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_INTENT(f) & H5F_ACC_RDWR);

    /* Compute end of block to extend */
    end = addr + size;

    /* Try extending the block at EOA */
    if((ret_value = H5F__try_extend_md(f, end, extra_requested)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending file")
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: extended = %t\n", FUNC, ret_value);
#endif

    /* If no extension so far, try to extend into a free-space section */
    if(ret_value == FALSE) {

        /* Try to extend the block into a free-space section */
        if(f->shared->fs_man_md) {
            if((ret_value = H5FS_sect_try_extend(f, f->shared->fs_man_md, addr, size, extra_requested, H5FS_ADD_RETURNED_SPACE, NULL)) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTEXTEND, FAIL, "error extending block in free space manager")
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Try to H5FS_sect_try_extend = %t\n", FUNC, ret_value);
#endif
        } /* end if */

    } /* end if */

done:
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Leaving: ret_value = %t\n", FUNC, ret_value);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV_try_extend() */


/*-------------------------------------------------------------------------
 * Function:    H5MV_try_shrink
 *
 * Purpose:     Try to shrink the size of a file with a block 
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MV_try_shrink(H5F_t *f, haddr_t addr, hsize_t size)
{
    H5MV_free_section_t *node = NULL;   /* Free space section pointer */
    htri_t ret_value = FALSE;               /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Entering - addr = %a, size = %Hu\n", FUNC, addr, size);
#endif

    /* check arguments */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(H5F_addr_defined(addr));
    HDassert(size > 0);

    /* Create free-space section for block */
    if(NULL == (node = H5MV__sect_new(addr, size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't initialize free space section")

    /* Check if the block can shrink the container */
    if((ret_value = H5MV__sect_can_shrink(&node->sect_info, f)) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTMERGE, FAIL, "can't check if section can shrink container")
    else if(ret_value > 0) {
        /* Shrink or absorb the section */
        if(H5MV__sect_shrink((H5FS_section_info_t **)&node, NULL) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSHRINK, FAIL, "can't shrink container")
    } /* end if */

done:
    /* Free section node allocated */
    if(node && H5MV__sect_free(&node->sect_info) < 0)
        HDONE_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")

#ifdef H5MV_VFD_SWMR_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV_try_shrink() */

/*
 * If need to find out the amount of free-space in the file, port routines in H5MF.c
 */
