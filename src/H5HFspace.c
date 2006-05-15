/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5HFspace.c
 *			May  2 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Space allocation routines for fractal heaps.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/


/****************/
/* Local Macros */
/****************/

#define H5HF_FSPACE_SHRINK      80              /* Percent of "normal" size to shrink serialized free space size */
#define H5HF_FSPACE_EXPAND      120             /* Percent of "normal" size to expand serialized free space size */

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



/*-------------------------------------------------------------------------
 * Function:	H5HF_space_start
 *
 * Purpose:	"Start up" free space for heap - open existing free space
 *              structure if one exists, otherwise create a new free space
 *              structure
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_space_start(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    const H5FS_section_class_t *classes[] = { /* Free space section classes implemented for fractal heap */
        H5FS_SECT_CLS_FHEAP_SINGLE,
        H5FS_SECT_CLS_FHEAP_RANGE,
        H5FS_SECT_CLS_FHEAP_INDIRECT};
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_space_start)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Allocate space for the section classes for the free list management code to use */
    hdr->nsect_classes = NELMTS(classes);
    if(NULL == (hdr->sect_cls = H5FL_SEQ_MALLOC(H5FS_section_class_t, hdr->nsect_classes)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for free space section class array ")

    /* Copy the section classes for this heap */
    /* (Initialization calls will be performed by the free list code) */
    for(u = 0; u < hdr->nsect_classes; u++) {
        /* Make certain that section class type can be used as an array index into this array */
        HDassert(u == classes[u]->type);

        HDmemcpy(&hdr->sect_cls[u], classes[u], sizeof(H5FS_section_class_t));
    } /* end for */

    /* Check for creating free space info for the heap */
    if(H5F_addr_defined(hdr->fs_addr)) {
        /* Open an existing free space structure for the heap */
        if(NULL == (hdr->fspace = H5FS_open(hdr->f, dxpl_id, hdr->fs_addr,
                H5HF_free_section_free_cb, hdr->nsect_classes, hdr->sect_cls, hdr)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize free space info")
    } /* end if */
    else {
        H5FS_create_t fs_create;        /* Free space creation parameters */

        /* Set the free space creation parameters */
        fs_create.client = H5FS_CLIENT_FHEAP_ID;
        fs_create.shrink_percent = H5HF_FSPACE_SHRINK;
        fs_create.expand_percent = H5HF_FSPACE_EXPAND;
        fs_create.max_sect_size = hdr->man_dtable.cparam.max_direct_size;
        fs_create.max_sect_addr = hdr->man_dtable.cparam.max_index;

        /* Create the free space structure for the heap */
        if(NULL == (hdr->fspace = H5FS_create(hdr->f, dxpl_id, &hdr->fs_addr,
                &fs_create, H5HF_free_section_free_cb,
                hdr->nsect_classes, hdr->sect_cls, hdr)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize free space info")
    } /* end else */

    /* Free space for heap is now open */
    hdr->fspace_open = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_space_start() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_space_find
 *
 * Purpose:	Attempt to find space in a fractal heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5HF_space_find(H5HF_hdr_t *hdr, hid_t dxpl_id, hsize_t request, H5HF_free_section_t **node)
{
    htri_t node_found;          /* Whether an existing free list node was found */
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_space_find)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(request);
    HDassert(node);

    /* Check if the free space for the heap has been initialized */
    if(!hdr->fspace_open)
        if(H5HF_space_start(hdr, dxpl_id) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize heap free space")

    /* Search for free space in the heap */
    if((node_found = H5FS_find(hdr->f, dxpl_id, hdr->fspace, request, (H5FS_section_info_t **)node)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap")

    /* Set return value */
    ret_value = node_found;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_space_find() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_space_add
 *
 * Purpose:	Add a section to the free space for the heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_space_add(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *node)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_space_add)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(node);
    HDassert(hdr->fspace_open);
    HDassert(hdr->fspace);

    /* Add to the free space for the heap */
    if(H5FS_add(hdr->f, dxpl_id, hdr->fspace, (H5FS_section_info_t *)node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "can't add section to heap free space")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_space_add() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_space_close
 *
 * Purpose:	Close the free space for the heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_space_close(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_space_close)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Check if the free space was ever opened */
    if(hdr->fspace_open) {
        /* Sanity check */
        HDassert(hdr->fspace);

        /* Close the free space for the heap */
        if(H5FS_close(hdr->f, dxpl_id, hdr->fspace) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release free space info")
        hdr->fspace = NULL;

        /* Release the memory for the free space section classes */
        H5FL_SEQ_FREE(H5FS_section_class_t, hdr->sect_cls);

        /* Free space is now closed */
        hdr->fspace_open = FALSE;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_space_close() */

