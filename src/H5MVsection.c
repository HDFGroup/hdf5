/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5MVsection.c
 *
 * Purpose:             Free-space section callbacks for VFD SWMR's metadata
 *                      file
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_FRIEND      /*suppress error about including H5Fpkg	  */
#include "H5MVmodule.h" /* This source code file is part of the H5MF module */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions			*/
#include "H5Eprivate.h" /* Error handling		  	*/
#include "H5Fpkg.h"     /* File access				*/
#include "H5MVpkg.h"    /* File memory management		*/

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

/* 'simple' section callbacks */
static htri_t H5MV__sect_can_merge(const H5FS_section_info_t *sect1, const H5FS_section_info_t *sect2,
                                   void *udata);
static herr_t H5MV__sect_merge(H5FS_section_info_t **sect1, H5FS_section_info_t *sect2, void *udata);
static herr_t H5MV__sect_valid(const H5FS_section_class_t *cls, const H5FS_section_info_t *sect);
static H5FS_section_info_t *H5MV__sect_split(H5FS_section_info_t *sect, hsize_t frag_size);

/*********************/
/* Package Variables */
/*********************/

/* Class info for "simple" free space sections */
H5FS_section_class_t H5MV_FSPACE_SECT_CLS_SIMPLE[1] = {{
    /* Class variables */
    H5MV_FSPACE_SECT_SIMPLE,                                      /* Section type                 */
    0,                                                            /* Extra serialized size        */
    H5FS_CLS_MERGE_SYM | H5FS_CLS_ADJUST_OK | H5FS_CLS_GHOST_OBJ, /* Class flags                  */
    NULL,                                                         /* Class private info           */

    /* Class methods */
    NULL, /* Initialize section class     */
    NULL, /* Terminate section class      */

    /* Object methods */
    NULL,                  /* Add section                  */
    NULL,                  /* Serialize section            */
    NULL,                  /* Deserialize section          */
    H5MV__sect_can_merge,  /* Can sections merge?          */
    H5MV__sect_merge,      /* Merge sections               */
    H5MV__sect_can_shrink, /* Can section shrink container?*/
    H5MV__sect_shrink,     /* Shrink container w/section   */
    H5MV__sect_free,       /* Free section                 */
    H5MV__sect_valid,      /* Check validity of section    */
    H5MV__sect_split,      /* Split section node for alignment */
    NULL,                  /* Dump debugging for section   */
}};

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5MF_free_section_t struct */
H5FL_DEFINE(H5MV_free_section_t);

/*
 * "simple" section callbacks
 */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_new
 *
 * Purpose:	    Create a new section and return it to the caller
 *
 * Return:	    Pointer to new section on success/NULL on failure
 *
 *-------------------------------------------------------------------------
 */
H5MV_free_section_t *
H5MV__sect_new(haddr_t sect_off, hsize_t sect_size)
{
    H5MV_free_section_t *sect;             /* 'Simple' free space section to add */
    H5MV_free_section_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments.  */
    HDassert(sect_size);

    /* Create free space section node */
    if (NULL == (sect = H5FL_MALLOC(H5MV_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free section node")

    /* Set the information passed in */
    sect->sect_info.addr = sect_off;
    sect->sect_info.size = sect_size;

    /* Set the section's class & state */
    sect->sect_info.type  = H5MV_FSPACE_SECT_SIMPLE;
    sect->sect_info.state = H5FS_SECT_LIVE;

    /* Set return value */
    ret_value = sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MV__sect_new() */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_free
 *
 * Purpose:	    Free a 'simple' section node
 *
 * Return:	    Success:	non-negative
 *		        Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MV__sect_free(H5FS_section_info_t *_sect)
{
    H5MV_free_section_t *sect = (H5MV_free_section_t *)_sect; /* File free section */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments. */
    HDassert(sect);

    /* Release the section */
    sect = H5FL_FREE(H5MV_free_section_t, sect);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5MV__sect_free() */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_can_merge
 *
 * Purpose:	    Can two sections of this type merge?
 *
 * Note:        Second section must be "after" first section
 *
 * Return:	    Success:	non-negative (TRUE/FALSE)
 *		        Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5MV__sect_can_merge(const H5FS_section_info_t *_sect1, const H5FS_section_info_t *_sect2,
                     void H5_ATTR_UNUSED *_udata)
{
    const H5MV_free_section_t *sect1     = (const H5MV_free_section_t *)_sect1; /* File free section */
    const H5MV_free_section_t *sect2     = (const H5MV_free_section_t *)_sect2; /* File free section */
    htri_t                     ret_value = FAIL;                                /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(sect1->sect_info.type == sect2->sect_info.type); /* Checks "MERGE_SYM" flag */
    HDassert(H5F_addr_lt(sect1->sect_info.addr, sect2->sect_info.addr));

    /* Check if second section adjoins first section */
    ret_value = H5F_addr_eq(sect1->sect_info.addr + sect1->sect_info.size, sect2->sect_info.addr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MV__sect_can_merge() */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_merge
 *
 * Purpose:	    Merge two sections of this type
 *
 * Note:        Second section always merges into first node
 *
 * Return:	    Success:	non-negative
 *		        Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MV__sect_merge(H5FS_section_info_t **_sect1, H5FS_section_info_t *_sect2, void H5_ATTR_UNUSED *_udata)
{
    H5MV_free_section_t **sect1     = (H5MV_free_section_t **)_sect1; /* File free section */
    H5MV_free_section_t * sect2     = (H5MV_free_section_t *)_sect2;  /* File free section */
    herr_t                ret_value = SUCCEED;                        /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(sect1);
    HDassert((*sect1)->sect_info.type == H5MV_FSPACE_SECT_SIMPLE);
    HDassert(sect2);
    HDassert(sect2->sect_info.type == H5MV_FSPACE_SECT_SIMPLE);
    HDassert(H5F_addr_eq((*sect1)->sect_info.addr + (*sect1)->sect_info.size, sect2->sect_info.addr));

    /* Add second section's size to first section */
    (*sect1)->sect_info.size += sect2->sect_info.size;

    /* Get rid of second section */
    if (H5MV__sect_free((H5FS_section_info_t *)sect2) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MV__sect_merge() */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_can_shrink
 *
 * Purpose:	    Can this section shrink the container?
 *
 * Return:	    Success:	non-negative (TRUE/FALSE)
 *		        Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5MV__sect_can_shrink(const H5FS_section_info_t *_sect, void *_udata)
{
    const H5MV_free_section_t *sect   = (const H5MV_free_section_t *)_sect; /* File free section */
    H5F_t *                    f      = (H5F_t *)_udata;
    H5F_shared_t *             shared = f->shared;
    haddr_t                    eoa;               /* End of address space in the file */
    haddr_t                    end;               /* End of section to extend */
    htri_t                     ret_value = FALSE; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(sect);

    /* Retrieve the end oa the file's address space */
    if (HADDR_UNDEF == (eoa = H5MV_get_vfd_swmr_md_eoa(shared)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "get_eoa request for VFD SWMR metadata file failed")

    /* Compute address of end of section to check */
    end = sect->sect_info.addr + sect->sect_info.size;

    /* Check if the section is exactly at the end of the allocated space in the file */
    if (H5F_addr_eq(end, eoa))
        /* Indicate shrinking can occur */
        ret_value = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MV__sect_can_shrink() */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_shrink
 *
 * Purpose:	    Shrink container with section
 *
 * Return:	    Success:	non-negative
 *		        Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5MV__sect_shrink(H5FS_section_info_t **_sect, void *_udata)
{
    H5F_t *               f         = (H5F_t *)_udata;
    H5F_shared_t *        shared    = f->shared;
    H5MV_free_section_t **sect      = (H5MV_free_section_t **)_sect; /* File free section */
    herr_t                ret_value = SUCCEED;                       /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(sect);
    HDassert(H5F_SHARED_INTENT(shared) & H5F_ACC_RDWR);

    /* Release section's space at EOA */
    if (H5MV__free_md(shared, (*sect)->sect_info.addr, (*sect)->sect_info.size) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "free request for VFD SWMR metadata file failed")

    /* Free the section */
    if (H5MV__sect_free(&(*sect)->sect_info) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't free simple section node")

    /* Mark section as freed, for free space manager */
    *sect = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5MV__sect_shrink() */

/*-------------------------------------------------------------------------
 * Function:	H5MV__sect_valid
 *
 * Purpose:	    Check the validity of a section
 *
 * Return:	    Success:	non-negative
 *              Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5MV__sect_valid(const H5FS_section_class_t H5_ATTR_UNUSED *cls, const H5FS_section_info_t *_sect)
{
    const H5MV_free_section_t *sect = (const H5MV_free_section_t *)_sect; /* File free section */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments. */
    HDassert(sect);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5MV__sect_valid() */

/*-------------------------------------------------------------------------
 * Function:    H5MV__sect_split
 *
 * Purpose: Split SECT into 2 sections: fragment for alignment & the aligned section
 *          SECT's addr and size are updated to point to the aligned section
 *
 * Return:  Success:    the fragment for aligning sect
 *          Failure:    null
 *
 *-------------------------------------------------------------------------
 */
static H5FS_section_info_t *
H5MV__sect_split(H5FS_section_info_t *sect, hsize_t frag_size)
{
    H5MV_free_section_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_STATIC

    /* Allocate space for new section */
    if (NULL == (ret_value = H5MV__sect_new(sect->addr, frag_size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL, "can't initialize free space section")

    /* Set new section's info */
    sect->addr += frag_size;
    sect->size -= frag_size;

done:
    FUNC_LEAVE_NOAPI((H5FS_section_info_t *)ret_value)
} /* end H5MV__sect_split() */
