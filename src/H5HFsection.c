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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Monday, May  1, 2006
 *
 * Purpose:	Free space section routines for fractal heaps.
 *
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


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Shared routines */
static H5HF_free_section_t *H5HF_sect_node_new(unsigned sect_type,
    haddr_t sect_addr, hsize_t sect_size, H5FS_section_state_t state);
static herr_t H5HF_sect_node_free(H5HF_free_section_t *sect,
    H5HF_indirect_t *parent);
static herr_t H5HF_sect_revive(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect);

/* 'single' section callbacks */
static H5FS_section_info_t *H5HF_sect_single_deserialize(const uint8_t *buf,
    haddr_t sect_addr, hsize_t sect_size);
static htri_t H5HF_sect_single_can_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static herr_t H5HF_sect_single_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static htri_t H5HF_sect_single_can_shrink(H5FS_section_info_t *sect,
    void *udata);
static herr_t H5HF_sect_single_free(H5FS_section_info_t *sect);

/* 'range' section routines */
static herr_t H5HF_sect_range_from_single(H5HF_hdr_t *hdr,
    H5HF_free_section_t *sect, H5HF_direct_t *dblock);

/* 'range' section callbacks */
static herr_t H5HF_sect_range_init_cls(H5FS_section_class_t *cls, const void *udata);
static herr_t H5HF_sect_range_serialize(const H5FS_section_info_t *sect, uint8_t *buf);
static H5FS_section_info_t *H5HF_sect_range_deserialize(const uint8_t *buf,
    haddr_t sect_addr, hsize_t sect_size);
static htri_t H5HF_sect_range_can_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static herr_t H5HF_sect_range_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static htri_t H5HF_sect_range_can_shrink(H5FS_section_info_t *sect,
    void *udata);
static herr_t H5HF_sect_range_shrink(H5FS_section_info_t **sect,
    void *udata);
static herr_t H5HF_sect_range_free(H5FS_section_info_t *sect);
static herr_t H5HF_sect_range_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);

/* 'indirect' section callbacks */
static herr_t H5HF_sect_indirect_init_cls(H5FS_section_class_t *cls, const void *udata);
static herr_t H5HF_sect_indirect_serialize(const H5FS_section_info_t *sect, uint8_t *buf);
static H5FS_section_info_t *H5HF_sect_indirect_deserialize(const uint8_t *buf,
    haddr_t sect_addr, hsize_t sect_size);
static herr_t H5HF_sect_indirect_free(H5FS_section_info_t *sect);
static herr_t H5HF_sect_indirect_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);


/*********************/
/* Package Variables */
/*********************/

/* Class info for "single" free space sections */
/* (No callbacks necessary) */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_SINGLE[1] = {{
    H5HF_FSPACE_SECT_SINGLE,		/* Section type                 */
    0,					/* Extra serialized size        */
    NULL,				/* Initialize section class     */
    NULL,				/* Serialize section            */
    H5HF_sect_single_deserialize,	/* Deserialize section          */
    H5HF_sect_single_can_merge,		/* Can sections merge?          */
    H5HF_sect_single_merge,		/* Merge sections               */
    H5HF_sect_single_can_shrink,	/* Can section shrink container?*/
    NULL,				/* Shrink container w/section   */
    H5HF_sect_single_free,		/* Free section                 */
    NULL,				/* Dump debugging for section   */
}};

/* Class info for "range" free space sections */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_RANGE[1] = {{
    H5HF_FSPACE_SECT_RANGE,		/* Section type                 */
    0,					/* Extra serialized size        */
    H5HF_sect_range_init_cls,		/* Initialize section class     */
    H5HF_sect_range_serialize,		/* Serialize section            */
    H5HF_sect_range_deserialize,	/* Deserialize section          */
    H5HF_sect_range_can_merge,		/* Can sections merge?          */
    H5HF_sect_range_merge,		/* Merge sections               */
    H5HF_sect_range_can_shrink,		/* Can section shrink container?*/
    H5HF_sect_range_shrink,		/* Shrink container w/section   */
    H5HF_sect_range_free,		/* Free section                 */
    H5HF_sect_range_debug,		/* Dump debugging for section   */
}};

/* Class info for "indirect" free space sections */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_INDIRECT[1] = {{
    H5HF_FSPACE_SECT_INDIRECT,		/* Section type                 */
    0,					/* Extra serialized size        */
    H5HF_sect_indirect_init_cls,	/* Initialize section class     */
    H5HF_sect_indirect_serialize,	/* Serialize section            */
    H5HF_sect_indirect_deserialize,	/* Deserialize section          */
    NULL,				/* Can sections merge?          */
    NULL,				/* Merge sections               */
    NULL,				/* Can section shrink container?*/
    NULL,				/* Shrink container w/section   */
    H5HF_sect_indirect_free,		/* Free section                 */
    H5HF_sect_indirect_debug,		/* Dump debugging for section   */
}};

/* Declare a free list to manage the H5HF_free_section_t struct */
H5FL_DEFINE(H5HF_free_section_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_node_new
 *
 * Purpose:	Allocate a free space section node of a particular type
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_free_section_t *
H5HF_sect_node_new(unsigned sect_type, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_state_t sect_state)
{
    H5HF_free_section_t *new_sect;      /* New section */
    H5HF_free_section_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_node_new)

    /* Check arguments. */
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);

    /* Create free list section node */
    if(NULL == (new_sect = H5FL_MALLOC(H5HF_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct block free list section")

    /* Set the information passed in */
    new_sect->sect_info.addr = sect_addr;
    new_sect->sect_info.size = sect_size;

    /* Set the section's class & state */
    new_sect->sect_info.type = sect_type;
    new_sect->sect_info.state = sect_state;

    /* Set return value */
    ret_value = new_sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_node_new() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_node_free
 *
 * Purpose:	Free a section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_node_free(H5HF_free_section_t *sect, H5HF_indirect_t *parent)
{
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_node_free)

    HDassert(sect);

    /* Release indirect block, if there was one */
    if(parent)
        if(H5HF_iblock_decr(parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on parent indirect block")

    /* Release the section */
    H5FL_FREE(H5HF_free_section_t, sect);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_node_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_revive
 *
 * Purpose:	Revive a section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_revive(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_revive)

    /* Sanity check */
    HDassert(hdr);
    HDassert(sect);

    /* Call appropriate 'revive' routine */
    switch(sect->sect_info.type) {
        case H5HF_FSPACE_SECT_INDIRECT:
            if(H5HF_sect_indirect_revive(hdr, dxpl_id, sect) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive range free section")
            break;

        case H5HF_FSPACE_SECT_RANGE:
            if(H5HF_sect_range_revive(hdr, dxpl_id, sect) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive range free section")
            break;

        case H5HF_FSPACE_SECT_SINGLE:
            if(H5HF_sect_single_revive(hdr, dxpl_id, sect) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
            break;

        default:
            HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "unknown section type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_revive() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_new
 *
 * Purpose:	Create a new 'single' section and return it to the caller
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 30 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_free_section_t *
H5HF_sect_single_new(hsize_t sect_off, size_t sect_size,
    H5HF_indirect_t *parent, unsigned par_entry,
    haddr_t dblock_addr, size_t dblock_size)
{
    H5HF_free_section_t *sect = NULL;   /* 'Single' free space section to add */
    hbool_t par_incr = FALSE;           /* Indicate that parent iblock has been incremented */
    H5HF_free_section_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_new)

    /*
     * Check arguments.
     */
    HDassert(sect_size);

    /* Create free space section node */
    if(NULL == (sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_SINGLE, sect_off, (hsize_t)sect_size, H5FS_SECT_LIVE)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for single section")

    /* Set the 'single' specific fields */
    sect->u.single.parent = parent;
    if(sect->u.single.parent) {
        if(H5HF_iblock_incr(sect->u.single.parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")
        par_incr = TRUE;
    } /* end if */
    sect->u.single.par_entry = par_entry;
    sect->u.single.dblock_addr = dblock_addr;
    sect->u.single.dblock_size = dblock_size;

    /* Set return value */
    ret_value = sect;

done:
    if(!ret_value && sect) {
        /* Check if we should decrement parent ref. count */
        if(par_incr) {
            HDassert(sect->u.single.parent);
            if(H5HF_iblock_decr(sect->u.single.parent) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't decrement reference count on parent indirect block")
        } /* end if */

        /* Release the section */
        H5FL_FREE(H5HF_free_section_t, sect);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_single_new() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_revive
 *
 * Purpose:	Update the memory information for a 'single' free section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_single_revive(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect)
{
    H5HF_indirect_t *sec_iblock;        /* Pointer to section indirect block */
    unsigned sec_entry;                 /* Entry within section indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_revive)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_SERIALIZED);

    /* Check for root direct block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        /* Set the information for the section */
        sect->u.single.parent = NULL;
        sect->u.single.par_entry = 0;

        /* Set direct block info */
        HDassert(H5F_addr_defined(hdr->man_dtable.table_addr));
        sect->u.single.dblock_addr =  hdr->man_dtable.table_addr;
        sect->u.single.dblock_size =  hdr->man_dtable.cparam.start_block_size;
    } /* end if */
    else {
        /* Look up indirect block containing direct blocks for range */
        if(H5HF_man_locate_block(hdr, dxpl_id, sect->sect_info.addr, FALSE, &sec_iblock, &sec_entry, H5AC_READ) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of section")

        /* Increment reference count on indirect block that free section is in */
        if(H5HF_iblock_incr(sec_iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

        /* Set the information for the section */
        sect->u.single.parent = sec_iblock;
        sect->u.single.par_entry = sec_entry;

        /* Set direct block info */
        sect->u.single.dblock_addr =  sec_iblock->ents[sec_entry].addr;
        sect->u.single.dblock_size =  hdr->man_dtable.row_block_size[sec_entry / hdr->man_dtable.cparam.width];

        /* Unlock indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, sec_iblock->addr, sec_iblock, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
        sec_iblock = NULL;
    } /* end else */

    /* Section is "live" now */
    sect->sect_info.state = H5FS_SECT_LIVE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_single_revive() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_reduce
 *
 * Purpose:	Reduce the size of a single section (possibly freeing it)
 *              and re-add it back to the free space manager for the heap
 *              (if it hasn't been freed)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_single_reduce(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect, size_t amt)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_reduce)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_SINGLE);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

    /* Check for eliminating the section */
    if(sect->sect_info.size == amt) {
        /* Free single section */
        if(H5HF_sect_single_free((H5FS_section_info_t *)sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free single section node")
    } /* end if */
    else {
        /* Adjust information for section */
        sect->sect_info.addr += amt;
        sect->sect_info.size -= amt;

        /* Re-insert section node into heap's free space */
        if(H5HF_space_add(hdr, dxpl_id, sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't re-add single section to free space manager")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_single_reduce() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_deserialize
 *
 * Purpose:	Deserialize a buffer into a "live" single section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_section_info_t *
H5HF_sect_single_deserialize(const uint8_t *buf, haddr_t sect_addr,
    hsize_t sect_size)
{
    H5HF_free_section_t *new_sect;      /* New section */
    H5FS_section_info_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_deserialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);

    /* Create free list section node */
    if(NULL == (new_sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_SINGLE, sect_addr, sect_size, H5FS_SECT_SERIALIZED)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "allocation failed for direct block free list section")

    /* Set return value */
    ret_value = (H5FS_section_info_t *)new_sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_can_merge
 *
 * Purpose:	Can two sections of this type merge?
 *
 * Note:        Second section must be "after" first section
 *
 * Return:	Success:	non-negative (TRUE/FALSE)
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_single_can_merge(H5FS_section_info_t *_sect1,
    H5FS_section_info_t *_sect2, void *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    size_t dblock_size;                 /* Section's direct block's size */
    size_t dblock_overhead;             /* Direct block's overhead */
    hid_t dxpl_id = udata->dxpl_id;     /* DXPL ID for operation */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_can_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_lt(sect1->sect_info.addr, sect2->sect_info.addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Check to see if we should revive either section */
    if(sect1->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_single_revive(hdr, dxpl_id, sect1) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
    if(sect2->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_revive(hdr, dxpl_id, sect2) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive free section")

    /* Check for section occupying entire direct block */
    dblock_size = sect1->u.single.dblock_size;
    dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
#ifdef QAK
HDfprintf(stderr, "%s: dblock_size = %u\n", FUNC, dblock_size);
#endif /* QAK */
    if((dblock_size - dblock_overhead) == sect1->sect_info.size) {
        H5HF_direct_t *dblock;          /* Pointer to direct block for section */
        haddr_t dblock_addr;            /* Section's direct block's address */

        /* Protect the direct block for the section */
        dblock_addr = sect1->u.single.dblock_addr;
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, sect1->u.single.parent, sect1->u.single.par_entry, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")
        HDassert(H5F_addr_eq(dblock->block_off + dblock_overhead, sect1->sect_info.addr));

        /* Convert 'single' section into 'range' section */
        if(H5HF_sect_range_from_single(hdr, sect1, dblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCONVERT, FAIL, "can't convert single section into range section")

        /* Destroy direct block */
        if(H5HF_man_dblock_destroy(hdr, dxpl_id, dblock, dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release direct block")
        dblock = NULL;

        /* Check if second section is a range section */
        if(sect2->sect_info.type == H5HF_FSPACE_SECT_RANGE) {
            htri_t status;      /* Status from range 'can merge' call */

            /* Check if two sections can merge now */
            /* (i.e. assumes responsibility for passing along 'can merge' callback) */
            if((status = H5HF_sect_range_can_merge((H5FS_section_info_t *)sect1, (H5FS_section_info_t *)sect2, udata)) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTMERGE, FAIL, "can't check for merging sections")
            HGOTO_DONE(status)
        } /* end if */
    } /* end if */

    /* Single section can only merge with other single sections */
    if(sect1->sect_info.type != sect2->sect_info.type)
        HGOTO_DONE(FALSE)

    /* Check if second section adjoins first section */
    /* (This can only occurs within a direct block, due to the direct block
     *  overhead at the beginning of a block)
     */
    if(H5F_addr_eq(sect1->sect_info.addr + sect1->sect_info.size, sect2->sect_info.addr))
        HGOTO_DONE(TRUE)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_can_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_merge
 *
 * Purpose:	Merge two sections of this type
 *
 * Note:        Second section always merges into first node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_single_merge(H5FS_section_info_t *_sect1, H5FS_section_info_t *_sect2,
    void UNUSED *udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect1->sect_info.state == H5FS_SECT_LIVE);
    HDassert(sect2);
    HDassert(sect2->sect_info.state == H5FS_SECT_LIVE);
    HDassert(H5F_addr_eq(sect1->sect_info.addr + sect1->sect_info.size, sect2->sect_info.addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info.size = %Hu, sect1->sect_info.addr = %a\n", FUNC, sect1->sect_info.size, sect1->sect_info.addr);
HDfprintf(stderr, "%s: sect2->sect_info.size = %Hu, sect2->sect_info.addr = %a\n", FUNC, sect2->sect_info.size, sect2->sect_info.addr);
#endif /* QAK */

    /* Add second section's size to first section */
    sect1->sect_info.size += sect2->sect_info.size;

    /* Get rid of second section */
    if(H5HF_sect_single_free((H5FS_section_info_t *)sect2) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Defer checking if 'single' section should be converted into 'range' section
     *  until next pass through the "can merge" callback, to make certain that
     *  removing a direct block in the root indirect block gives neighboring
     *  "range" sections a chance to deserialize.  *ick*  -QAK
     */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_can_shrink
 *
 * Purpose:	Can this section shrink the container?
 *
 * Note:        This isn't actually shrinking the heap (since that's already
 *              been done) as much as it's cleaning up _after_ the heap
 *              shrink.
 *
 * Return:	Success:	non-negative (TRUE/FALSE)
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_single_can_shrink(H5FS_section_info_t *_sect, void *_udata)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    size_t dblock_size;                 /* Section's direct block's size */
    size_t dblock_overhead;             /* Direct block's overhead */
    hid_t dxpl_id = udata->dxpl_id;     /* DXPL ID for operation */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_can_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Check to see if we should revive section */
    if(sect->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_single_revive(hdr, dxpl_id, sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")

    /* Check for section occupying entire direct block */
    dblock_size = sect->u.single.dblock_size;
    dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
#ifdef QAK
HDfprintf(stderr, "%s: dblock_size = %u\n", FUNC, dblock_size);
#endif /* QAK */
    if((dblock_size - dblock_overhead) == sect->sect_info.size) {
        H5HF_direct_t *dblock;          /* Pointer to direct block for section */
        haddr_t dblock_addr;            /* Section's direct block's address */
        htri_t status;                  /* Status from range 'can shrink' call */

        /* Protect the direct block for the section */
        dblock_addr = sect->u.single.dblock_addr;
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, sect->u.single.parent, sect->u.single.par_entry, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")
        HDassert(H5F_addr_eq(dblock->block_off + dblock_overhead, sect->sect_info.addr));

        /* Convert 'single' section into 'range' section */
        if(H5HF_sect_range_from_single(hdr, sect, dblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCONVERT, FAIL, "can't convert single section into range section")

        /* Destroy direct block */
        if(H5HF_man_dblock_destroy(hdr, dxpl_id, dblock, dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release direct block")
        dblock = NULL;

        /* Check if section can shrink container now */
        /* (i.e. assumes responsibility for passing along 'can shrink' callback) */
        if((status = H5HF_sect_range_can_shrink((H5FS_section_info_t *)sect, udata)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't check for shrinking container")
        HGOTO_DONE(status)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_can_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_free
 *
 * Purpose:	Free a 'single' section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_single_free(H5FS_section_info_t *_sect)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;   /* Pointer to section to free */
    H5HF_indirect_t *parent = NULL;     /* Parent indirect block for section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_free)

    HDassert(sect);

    /* Check for live reference to an indirect block */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        /* Get parent indirect block, if there was one */
        if(sect->u.single.parent)
            parent = sect->u.single.parent;
    } /* end if */

    /* Release the section */
    if(H5HF_sect_node_free(sect, parent) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_single_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_init_cls
 *
 * Purpose:	Initialize the "range" class structure
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_init_cls(H5FS_section_class_t *cls, const void UNUSED *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_range_init_cls)

    /* Check arguments. */
    HDassert(cls);

    /* Set the size of all serialized objects of this class of sections */
    cls->serial_size = 2                        /* Row */
        + 2                                     /* Column */
        + 2;                                    /* # of entries */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_range_init_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_add
 *
 * Purpose:	Add a new 'range' section to the free space manager for this
 *              heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 30 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_range_add(H5HF_hdr_t *hdr, hid_t dxpl_id, hsize_t sect_off,
    hsize_t sect_size, H5HF_indirect_t *iblock,
    unsigned row, unsigned col, unsigned nentries)
{
    H5HF_free_section_t *sect = NULL;   /* 'Range' free space section to add */
    hbool_t iblock_incr = FALSE;        /* Indicate that parent iblock has been incremented */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_add)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(sect_size);
    HDassert(nentries);
    HDassert(row < hdr->man_dtable.max_direct_rows);    /* Can't handle ranges on indirect blocks */

    /* Create free space section node */
    if(NULL == (sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_RANGE, sect_off, sect_size, H5FS_SECT_LIVE)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for range section")

    /* Set the 'range' specific fields */
    sect->u.range.iblock = iblock;
    if(H5HF_iblock_incr(sect->u.range.iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
    iblock_incr = TRUE;
    sect->u.range.row = row;
    sect->u.range.col = col;
    sect->u.range.num_entries = nentries;

    /* Add new free space to the free space manager for this heap */
    if(H5HF_space_add(hdr, dxpl_id, sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add range section to free space manager")

done:
    if(ret_value < 0 && sect) {
        /* Check if we should decrement parent ref. count */
        if(iblock_incr)
            if(H5HF_iblock_decr(sect->u.range.iblock) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

        /* Release the section */
        H5FL_FREE(H5HF_free_section_t, sect);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_range_add() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_revive
 *
 * Purpose:	Update the memory information for a 'range' free section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_range_revive(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect)
{
    H5HF_indirect_t *sec_iblock;        /* Pointer to section indirect block */
    unsigned sec_entry;                 /* Entry within section indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_revive)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_SERIALIZED);

    /* Look up indirect block containing direct blocks for range */
    if(H5HF_man_locate_block(hdr, dxpl_id, sect->sect_info.addr, FALSE, &sec_iblock, &sec_entry, H5AC_READ) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of section")

    /* Increment reference count on indirect block that free section is in */
    if(H5HF_iblock_incr(sec_iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Set the pointer to the section's indirect block */
    sect->u.range.iblock = sec_iblock;

    /* Unlock indirect block */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, sec_iblock->addr, sec_iblock, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    sec_iblock = NULL;

    /* Section is "live" now */
    sect->sect_info.state = H5FS_SECT_LIVE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_range_revive() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_reduce
 *
 * Purpose:	Reduce the size of a range section (possibly freeing it)
 *              and re-add it back to the free space manager for the heap
 *              (if it hasn't been freed)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_range_reduce(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_reduce)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_RANGE);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

    /* Check for eliminating the section */
    if(sect->u.range.num_entries == 1) {
        /* Free range section */
        if(H5HF_sect_range_free((H5FS_section_info_t *)sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free range section node")
    } /* end if */
    else {
        /* Adjust section information */
        sect->sect_info.addr += hdr->man_dtable.row_block_size[sect->u.range.row];

        /* Adjust range information */
        sect->u.range.col++;
        sect->u.range.num_entries--;

        /* Add section back to free space list */
        if(H5HF_space_add(hdr, dxpl_id, sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't re-add range section to free space manager")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_range_reduce() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_from_single
 *
 * Purpose:	Convert a 'single' section into a 'range' section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_from_single(H5HF_hdr_t *hdr, H5HF_free_section_t *sect,
    H5HF_direct_t *dblock)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_range_from_single)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(dblock);
#ifdef QAK
HDfprintf(stderr, "%s: sect.sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_range_from_single", sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: dblock->parent = %p\n", "H5HF_sect_range_from_single", dblock->parent);
HDfprintf(stderr, "%s: hdr->man_dtable.curr_root_rows = %u\n", "H5HF_sect_range_from_single", hdr->man_dtable.curr_root_rows);
#endif /* QAK */

    /* Update information for range block */
    sect->sect_info.addr = dblock->block_off;
    sect->sect_info.type = H5HF_FSPACE_SECT_RANGE;
    sect->u.range.iblock = dblock->parent;
    sect->u.range.row = dblock->par_entry / hdr->man_dtable.cparam.width;
    sect->u.range.col = dblock->par_entry % hdr->man_dtable.cparam.width;
    sect->u.range.num_entries = 1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_sect_range_from_single() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_serialize
 *
 * Purpose:	Serialize a "live" range section into a buffer
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_serialize(const H5FS_section_info_t *_sect, uint8_t *buf)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_range_serialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(sect);

    /* Range's row */
    UINT16ENCODE(buf, sect->u.range.row);

    /* Range's column */
    UINT16ENCODE(buf, sect->u.range.col);

    /* Range's # of entries */
    UINT16ENCODE(buf, sect->u.range.num_entries);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_range_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_deserialize
 *
 * Purpose:	Deserialize a buffer into a "live" range section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_section_info_t *
H5HF_sect_range_deserialize(const uint8_t *buf, haddr_t sect_addr,
    hsize_t sect_size)
{
    H5HF_free_section_t *new_sect;      /* New section */
    H5FS_section_info_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_deserialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);

    /* Create free list section node */
    if(NULL == (new_sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_RANGE, sect_addr, sect_size, H5FS_SECT_SERIALIZED)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "allocation failed for direct block free list section")

    /* Range's row */
    UINT16DECODE(buf, new_sect->u.range.row);

    /* Range's column */
    UINT16DECODE(buf, new_sect->u.range.col);

    /* Range's # of entries */
    UINT16DECODE(buf, new_sect->u.range.num_entries);

    /* Set return value */
    ret_value = (H5FS_section_info_t *)new_sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_range_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_can_merge
 *
 * Purpose:	Can two sections of this type merge?
 *
 * Note:        Second section must be "after" first section
 *
 * Return:	Success:	non-negative (TRUE/FALSE)
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_range_can_merge(H5FS_section_info_t *_sect1,
    H5FS_section_info_t *_sect2, void *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    hid_t dxpl_id = udata->dxpl_id;     /* DXPL ID for operation */
    hsize_t sect2_off;                  /* Offset of second section in heap */
    size_t dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr); /* Direct block's overhead */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_can_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_lt(sect1->sect_info.addr, sect2->sect_info.addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */
    /* Check to see if we should revive either section */
    if(sect1->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_range_revive(hdr, dxpl_id, sect1) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
    if(sect2->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_revive(hdr, dxpl_id, sect2) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive free section")

    /* Check for special case of delayed conversion of 2nd section from
     *  single -> range section
     */
    if(sect2->sect_info.type == H5HF_FSPACE_SECT_SINGLE) {
        size_t dblock_size;                 /* Section's direct block's size */

        /* Check for section occupying entire direct block */
        dblock_size = sect2->u.single.dblock_size;
        if((dblock_size - dblock_overhead) == sect2->sect_info.size) {
            H5HF_direct_t *dblock;          /* Pointer to direct block for section */
            haddr_t dblock_addr;            /* Section's direct block's address */

            /* Protect the direct block for the section */
            dblock_addr = sect2->u.single.dblock_addr;
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */
            if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, sect2->u.single.parent, sect2->u.single.par_entry, H5AC_READ)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")
            HDassert(H5F_addr_eq(dblock->block_off + dblock_overhead, sect2->sect_info.addr));

            /* Convert 'single' section into 'range' section */
            if(H5HF_sect_range_from_single(hdr, sect2, dblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTCONVERT, FAIL, "can't convert single section into range section")

            /* Destroy direct block */
            if(H5HF_man_dblock_destroy(hdr, dxpl_id, dblock, dblock_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release direct block")
            dblock = NULL;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", FUNC, hdr->man_iter_off);
#endif /* QAK */
        } /* end if */
    } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: sect1.u.range = {%p, %u, %u, %u}\n", FUNC, sect1->u.range.iblock, sect1->u.range.row, sect1->u.range.col, sect1->u.range.num_entries);
#endif /* QAK */

    /* Range section can only merge with other range sections */
    if(sect1->sect_info.type != sect2->sect_info.type)
        HGOTO_DONE(FALSE)

#ifdef QAK
HDfprintf(stderr, "%s: sect2.u.range = {%p, %u, %u, %u}\n", FUNC, sect2->u.range.iblock, sect2->u.range.row, sect2->u.range.col, sect2->u.range.num_entries);
HDfprintf(stderr, "%s: sect2.u.range.iblock->nchildren = %u\n", FUNC, sect2->u.range.iblock->nchildren);
#endif /* QAK */

    /* Check if second section is in indirect block that's being deleted */
    if(sect2->u.range.iblock->nchildren == 0)
        HGOTO_DONE(TRUE)

    /* Check if second section is past end of "next block" iterator */
    sect2_off = sect2->u.range.iblock->block_off;
    sect2_off += hdr->man_dtable.row_block_off[sect2->u.range.row];
    sect2_off += hdr->man_dtable.row_block_size[sect2->u.range.row] * sect2->u.range.col;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", FUNC, hdr->man_iter_off);
HDfprintf(stderr, "%s: sect2.u.range.iblock->block_off = %Hu\n", FUNC, sect2->u.range.iblock->block_off);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[%u] = %Hu\n", FUNC, sect2->u.range.row, hdr->man_dtable.row_block_off[sect2->u.range.row]);
HDfprintf(stderr, "%s: sect2_off = %Hu\n", FUNC, sect2_off);
#endif /* QAK */
    if(sect2_off > hdr->man_iter_off)
        HGOTO_DONE(TRUE)

    /* Check if second section adjoins first section & is in the same row */
    if(H5F_addr_eq(sect1->sect_info.addr + (sect1->u.range.num_entries * (sect1->sect_info.size + dblock_overhead)), sect2->sect_info.addr) &&
            sect1->u.range.row == sect2->u.range.row)
        HGOTO_DONE(TRUE)

done:
#ifdef QAK
HDfprintf(stderr, "%s: ret_value = %t\n", FUNC, ret_value);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_range_can_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_merge
 *
 * Purpose:	Merge two sections of this type
 *
 * Note:        Second section always merges into first node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_merge(H5FS_section_info_t *_sect1, H5FS_section_info_t *_sect2,
    void *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect1->sect_info.state == H5FS_SECT_LIVE);
    HDassert(sect1->sect_info.type == H5HF_FSPACE_SECT_RANGE);
    HDassert(sect2);
    HDassert(sect2->sect_info.state == H5FS_SECT_LIVE);
    HDassert(sect2->sect_info.type == H5HF_FSPACE_SECT_RANGE);

#ifdef QAK
HDfprintf(stderr, "%s: sect1.sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect1.u.range = {%p, %u, %u, %u}\n", FUNC, sect1->u.range.iblock, sect1->u.range.row, sect1->u.range.col, sect1->u.range.num_entries);
HDfprintf(stderr, "%s: sect2.sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2.u.range = {%p, %u, %u, %u}\n", FUNC, sect2->u.range.iblock, sect2->u.range.row, sect2->u.range.col, sect2->u.range.num_entries);
#endif /* QAK */

    /* Add second section's size to first section, if it's in the same row */
    if(sect1->u.range.row == sect2->u.range.row)
        sect1->u.range.num_entries += sect2->u.range.num_entries;

    /* Get rid of second section */
    if(H5HF_sect_range_free((H5FS_section_info_t *)sect2) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Check if 'range' section should be converted into 'indirect' section (?) */
    if(sect1->u.range.num_entries == hdr->man_dtable.cparam.width &&
            sect1->u.range.row >= hdr->man_dtable.max_direct_rows) {
HDfprintf(stderr, "%s: converting range section to indirect section not supported yet!\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "converting range section to indirect section not supported yet")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_range_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_can_shrink
 *
 * Purpose:	Can this section shrink the container?
 *
 * Note:        This isn't actually shrinking the heap (since that's already
 *              been done) as much as it's cleaning up _after_ the heap
 *              shrink.
 *
 * Return:	Success:	non-negative (TRUE/FALSE)
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_range_can_shrink(H5FS_section_info_t *_sect, void UNUSED *_udata)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;   /* Fractal heap free section */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_range_can_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_range_can_shrink", sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect->u.range = {%p, %u, %u, %u}\n", "H5HF_sect_range_can_shrink", sect->u.range.iblock, sect->u.range.row, sect->u.range.col, sect->u.range.num_entries);
if(sect->u.range.iblock != NULL)
    HDfprintf(stderr, "%s: sect->u.range.iblock->nchildren = %u\n", "H5HF_sect_range_can_shrink", sect->u.range.iblock->nchildren);
#endif /* QAK */

    /* If section has no parent, it should go away */
    if(sect->u.range.iblock == NULL)
        HGOTO_DONE(TRUE)

    /* If section is in an indirect block with no children, it should go away */
    if(sect->u.range.iblock->nchildren == 0)
        HGOTO_DONE(TRUE)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_range_can_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_shrink
 *
 * Purpose:	Shrink container with section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_shrink(H5FS_section_info_t **_sect, void UNUSED *_udata)
{
    H5HF_free_section_t **sect = (H5HF_free_section_t **)_sect;   /* Fractal heap free section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(*sect);
    HDassert((*sect)->sect_info.state == H5FS_SECT_LIVE);

#ifdef QAK
HDfprintf(stderr, "%s: (*sect).sect_info = {%a, %Hu, %u}\n", FUNC, (*sect)->sect_info.addr, (*sect)->sect_info.size, (*sect)->sect_info.type);
HDfprintf(stderr, "%s: (*sect).u.range = {%p, %u, %u, %u}\n", FUNC, (*sect)->u.range.iblock, (*sect)->u.range.row, (*sect)->u.range.col, (*sect)->u.range.num_entries);
#endif /* QAK */

    /* Get rid of section */
    if(H5HF_sect_range_free((H5FS_section_info_t *)*sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Indicate that the section has been released */
    *sect = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_range_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_free
 *
 * Purpose:	Free a 'range' section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_free(H5FS_section_info_t *_sect)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;   /* Pointer to section to free */
    H5HF_indirect_t *parent = NULL;     /* Parent indirect block for section */
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_free)

    HDassert(sect);

    /* Check for live reference to an indirect block */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        /* Get parent indirect block, if there was one */
        if(sect->u.range.iblock)
            parent = sect->u.range.iblock;
    } /* end if */

    /* Release the section */
    if(H5HF_sect_node_free(sect, parent) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_range_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_range_debug
 *
 * Purpose:	Dump debugging information about an range free space section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_range_debug(const H5FS_section_info_t *_sect,
    FILE *stream, int indent, int fwidth)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;       /* Section to dump info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_range_debug)

    /* Check arguments. */
    HDassert(sect);

    /* Print range section information */
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Row:",
	      sect->u.range.row);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Column:",
	      sect->u.range.col);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of entries:",
	      sect->u.range.num_entries);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_range_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_init_cls
 *
 * Purpose:	Initialize the "indirect" class structure
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_init_cls(H5FS_section_class_t *cls, const void UNUSED *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_init_cls)

    /* Check arguments. */
    HDassert(cls);

    /* Set the size of all serialized objects of this class of sections */
    cls->serial_size = 2                        /* Row */
        + 2                                     /* Column */
        + 2                                     /* # of entries */
        + 2                                     /* Indirect row */
        + 2;                                    /* Indirect # of rows */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_indirect_init_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_add
 *
 * Purpose:	Add a new 'indirect' section to the free space manager for this
 *              heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 30 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_indirect_add(H5HF_hdr_t *hdr, hid_t dxpl_id,
    hsize_t sect_off, hsize_t sect_size, H5HF_indirect_t *iblock,
    unsigned row, unsigned col, unsigned nentries,
    unsigned indir_row, unsigned indir_nrows)
{
    H5HF_free_section_t *sect = NULL;   /* 'Indirect' free space section to add */
    hbool_t iblock_incr = FALSE;        /* Indicate that parent iblock has been incremented */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_add)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(sect_size);
    HDassert(nentries);

    /* Create free space section node */
    if(NULL == (sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_INDIRECT, sect_off, sect_size, H5FS_SECT_LIVE)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for indirect section")

    /* Set the 'indirect' specific fields */
    sect->u.indirect.iblock = iblock;
    if(H5HF_iblock_incr(sect->u.indirect.iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
    iblock_incr = TRUE;
    sect->u.indirect.row = row;
    sect->u.indirect.col = col;
    sect->u.indirect.num_entries = nentries;
    sect->u.indirect.indir_row = indir_row;
    sect->u.indirect.indir_nrows = indir_nrows;

    /* Add new free space to the free space manager for the heap */
    if(H5HF_space_add(hdr, dxpl_id, sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect section to free space")

done:
    if(ret_value < 0 && sect) {
        /* Check if we should decrement parent ref. count */
        if(iblock_incr)
            if(H5HF_iblock_decr(sect->u.indirect.iblock) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

        /* Release the section */
        H5FL_FREE(H5HF_free_section_t, sect);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_add() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_revive
 *
 * Purpose:	Update the memory information for a 'indirect' free section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_indirect_revive(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect)
{
    H5HF_indirect_t *sec_iblock;        /* Pointer to section indirect block */
    unsigned sec_entry;                 /* Entry within section indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_revive)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_SERIALIZED);

    /* Look up indirect block containing indirect blocks for section */
    if(H5HF_man_locate_block(hdr, dxpl_id, sect->sect_info.addr, TRUE, &sec_iblock, &sec_entry, H5AC_READ) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of section")

    /* Increment reference count on indirect block that free section is in */
    if(H5HF_iblock_incr(sec_iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Set the pointer to the section's indirect block */
    sect->u.indirect.iblock = sec_iblock;

    /* Unlock indirect block */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, sec_iblock->addr, sec_iblock, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    sec_iblock = NULL;

    /* Section is "live" now */
    sect->sect_info.state = H5FS_SECT_LIVE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_revive() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_reduce
 *
 * Purpose:	Reduce the size of a indirect section (possibly freeing it)
 *              and re-add it back to the free space manager for the heap
 *              (if it hasn't been freed)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_indirect_reduce(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_reduce)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_INDIRECT);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

    /* Check for eliminating the section */
    if(sect->u.indirect.num_entries == 1) {
        /* Free indirect section */
        if(H5HF_sect_indirect_free((H5FS_section_info_t *)sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free indirect section node")
    } /* end if */
    else {
        /* Adjust section information */
        sect->sect_info.addr += hdr->man_dtable.row_block_size[sect->u.indirect.row];

        /* Adjust range information */
        sect->u.indirect.col++;
        sect->u.indirect.num_entries--;

        /* Add 'indirect' section back to free space list */
        if(H5HF_space_add(hdr, dxpl_id, sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't re-add indirect section to free space manager")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_reduce() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_serialize
 *
 * Purpose:	Serialize a "live" indirect section into a buffer
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_serialize(const H5FS_section_info_t *_sect, uint8_t *buf)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_serialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(_sect);

    /* Range's row */
    UINT16ENCODE(buf, sect->u.indirect.row);

    /* Range's column */
    UINT16ENCODE(buf, sect->u.indirect.col);

    /* Range's # of entries */
    UINT16ENCODE(buf, sect->u.indirect.num_entries);

    /* Range's indirect row */
    UINT16ENCODE(buf, sect->u.indirect.indir_row);

    /* Range's indirect # of rows */
    UINT16ENCODE(buf, sect->u.indirect.indir_nrows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_indirect_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_deserialize
 *
 * Purpose:	Deserialize a buffer into a "live" indirect section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_section_info_t *
H5HF_sect_indirect_deserialize(const uint8_t *buf, haddr_t sect_addr,
    hsize_t sect_size)
{
    H5HF_free_section_t *new_sect;      /* New section */
    H5FS_section_info_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_deserialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);

    /* Create free list section node */
    if(NULL == (new_sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_INDIRECT, sect_addr, sect_size, H5FS_SECT_SERIALIZED)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "allocation failed for direct block free list section")

    /* Range's row */
    UINT16DECODE(buf, new_sect->u.indirect.row);

    /* Range's column */
    UINT16DECODE(buf, new_sect->u.indirect.col);

    /* Range's # of entries */
    UINT16DECODE(buf, new_sect->u.indirect.num_entries);

    /* Range's indirect row */
    UINT16DECODE(buf, new_sect->u.indirect.indir_row);

    /* Range's indirect # of rows */
    UINT16DECODE(buf, new_sect->u.indirect.indir_nrows);

    /* Set return value */
    ret_value = (H5FS_section_info_t *)new_sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_indirect_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_free
 *
 * Purpose:	Free a 'indirect' section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_free(H5FS_section_info_t *_sect)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;   /* Pointer to section to free */
    H5HF_indirect_t *parent = NULL;     /* Parent indirect block for section */
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_free)

    HDassert(sect);

    /* Check for live reference to an indirect block */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        /* Get parent indirect block, if there was one */
        if(sect->u.indirect.iblock)
            parent = sect->u.indirect.iblock;
    } /* end if */

    /* Release the sections */
    if(H5HF_sect_node_free(sect, parent) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_indirect_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_debug
 *
 * Purpose:	Dump debugging information about an indirect free space section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_debug(const H5FS_section_info_t *_sect,
    FILE *stream, int indent, int fwidth)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;       /* Section to dump info */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_debug)

    /* Check arguments. */
    HDassert(sect);

    /* Print indirect section information */
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Row:",
	      sect->u.indirect.row);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Column:",
	      sect->u.indirect.col);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Number of entries:",
	      sect->u.indirect.num_entries);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Indirect row:",
	      sect->u.indirect.indir_row);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Indirect number of rows:",
	      sect->u.indirect.indir_nrows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_indirect_debug() */

