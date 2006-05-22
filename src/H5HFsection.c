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
static herr_t H5HF_sect_node_alloc(H5FS_section_class_t *sect_cls,
    unsigned sect_type, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static herr_t H5HF_sect_node_free(H5HF_free_section_t *sect,
    H5HF_indirect_t *parent);

/* 'single' section callbacks */
static herr_t H5HF_sect_single_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static htri_t H5HF_sect_single_can_merge(const H5FS_section_info_t *sect1,
    const H5FS_section_info_t *sect2, void *udata);
static herr_t H5HF_sect_single_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static htri_t H5HF_sect_single_can_shrink(H5FS_section_info_t *sect, void *udata);
static herr_t H5HF_sect_single_shrink(H5FS_section_info_t **sect, void *udata);
static herr_t H5HF_sect_single_free(H5FS_section_info_t *sect);

/* 'range' section callbacks */
static herr_t H5HF_sect_range_init_cls(H5FS_section_class_t *cls, const void *udata);
static herr_t H5HF_sect_range_serialize(const H5FS_section_info_t *sect, uint8_t *buf);
static herr_t H5HF_sect_range_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static herr_t H5HF_sect_range_free(H5FS_section_info_t *sect);
static herr_t H5HF_sect_range_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);

/* 'indirect' section callbacks */
static herr_t H5HF_sect_indirect_init_cls(H5FS_section_class_t *cls, const void *udata);
static herr_t H5HF_sect_indirect_serialize(const H5FS_section_info_t *sect, uint8_t *buf);
static herr_t H5HF_sect_indirect_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static herr_t H5HF_sect_indirect_free(H5FS_section_info_t *sect);
static herr_t H5HF_sect_indirect_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);


/*********************/
/* Package Variables */
/*********************/

/* Class info for "single" free space sections */
/* (No callbacks necessary) */
H5FS_section_class_t H5FS_SECT_CLS_FHEAP_SINGLE[1] = {{
    H5FS_SECT_FHEAP_SINGLE,		/* Section type                 */
    0,					/* Extra serialized size        */
    NULL,				/* Initialize section class     */
    NULL,				/* Serialize section            */
    H5HF_sect_single_deserialize,	/* Deserialize section          */
    H5HF_sect_single_can_merge,		/* Can sections merge?          */
    H5HF_sect_single_merge,		/* Merge sections               */
    H5HF_sect_single_can_shrink,	/* Can section shrink container?*/
    H5HF_sect_single_shrink,		/* Shrink container w/section   */
    H5HF_sect_single_free,		/* Free section                 */
    NULL,				/* Dump debugging for section   */
}};

/* Class info for "range" free space sections */
H5FS_section_class_t H5FS_SECT_CLS_FHEAP_RANGE[1] = {{
    H5FS_SECT_FHEAP_RANGE,		/* Section type                 */
    0,					/* Extra serialized size        */
    H5HF_sect_range_init_cls,		/* Initialize section class     */
    H5HF_sect_range_serialize,		/* Serialize section            */
    H5HF_sect_range_deserialize,	/* Deserialize section          */
    NULL,				/* Can sections merge?          */
    NULL,				/* Merge sections               */
    NULL,				/* Can section shrink container?*/
    NULL,				/* Shrink container w/section   */
    H5HF_sect_range_free,		/* Free section                 */
    H5HF_sect_range_debug,		/* Dump debugging for section   */
}};

/* Class info for "indirect" free space sections */
H5FS_section_class_t H5FS_SECT_CLS_FHEAP_INDIRECT[1] = {{
    H5FS_SECT_FHEAP_INDIRECT,		/* Section type                 */
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
 * Function:	H5HF_sect_node_alloc
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
static herr_t
H5HF_sect_node_alloc(H5FS_section_class_t *sect_cls, unsigned sect_type,
    haddr_t sect_addr, hsize_t sect_size, H5FS_section_info_t **sect)
{
    H5HF_free_section_t *new_sect;      /* New section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_node_alloc)

    /* Check arguments. */
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);
    HDassert(sect);

    /* Create free list section node */
    if(NULL == (new_sect = H5FL_MALLOC(H5HF_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

    /* Set the information passed in */
    new_sect->sect_info.addr = sect_addr;
    new_sect->sect_info.size = sect_size;

    /* Set the section's class & state */
    new_sect->sect_info.cls = &sect_cls[sect_type];
    new_sect->sect_info.state = H5FS_SECT_SERIALIZED;

    /* Update the return parameter */
    *sect = (H5FS_section_info_t *)new_sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_node_alloc() */


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
static herr_t
H5HF_sect_single_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_deserialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);
    HDassert(sect);

    /* Create free list section node */
    if(H5HF_sect_node_alloc(sect_cls, H5FS_SECT_FHEAP_SINGLE, sect_addr, sect_size, sect) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "allocation failed for direct block free list section")

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
H5HF_sect_single_can_merge(const H5FS_section_info_t *sect1,
    const H5FS_section_info_t *sect2, void UNUSED *udata)
{
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_single_can_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_lt(sect1->addr, sect2->addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->size = %Hu, sect1->addr = %a\n", "H5HF_sect_single_can_merge", sect1->size, sect1->addr);
HDfprintf(stderr, "%s: sect2->size = %Hu, sect2->addr = %a\n", "H5HF_sect_single_can_merge", sect2->size, sect2->addr);
#endif /* QAK */

    /* Single section can only merge with other single sections */
    if(sect1->cls->type != sect2->cls->type)
        HGOTO_DONE(FALSE)

    /* Check if second section adjoins first section */
    /* (This can only occurs within a direct block, due to the direct block
     *  overhead at the beginning of a block)
     */
    if(H5F_addr_eq(sect1->addr + sect1->size, sect2->addr))
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
H5HF_sect_single_merge(H5FS_section_info_t *sect1, H5FS_section_info_t *sect2,
    void UNUSED *udata)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_eq(sect1->addr + sect1->size, sect2->addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->size = %Hu, sect1->addr = %a\n", "H5HF_sect_single_merge", sect1->size, sect1->addr);
HDfprintf(stderr, "%s: sect2->size = %Hu, sect2->addr = %a\n", "H5HF_sect_single_merge", sect2->size, sect2->addr);
#endif /* QAK */

    /* Add second section's size to first section */
    sect1->size += sect2->size;

    /* Get rid of second section */
    if(H5HF_sect_single_free(sect2) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_can_shrink
 *
 * Purpose:	Can this section shrink the heap?
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
H5HF_sect_single_can_shrink(H5FS_section_info_t *_sect, void *_udata)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr;                    /* Fractal heap header */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block for section */
    haddr_t dblock_addr;                /* Section's direct block's address */
    size_t dblock_size;                 /* Section's direct block's size */
    size_t dblock_overhead;             /* Direct block's overhead */
    hid_t dxpl_id;                      /* DXPL ID for operation */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_can_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(udata);
    HDassert(udata->hdr);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info.size = %Hu, sect->sect_info.addr = %a\n", FUNC, sect->sect_info.size, sect->sect_info.addr);
#endif /* QAK */

    /* Initialize some local convenience variables */
    hdr = udata->hdr;
    dxpl_id = udata->dxpl_id;

    /* Revive the section, if it's still serialized */
    if(sect->sect_info.state != H5FS_SECT_LIVE) {
        if(H5HF_sect_single_revive(hdr, dxpl_id, sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
    } /* end if */

    /* Protect the direct block for the section */
    dblock_addr = sect->u.single.dblock_addr;
    dblock_size = sect->u.single.dblock_size;
#ifdef QAK
HDfprintf(stderr, "%s: dblock_size = %u, dblock_addr = %a\n", FUNC, dblock_size, dblock_addr);
#endif /* QAK */
    if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, sect->u.single.parent, sect->u.single.par_entry, H5AC_READ)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")

    /* Check for section occupying entire direct block */
    dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
    if(H5F_addr_eq(dblock->block_off + dblock_overhead, sect->sect_info.addr) &&
            (dblock_size - dblock_overhead) == sect->sect_info.size) {
        /* Stash the direct block pointer away for the 'shrink' callback */
        udata->dblock = dblock;

        /* Indicate that the heap can be shrunk */
        HGOTO_DONE(TRUE)
    } /* end if */

done:
    /* Unprotect the direct block, if we aren't going to use it in the 'shrink' callback */
    if(ret_value != TRUE)
        if(dblock && H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_can_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_shrink
 *
 * Purpose:	Shrink heap w/section
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
H5HF_sect_single_shrink(H5FS_section_info_t **_sect, void *_udata)
{
    H5HF_free_section_t **sect = (H5HF_free_section_t **)_sect;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(*sect);
    HDassert((*sect)->sect_info.state == H5FS_SECT_LIVE);
    HDassert(udata);
    HDassert(udata->hdr);
    HDassert(udata->dblock);

#ifdef QAK
HDfprintf(stderr, "%s: (*sect)->sect_info.size = %Hu, (*sect)->sect_info.addr = %a\n", "H5HF_sect_single_shrink", (*sect)->sect_info.size, (*sect)->sect_info.addr);
#endif /* QAK */

    /* Destroy direct block */
    if(H5HF_man_dblock_destroy(udata->hdr, udata->dxpl_id, udata->dblock, (*sect)->u.single.dblock_addr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release direct block")

    /* Mark section as "dead", since it's direct block is destroyed */
    (*sect)->sect_info.state = H5FS_SECT_SERIALIZED;

    /* Release section */
    if(H5HF_sect_single_free((H5FS_section_info_t *)*sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Set section pointer to NULL, to indicate that the section was released */
    *sect = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_shrink() */


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
herr_t
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
static herr_t
H5HF_sect_range_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_range_deserialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);
    HDassert(sect);

    /* Create free list section node */
    if(H5HF_sect_node_alloc(sect_cls, H5FS_SECT_FHEAP_RANGE, sect_addr, sect_size, sect) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "allocation failed for direct block free list section")

    /* Range's row */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.range.row);

    /* Range's column */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.range.col);

    /* Range's # of entries */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.range.num_entries);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_range_deserialize() */


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
herr_t
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
static herr_t
H5HF_sect_indirect_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_deserialize)

    /* Check arguments. */
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);
    HDassert(sect);

    /* Create free list section node */
    if(H5HF_sect_node_alloc(sect_cls, H5FS_SECT_FHEAP_INDIRECT, sect_addr, sect_size, sect) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "allocation failed for direct block free list section")

    /* Range's row */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.indirect.row);

    /* Range's column */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.indirect.col);

    /* Range's # of entries */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.indirect.num_entries);

    /* Range's indirect row */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.indirect.indir_row);

    /* Range's indirect # of rows */
    UINT16DECODE(buf, ((H5HF_free_section_t *)*sect)->u.indirect.indir_nrows);

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
herr_t
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

