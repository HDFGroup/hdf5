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

/*
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
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
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Typedef for "class private" information for indirect sections */
typedef struct {
    H5HF_hdr_t *hdr;            /* Pointer to fractal heap header */
} H5HF_sect_indirect_private_t;


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

/* 'single' section callbacks */
static H5FS_section_info_t *H5HF_sect_single_deserialize(const H5FS_section_class_t *cls,
    hid_t dxpl_id, const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    unsigned *des_flags);
static htri_t H5HF_sect_single_can_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static herr_t H5HF_sect_single_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static htri_t H5HF_sect_single_can_shrink(H5FS_section_info_t *sect,
    void *udata);
static herr_t H5HF_sect_single_shrink(H5FS_section_info_t **_sect,
    void *udata);
static herr_t H5HF_sect_single_free(H5FS_section_info_t *sect);
static herr_t H5HF_sect_single_valid(const H5FS_section_class_t *cls,
    const H5FS_section_info_t *sect);

/* 'row' section routines */
static H5HF_free_section_t *H5HF_sect_row_create(haddr_t sect_off,
    hsize_t sect_size, hbool_t is_first, unsigned row, unsigned col,
    unsigned nentries, H5HF_free_section_t *under_sect);
static herr_t H5HF_sect_row_first(H5HF_hdr_t *hdr, H5HF_free_section_t *sect);
static herr_t H5HF_sect_row_from_single(H5HF_hdr_t *hdr,
    H5HF_free_section_t *sect, H5HF_direct_t *dblock);
static herr_t H5HF_sect_row_iblock_same(const H5HF_free_section_t *sect1,
    const H5HF_free_section_t *sect2);

/* 'row' section callbacks */
static herr_t H5HF_sect_row_init_cls(H5FS_section_class_t *cls, void *udata);
static herr_t H5HF_sect_row_term_cls(H5FS_section_class_t *cls);
static herr_t H5HF_sect_row_serialize(const H5FS_section_class_t *cls,
    const H5FS_section_info_t *sect, uint8_t *buf);
static H5FS_section_info_t *H5HF_sect_row_deserialize(const H5FS_section_class_t *cls,
    hid_t dxpl_id, const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    unsigned *des_flags);
static htri_t H5HF_sect_row_can_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static herr_t H5HF_sect_row_merge(H5FS_section_info_t *sect1,
    H5FS_section_info_t *sect2, void *udata);
static htri_t H5HF_sect_row_can_shrink(H5FS_section_info_t *sect,
    void *udata);
static herr_t H5HF_sect_row_shrink(H5FS_section_info_t **sect,
    void *udata);
static herr_t H5HF_sect_row_free(H5FS_section_info_t *sect);
static herr_t H5HF_sect_row_valid(const H5FS_section_class_t *cls,
    const H5FS_section_info_t *sect);

/* 'indirect' section routines */
static H5HF_free_section_t *H5HF_sect_indirect_new(haddr_t sect_off,
    hsize_t sect_size, H5HF_indirect_t *iblock, hsize_t iblock_off,
    unsigned row, unsigned col, unsigned nentries);
static herr_t H5HF_sect_indirect_init_rows(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect, hbool_t first_child, unsigned space_flags,
    unsigned start_row, unsigned start_col, unsigned end_row, unsigned end_col);
static H5HF_free_section_t *H5HF_sect_indirect_for_row(H5HF_hdr_t *hdr,
    H5HF_indirect_t *iblock, H5HF_free_section_t *row_sect);
static herr_t H5HF_sect_indirect_decr(H5HF_free_section_t *sect);
static herr_t H5HF_sect_indirect_revive_row(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect);
static herr_t H5HF_sect_indirect_revive(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect, H5HF_indirect_t *sect_iblock);
static herr_t H5HF_sect_indirect_reduce_row(H5HF_hdr_t *hdr,
    H5HF_free_section_t *row_sect, hbool_t *alloc_from_start);
static herr_t H5HF_sect_indirect_reduce(H5HF_hdr_t *hdr,
    H5HF_free_section_t *sect, unsigned child_entry);
static herr_t H5HF_sect_indirect_first(H5HF_hdr_t *hdr, H5HF_free_section_t *sect);
static hbool_t H5HF_sect_indirect_is_first(H5HF_free_section_t *sect);
static H5HF_indirect_t * H5HF_sect_indirect_get_iblock(H5HF_free_section_t *sect);
static herr_t H5HF_sect_indirect_span_size(H5HF_hdr_t *hdr,
    H5HF_free_section_t *sect);
static herr_t H5HF_sect_indirect_iblock_entries(H5HF_hdr_t *hdr,
    H5HF_free_section_t *sect);
static herr_t H5HF_sect_indirect_iblock_same(const H5HF_free_section_t *sect1,
    const H5HF_free_section_t *sect2);
static hbool_t H5HF_sect_indirect_is_last_row(const H5HF_free_section_t *row_sect);
static herr_t H5HF_sect_indirect_merge_row(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect1, H5HF_free_section_t *sect2);
static herr_t H5HF_sect_indirect_shrink_row(H5HF_free_section_t *row_sect);
static herr_t H5HF_sect_indirect_serialize(H5HF_hdr_t *hdr,
    const H5HF_free_section_t *sect, uint8_t *buf);
static H5FS_section_info_t *H5HF_sect_indirect_deserialize(H5HF_hdr_t *hdr,
    hid_t dxpl_id, const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    unsigned *des_flags);
static herr_t H5HF_sect_indirect_free(H5HF_free_section_t *sect);
static herr_t H5HF_sect_indirect_valid(const H5FS_section_class_t *row_cls,
    const H5HF_free_section_t *row_sect);

/* 'indirect' section callbacks */
static herr_t H5HF_sect_indirect_init_cls(H5FS_section_class_t *cls, void *udata);
static herr_t H5HF_sect_indirect_term_cls(H5FS_section_class_t *cls);
static herr_t H5HF_sect_indirect_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);


/*********************/
/* Package Variables */
/*********************/

/* Class info for "single" free space sections */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_SINGLE[1] = {{
    /* Class variables */
    H5HF_FSPACE_SECT_SINGLE,		/* Section type                 */
    0,					/* Extra serialized size        */
    0,					/* Class flags                  */
    NULL,				/* Class private info           */

    /* Class methods */
    NULL,				/* Initialize section class     */
    NULL,				/* Terminate section class      */

    /* Object methods */
    NULL,				/* Serialize section            */
    H5HF_sect_single_deserialize,	/* Deserialize section          */
    H5HF_sect_single_can_merge,		/* Can sections merge?          */
    H5HF_sect_single_merge,		/* Merge sections               */
    H5HF_sect_single_can_shrink,	/* Can section shrink container?*/
    H5HF_sect_single_shrink,		/* Shrink container w/section   */
    H5HF_sect_single_free,		/* Free section                 */
    H5HF_sect_single_valid,		/* Check validity of section    */
    NULL,				/* Dump debugging for section   */
}};

/* Class info for "first row" free space sections */
/* (Same as "normal" row sections, except they also act as a proxy for the
 *      underlying indirect section
 */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_FIRST_ROW[1] = {{
    /* Class variables */
    H5HF_FSPACE_SECT_FIRST_ROW,		/* Section type                 */
    0,					/* Extra serialized size        */
    0,					/* Class flags                  */
    NULL,				/* Class private info           */

    /* Class methods */
    H5HF_sect_row_init_cls,		/* Initialize section class     */
    H5HF_sect_row_term_cls,		/* Terminate section class      */

    /* Object methods */
    H5HF_sect_row_serialize,		/* Serialize section            */
    H5HF_sect_row_deserialize,		/* Deserialize section          */
    H5HF_sect_row_can_merge,		/* Can sections merge?          */
    H5HF_sect_row_merge,		/* Merge sections               */
    H5HF_sect_row_can_shrink,		/* Can section shrink container?*/
    H5HF_sect_row_shrink,		/* Shrink container w/section   */
    H5HF_sect_row_free,			/* Free section                 */
    H5HF_sect_row_valid,		/* Check validity of section    */
    NULL,				/* Dump debugging for section   */
}};

/* Class info for "normal row" free space sections */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_NORMAL_ROW[1] = {{
    /* Class variables */
    H5HF_FSPACE_SECT_NORMAL_ROW,	/* Section type                 */
    0,					/* Extra serialized size        */
    H5FS_CLS_GHOST_OBJ,			/* Class flags                  */
    NULL,				/* Class private info           */

    /* Class methods */
    NULL,				/* Initialize section class     */
    NULL,				/* Terminate section class      */

    /* Object methods */
    NULL,				/* Serialize section            */
    NULL,				/* Deserialize section          */
    H5HF_sect_row_can_merge,		/* Can sections merge?          */
    H5HF_sect_row_merge,		/* Merge sections               */
    H5HF_sect_row_can_shrink,		/* Can section shrink container?*/
    H5HF_sect_row_shrink,		/* Shrink container w/section   */
    H5HF_sect_row_free,			/* Free section                 */
    H5HF_sect_row_valid,		/* Check validity of section    */
    NULL,				/* Dump debugging for section   */
}};

/* Class info for "indirect" free space sections */
/* (No object callbacks necessary - objects of this class should never be in
 *      section manager)
 */
H5FS_section_class_t H5HF_FSPACE_SECT_CLS_INDIRECT[1] = {{
    /* Class variables */
    H5HF_FSPACE_SECT_INDIRECT,		/* Section type                 */
    0,					/* Extra serialized size        */
    H5FS_CLS_GHOST_OBJ,			/* Class flags                  */
    NULL,				/* Class private info           */

    /* Class methods */
    H5HF_sect_indirect_init_cls,	/* Initialize section class     */
    H5HF_sect_indirect_term_cls,	/* Terminate section class      */

    /* Object methods */
    NULL,				/* Serialize section            */
    NULL,				/* Deserialize section          */
    NULL,				/* Can sections merge?          */
    NULL,				/* Merge sections               */
    NULL,				/* Can section shrink container?*/
    NULL,				/* Shrink container w/section   */
    NULL,				/* Free section                 */
    NULL,				/* Check validity of section    */
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
H5HF_sect_node_free(H5HF_free_section_t *sect, H5HF_indirect_t *iblock)
{
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_node_free)

    HDassert(sect);

    /* Release indirect block, if there was one */
    if(iblock)
        if(H5HF_iblock_decr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on section's indirect block")

    /* Release the section */
    H5FL_FREE(H5HF_free_section_t, sect);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_node_free() */


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
        if(H5HF_space_add(hdr, dxpl_id, sect, 0) < 0)
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
H5HF_sect_single_deserialize(const H5FS_section_class_t UNUSED *cls,
    hid_t UNUSED dxpl_id, const uint8_t *buf, haddr_t sect_addr,
    hsize_t sect_size, unsigned UNUSED *des_flags)
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
    H5FS_section_info_t *_sect2, void UNUSED *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_single_can_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_lt(sect1->sect_info.addr, sect2->sect_info.addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_single_can_merge", sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_single_can_merge", sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Single section can only merge with other single sections */
    if(sect1->sect_info.type != sect2->sect_info.type)
        HGOTO_DONE(FALSE)

    /* Check if second section adjoins first section */
    /* (This can only occur within a direct block, due to the direct block
     *  overhead at the beginning of a block, so no need to check if sections
     *  are actually within the same direct block)
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
    void *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    size_t dblock_size;                 /* Section's direct block's size */
    size_t dblock_overhead;             /* Direct block's overhead */
    hid_t dxpl_id = udata->dxpl_id;     /* DXPL ID for operation */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_eq(sect1->sect_info.addr + sect1->sect_info.size, sect2->sect_info.addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Add second section's size to first section */
    sect1->sect_info.size += sect2->sect_info.size;

    /* Get rid of second section */
    if(H5HF_sect_single_free((H5FS_section_info_t *)sect2) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Check to see if we should revive first section */
    if(sect1->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_single_revive(hdr, dxpl_id, sect1) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")

    /* Check for section occupying entire direct block */
    /* (and not the root direct block) */
    dblock_size = sect1->u.single.dblock_size;
    dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
#ifdef QAK
HDfprintf(stderr, "%s: dblock_size = %u\n", FUNC, dblock_size);
HDfprintf(stderr, "%s: dblock_overhead = %Zu\n", FUNC, dblock_overhead);
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", FUNC, hdr->man_iter_off);
#endif /* QAK */
    if((dblock_size - dblock_overhead) == sect1->sect_info.size &&
            hdr->man_dtable.curr_root_rows > 0) {
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

        /* Convert 'single' section into 'row' section */
        if(H5HF_sect_row_from_single(hdr, sect1, dblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCONVERT, FAIL, "can't convert single section into row section")

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect1->u.row = {%p, %u, %u, %u, %t}\n", FUNC, sect1->u.row.under, sect1->u.row.row, sect1->u.row.col, sect1->u.row.num_entries, sect1->u.row.checked_out);
#endif /* QAK */

        /* Destroy direct block */
        if(H5HF_man_dblock_destroy(hdr, dxpl_id, dblock, dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release direct block")
        dblock = NULL;
    } /* end if */

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
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_single_can_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Check for section occupying entire direct block */
    dblock_size = sect->u.single.dblock_size;
    dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
#ifdef QAK
HDfprintf(stderr, "%s: dblock_size = %Zu\n", FUNC, dblock_size);
HDfprintf(stderr, "%s: dblock_overhead = %Zu\n", FUNC, dblock_overhead);
#endif /* QAK */
    if((dblock_size - dblock_overhead) == sect->sect_info.size) {
        /* We shouldn't ever have a single section that occupies an entire
         *      direct block, unless it's in the root direct block (and
         *      therefore there's no parent indirect block to hook into)
         */
        HDassert(hdr->man_dtable.curr_root_rows == 0);
        HGOTO_DONE(TRUE)
    } /* end if */
    else {
        /* We shouldn't have a situation where the 'next block' iterator
         *      is moved before a direct block that still has objects within it.
         */
        HDassert(hdr->man_dtable.curr_root_rows == 0 || hdr->man_iter_off > sect->sect_info.addr);
        HGOTO_DONE(FALSE)
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_single_can_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_single_shrink
 *
 * Purpose:	Shrink container with section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_single_shrink(H5FS_section_info_t **_sect, void UNUSED *_udata)
{
    H5HF_free_section_t **sect = (H5HF_free_section_t **)_sect;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    hid_t dxpl_id = udata->dxpl_id;     /* DXPL ID for operation */
    H5HF_direct_t *dblock;          /* Pointer to direct block for section */
    haddr_t dblock_addr;            /* Section's direct block's address */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_single_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(*sect);
    HDassert((*sect)->sect_info.type == H5HF_FSPACE_SECT_SINGLE);

#ifdef QAK
HDfprintf(stderr, "%s: (*sect).sect_info = {%a, %Hu, %u}\n", FUNC, (*sect)->sect_info.addr, (*sect)->sect_info.size, (*sect)->sect_info.type);
#endif /* QAK */

    /* Check to see if we should revive either section */
    if((*sect)->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_single_revive(hdr, dxpl_id, (*sect)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")

    /* Protect the direct block for the section */
    dblock_addr = (*sect)->u.single.dblock_addr;
    HDassert(dblock_addr == hdr->man_dtable.table_addr);
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */
    if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, 
            (*sect)->u.single.dblock_size, (*sect)->u.single.parent, (*sect)->u.single.par_entry, H5AC_READ)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")
    HDassert(H5F_addr_eq(dblock->block_off + (*sect)->u.single.dblock_size, (*sect)->sect_info.addr + (*sect)->sect_info.size));

    /* Destroy direct block */
    if(H5HF_man_dblock_destroy(hdr, dxpl_id, dblock, dblock_addr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release direct block")
    dblock = NULL;

    /* Get rid of section */
    if(H5HF_sect_single_free((H5FS_section_info_t *)*sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Indicate that the section has been released */
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
 * Function:	H5HF_sect_single_valid
 *
 * Purpose:	Check the validity of a section
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, July 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_single_valid(const H5FS_section_class_t *cls, const H5FS_section_info_t *_sect)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;   /* Pointer to section to check */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_single_valid)

    HDassert(cls);
    HDassert(sect);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_single_valid", sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        /* Check if this section is not in a direct block that is the root direct block */
        /* (not enough information to check on a single section in a root direct block) */
        if(sect->u.single.parent != NULL) {
            H5HF_indirect_t *iblock;    /* Indirect block that section's direct block resides in */
            H5HF_direct_t *dblock;      /* Direct block for section */
            herr_t status;              /* Generic status value */

#ifdef QAK
HDfprintf(stderr, "%s: sect->u.single = {%p, %u, %a, %Zu}\n", "H5HF_sect_single_valid", sect->u.single.parent, sect->u.single.par_entry, sect->u.single.dblock_addr, sect->u.single.dblock_size);
#endif /* QAK */
            /* Sanity check settings for section's direct block's parent */
            iblock = sect->u.single.parent;
            HDassert(H5F_addr_defined(iblock->ents[sect->u.single.par_entry].addr));
            HDassert(H5F_addr_eq(iblock->ents[sect->u.single.par_entry].addr,
                    sect->u.single.dblock_addr));

            /* Protect the direct block for the section */
            dblock = H5HF_man_dblock_protect(iblock->hdr, H5AC_dxpl_id, sect->u.single.dblock_addr, sect->u.single.dblock_size, iblock, sect->u.single.par_entry, H5AC_READ);
            HDassert(dblock);

            /* Sanity check settings for section */
            HDassert(sect->u.single.dblock_size > 0);
            HDassert(sect->u.single.dblock_size == dblock->size);
            HDassert(dblock->size > sect->sect_info.size);
            HDassert(H5F_addr_lt(dblock->block_off, sect->sect_info.addr));
            HDassert(H5F_addr_ge((dblock->block_off + dblock->size),
                    (sect->sect_info.addr + sect->sect_info.size)));

            /* Release direct block */
            status = H5AC_unprotect(iblock->hdr->f, H5AC_dxpl_id, H5AC_FHEAP_DBLOCK, sect->u.single.dblock_addr, dblock, H5AC__NO_FLAGS_SET);
            HDassert(status >= 0);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* H5HF_sect_single_valid() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_create
 *
 * Purpose:	Create a new 'row' section
 *
 * Return:	Success:	pointer to new section
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_free_section_t *
H5HF_sect_row_create(haddr_t sect_off, hsize_t sect_size, hbool_t is_first,
    unsigned row, unsigned col, unsigned nentries, H5HF_free_section_t *under_sect)
{
    H5HF_free_section_t *sect = NULL;   /* 'Row' section created */
    H5HF_free_section_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_create)

    /* Check arguments. */
    HDassert(sect_size);
    HDassert(nentries);
    HDassert(under_sect);

    /* Create 'row' free space section node */
    /* ("inherits" underlying indirect section's state) */
    if(NULL == (sect = H5HF_sect_node_new((unsigned)(is_first ? H5HF_FSPACE_SECT_FIRST_ROW : H5HF_FSPACE_SECT_NORMAL_ROW), sect_off, sect_size, under_sect->sect_info.state)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for row section")

    /* Set the 'row' specific fields */
    sect->u.row.under = under_sect;
    sect->u.row.row = row;
    sect->u.row.col = col;
    sect->u.row.num_entries = nentries;
    sect->u.row.checked_out = FALSE;

    /* Set return value */
    ret_value = sect;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_from_single
 *
 * Purpose:	Convert a 'single' section into a 'row' section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_from_single(H5HF_hdr_t *hdr, H5HF_free_section_t *sect,
    H5HF_direct_t *dblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_from_single)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(dblock);
#ifdef QAK
HDfprintf(stderr, "%s: sect.sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: dblock->parent = %p\n", FUNC, dblock->parent);
HDfprintf(stderr, "%s: hdr->man_dtable.curr_root_rows = %u\n", FUNC, hdr->man_dtable.curr_root_rows);
#endif /* QAK */

    /* Convert 'single' section information to 'row' section info */
    sect->sect_info.addr = dblock->block_off;
    sect->sect_info.type = H5HF_FSPACE_SECT_FIRST_ROW;
    sect->u.row.row = dblock->par_entry / hdr->man_dtable.cparam.width;
    sect->u.row.col = dblock->par_entry % hdr->man_dtable.cparam.width;
    sect->u.row.num_entries = 1;
    sect->u.row.checked_out = FALSE;

    /* Create indirect section that underlies the row section */
    if(NULL == (sect->u.row.under = H5HF_sect_indirect_for_row(hdr, dblock->parent, sect)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "serializing row section not supported yet")

    /* Release single section's hold on underlying indirect block */
    if(H5HF_iblock_decr(dblock->parent) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_row_from_single() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_iblock_same
 *
 * Purpose:	Check if two row sections are located in the same indirect block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_row_iblock_same(const H5HF_free_section_t *row_sect1,
    const H5HF_free_section_t *row_sect2)
{
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_row_iblock_same)

    /*
     * Check arguments.
     */
    HDassert(row_sect1);
    HDassert(row_sect2);

    /* Compare the underlying indirect sections */
    ret_value = H5HF_sect_indirect_iblock_same(row_sect1->u.row.under, row_sect2->u.row.under);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_row_iblock_same() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_revive
 *
 * Purpose:	Update the memory information for a 'row' free section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_row_revive(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_revive)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->u.row.under);
    HDassert(sect->sect_info.state == H5FS_SECT_SERIALIZED);

    /* Pass along "revive" request to underlying indirect section */
    /* (which will mark this section as "live") */
    if(H5HF_sect_indirect_revive_row(hdr, dxpl_id, sect->u.row.under) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTREVIVE, FAIL, "can't revive indirect section")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_row_revive() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_reduce
 *
 * Purpose:	Reduce the size of a row section (possibly freeing it)
 *              and re-add it back to the free space manager for the heap
 *              (if it hasn't been freed)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_row_reduce(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *sect,
    unsigned *entry_p)
{
    hbool_t alloc_from_start;           /* Whether to allocate from the end of the row */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_reduce)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sect->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);
    HDassert(entry_p);
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect->u.row = {%p, %u, %u, %u, %t}\n", FUNC, sect->u.row.under, sect->u.row.row, sect->u.row.col, sect->u.row.num_entries, sect->u.row.checked_out);
#endif /* QAK */

    /* Mark the row as checked out from the free space manager */
    HDassert(sect->u.row.checked_out == FALSE);
    sect->u.row.checked_out = TRUE;

    /* Forward row section to indirect routines, to handle reducing underlying indirect section */
    alloc_from_start = FALSE;
    if(H5HF_sect_indirect_reduce_row(hdr, sect, &alloc_from_start) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce underlying section")

    /* Determine entry allocated */
    *entry_p = (sect->u.row.row * hdr->man_dtable.cparam.width) + sect->u.row.col;
    if(!alloc_from_start)
        *entry_p += (sect->u.row.num_entries - 1);

    /* Check for eliminating the section */
    if(sect->u.row.num_entries == 1) {
#ifdef QAK
HDfprintf(stderr, "%s: Freeing row section\n", FUNC);
#endif /* QAK */
        /* Free row section */
        if(H5HF_sect_row_free((H5FS_section_info_t *)sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free row section node")
    } /* end if */
    else {
        /* Check whether to allocate from the beginning or end of the row */
        if(alloc_from_start) {
            /* Adjust section start */
            sect->sect_info.addr += hdr->man_dtable.row_block_size[sect->u.row.row];
            sect->u.row.col++;
        } /* end else */

        /* Adjust span of blocks covered */
        sect->u.row.num_entries--;

        /* Check the row back in */
        sect->u.row.checked_out = FALSE;

        /* Add 'row' section back to free space list */
        if(H5HF_space_add(hdr, dxpl_id, sect, 0) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't re-add indirect section to free space manager")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_row_reduce() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_first
 *
 * Purpose:	Make row a "first row"
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 10 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_first(H5HF_hdr_t *hdr, H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_first)

    /* Sanity check */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);

    /* If the row is already checked out from the free space manager, just
     *  change it's class directly and the free space manager will adjust when
     *  it is checked back in.
     */
    if(sect->u.row.checked_out)
        sect->sect_info.type = H5HF_FSPACE_SECT_FIRST_ROW;
    else {
        /* Change row section to be the "first row" */
        if(H5HF_space_sect_change_class(hdr, sect, H5HF_FSPACE_SECT_FIRST_ROW) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSET, FAIL, "can't set row section to be first row")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_row_first() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_get_iblock
 *
 * Purpose:	Retrieve the indirect block for a row section
 *
 * Return:	Pointer to indirect block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  9 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_indirect_t *
H5HF_sect_row_get_iblock(H5HF_free_section_t *sect)
{
    H5HF_indirect_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_row_get_iblock)

    /*
     * Check arguments.
     */
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sect->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

    ret_value = H5HF_sect_indirect_get_iblock(sect->u.row.under);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_row_get_iblock() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_init_cls
 *
 * Purpose:	Initialize the "row" section class structure
 *
 * Note:	Since 'row' sections are proxies for 'indirect' sections, this
 *              routine forwards call to 'indirect' class initialization
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_init_cls(H5FS_section_class_t *cls, void *_udata)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_init_cls)

    /* Check arguments. */
    HDassert(cls);
    HDassert(!cls->cls_private);

    /* Forward call to indirect class initialization */
    if(H5HF_sect_indirect_init_cls(cls, _udata) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize row section class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_init_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_term_cls
 *
 * Purpose:	Terminate the "row" section class structure
 *
 * Note:	Since 'row' sections are proxies for 'indirect' sections, this
 *              routine forwards call to 'indirect' class termination
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_term_cls(H5FS_section_class_t *cls)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_term_cls)

    /* Check arguments. */
    HDassert(cls);

    /* Forward call to indirect class termination */
    if(H5HF_sect_indirect_term_cls(cls) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't terminate row section class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_term_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_serialize
 *
 * Purpose:	Serialize a "live" row section into a buffer
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_serialize(const H5FS_section_class_t *cls,
    const H5FS_section_info_t *_sect, uint8_t *buf)
{
    H5HF_hdr_t *hdr;                    /* Fractal heap header */
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_serialize)

    /* Check arguments. */
    HDassert(cls);
    HDassert(buf);
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW);
    HDassert(sect->sect_info.addr == sect->u.row.under->sect_info.addr);

    /* Forward to indirect routine to serialize underlying section */
    hdr = ((H5HF_sect_indirect_private_t *)(cls->cls_private))->hdr;
    if(H5HF_sect_indirect_serialize(hdr, sect->u.row.under, buf) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSERIALIZE, FAIL, "can't serialize row section's underlying indirect section")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_deserialize
 *
 * Purpose:	Deserialize a buffer into a "live" row section
 *
 * Note:        Actually this routine just forwards to the 'indirect'
 *              deserialize routine, which creates the row section.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, July 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_section_info_t *
H5HF_sect_row_deserialize(const H5FS_section_class_t *cls, hid_t dxpl_id,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    unsigned *des_flags)
{
    H5HF_hdr_t *hdr;                    /* Fractal heap header */
    H5FS_section_info_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_deserialize)

    /* Check arguments. */
    HDassert(cls);
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);

    /* Forward to indirect routine to deserialize underlying section */
    hdr = ((H5HF_sect_indirect_private_t *)(cls->cls_private))->hdr;
    if(NULL ==  (ret_value = H5HF_sect_indirect_deserialize(hdr, dxpl_id, buf,
            sect_addr, sect_size, des_flags)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDECODE, NULL, "can't deserialize row section's underlying indirect section")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_can_merge
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
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_row_can_merge(H5FS_section_info_t *_sect1,
    H5FS_section_info_t *_sect2, void *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_row_can_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect2);
    HDassert(H5F_addr_lt(sect1->sect_info.addr, sect2->sect_info.addr));

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_row_can_merge", sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_row_can_merge", sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

#ifdef QAK
HDfprintf(stderr, "%s: sect1->u.row = {%p, %u, %u, %u, %t}\n", "H5HF_sect_row_can_merge", sect1->u.row.under, sect1->u.row.row, sect1->u.row.col, sect1->u.row.num_entries, sect1->u.row.checked_out);
if(sect2->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sect2->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW)
    HDfprintf(stderr, "%s: sect2->u.row = {%p, %u, %u, %u, %t}\n", "H5HF_sect_row_can_merge", sect2->u.row.under, sect2->u.row.row, sect2->u.row.col, sect2->u.row.num_entries, sect2->u.row.checked_out);
#endif /* QAK */

    /* Row section can only merge with other row sections */
    if(!((sect1->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
                sect1->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW)
            && (sect2->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
                sect2->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW)))
        HGOTO_DONE(FALSE)

#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", "H5HF_sect_row_can_merge", hdr->man_iter_off);
#endif /* QAK */
    /* If second section is after current 'next block' iterator and it's the
     *  last row in the underlying indirect section, it should go away
     */
    if(sect2->sect_info.addr >= hdr->man_iter_off && H5HF_sect_indirect_is_last_row(sect2))
        HGOTO_DONE(TRUE)

    /* Check if second section shares the same underlying indirect block as
     *  the first section, but doesn't already have same underlying indirect
     *  section.
     */
    if(H5HF_sect_row_iblock_same(sect1, sect2)) {
        HDassert((sect1->u.row.row != sect2->u.row.row) ||
                (((sect1->u.row.col + sect1->u.row.num_entries) - 1)
                    < sect2->u.row.col));
        if(sect1->u.row.under != sect2->u.row.under) {
            size_t dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr); /* Direct block's overhead */

#ifdef QAK
HDfprintf(stderr, "%s: dblock_overhead = %Zu\n", "H5HF_sect_row_can_merge", dblock_overhead);
#endif /* QAK */
            /* Check if second section adjoins first section */
            if(H5F_addr_eq(sect1->sect_info.addr + (sect1->u.row.num_entries * (sect1->sect_info.size + dblock_overhead)), sect2->sect_info.addr)) {
                udata->adjoin = TRUE;
                HGOTO_DONE(TRUE)
            } /* end if */
        } /* end if */
    } /* end if */

done:
#ifdef QAK
HDfprintf(stderr, "%s: ret_value = %t\n", "H5HF_sect_row_can_merge", ret_value);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_can_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_merge
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
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_merge(H5FS_section_info_t *_sect1, H5FS_section_info_t *_sect2,
    void *_udata)
{
    H5HF_free_section_t *sect1 = (H5HF_free_section_t *)_sect1;   /* Fractal heap free section */
    H5HF_free_section_t *sect2 = (H5HF_free_section_t *)_sect2;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    hid_t dxpl_id = udata->dxpl_id;     /* DXPL ID for operation */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_merge)

    /* Check arguments. */
    HDassert(sect1);
    HDassert(sect1->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sect1->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);
    HDassert(sect2);
    HDassert(sect2->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sect2->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);

#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Check to see if we should revive second section */
    if(sect2->sect_info.state != H5FS_SECT_LIVE)
        if(H5HF_sect_row_revive(hdr, dxpl_id, sect2) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
#ifdef QAK
HDfprintf(stderr, "%s: sect2->u.row = {%p, %u, %u, %u}\n", FUNC, sect2->u.row.under, sect2->u.row.row, sect2->u.row.col, sect2->u.row.num_entries);
#endif /* QAK */

    /* Check if rows adjoin each other */
    /* (as opposed to second row just being after the end of the heap and
     *  needed to be pruned away)
     */
    if(udata->adjoin) {
#ifdef QAK
HDfprintf(stderr, "%s: Merging underlying indirect blocks\n", FUNC);
#endif /* QAK */
        /* Check to see if we should revive first section */
        if(sect1->sect_info.state != H5FS_SECT_LIVE)
            if(H5HF_sect_row_revive(hdr, dxpl_id, sect1) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
#ifdef QAK
HDfprintf(stderr, "%s: sect1->u.row = {%p, %u, %u, %u}\n", FUNC, sect1->u.row.under, sect1->u.row.row, sect1->u.row.col, sect1->u.row.num_entries);
#endif /* QAK */

        /* Merge rows' underlying indirect sections together */
        if(H5HF_sect_indirect_merge_row(hdr, dxpl_id, sect1, sect2) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTMERGE, FAIL, "can't merge underlying indirect sections")
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: Shrinking away second section\n", FUNC);
#endif /* QAK */
        /* Shrink size of underlying indirect section */
        if(H5HF_sect_indirect_shrink_row(sect2) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't shrink underlying indirect section")

        /* Get rid of second section */
        if(H5HF_sect_row_free((H5FS_section_info_t *)sect2) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")
        sect2 = NULL;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_merge() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_can_shrink
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
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_row_can_shrink(H5FS_section_info_t *_sect, void UNUSED *_udata)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;   /* Fractal heap free section */
    H5HF_add_ud1_t *udata = (H5HF_add_ud1_t *)_udata;   /* User callback data */
    H5HF_hdr_t *hdr = udata->hdr;       /* Fractal heap header */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_row_can_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sect->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_row_can_shrink", sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect->u.row = {%p, %u, %u, %u}\n", "H5HF_sect_row_can_shrink", sect->u.row.under, sect->u.row.row, sect->u.row.col, sect->u.row.num_entries);
#endif /* QAK */

    /* Check if section is past end of "next block" iterator */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", "H5HF_sect_row_can_shrink", hdr->man_iter_off);
#endif /* QAK */
    if(sect->sect_info.addr >= hdr->man_iter_off && H5HF_sect_indirect_is_last_row(sect))
        HGOTO_DONE(TRUE)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_can_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_shrink
 *
 * Purpose:	Shrink container with section
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_shrink(H5FS_section_info_t **_sect, void UNUSED *_udata)
{
    H5HF_free_section_t **sect = (H5HF_free_section_t **)_sect;   /* Fractal heap free section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_shrink)

    /* Check arguments. */
    HDassert(sect);
    HDassert(*sect);
    HDassert((*sect)->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            (*sect)->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);

#ifdef QAK
HDfprintf(stderr, "%s: (*sect)->sect_info = {%a, %Hu, %u, %s}\n", FUNC, (*sect)->sect_info.addr, (*sect)->sect_info.size, (*sect)->sect_info.type, ((*sect)->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: (*sect)->u.row = {%p, %u, %u, %u}\n", FUNC, (*sect)->u.row.under, (*sect)->u.row.row, (*sect)->u.row.col, (*sect)->u.row.num_entries);
#endif /* QAK */

    /* Shrink size of underlying indirect section */
    if(H5HF_sect_indirect_shrink_row(*sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't shrink underlying indirect section")

    /* Get rid of section */
    if(H5HF_sect_row_free((H5FS_section_info_t *)*sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

    /* Indicate that the section has been released */
    *sect = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_row_shrink() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_free
 *
 * Purpose:	Free a 'row' section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_free(H5FS_section_info_t *_sect)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;   /* Pointer to section to free */
    herr_t ret_value = SUCCEED;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_row_free)

    HDassert(sect);
    HDassert(sect->u.row.under);

    /* Decrement the ref. count on the row section's underlying indirect section */
    if(H5HF_sect_indirect_decr(sect->u.row.under) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't detach section node")

    /* Release the section */
    if(H5HF_sect_node_free(sect, NULL) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_row_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_row_valid
 *
 * Purpose:	Check the validity of a section
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, July 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_row_valid(const H5FS_section_class_t *cls, const H5FS_section_info_t *_sect)
{
    const H5HF_free_section_t *sect = (const H5HF_free_section_t *)_sect;   /* Pointer to section to check */
    const H5HF_free_section_t *indir_sect;   /* Pointer to underlying indirect section */
    unsigned indir_idx;         /* Index of row in underlying indirect section's row array */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_row_valid)

    HDassert(cls);
    HDassert(sect);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_row_valid", sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect->u.row = {%p, %u, %u, %u, %t}\n", "H5HF_sect_row_valid", sect->u.row.under, sect->u.row.row, sect->u.row.col, sect->u.row.num_entries, sect->u.row.checked_out);
#endif /* QAK */
    /* Sanity checking on the row */
    HDassert(sect->u.row.under);
    HDassert(sect->u.row.num_entries);
    HDassert(sect->u.row.checked_out == FALSE);
    indir_sect = sect->u.row.under;
    indir_idx = sect->u.row.row - indir_sect->u.indirect.row;
#ifdef QAK
HDfprintf(stderr, "%s: indir_idx = %u\n", "H5HF_sect_row_valid", indir_idx);
#endif /* QAK */
    HDassert(indir_sect->u.indirect.dir_rows[indir_idx] == sect);

    /* Different checking for different kinds of rows */
    if(sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW) {
        /* Some extra sanity checks on the row */
        HDassert(sect->u.row.row == indir_sect->u.indirect.row);

        /* Check that the row's underlying indirect section is valid */
        H5HF_sect_indirect_valid(cls, sect);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* H5HF_sect_row_valid() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_span_size
 *
 * Purpose:	Compute the span size covered by an indirect section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  3 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_span_size(H5HF_hdr_t *hdr, H5HF_free_section_t *sect)
{
    unsigned start_row;         /* Row for first block covered */
    unsigned start_col;         /* Column for first block covered */
    unsigned start_entry;       /* Entry for first block covered */
    unsigned end_row;           /* Row for last block covered */
    unsigned end_col;           /* Column for last block covered */
    unsigned end_entry;         /* Entry for last block covered */
    hsize_t acc_span_size;      /* Accumulated span size */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_span_size)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->u.indirect.num_entries > 0);

    /* Compute starting entry, column & row */
    start_row = sect->u.indirect.row;
    start_col = sect->u.indirect.col;
    start_entry = (start_row * hdr->man_dtable.cparam.width) + start_col;

    /* Compute ending entry, column & row */
    end_entry = (start_entry + sect->u.indirect.num_entries) - 1;
    end_row = end_entry / hdr->man_dtable.cparam.width;
    end_col = end_entry % hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: start_row = %u, start_col = %u, start_entry = %u\n", "H5HF_sect_indirect_span_size", start_row, start_col, start_entry);
HDfprintf(stderr, "%s: end_row = %u, end_col = %u, end_entry = %u\n", "H5HF_sect_indirect_span_size", end_row, end_col, end_entry);
#endif /* QAK */

    /* Initialize accumulated span size */
    acc_span_size = 0;

    /* Compute span size covered */

    /* Check for multi-row span */
    if(start_row != end_row) {
        /* Accomodate partial starting row */
        if(start_col > 0) {
            acc_span_size = hdr->man_dtable.row_block_size[start_row] *
                    (hdr->man_dtable.cparam.width - start_col);
            start_row++;
        } /* end if */

        /* Accumulate full rows */
        while(start_row < end_row) {
            acc_span_size += hdr->man_dtable.row_block_size[start_row] *
                    hdr->man_dtable.cparam.width;
            start_row++;
        } /* end while */

        /* Accomodate partial ending row */
        acc_span_size += hdr->man_dtable.row_block_size[start_row] *
                (end_col + 1);
    } /* end if */
    else {
        /* Span is in same row */
        acc_span_size = hdr->man_dtable.row_block_size[start_row] *
                ((end_col - start_col) + 1);
    } /* end else */

    /* Set the span size for the indirect section */
#ifdef QAK
HDfprintf(stderr, "%s: acc_span_size = %Hu\n", "H5HF_sect_indirect_span_size", acc_span_size);
#endif /* QAK */
    sect->u.indirect.span_size = acc_span_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_sect_indirect_span_size() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_iblock_entries
 *
 * Purpose:	Compute the number of entries for the iblock that an
 *              indirect section is located in
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  3 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_iblock_entries(H5HF_hdr_t *hdr, H5HF_free_section_t *sect)
{
    unsigned nrows;                     /* Number of rows in target indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_iblock_entries)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);

    /* If the section is 'live', just use it's indirect block */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        HDassert(sect->u.indirect.u.iblock);
        nrows = sect->u.indirect.u.iblock->nrows;
    } /* end if */
    else {
        hsize_t curr_off;               /* Current search offset */
        hsize_t iblock_off;             /* Temporary copy of indirect block's offset */
        unsigned curr_row, curr_col;    /* Current search row & column */

        /* Set starting offset for search */
        iblock_off = sect->u.indirect.u.iblock_off;

        curr_off = 0;
        do {
            /* Compute block's new offset */
            iblock_off -= curr_off;

            /* Look up row & column in new indirect block for object */
            if(H5HF_dtable_lookup(&hdr->man_dtable, iblock_off, &curr_row, &curr_col) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of object")
            HDassert(curr_row >= hdr->man_dtable.max_direct_rows);

            /* Compute offset of block indicated */
            curr_off = hdr->man_dtable.row_block_off[curr_row] + 
                    hdr->man_dtable.row_block_size[curr_row] * curr_col;
        } while(curr_off != iblock_off);

        /* Compute # of rows in indirect block */
        nrows = (H5V_log2_gen(hdr->man_dtable.row_block_size[curr_row]) - hdr->man_dtable.first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u\n", FUNC, nrows);
#endif /* QAK */
    } /* end else */
    HDassert(nrows);

    /* Compute number of entries in indirect block */
    sect->u.indirect.iblock_entries = hdr->man_dtable.cparam.width * nrows;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_iblock_entries() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_iblock_same
 *
 * Purpose:	Check if two indirect sections are located in the same indirect block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5HF_sect_indirect_iblock_same(const H5HF_free_section_t *sect1,
    const H5HF_free_section_t *sect2)
{
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_iblock_same)

    /*
     * Check arguments.
     */
    HDassert(sect1);
    HDassert(sect2);

    ret_value = ((sect1->sect_info.state == H5FS_SECT_LIVE ?  sect1->u.indirect.u.iblock->block_off : sect1->u.indirect.u.iblock_off)
            == (sect2->sect_info.state == H5FS_SECT_LIVE ?  sect2->u.indirect.u.iblock->block_off : sect2->u.indirect.u.iblock_off));

#ifdef QAK
HDfprintf(stderr, "%s: Leaving, ret_value = %t\n", "H5HF_sect_indirect_iblock_same", ret_value);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_iblock_same() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_is_last_row
 *
 * Purpose:	Check if a row is the last row in an indirect section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 18 2006
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5HF_sect_indirect_is_last_row(const H5HF_free_section_t *row_sect)
{
    const H5HF_free_section_t *sect;    /* Underlying indirect section */
    hbool_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_is_last_row)

    /*
     * Check arguments.
     */
    HDassert(row_sect);

    /* Check for any child indirect sections or if this row is not the last row */
    sect = row_sect->u.row.under;
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.indir_nents = %u\n", "H5HF_sect_indirect_is_last_row", sect->u.indirect.indir_nents);
HDfprintf(stderr, "%s: sect->u.indirect.row = %u\n", "H5HF_sect_indirect_is_last_row", sect->u.indirect.row);
HDfprintf(stderr, "%s: sect->u.indirect.dir_nrows = %u\n", "H5HF_sect_indirect_is_last_row", sect->u.indirect.dir_nrows);
HDfprintf(stderr, "%s: row_sect->u.row.row = %u\n", "H5HF_sect_indirect_is_last_row", row_sect->u.row.row);
#endif /* QAK */
    if(sect->u.indirect.indir_nents > 0)
        ret_value = FALSE;
    else if(((sect->u.indirect.row + sect->u.indirect.dir_nrows) - 1) ==
            row_sect->u.row.row)
        ret_value = TRUE;
    else
        ret_value = FALSE;

#ifdef QAK
HDfprintf(stderr, "%s: ret_value = %t\n", "H5HF_sect_indirect_is_last_row", ret_value);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_is_last_row() */


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
 *              Monday, July  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_init_cls(H5FS_section_class_t *cls, void *_udata)
{
    H5HF_hdr_t *hdr = (H5HF_hdr_t *)_udata; /* Fractal heap header */
    H5HF_sect_indirect_private_t *cls_prvt;    /* Pointer to class private info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_init_cls)

    /* Check arguments. */
    HDassert(cls);
    HDassert(!cls->cls_private);

    /* Allocate & initialize the class-private (i.e. private shared) information
     * for this type of section
     */
    if(NULL == (cls_prvt = H5MM_malloc(sizeof(H5HF_sect_indirect_private_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    cls_prvt->hdr = hdr;
    cls->cls_private = cls_prvt;

    /* Set the size of all serialized objects of this class of sections */
    cls->serial_size = hdr->heap_off_size  /* Indirect block's offset in "heap space" */
        + 2                                     /* Row */
        + 2                                     /* Column */
        + 2;                                    /* # of entries */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_sect_indirect_init_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_term_cls
 *
 * Purpose:	Terminate the "indirect" class structure
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, July  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_term_cls(H5FS_section_class_t *cls)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_term_cls)

    /* Check arguments. */
    HDassert(cls);

    /* Free the class private information */
    cls->cls_private = H5MM_xfree(cls->cls_private);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_indirect_term_cls() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_new
 *
 * Purpose:	Create a new 'indirect' section for other routines to finish
 *              initializing.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_free_section_t *
H5HF_sect_indirect_new(haddr_t sect_off, hsize_t sect_size,
    H5HF_indirect_t *iblock, hsize_t iblock_off, unsigned row, unsigned col,
    unsigned nentries)
{
    H5HF_free_section_t *sect = NULL;   /* 'Indirect' free space section to add */
    hbool_t iblock_incr = FALSE;        /* Indicate that parent iblock has been incremented */
    H5HF_free_section_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_new)

    /*
     * Check arguments.
     */
    HDassert(nentries);

    /* Create free space section node */
    if(NULL == (sect = H5HF_sect_node_new(H5HF_FSPACE_SECT_INDIRECT, sect_off,
            sect_size, (iblock ? H5FS_SECT_LIVE : H5FS_SECT_SERIALIZED))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for indirect section")

    /* Set the 'indirect' specific fields */
    if(iblock) {
        sect->u.indirect.u.iblock = iblock;
        if(H5HF_iblock_incr(sect->u.indirect.u.iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")
        iblock_incr = TRUE;
    } /* end if */
    else
        sect->u.indirect.u.iblock_off = iblock_off;
    sect->u.indirect.row = row;
    sect->u.indirect.col = col;
    sect->u.indirect.num_entries = nentries;

    /* Reset the non-stored fields, they are computed later, on demand */
    sect->u.indirect.span_size = 0;
    sect->u.indirect.iblock_entries = 0;

    /* This indirect section doesn't (currently) have a parent */
    sect->u.indirect.parent = NULL;
    sect->u.indirect.par_entry = 0;

    /* Set return value */
    ret_value = sect;

done:
    if(!ret_value && sect) {
        /* Check if we should decrement parent ref. count */
        if(iblock_incr)
            if(H5HF_iblock_decr(sect->u.indirect.u.iblock) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't decrement reference count on shared indirect block")

        /* Release the section */
        H5FL_FREE(H5HF_free_section_t, sect);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_new() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_row
 *
 * Purpose:	Create the underlying indirect section for a new row section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_free_section_t *
H5HF_sect_indirect_for_row(H5HF_hdr_t *hdr, H5HF_indirect_t *iblock, 
    H5HF_free_section_t *row_sect)
{
    H5HF_free_section_t *sect = NULL;   /* 'Indirect' free space section to add */
    H5HF_free_section_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_for_row)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(row_sect);
    HDassert(row_sect->u.row.row < hdr->man_dtable.max_direct_rows);
#ifdef QAK
HDfprintf(stderr, "%s: Entering\n", FUNC);
#endif /* QAK */

    /* Create free space section node */
    if(NULL == (sect = H5HF_sect_indirect_new(row_sect->sect_info.addr, 
            row_sect->sect_info.size, iblock, iblock->block_off,
            row_sect->u.row.row, row_sect->u.row.col, row_sect->u.row.num_entries)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't create indirect section")

    /* Set # of direct rows covered */
    sect->u.indirect.dir_nrows = 1;

    /* Allocate space for the derived row sections */
    if(NULL == (sect->u.indirect.dir_rows = H5MM_malloc(sizeof(H5HF_free_section_t *))))
        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "allocation failed for row section pointer array")

    /* Atatch the new row section to indirect section */
    sect->u.indirect.dir_rows[0] = row_sect;
    sect->u.indirect.rc = 1;

    /* No indirect rows in current section */
    sect->u.indirect.indir_nents = 0;
    sect->u.indirect.indir_ents = NULL;

    /* Set return value */
    ret_value = sect;

done:
    if(!ret_value && sect)
        if(H5HF_sect_indirect_free(sect) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTRELEASE, NULL, "can't free indirect section node")

#ifdef QAK
HDfprintf(stderr, "%s: Leaving\n", FUNC);
#endif /* QAK */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_for_row() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_init_rows
 *
 * Purpose:	Initialize the derived row sections for a newly created
 *              indirect section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_init_rows(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect, hbool_t first_child, unsigned space_flags,
    unsigned start_row, unsigned start_col,
    unsigned end_row, unsigned end_col)
{
    hsize_t curr_off;                   /* Offset of new section in "heap space" */
    size_t dblock_overhead;             /* Direct block's overhead */
    unsigned row_entries;               /* # of entries in row */
    unsigned row_col;                   /* Column within current row */
    unsigned curr_entry;                /* Current entry within indirect section */
    unsigned curr_indir_entry;          /* Current indirect entry within indirect section */
    unsigned curr_row;                  /* Current row within indirect section */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_init_rows)
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: first_child = %t\n", FUNC, first_child);
HDfprintf(stderr, "%s: start_row = %u, start_col = %u\n", FUNC, start_row, start_col);
HDfprintf(stderr, "%s: end_row = %u, end_col = %u\n", FUNC, end_row, end_col);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(sect);

    /* Reset reference count for indirect section */
    sect->u.indirect.rc = 0;

    /* Set up direct block information, if necessary */
    if(start_row < hdr->man_dtable.max_direct_rows) {
        unsigned max_direct_row;            /* Max. direct row covered */

        /* Compute max. direct row covered by indirect section */
        max_direct_row = MIN(end_row, (hdr->man_dtable.max_direct_rows - 1));

        /* Compute # of direct rows covered */
        sect->u.indirect.dir_nrows = (max_direct_row - start_row) + 1;

        /* Allocate space for the derived row sections */
        if(NULL == (sect->u.indirect.dir_rows = H5MM_malloc(sizeof(H5HF_free_section_t *) * sect->u.indirect.dir_nrows)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "allocation failed for row section pointer array")
    } /* end if */
    else {
        /* No rows of direct blocks covered, reset direct row information */
        sect->u.indirect.dir_nrows = 0;
        sect->u.indirect.dir_rows = NULL;
    } /* end else */

    /* Set up indirect block information, if necessary */
    if(end_row >= hdr->man_dtable.max_direct_rows) {
        unsigned indirect_start_row;    /* Row to start indirect entries on */
        unsigned indirect_start_col;    /* Column to start indirect entries on */
        unsigned indirect_start_entry;  /* Index of starting indirect entry */
        unsigned indirect_end_entry;    /* Index of ending indirect entry */

        /* Compute starting indirect entry */
        if(start_row < hdr->man_dtable.max_direct_rows) {
            indirect_start_row = hdr->man_dtable.max_direct_rows;
            indirect_start_col = 0;
        } /* end if */
        else {
            indirect_start_row = start_row;
            indirect_start_col = start_col;
        } /* end else */
        indirect_start_entry = (indirect_start_row * hdr->man_dtable.cparam.width)
            + indirect_start_col;

        /* Compute ending indirect entry */
        indirect_end_entry = (end_row * hdr->man_dtable.cparam.width) +
            end_col;

        /* Compute # of indirect entries covered */
        sect->u.indirect.indir_nents = (indirect_end_entry - indirect_start_entry) + 1;

        /* Allocate space for the child indirect sections */
        if(NULL == (sect->u.indirect.indir_ents = H5MM_malloc(sizeof(H5HF_free_section_t *) * sect->u.indirect.indir_nents)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "allocation failed for indirect section pointer array")
    } /* end if */
    else {
        /* No indirect block entries covered, reset indirect row information */
        sect->u.indirect.indir_nents = 0;
        sect->u.indirect.indir_ents = NULL;
    } /* end else */

    /* Set up initial row information */
    if(start_row == end_row)
        row_entries = (end_col - start_col) + 1;
    else
        row_entries = hdr->man_dtable.cparam.width - start_col;
    row_col = start_col;

    /* Loop over creating the sections covered by this indirect section */
    curr_off = sect->sect_info.addr;
    curr_entry = (start_row * hdr->man_dtable.cparam.width) + start_col;
    curr_row = 0;
    curr_indir_entry = 0;
    dblock_overhead = H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
    for(u = start_row; u <= end_row; u++, curr_row++) {
        if(u < hdr->man_dtable.max_direct_rows) {
            H5HF_free_section_t *row_sect = NULL;   /* 'Row' free space section to add */

#ifdef QAK
HDfprintf(stderr, "%s: Creating direct row, row_col = %u, row_entries = %u\n", FUNC, row_col, row_entries);
#endif /* QAK */
            /* Create 'row' free space section node */
            if(NULL == (row_sect = H5HF_sect_row_create(curr_off,
                    (hdr->man_dtable.row_block_size[u] - dblock_overhead), first_child, u, row_col,
                    row_entries, sect)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "creation failed for child row section")

            /* Add new row section to array for indirect section */
            sect->u.indirect.dir_rows[curr_row] = row_sect;

            /* Add new row section to free space manager for the heap */
            if(H5HF_space_add(hdr, dxpl_id, row_sect, space_flags) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add row section to free space")

            /* Add new row section to array for indirect section */
            sect->u.indirect.rc++;

            /* Advance the offset to the next section */
            curr_off += row_entries * hdr->man_dtable.row_block_size[u];

            /* Advance the current entry to the next row*/
            curr_entry += row_entries;

            /* Reset the 'first child' flag */
            first_child = FALSE;
        } /* end if */
        else {
            H5HF_indirect_t *child_iblock;      /* Child indirect block */
            H5HF_free_section_t *child_sect;    /* Child 'indirect' section to add */
            unsigned child_nrows;               /* Number of child rows in indirect blocks for this row */
            unsigned child_nentries;            /* Number of child entries in indirect blocks for this row */
            unsigned v;         /* Local index variable */

            /* Compute info about row's indirect blocks for child section */
            child_nrows = H5HF_dtable_size_to_rows(&hdr->man_dtable, hdr->man_dtable.row_block_size[u]);
            child_nentries = child_nrows * hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: child_nrows = %u\n", FUNC, child_nrows);
HDfprintf(stderr, "%s: child_nentries = %u\n", FUNC, child_nentries);
HDfprintf(stderr, "%s: row_entries = %u\n", FUNC, row_entries);
#endif /* QAK */

            /* Add an indirect section for each indirect block in the row */
            for(v = 0; v < row_entries; v++) {
                /* Try to get the child section's indirect block, if it's available */
                if(sect->sect_info.state == H5FS_SECT_LIVE) {
                    haddr_t child_iblock_addr;          /* Child indirect block's address on disk */

                    /* Get the address of the child indirect block */
                    if(H5HF_man_iblock_entry_addr(sect->u.indirect.u.iblock, curr_entry, &child_iblock_addr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve child indirect block's address")
#ifdef QAK
HDfprintf(stderr, "%s: child_iblock_addr = %a\n", FUNC, child_iblock_addr);
#endif /* QAK */

                    /* If the child indirect block's address is defined, protect it */
                    if(H5F_addr_defined(child_iblock_addr)) {
                        if(NULL == (child_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, child_iblock_addr, child_nrows, NULL, 0, H5AC_WRITE)))
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
                    } /* end if */
                    else
                        child_iblock = NULL;
                } /* end if */
                else
                    child_iblock = NULL;

                /* Create free space section node */
                if(NULL == (child_sect = H5HF_sect_indirect_new(curr_off, (hsize_t)0,
                        child_iblock, curr_off, 0, 0, child_nentries)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create indirect section")

                /* Initialize rows for new indirect section */
                if(H5HF_sect_indirect_init_rows(hdr, dxpl_id, child_sect,
                        first_child, space_flags, 0, 0, (child_nrows - 1),
                        (hdr->man_dtable.cparam.width - 1)) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize indirect section")

                /* If we have a valid child indirect block, release it now */
                if(child_iblock)
                    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock->addr, child_iblock, H5AC__NO_FLAGS_SET) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

                /* Attach child section to this section */
                child_sect->u.indirect.parent = sect;
                child_sect->u.indirect.par_entry = curr_entry;
                sect->u.indirect.indir_ents[curr_indir_entry] = child_sect;
                sect->u.indirect.rc++;

                /* Advance the offset for the next section */
                curr_off += hdr->man_dtable.row_block_size[u];

                /* Advance to the next entry */
                curr_entry++;
                curr_indir_entry++;

                /* Reset the 'first child' flag */
                first_child = FALSE;
            } /* end for */
        } /* end else */

        /* Compute the # of entries for the next row */
        if(u < (end_row - 1))
            row_entries = hdr->man_dtable.cparam.width;
        else
            row_entries = end_col + 1;

        /* Reset column for all other rows */
        row_col = 0;
    } /* end for */
    /* Make certain we've tracked the section's dependents correctly */
    HDassert(sect->u.indirect.rc == 
            (sect->u.indirect.indir_nents + sect->u.indirect.dir_nrows));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_init_rows() */


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
 *		July  3 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_sect_indirect_add(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_indirect_t *iblock, unsigned start_entry, unsigned nentries,
    hsize_t *end_off /*out*/)
{
    H5HF_free_section_t *sect = NULL;   /* 'Indirect3' free space section to add */
    hsize_t sect_off;                   /* Offset of section in heap space */
    unsigned start_row;                 /* Start row in indirect block */
    unsigned start_col;                 /* Start column in indirect block */
    unsigned end_entry;                 /* End entry in indirect block */
    unsigned end_row;                   /* End row in indirect block */
    unsigned end_col;                   /* End column in indirect block */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_add)
#ifdef QAK
HDfprintf(stderr, "%s: start_entry = %u, nentries = %u\n", FUNC, start_entry, nentries);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(nentries);

    /* Compute starting column & row */
    start_row = start_entry / hdr->man_dtable.cparam.width;
    start_col = start_entry % hdr->man_dtable.cparam.width;

    /* Compute end column & row */
    end_entry = (start_entry + nentries) - 1;
    end_row = end_entry / hdr->man_dtable.cparam.width;
    end_col = end_entry % hdr->man_dtable.cparam.width;

    /* Initialize information for rows skipped over */
    sect_off = iblock->block_off;
    for(u = 0; u < start_row; u++)
        sect_off += hdr->man_dtable.row_block_size[u] * hdr->man_dtable.cparam.width;
    sect_off += hdr->man_dtable.row_block_size[start_row] * start_col;
#ifdef QAK
HDfprintf(stderr, "%s: sect_off = %Hu\n", FUNC, sect_off);
#endif /* QAK */

    /* Create free space section node */
    if(NULL == (sect = H5HF_sect_indirect_new(sect_off, (hsize_t)0, iblock,
            iblock->block_off, start_row, start_col, nentries)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create indirect section")

    /* Initialize rows for new indirect section */
    if(H5HF_sect_indirect_init_rows(hdr, dxpl_id, sect, TRUE, 0, start_row,
            start_col, end_row, end_col) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize indirect section")

    /* Get end of section offset to return, if requested */
    if(end_off) {
        /* Set the span size of the new section */
        if(H5HF_sect_indirect_span_size(hdr, sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute span size of section")

        /* Compute the end of the section */
        *end_off = sect_off + sect->u.indirect.span_size;
#ifdef QAK
HDfprintf(stderr, "%s: *end_off = %Hu\n", FUNC, *end_off);
#endif /* QAK */
    } /* end if */

done:
    if(ret_value < 0 && sect)
        if(H5HF_sect_indirect_free(sect) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free indirect section node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_add() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_decr
 *
 * Purpose:	Decrement ref. count on indirect section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_decr(H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_decr)

    /*
     * Check arguments.
     */
    HDassert(sect);
    HDassert(sect->u.indirect.rc);
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.rc = %u\n", FUNC, sect->u.indirect.rc);
#endif /* QAK */

    /* Decrement ref. count for indirect section */
    sect->u.indirect.rc--;

    /* If the indirect section's ref. count drops to zero, free the section */
    if(sect->u.indirect.rc == 0) {
        /* Decrement ref. count on indirect section's parent */
        if(sect->u.indirect.parent)
            if(H5HF_sect_indirect_decr(sect->u.indirect.parent) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't decrement ref. count on parent indirect section")

        /* Free indirect section */
        if(H5HF_sect_indirect_free(sect) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free indirect section node")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_revive_row
 *
 * Purpose:	Update the memory information for a 'indirect' free section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  3 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_revive_row(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *sect)
{
    H5HF_indirect_t *sec_iblock;        /* Pointer to section indirect block */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_revive_row)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_SERIALIZED);

    /* Look up indirect block containing indirect blocks for section */
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info.addr = %a\n", FUNC, sect->sect_info.addr);
HDfprintf(stderr, "%s: sect->u.indirect.u.iblock_off = %Hu\n", FUNC, sect->u.indirect.u.iblock_off);
#endif /* QAK */
    if(H5HF_man_locate_block(hdr, dxpl_id, sect->sect_info.addr, FALSE, &sec_iblock, NULL, H5AC_READ) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of section")

    /* Increment reference count on indirect block that free section is in */
    if(H5HF_iblock_incr(sec_iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Set the pointer to the section's indirect block */
    sect->u.indirect.u.iblock = sec_iblock;

    /* Unlock indirect block */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, sec_iblock->addr, sec_iblock, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    sec_iblock = NULL;

    /* Section is "live" now */
    sect->sect_info.state = H5FS_SECT_LIVE;

    /* Loop over derived row sections and mark them all as 'live' now */
    for(u = 0; u < sect->u.indirect.dir_nrows; u++)
        sect->u.indirect.dir_rows[u]->sect_info.state = H5FS_SECT_LIVE;

    /* Revive parent indirect section, if there is one */
    if(sect->u.indirect.parent && sect->u.indirect.parent->sect_info.state == H5FS_SECT_SERIALIZED)
        if(H5HF_sect_indirect_revive(hdr, dxpl_id, sect->u.indirect.parent, sect->u.indirect.u.iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTREVIVE, FAIL, "can't revive indirect section")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_revive_row() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_revive
 *
 * Purpose:	Update the memory information for a 'indirect' free section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 10 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_revive(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *sect, H5HF_indirect_t *sect_iblock)
{
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_revive)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
    HDassert(sect->sect_info.state == H5FS_SECT_SERIALIZED);
    HDassert(sect_iblock);

#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info.addr = %a\n", FUNC, sect->sect_info.addr);
HDfprintf(stderr, "%s: sect->u.indirect.u.iblock_off = %Hu\n", FUNC, sect->u.indirect.u.iblock_off);
#endif /* QAK */
    /* Increment reference count on indirect block that free section is in */
    if(H5HF_iblock_incr(sect_iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Set the pointer to the section's indirect block */
    sect->u.indirect.u.iblock = sect_iblock;

    /* Section is "live" now */
    sect->sect_info.state = H5FS_SECT_LIVE;

    /* Loop over derived row sections and mark them all as 'live' now */
    for(u = 0; u < sect->u.indirect.dir_nrows; u++)
        sect->u.indirect.dir_rows[u]->sect_info.state = H5FS_SECT_LIVE;

    /* Revive parent indirect section, if there is one */
    if(sect->u.indirect.parent && sect->u.indirect.parent->sect_info.state == H5FS_SECT_SERIALIZED)
        if(H5HF_sect_indirect_revive(hdr, dxpl_id, sect->u.indirect.parent, sect->u.indirect.u.iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTREVIVE, FAIL, "can't revive indirect section")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_revive() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_reduce_row
 *
 * Purpose:	Remove a block from an indirect section (possibly freeing it)
 *              and re-add it back to the free space manager for the heap
 *              (if it hasn't been freed)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 10 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_reduce_row(H5HF_hdr_t *hdr, H5HF_free_section_t *row_sect,
    hbool_t *alloc_from_start)
{
    H5HF_free_section_t *sect;          /* Indirect3 section underlying row section */
    unsigned row_start_entry;           /* Entry for first block covered in row section */
    unsigned row_end_entry;             /* Entry for last block covered in row section */
    unsigned row_entry;                 /* Entry to allocate in row section */
    unsigned start_entry;               /* Entry for first block covered */
    unsigned start_row;                 /* Start row in indirect block */
    unsigned start_col;                 /* Start column in indirect block */
    unsigned end_entry;                 /* Entry for last block covered */
    unsigned end_row;                   /* End row in indirect block */
    unsigned end_col;                   /* End column in indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_reduce_row)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(row_sect);

    /* Compute starting & ending information for row section */
    row_start_entry = (row_sect->u.row.row * hdr->man_dtable.cparam.width) + row_sect->u.row.col;
    row_end_entry = (row_start_entry + row_sect->u.row.num_entries) - 1;
#ifdef QAK
HDfprintf(stderr, "%s: row_sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, row_sect->sect_info.addr, row_sect->sect_info.size, row_sect->sect_info.type, (row_sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: row_sect->u.row.row = %u, row_sect->u.row.col = %u, row_sect->u.row.num_entries = %u\n", FUNC, row_sect->u.row.row, row_sect->u.row.col, row_sect->u.row.num_entries);
HDfprintf(stderr, "%s: row_start_entry = %u, row_end_entry = %u\n", FUNC, row_start_entry, row_end_entry);
#endif /* QAK */

    /* Compute starting & ending information for indirect section */
    sect = row_sect->u.row.under;
    start_row = sect->u.indirect.row;
    start_col = sect->u.indirect.col;
    start_entry = (start_row * hdr->man_dtable.cparam.width) + start_col;
    end_entry = (start_entry + sect->u.indirect.num_entries) - 1;
    end_row = end_entry / hdr->man_dtable.cparam.width;
    end_col = end_entry % hdr->man_dtable.cparam.width;

    /* Additional sanity check */
    HDassert(sect->u.indirect.dir_rows);
    HDassert(sect->u.indirect.dir_rows[(row_sect->u.row.row - start_row)] == row_sect);
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect->u.indirect.parent = %p, sect->u.indirect.par_entry = %u\n", FUNC, sect->u.indirect.parent, sect->u.indirect.par_entry);
HDfprintf(stderr, "%s: start_entry = %u, start_row = %u, start_col = %u\n", FUNC, start_entry, start_row, start_col);
HDfprintf(stderr, "%s: end_entry = %u, end_row = %u, end_col = %u\n", FUNC, end_entry, end_row, end_col);
#endif /* QAK */

    /* Check if we should allocate from end of indirect section */
    if(row_end_entry == end_entry && start_row != end_row) {
        *alloc_from_start = FALSE;
        row_entry = row_end_entry;
#ifdef QAK
HDfprintf(stderr, "%s: Row is at end of indirect section\n", FUNC);
#endif /* QAK */
    } /* end if */
    else {
        *alloc_from_start = TRUE;
        row_entry = row_start_entry;
#ifdef QAK
HDfprintf(stderr, "%s: Row is NOT at end of indirect section\n", FUNC);
#endif /* QAK */
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: row_entry = %u\n", FUNC, row_entry);
#endif /* QAK */

    /* Check if we have a parent section to be detached from */
    if(sect->u.indirect.parent) {
        hbool_t is_first;       /* Flag to indicate that this section is the first section in hierarchy */

        /* Check if this section is the first section */
        is_first = H5HF_sect_indirect_is_first(sect);

        /* Remove this indirect section from parent indirect section */
        if(H5HF_sect_indirect_reduce(hdr, sect->u.indirect.parent, sect->u.indirect.par_entry) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce parent indirect section")
        sect->u.indirect.parent = NULL;
        sect->u.indirect.par_entry = 0;

        /* If we weren't the first section, set "first row" for this indirect section */
        if(!is_first)
            if(H5HF_sect_indirect_first(hdr, sect) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't make new 'first row' for indirect section")
    } /* end if */

    /* Adjust indirect section's span size, if it's set */
    if(sect->u.indirect.span_size > 0)
        sect->u.indirect.span_size -= row_sect->sect_info.size;

    /* Check how to adjust section for allocated entry */
    if(sect->u.indirect.num_entries > 1) {
        if(row_entry == start_entry) {
#ifdef QAK
HDfprintf(stderr, "%s: Entry is at start of indirect section\n", FUNC);
#endif /* QAK */
            /* Adjust section start */
            sect->sect_info.addr += hdr->man_dtable.row_block_size[sect->u.indirect.row];

            /* Adjust block coordinates of span */
            sect->u.indirect.col++;
            if(sect->u.indirect.col == hdr->man_dtable.cparam.width) {
                HDassert(row_sect->u.row.num_entries == 1);

                /* Adjust direct row information */
                sect->u.indirect.dir_nrows--;
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.dir_nrows = %u\n", FUNC, sect->u.indirect.dir_nrows);
#endif /* QAK */

                /* Adjust direct row sections for indirect section */
                if(sect->u.indirect.dir_nrows > 0) {
                    HDmemmove(&sect->u.indirect.dir_rows[0],
                            &sect->u.indirect.dir_rows[1],
                            sect->u.indirect.dir_nrows * sizeof(H5HF_free_section_t *));
                    HDassert(sect->u.indirect.dir_rows[0]);

                    /* Make new "first row" in indirect section */
                    if(row_sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW)
                        if(H5HF_sect_row_first(hdr, sect->u.indirect.dir_rows[0]) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't make new 'first row' for indirect section")
                } /* end if */
                else {
                    /* Sanity check */
                    HDassert(sect->u.indirect.indir_ents);

                    /* Eliminate direct rows for this section */
                    sect->u.indirect.dir_rows = H5MM_xfree(sect->u.indirect.dir_rows);

                    /* Make new "first row" in indirect section */
                    if(row_sect->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW)
                        if(H5HF_sect_indirect_first(hdr, sect->u.indirect.indir_ents[0]) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't make new 'first row' for child indirect section")
                } /* end else */

                sect->u.indirect.row++;
                sect->u.indirect.col = 0;
            } /* end if */

            /* Adjust number of entries covered */
            sect->u.indirect.num_entries--;
        } /* end if */
        else if(row_entry == end_entry) {
            unsigned new_end_row;       /* New end row for entries */

#ifdef QAK
HDfprintf(stderr, "%s: Entry is at end of indirect section\n", FUNC);
#endif /* QAK */
            /* Sanity check */
            HDassert(sect->u.indirect.indir_ents == NULL);

            /* Adjust number of entries covered */
            sect->u.indirect.num_entries--;

            /* Check for eliminating a direct row */
            new_end_row = ((start_entry + sect->u.indirect.num_entries) - 1) / hdr->man_dtable.cparam.width;
            HDassert(new_end_row <= end_row);
            if(new_end_row < end_row) {
                HDassert(new_end_row == (end_row - 1));
                sect->u.indirect.dir_nrows--;
            } /* end if */
        } /* end if */
        else {
            H5HF_free_section_t *peer_sect;     /* Peer indirect section */
            H5HF_indirect_t *iblock;    /* Pointer to indirect block for this section */
            hsize_t iblock_off;         /* Section's indirect block's offset in "heap space" */
            unsigned peer_nentries;     /* Number of entries in new peer indirect section */
            unsigned peer_dir_nrows;    /* Number of direct rows in new peer indirect section */
            unsigned new_start_row;     /* New starting row for current indirect section */
            unsigned u;                 /* Local index variable */

#ifdef QAK
HDfprintf(stderr, "%s: Entry is in middle of indirect section\n", FUNC);
#endif /* QAK */
            /* Sanity checks */
            HDassert(row_sect->u.row.col == 0);
            HDassert(row_sect->u.row.row > 0);
            HDassert(row_sect->u.row.row < hdr->man_dtable.max_direct_rows);
            HDassert(row_sect->u.row.num_entries == hdr->man_dtable.cparam.width);
            HDassert(row_sect->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW);

            /* Compute basic information about peer & current indirect sections */
            new_start_row = row_sect->u.row.row;
            peer_nentries = row_entry - start_entry;
            peer_dir_nrows = new_start_row - start_row;
#ifdef QAK
HDfprintf(stderr, "%s: peer_nentries = %u, peer_dir_nrows = %u\n", FUNC, peer_nentries, peer_dir_nrows);
HDfprintf(stderr, "%s: new_start_row = %u\n", FUNC, new_start_row);
#endif /* QAK */

            /* Get indirect block information for peer */
            if(sect->sect_info.state == H5FS_SECT_LIVE) {
                iblock = sect->u.indirect.u.iblock;
                iblock_off = sect->u.indirect.u.iblock->block_off;
            } /* end if */
            else {
                iblock = NULL;
                iblock_off = sect->u.indirect.u.iblock_off;
            } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: iblock = %p, iblock_off = %Hu\n", FUNC, iblock, iblock_off);
#endif /* QAK */

            /* Create peer indirect section */
            if(NULL == (peer_sect = H5HF_sect_indirect_new(sect->sect_info.addr,
                    sect->sect_info.size, iblock, iblock_off, start_row, start_col,
                    peer_nentries)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create indirect section")

            /* Set up direct row & indirect entry information for peer section */
            peer_sect->u.indirect.dir_nrows = peer_dir_nrows;
            if(NULL == (peer_sect->u.indirect.dir_rows = H5MM_malloc(sizeof(H5HF_free_section_t *) * peer_dir_nrows)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "allocation failed for row section pointer array")
            peer_sect->u.indirect.indir_nents = 0;
            peer_sect->u.indirect.indir_ents = NULL;

            /* Transfer row sections between current & peer sections */
            HDmemcpy(&peer_sect->u.indirect.dir_rows[0],
                &sect->u.indirect.dir_rows[0], 
                (sizeof(H5HF_free_section_t *) * peer_dir_nrows));
            HDmemmove(&sect->u.indirect.dir_rows[0], 
                &sect->u.indirect.dir_rows[peer_dir_nrows], 
                (sizeof(H5HF_free_section_t *) * (sect->u.indirect.dir_nrows - peer_dir_nrows)));
            sect->u.indirect.dir_nrows -= peer_dir_nrows;
            HDassert(row_sect == sect->u.indirect.dir_rows[0]);

            /* Re-target transferred row sections to point to new underlying indirect section */
            for(u = 0; u < peer_dir_nrows; u++)
                peer_sect->u.indirect.dir_rows[u]->u.row.under = peer_sect;

            /* Change first row section in indirect section to be the "first row" */
            /* (But we don't have to tell the free space manager about it,
             *  because the row section is "checked out" from the free space
             * manager currently.
             */
            row_sect->sect_info.type = H5HF_FSPACE_SECT_FIRST_ROW;

            /* Adjust reference counts for current & peer sections */
            peer_sect->u.indirect.rc = peer_dir_nrows;
            sect->u.indirect.rc -= peer_dir_nrows;

            /* Transfer/update cached information about indirect block */
            peer_sect->u.indirect.iblock_entries = sect->u.indirect.iblock_entries;
            peer_sect->u.indirect.span_size = row_sect->sect_info.addr - peer_sect->sect_info.addr;
            if(sect->u.indirect.span_size > 0)
                /* (span for row section has already been removed) */
                sect->u.indirect.span_size -= peer_sect->u.indirect.span_size;

            /* Update information for current section */
            sect->sect_info.addr = row_sect->sect_info.addr + hdr->man_dtable.row_block_size[new_start_row];
            sect->u.indirect.row = new_start_row;
            sect->u.indirect.col = row_sect->u.row.col + 1;
            sect->u.indirect.num_entries -= (peer_nentries + 1); /* Transferred entries, plus the entry allocated out of the row */

            /* Make certain we've tracked the sections' dependents correctly */
            HDassert(sect->u.indirect.rc == 
                    (sect->u.indirect.indir_nents + sect->u.indirect.dir_nrows));
            HDassert(peer_sect->u.indirect.rc == 
                    (peer_sect->u.indirect.indir_nents + peer_sect->u.indirect.dir_nrows));
        } /* end else */
    } /* end if */
    else {
        sect->u.indirect.num_entries--;
        sect->u.indirect.dir_nrows--;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_reduce_row() */


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
 *		July 10 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_reduce(H5HF_hdr_t *hdr, H5HF_free_section_t *sect,
    unsigned child_entry)
{
    unsigned start_entry;               /* Entry for first block covered */
    unsigned start_row;                 /* Start row in indirect block */
    unsigned start_col;                 /* Start column in indirect block */
    unsigned end_entry;                 /* Entry for last block covered */
    unsigned end_row;                   /* End row in indirect block */
    unsigned end_col;                   /* End column in indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_reduce)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sect);
#ifdef QAK
HDfprintf(stderr, "%s: child_entry = %u\n", FUNC, child_entry);
#endif /* QAK */

    /* Compute starting & ending information for indirect section */
    start_row = sect->u.indirect.row;
    start_col = sect->u.indirect.col;
    start_entry = (start_row * hdr->man_dtable.cparam.width) + start_col;
    end_entry = (start_entry + sect->u.indirect.num_entries) - 1;
    end_row = end_entry / hdr->man_dtable.cparam.width;
    end_col = end_entry % hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect->u.indirect.parent = %p, sect->u.indirect.par_entry = %u\n", FUNC, sect->u.indirect.parent, sect->u.indirect.par_entry);
HDfprintf(stderr, "%s: start_entry = %u, start_row = %u, start_col = %u\n", FUNC, start_entry, start_row, start_col);
HDfprintf(stderr, "%s: end_entry = %u, end_row = %u, end_col = %u\n", FUNC, end_entry, end_row, end_col);
#endif /* QAK */

    /* Check how to adjust section for allocated entry */
    if(sect->u.indirect.num_entries > 1) {
        /* Check if we have a parent section to be detached from */
        if(sect->u.indirect.parent) {
            hbool_t is_first;       /* Flag to indicate that this section is the first section in hierarchy */

            /* Check if this section is the first section */
            is_first = H5HF_sect_indirect_is_first(sect);

            /* Reduce parent indirect section */
            if(H5HF_sect_indirect_reduce(hdr, sect->u.indirect.parent, sect->u.indirect.par_entry) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce parent indirect section")
            sect->u.indirect.parent = NULL;
            sect->u.indirect.par_entry = 0;

            /* If we weren't the first section, set "first row" for this indirect section */
            if(!is_first)
                if(H5HF_sect_indirect_first(hdr, sect) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't make new 'first row' for indirect section")
        } /* end if */

        /* Check if we can allocate from start of indirect section */
        if(child_entry == start_entry) {
#ifdef QAK
HDfprintf(stderr, "%s: Child is at start of indirect section\n", FUNC);
#endif /* QAK */
            /* Sanity check */
            HDassert(sect->u.indirect.dir_rows == NULL);
            HDassert(sect->u.indirect.indir_ents);

            /* Adjust section start */
            sect->sect_info.addr += hdr->man_dtable.row_block_size[start_row];

            /* Adjust span of blocks covered */
            sect->u.indirect.col++;
            if(sect->u.indirect.col == hdr->man_dtable.cparam.width) {
                sect->u.indirect.row++;
                sect->u.indirect.col = 0;
            } /* end if */
            sect->u.indirect.num_entries--;
            if(sect->u.indirect.span_size > 0)
                sect->u.indirect.span_size -= hdr->man_dtable.row_block_size[start_row];

            /* Adjust indirect entry information */
            sect->u.indirect.indir_nents--;
            HDmemmove(&sect->u.indirect.indir_ents[0],
                    &sect->u.indirect.indir_ents[1],
                    sect->u.indirect.indir_nents * sizeof(H5HF_free_section_t *));
            HDassert(sect->u.indirect.indir_ents[0]);

            /* Make new "first row" in new first indirect child section */
            if(H5HF_sect_indirect_first(hdr, sect->u.indirect.indir_ents[0]) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't make new 'first row' for child indirect section")
        } /* end if */
        else if(child_entry == end_entry) {
#ifdef QAK
HDfprintf(stderr, "%s: Child is at end of indirect section\n", FUNC);
#endif /* QAK */
            /* Sanity check */
            HDassert(sect->u.indirect.indir_ents);

            /* Adjust span of blocks covered */
            sect->u.indirect.num_entries--;
            if(sect->u.indirect.span_size > 0)
                sect->u.indirect.span_size -= hdr->man_dtable.row_block_size[end_row];

            /* Adjust indirect entry information */
            sect->u.indirect.indir_nents--;
            if(sect->u.indirect.indir_nents == 0)
                sect->u.indirect.indir_ents = H5MM_xfree(sect->u.indirect.indir_ents);
        } /* end if */
        else {
            H5HF_free_section_t *peer_sect;     /* Peer indirect section */
            H5HF_indirect_t *iblock;    /* Pointer to indirect block for this section */
            hsize_t iblock_off;         /* Section's indirect block's offset in "heap space" */
            haddr_t peer_sect_addr;     /* Address of new peer section in "heap space" */
            unsigned peer_nentries;     /* Number of entries in new peer indirect section */
            unsigned peer_start_row;    /* Starting row for new peer indirect section */
            unsigned peer_start_col;    /* Starting column for new peer indirect section */
            unsigned child_row;         /* Row where child entry is located */
            unsigned new_nentries;      /* New number of entries for current indirect section */
            unsigned old_nentries;      /* Old number of entries for current indirect section */
            unsigned u;                 /* Local index variable */

#ifdef QAK
HDfprintf(stderr, "%s: Child is in middle of indirect section\n", FUNC);
#endif /* QAK */
            /* Sanity check */
            HDassert(sect->u.indirect.indir_ents);

            /* Compute basic information about peer & current indirect sections */
            peer_nentries = end_entry - child_entry;
            peer_start_row = (child_entry + 1) / hdr->man_dtable.cparam.width;
            peer_start_col = (child_entry + 1) % hdr->man_dtable.cparam.width;
            child_row = child_entry / hdr->man_dtable.cparam.width;
            new_nentries = sect->u.indirect.num_entries - (peer_nentries + 1);
            old_nentries = sect->u.indirect.num_entries;
#ifdef QAK
HDfprintf(stderr, "%s: peer_nentries = %u, peer_start_row = %u, peer_start_col = %u\n", FUNC, peer_nentries, peer_start_row, peer_start_col);
HDfprintf(stderr, "%s: new_nentries = %u\n", FUNC, new_nentries);
#endif /* QAK */

            /* Get indirect block information for peer */
            if(sect->sect_info.state == H5FS_SECT_LIVE) {
                iblock = sect->u.indirect.u.iblock;
                iblock_off = sect->u.indirect.u.iblock->block_off;
            } /* end if */
            else {
                iblock = NULL;
                iblock_off = sect->u.indirect.u.iblock_off;
            } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: iblock = %p, iblock_off = %Hu\n", FUNC, iblock, iblock_off);
#endif /* QAK */

            /* Update the number of entries in current section & calculate it's span size */
            /* (Will use this to compute the section address for th peer section */
            sect->u.indirect.num_entries = new_nentries;
            if(H5HF_sect_indirect_span_size(hdr, sect) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute span size of section")
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.span_size = %Hu\n", FUNC, sect->u.indirect.span_size);
#endif /* QAK */

            /* Compute address of peer indirect section */
            peer_sect_addr = sect->sect_info.addr;
            peer_sect_addr += sect->u.indirect.span_size;
            peer_sect_addr += hdr->man_dtable.row_block_size[child_row];
#ifdef QAK
HDfprintf(stderr, "%s: peer_sect_addr = %a\n", FUNC, peer_sect_addr);
#endif /* QAK */

            /* Create peer indirect section */
            if(NULL == (peer_sect = H5HF_sect_indirect_new(peer_sect_addr,
                    sect->sect_info.size, iblock, iblock_off, peer_start_row,
                    peer_start_col, peer_nentries)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create indirect section")

            /* Set up direct row & indirect entry information for peer section */
            peer_sect->u.indirect.dir_nrows = 0;
            peer_sect->u.indirect.dir_rows = NULL;
            peer_sect->u.indirect.indir_nents = peer_nentries;
            if(NULL == (peer_sect->u.indirect.indir_ents = H5MM_malloc(sizeof(H5HF_free_section_t *) * peer_nentries)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "allocation failed for indirect section pointer array")

            /* Transfer child indirect sections between current & peer sections */
            HDmemcpy(&peer_sect->u.indirect.indir_ents[0],
                &sect->u.indirect.indir_ents[old_nentries - peer_nentries], 
                (sizeof(H5HF_free_section_t *) * peer_nentries));
            sect->u.indirect.indir_nents -= (peer_nentries + 1); /* Transferred blocks, plus child entry */
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.indir_nents = %u, old_nentries = %u\n", FUNC, sect->u.indirect.indir_nents, old_nentries);
#endif /* QAK */

            /* Re-target transferred row sections to point to new underlying indirect section */
            for(u = 0; u < peer_nentries; u++)
                peer_sect->u.indirect.indir_ents[u]->u.indirect.parent = peer_sect;

            /* Make new "first row" in peer section */
            if(H5HF_sect_indirect_first(hdr, peer_sect->u.indirect.indir_ents[0]) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't make new 'first row' for peer indirect section")

            /* Adjust reference counts for current & peer sections */
            peer_sect->u.indirect.rc = peer_nentries;
            sect->u.indirect.rc -= peer_nentries;
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.rc = %u\n", FUNC, sect->u.indirect.rc);
HDfprintf(stderr, "%s: peer_sect->u.indirect.rc = %u\n", FUNC, peer_sect->u.indirect.rc);
#endif /* QAK */

            /* Transfer cached information about indirect block */
            peer_sect->u.indirect.iblock_entries = sect->u.indirect.iblock_entries;

            /* Make certain we've tracked the sections' dependents correctly */
            /* (Note modified on current section's ref. count, since we haven't
             *  detached the child section yet)
             */
            HDassert((sect->u.indirect.rc - 1) == 
                    (sect->u.indirect.indir_nents + sect->u.indirect.dir_nrows));
            HDassert(peer_sect->u.indirect.rc == 
                    (peer_sect->u.indirect.indir_nents + peer_sect->u.indirect.dir_nrows));
        } /* end else */
    } /* end if */
    else {
        sect->u.indirect.num_entries--;
        sect->u.indirect.indir_nents--;
    } /* end else */

    /* Decrement # of sections which depend on this row */
    /* (Must be last as section can be freed) */
    if(H5HF_sect_indirect_decr(sect) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't decrement section's ref. count ")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_reduce() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_is_first
 *
 * Purpose:	Check if indirect section is first in all parents
 *
 * Return:	Non-negative (TRUE/FALSE) on success/<can't fail>
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 17 2006
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5HF_sect_indirect_is_first(H5HF_free_section_t *sect)
{
    hbool_t ret_value = FALSE;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_is_first)

    /* Sanity check */
    HDassert(sect);

    /* Recurse to parent */
    if(sect->u.indirect.parent) {
        if(sect->sect_info.addr == sect->u.indirect.parent->sect_info.addr)
            ret_value = H5HF_sect_indirect_is_first(sect->u.indirect.parent);
    } /* end if */
    else
        ret_value = TRUE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_first() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_first
 *
 * Purpose:	Make new 'first row' for indirect section
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July 10 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_first(H5HF_hdr_t *hdr, H5HF_free_section_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_first)

    /* Sanity check */
    HDassert(hdr);
    HDassert(sect);

    /* Check if this indirect section has direct block rows */
    if(sect->u.indirect.dir_nrows > 0) {
        /* Sanity checks */
        HDassert(sect->u.indirect.row == 0);
        HDassert(sect->u.indirect.col == 0);
        HDassert(sect->u.indirect.dir_rows[0]);

        /* Change first row section in indirect section to be the "first row" */
        if(H5HF_sect_row_first(hdr, sect->u.indirect.dir_rows[0]) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSET, FAIL, "can't set row section to be first row")
    } /* end if */
    else {
        /* Sanity checks */
        HDassert(sect->u.indirect.indir_nents > 0);
        HDassert(sect->u.indirect.indir_ents[0]);

        /* Forward to first child indirect section */
        if(H5HF_sect_indirect_first(hdr, sect->u.indirect.indir_ents[0]) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSET, FAIL, "can't set child indirect section to be first row")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_first() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_get_iblock
 *
 * Purpose:	Retrieve the indirect block for a indirect section
 *
 * Return:	Pointer to indirect block on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		July  9 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_indirect_t *
H5HF_sect_indirect_get_iblock(H5HF_free_section_t *sect)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_get_iblock)

    /*
     * Check arguments.
     */
    HDassert(sect);
    HDassert(sect->sect_info.type == H5HF_FSPACE_SECT_INDIRECT);
    HDassert(sect->sect_info.state == H5FS_SECT_LIVE);

    FUNC_LEAVE_NOAPI(sect->u.indirect.u.iblock)
} /* end H5HF_sect_indirect_get_iblock() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_merge_row
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
 *              Tuesday, July 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_merge_row(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t *row_sect1, H5HF_free_section_t *row_sect2)
{
    H5HF_free_section_t *sect1, *sect2; /* Indirect sections underlying row sections */
    hsize_t iblock_off1, iblock_off2;   /* The offset of the indirect blocks for the indirect sections (in "heap space") */
    unsigned start_entry1;              /* Start entry for section #1 */
    unsigned start_row1, start_col1;    /* Starting row & column for section #1 */
    unsigned end_entry1;                /* End entry for section #1 */
    unsigned end_row1, end_col1;        /* Ending row & column for section #1 */
    unsigned start_entry2;              /* Start entry for section #2 */
    unsigned start_row2, start_col2;    /* Starting row & column for section #2 */
    unsigned end_row2, end_col2;        /* Ending row & column for section #2 */
    unsigned end_entry2;                /* End entry for section #2 */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_merge_row)

    /* Sanity check parameters */
    HDassert(hdr);
    HDassert(row_sect1);
    HDassert(row_sect1->u.row.under);
    HDassert(row_sect1->sect_info.state == H5FS_SECT_LIVE);
    HDassert(row_sect2);
    HDassert(row_sect2->u.row.under);
    HDassert(row_sect2->sect_info.state == H5FS_SECT_LIVE);
    HDassert(row_sect2->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW);

    /* Set up indirect section information */
    sect1 = row_sect1->u.row.under;
    sect2 = row_sect2->u.row.under;
#ifdef QAK
HDfprintf(stderr, "%s: sect1->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect1->sect_info.addr, sect1->sect_info.size, sect1->sect_info.type, (sect1->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
HDfprintf(stderr, "%s: sect2->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect2->sect_info.addr, sect2->sect_info.size, sect2->sect_info.type, (sect2->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Sanity check some assumptions about the indirect sections */
    HDassert(sect1->u.indirect.dir_nrows > 0);
    HDassert(sect1->u.indirect.dir_rows);
    HDassert(sect1->u.indirect.indir_nents == 0);
    HDassert(sect1->u.indirect.indir_ents == NULL);
    HDassert(sect2->u.indirect.dir_nrows > 0);
    HDassert(sect2->u.indirect.dir_rows);

    /* Set up span information */
    start_row1 = sect1->u.indirect.row;
    start_col1 = sect1->u.indirect.col;
    start_entry1 = (start_row1 * hdr->man_dtable.cparam.width) + start_col1;
    end_entry1 = (start_entry1 + sect1->u.indirect.num_entries) - 1;
    end_row1 = end_entry1 / hdr->man_dtable.cparam.width;
    end_col1 = end_entry1 % hdr->man_dtable.cparam.width;
    HDassert(end_row1 == row_sect1->u.row.row);
#ifdef QAK
HDfprintf(stderr, "%s: start_row1 = %u, start_col1 = %u, start_entry1 = %u\n", FUNC, start_row1, start_col1, start_entry1);
HDfprintf(stderr, "%s: sect1->u.indirect.num_entries = %u\n", FUNC, sect1->u.indirect.num_entries);
HDfprintf(stderr, "%s: end_row1 = %u, end_col1 = %u, end_entry1 = %u\n", FUNC, end_row1, end_col1, end_entry1);
#endif /* QAK */
    start_row2 = sect2->u.indirect.row;
    start_col2 = sect2->u.indirect.col;
    start_entry2 = (start_row2 * hdr->man_dtable.cparam.width) + start_col2;
    end_entry2 = (start_entry2 + sect2->u.indirect.num_entries) - 1;
    end_row2 = end_entry2 / hdr->man_dtable.cparam.width;
    end_col2 = end_entry2 % hdr->man_dtable.cparam.width;
    HDassert(start_row2 == row_sect2->u.row.row);
#ifdef QAK
HDfprintf(stderr, "%s: start_row2 = %u, start_col2 = %u, start_entry2 = %u\n", FUNC, start_row2, start_col2, start_entry2);
HDfprintf(stderr, "%s: sect2->u.indirect.num_entries = %u\n", FUNC, sect2->u.indirect.num_entries);
HDfprintf(stderr, "%s: end_row2 = %u, end_col2 = %u, end_entry2 = %u\n", FUNC, end_row2, end_col2, end_entry2);
#endif /* QAK */
    if(sect1->sect_info.state == H5FS_SECT_LIVE)
        iblock_off1 = sect1->u.indirect.u.iblock->block_off;
    else
        iblock_off1 = sect1->u.indirect.u.iblock_off;
    if(sect2->sect_info.state == H5FS_SECT_LIVE)
        iblock_off2 = sect2->u.indirect.u.iblock->block_off;
    else
        iblock_off2 = sect2->u.indirect.u.iblock_off;
#ifdef QAK
HDfprintf(stderr, "%s: iblock_off1 = %Hu\n", FUNC, iblock_off1);
HDfprintf(stderr, "%s: iblock_off2 = %Hu\n", FUNC, iblock_off2);
#endif /* QAK */

    /* Determine if the two indirect sections are in the same indirect block */
    if(iblock_off1 == iblock_off2) {
        unsigned src_row2;      /* Source row for copying from second section */
        unsigned new_dir_nrows; /* New value for number of direct rows in first section */
        unsigned nrows_moved;   /* Number of rows to move from second section to first */
        unsigned u;                 /* Local index variable */

#ifdef QAK
HDfprintf(stderr, "%s: sect1->u.indirect.dir_nrows = %u\n", FUNC, sect1->u.indirect.dir_nrows);
HDfprintf(stderr, "%s: sect2->u.indirect.dir_nrows = %u\n", FUNC, sect2->u.indirect.dir_nrows);
#endif /* QAK */

        /* Check for sections sharing a row */
        if(end_row1 == start_row2) {
#ifdef QAK
HDfprintf(stderr, "%s: Sections share a row\n", FUNC);
#endif /* QAK */

            /* Adjust info for first row section, to absorb second row section */
            HDassert((row_sect1->u.row.col + row_sect1->u.row.num_entries) == row_sect2->u.row.col);
            row_sect1->u.row.num_entries += row_sect2->u.row.num_entries;

            /* Set up parameters for transfer of rows */
            src_row2 = 1;
            nrows_moved = sect2->u.indirect.dir_nrows - 1;
            new_dir_nrows = (sect1->u.indirect.dir_nrows + sect2->u.indirect.dir_nrows) - 1;
        } /* end if */
        else {
#ifdef QAK
HDfprintf(stderr, "%s: Sections don't share a row\n", FUNC);
#endif /* QAK */

            /* Set up parameters for transfer of rows */
            src_row2  = 0;
            nrows_moved = sect2->u.indirect.dir_nrows;
            new_dir_nrows = sect1->u.indirect.dir_nrows + sect2->u.indirect.dir_nrows;
        } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: src_row2 = %u\n", FUNC, src_row2);
HDfprintf(stderr, "%s: nrows_moved = %u\n", FUNC, nrows_moved);
HDfprintf(stderr, "%s: new_dir_nrows = %u\n", FUNC, new_dir_nrows);
#endif /* QAK */

        /* Check if we need to move additional rows */
        if(nrows_moved > 0) {
            /* Extend the first section's row array */
            if(NULL == (sect1->u.indirect.dir_rows = H5MM_realloc(sect1->u.indirect.dir_rows, sizeof(H5HF_free_section_t *) * new_dir_nrows)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "allocation failed for row section pointer array")

            /* Transfer the second section's rows to first section */
            HDmemcpy(&sect1->u.indirect.dir_rows[sect1->u.indirect.dir_nrows], 
                &sect2->u.indirect.dir_rows[src_row2], 
                (sizeof(H5HF_free_section_t *) * nrows_moved));

            /* Re-target the row sections moved from second section */
            for(u = sect1->u.indirect.dir_nrows; u < new_dir_nrows; u++)
                sect1->u.indirect.dir_rows[u]->u.row.under = sect1;

            /* Adjust reference counts to account for transferred rows */
            sect1->u.indirect.rc += nrows_moved;
            sect2->u.indirect.rc -= nrows_moved;
        } /* end if */

        /* Check for indirect sections in second section */
        if(sect2->u.indirect.indir_nents > 0) {
            HDassert(sect2->u.indirect.rc > 0);
            HDassert(sect2->u.indirect.indir_ents);

            /* Copy over any child indirect sections in second section */

            /* Re-target the child indirect sections moved from second section */

            /* Adjust reference counts for transferred child indirect sections */

HDfprintf(stderr, "%s: moving child indirect sections between indirect sections not supported yet!\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "moving child indirect sections between two indirect sections not supported yet")
        } /* end if */

        /* Update information for first section */
        sect1->u.indirect.dir_nrows = new_dir_nrows;
        sect1->u.indirect.num_entries += sect2->u.indirect.num_entries;
        if(sect1->u.indirect.span_size) {
            if(sect2->u.indirect.span_size)
                sect1->u.indirect.span_size += sect2->u.indirect.span_size;
            else
                sect1->u.indirect.span_size = 0;
        } /* end if */
        if(sect1->u.indirect.iblock_entries == 0)
            sect1->u.indirect.iblock_entries = sect2->u.indirect.iblock_entries;

        /* Make certain we've tracked the first section's dependents correctly */
        HDassert(sect1->u.indirect.rc == 
                (sect1->u.indirect.indir_nents + sect1->u.indirect.dir_nrows));

        /* Wrap up, for indirect sections not sharing a row */
        /* (want this to be after the first indirection section is consistent again) */
        if(end_row1 == start_row2) {
#ifdef QAK
HDfprintf(stderr, "%s: Finishing sections share a row\n", FUNC);
#endif /* QAK */
            /* Release second row section */
            /* (also releases second indirect section, since all of it's other
             *  dependents are gone)
             */
            HDassert(sect2->u.indirect.rc == 1);
            if(H5HF_sect_row_free((H5FS_section_info_t *)row_sect2) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free row section")
        } /* end if */
        else {
#ifdef QAK
HDfprintf(stderr, "%s: Finishing sections don't share a row\n", FUNC);
#endif /* QAK */
            /* Decrement ref. count on second indirect section's parent */
            HDassert(sect2->u.indirect.rc == 0);
            if(sect2->u.indirect.parent)
                if(H5HF_sect_indirect_decr(sect2->u.indirect.parent) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't decrement ref. count on parent indirect section")

            /* Free second indirect section */
            if(H5HF_sect_indirect_free(sect2) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free indirect section node")

            /* Re-add the second section's first row */
            /* (it's already been added to first section, but it's been removed
             *  from the free space manager and needs to be re-added)
            */
#ifdef QAK
HDfprintf(stderr, "%s: Re-inserting second row section\n", FUNC);
#endif /* QAK */
            row_sect2->sect_info.type = H5HF_FSPACE_SECT_NORMAL_ROW;
            HDassert(row_sect2->u.row.under == sect1);
            if(H5HF_space_add(hdr, dxpl_id, row_sect2, H5FS_ADD_RETURNED_SPACE) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't re-add second row section to free space")
#ifdef QAK
HDfprintf(stderr, "%s: Done re-inserting second row section\n", FUNC);
#endif /* QAK */
        } /* end else */
    } /* end if */
    else {
HDfprintf(stderr, "%s: merging two indirect sections in different indirect blocks not supported yet!\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "merging two indirect sections in different indirect blocks not supported yet")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_sect_indirect_merge_row() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_shrink_row
 *
 * Purpose:	"Shrink" an indirect by eliminating it's last row
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_shrink_row(H5HF_free_section_t *row_sect)
{
    H5HF_free_section_t *sect;          /* Indirect section underlying row section */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_shrink_row)

    /* Sanity check parameters */
    HDassert(row_sect);
    HDassert(row_sect->u.row.under);

    /* Set up indirect section information */
    sect = row_sect->u.row.under;

    /* Sanity check some assumptions about the indirect section */
    HDassert(sect->u.indirect.dir_nrows > 0);
    HDassert(sect->u.indirect.dir_rows);
    HDassert(sect->u.indirect.indir_nents == 0);
    HDassert(sect->u.indirect.indir_ents == NULL);

    /* Decrement the number of rows */
    /* (other cleanup will be taken care of when the section is freed) */
    sect->u.indirect.dir_nrows--;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_sect_indirect_shrink_row() */


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
 *              Monday, July  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_serialize(H5HF_hdr_t *hdr, const H5HF_free_section_t *sect,
    uint8_t *buf)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_serialize)

    /* Check arguments. */
    HDassert(hdr);
    HDassert(sect);
    HDassert(buf);
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", FUNC, sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* QAK */

    /* Check if this indirect section has a parent & forward if this section is first */
    if(sect->u.indirect.parent) {
        if(sect->sect_info.addr == sect->u.indirect.parent->sect_info.addr)
            if(H5HF_sect_indirect_serialize(hdr, sect->u.indirect.parent, buf) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTSERIALIZE, FAIL, "can't serialize indirect section's parent indirect section")
    } /* end if */
    else {
        /* Indirect range's indirect block's block offset */
        if(sect->sect_info.state == H5FS_SECT_LIVE) {
            HDassert(sect->u.indirect.u.iblock);
            UINT64ENCODE_VAR(buf, sect->u.indirect.u.iblock->block_off, hdr->heap_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.u.iblock->block_off = %Hu\n", FUNC, sect->u.indirect.u.iblock->block_off);
#endif /* QAK */
        } /* end if */
        else
{
            UINT64ENCODE_VAR(buf, sect->u.indirect.u.iblock_off, hdr->heap_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.u.iblock_off = %Hu\n", FUNC, sect->u.indirect.u.iblock_off);
#endif /* QAK */
}

        /* Indirect range's row */
        UINT16ENCODE(buf, sect->u.indirect.row);
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.row = %u\n", FUNC, sect->u.indirect.row);
#endif /* QAK */

        /* Indirect range's column */
        UINT16ENCODE(buf, sect->u.indirect.col);
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.col = %u\n", FUNC, sect->u.indirect.col);
#endif /* QAK */

        /* Indirect range's # of entries */
        UINT16ENCODE(buf, sect->u.indirect.num_entries);
#ifdef QAK
HDfprintf(stderr, "%s: sect->u.indirect.num_entries = %u\n", FUNC, sect->u.indirect.num_entries);
#endif /* QAK */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
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
 *              Monday, July  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_section_info_t *
H5HF_sect_indirect_deserialize(H5HF_hdr_t *hdr, hid_t dxpl_id,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    unsigned *des_flags)
{
    H5HF_free_section_t *new_sect;      /* New indirect section */
    hsize_t iblock_off;                 /* Indirect block's offset */ 
    unsigned start_row;                 /* Indirect3 section's start row */
    unsigned start_col;                 /* Indirect3 section's start column */
    unsigned nentries;                  /* Indirect3 section's number of entries */
    unsigned start_entry;               /* Start entry in indirect block */
    unsigned end_entry;                 /* End entry in indirect block */
    unsigned end_row;                   /* End row in indirect block */
    unsigned end_col;                   /* End column in indirect block */
    H5FS_section_info_t *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_deserialize)

    /* Check arguments. */
    HDassert(hdr);
    HDassert(buf);
    HDassert(H5F_addr_defined(sect_addr));
    HDassert(sect_size);
#ifdef QAK
HDfprintf(stderr, "%s: sect_addr = %a, sect_size = %Hu\n", FUNC, sect_addr, sect_size);
#endif /* QAK */

    /* Indirect range's indirect block's block offset */
    UINT64DECODE_VAR(buf, iblock_off, hdr->heap_off_size);
#ifdef QAK
HDfprintf(stderr, "%s: iblock_off = %Hu\n", FUNC, iblock_off);
#endif /* QAK */

    /* Indirect3 section's row */
    UINT16DECODE(buf, start_row);
#ifdef QAK
HDfprintf(stderr, "%s: start_row = %u\n", FUNC, start_row);
#endif /* QAK */

    /* Indirect3 section's column */
    UINT16DECODE(buf, start_col);
#ifdef QAK
HDfprintf(stderr, "%s: start_col = %u\n", FUNC, start_col);
#endif /* QAK */

    /* Indirect3 section's # of entries */
    UINT16DECODE(buf, nentries);
#ifdef QAK
HDfprintf(stderr, "%s: nentries = %u\n", FUNC, nentries);
#endif /* QAK */

    /* Create free space section node */
    if(NULL == (new_sect = H5HF_sect_indirect_new(sect_addr, sect_size, NULL,
            iblock_off, start_row, start_col, nentries)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't create indirect section")

    /* Compute start entry */
    start_entry = (start_row * hdr->man_dtable.cparam.width) + start_col;

    /* Compute end column & row */
    end_entry = (start_entry + nentries) - 1;
    end_row = end_entry / hdr->man_dtable.cparam.width;
    end_col = end_entry % hdr->man_dtable.cparam.width;

    /* Initialize rows for new indirect section */
    if(H5HF_sect_indirect_init_rows(hdr, dxpl_id, new_sect, TRUE, H5FS_ADD_DESERIALIZING,
            new_sect->u.indirect.row, new_sect->u.indirect.col, end_row, end_col) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't initialize indirect section")

    /* Indicate that this section shouldn't be added to free space manager's list */
    *des_flags |= H5FS_DESERIALIZE_NO_ADD;

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
 *              Monday, July  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_free(H5HF_free_section_t *sect)
{
    H5HF_indirect_t *iblock = NULL;     /* Indirect block for section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_sect_indirect_free)

    HDassert(sect);

    /* Release the memory for tracking direct rows */
    sect->u.indirect.dir_rows = H5MM_xfree(sect->u.indirect.dir_rows);

    /* Release the memory for tracking indirect entries */
    sect->u.indirect.indir_ents = H5MM_xfree(sect->u.indirect.indir_ents);

    /* Check for live reference to an indirect block */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        /* Get indirect block, if there was one */
        if(sect->u.indirect.u.iblock)
            iblock = sect->u.indirect.u.iblock;
    } /* end if */

    /* Release the sections */
    if(H5HF_sect_node_free(sect, iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't free section node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_sect_indirect_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_sect_indirect_valid
 *
 * Purpose:	Check the validity of a section
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, July 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_sect_indirect_valid(const H5FS_section_class_t *row_cls, const H5HF_free_section_t *row_sect)
{
    const H5HF_free_section_t *sect;    /* Pointer to underlying indirect section */
    H5HF_sect_indirect_private_t *cls_prvt;    /* Pointer to class private info */
    const H5HF_hdr_t *hdr;      /* Fractal heap header */
    hsize_t iblock_off;         /* Indirect block's offset in "heap space" */
    unsigned start_row;         /* Row for first block covered */
    unsigned start_col;         /* Column for first block covered */
    unsigned start_entry;       /* Entry for first block covered */
    unsigned end_row;           /* Row for last block covered */
    unsigned end_col;           /* Column for last block covered */
    unsigned end_entry;         /* Entry for last block covered */
    unsigned dir_nrows;         /* Number of direct rows in section */
    unsigned u;                 /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_valid)

    /* Sanity check arguments */
    HDassert(row_cls);
    HDassert(row_sect);

    /* Set up indirect section */
    sect = row_sect->u.row.under;
    cls_prvt = row_cls->cls_private;
    hdr = cls_prvt->hdr;
    HDassert(sect);
    HDassert(hdr);
#ifdef QAK
HDfprintf(stderr, "%s: sect->sect_info = {%a, %Hu, %u, %s}\n", "H5HF_sect_indirect_valid", sect->sect_info.addr, sect->sect_info.size, sect->sect_info.type, (sect->sect_info.state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
if(sect->sect_info.state == H5FS_SECT_LIVE)
    HDfprintf(stderr, "%s: sect->u.indirect = {%p, ", "H5HF_sect_indirect_valid", sect->u.indirect.u.iblock);
else
    HDfprintf(stderr, "%s: sect->u.indirect = {%Hu, ", "H5HF_sect_indirect_valid", sect->u.indirect.u.iblock_off);
HDfprintf(stderr, "%u, %u, %u}\n", sect->u.indirect.row, sect->u.indirect.col, sect->u.indirect.num_entries);
#endif /* QAK */

    /* Retrieve the indirect block's offset */
    if(sect->sect_info.state == H5FS_SECT_LIVE)
        iblock_off = sect->u.indirect.u.iblock->block_off;
    else
        iblock_off = sect->u.indirect.u.iblock_off;

#ifdef QAK
HDfprintf(stderr, "%s: iblock_off = %Hu\n", "H5HF_sect_indirect_valid", iblock_off);
#endif /* QAK */

    /* Compute starting entry, column & row */
    start_row = sect->u.indirect.row;
    start_col = sect->u.indirect.col;
    start_entry = (start_row * hdr->man_dtable.cparam.width) + start_col;

    /* Compute ending entry, column & row */
    end_entry = (start_entry + sect->u.indirect.num_entries) - 1;
    end_row = end_entry / hdr->man_dtable.cparam.width;
    end_col = end_entry % hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: start_row = %u, start_col = %u, start_entry = %u\n", "H5HF_sect_indirect_valid", start_row, start_col, start_entry);
HDfprintf(stderr, "%s: end_row = %u, end_col = %u, end_entry = %u\n", "H5HF_sect_indirect_valid", end_row, end_col, end_entry);
#endif /* QAK */

    /* Iterate over direct rows, checking pointer references */
    dir_nrows = (end_row - start_row) + 1;
    HDassert(dir_nrows == sect->u.indirect.dir_nrows);
    for(u = 0; u < dir_nrows; u++) {
        const H5HF_free_section_t *tmp_row_sect;    /* Pointer to row section */

        tmp_row_sect = sect->u.indirect.dir_rows[u];
        HDassert(tmp_row_sect->u.row.under == sect);
        HDassert(tmp_row_sect->u.row.row == (start_row + u));
        if(u < (dir_nrows - 1)) {
            const H5HF_free_section_t *tmp2_row_sect;    /* Pointer to row section */

            tmp2_row_sect = sect->u.indirect.dir_rows[u + 1];
            HDassert(tmp_row_sect->u.row.row != tmp2_row_sect->u.row.row);
        } /* end if */
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* H5HF_sect_indirect_valid() */


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
 *              Monday, July  3, 2006
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

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_indirect_debug() */

