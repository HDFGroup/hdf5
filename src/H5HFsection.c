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
static herr_t H5HF_sect_single_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static herr_t H5HF_sect_range_init(H5FS_section_class_t *cls, const void *udata);
static herr_t H5HF_sect_range_serialize(const H5FS_section_info_t *sect, uint8_t *buf);
static herr_t H5HF_sect_range_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static herr_t H5HF_sect_range_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);
static herr_t H5HF_sect_indirect_init(H5FS_section_class_t *cls, const void *udata);
static herr_t H5HF_sect_indirect_serialize(const H5FS_section_info_t *sect, uint8_t *buf);
static herr_t H5HF_sect_indirect_deserialize(H5FS_section_class_t *sect_cls,
    const uint8_t *buf, haddr_t sect_addr, hsize_t sect_size,
    H5FS_section_info_t **sect);
static herr_t H5HF_sect_indirect_debug(const H5FS_section_info_t *sect,
    FILE *stream, int indent, int fwidth);


/*********************/
/* Package Variables */
/*********************/

/* Class info for "single" free space sections */
/* (No callbacks necessary) */
H5FS_section_class_t H5FS_SECT_CLS_FHEAP_SINGLE[1] = {{
    H5FS_SECT_FHEAP_SINGLE,
    0,
    NULL,
    NULL,
    H5HF_sect_single_deserialize,
    NULL,
}};

/* Class info for "range" free space sections */
H5FS_section_class_t H5FS_SECT_CLS_FHEAP_RANGE[1] = {{
    H5FS_SECT_FHEAP_RANGE,
    0,
    H5HF_sect_range_init,
    H5HF_sect_range_serialize,
    H5HF_sect_range_deserialize,
    H5HF_sect_range_debug,
}};

/* Class info for "indirect" free space sections */
H5FS_section_class_t H5FS_SECT_CLS_FHEAP_INDIRECT[1] = {{
    H5FS_SECT_FHEAP_INDIRECT,
    0,
    H5HF_sect_indirect_init,
    H5HF_sect_indirect_serialize,
    H5HF_sect_indirect_deserialize,
    H5HF_sect_indirect_debug,
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
 * Function:	H5HF_free_section_free_cb
 *
 * Purpose:	Free a free section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 13, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_free_section_free_cb(void *_sect, void UNUSED *key, void UNUSED *op_data)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;
    herr_t ret_value = 0;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_free_section_free_cb)

    HDassert(sect);

    /* Check for live reference to an indirect block */
    if(sect->sect_info.state == H5FS_SECT_LIVE) {
        H5HF_indirect_t *iblock;        /* Indirect block referenced */

        /* Find indirect block that free section references */
        switch(sect->sect_info.cls->type) {
            case H5FS_SECT_FHEAP_SINGLE:
                iblock = sect->u.single.parent;
                break;

            case H5FS_SECT_FHEAP_RANGE:
                iblock = sect->u.range.iblock;
                break;

             case H5FS_SECT_FHEAP_INDIRECT:
                iblock = sect->u.indirect.iblock;
                break;
        } /* end switch */

        /* Release indirect block, if there was one */
        if(iblock)
            if(H5HF_iblock_decr(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")
    } /* end if */

    /* Release the sections */
    H5FL_FREE(H5HF_free_section_t, sect);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_free_section_free_cb() */


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
 * Function:	H5HF_sect_range_init
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
H5HF_sect_range_init(H5FS_section_class_t *cls, const void UNUSED *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_range_init)

    /* Check arguments. */
    HDassert(cls);

    /* Set the size of all serialized objects of this class of sections */
    cls->serial_size = 2                        /* Row */
        + 2                                     /* Column */
        + 2;                                    /* # of entries */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_range_init() */


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
 * Function:	H5HF_sect_indirect_init
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
H5HF_sect_indirect_init(H5FS_section_class_t *cls, const void UNUSED *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_sect_indirect_init)

    /* Check arguments. */
    HDassert(cls);

    /* Set the size of all serialized objects of this class of sections */
    cls->serial_size = 2                        /* Row */
        + 2                                     /* Column */
        + 2                                     /* # of entries */
        + 2                                     /* Indirect row */
        + 2;                                    /* Indirect # of rows */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_sect_indirect_init() */


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

