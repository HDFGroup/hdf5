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
 *              Thursday, February  3, 2006
 *
 * Purpose:	Fractal heap testing functions.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */
#define H5HF_TESTING		/*suppress warning about H5HF testing funcs*/

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
 * Function:	H5HF_get_cparam_test
 *
 * Purpose:	Retrieve the parameters used to create the fractal heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_get_cparam_test(const H5HF_t *fh, H5HF_create_t *cparam)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_cparam_test)

    /* Check arguments. */
    HDassert(fh);
    HDassert(cparam);

    /* Get fractal heap creation parameters */
    cparam->max_man_size = fh->hdr->max_man_size;
    HDmemcpy(&(cparam->managed), &(fh->hdr->man_dtable.cparam), sizeof(H5HF_dtable_cparam_t));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_get_cparam_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_max_root_rows
 *
 * Purpose:	Retrieve the max. # of rows in the root indirect block
 *
 * Return:	Success:	Max. # of rows in root indirect block
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5HF_get_max_root_rows(const H5HF_t *fh)
{
    unsigned	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_max_root_rows)

    /* Check arguments. */
    HDassert(fh);

    /* Return max. # of rows in root indirect block */
    ret_value = fh->hdr->man_dtable.max_root_rows;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_max_root_rows() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_dtable_width_test
 *
 * Purpose:	Retrieve the width of the doubling table for a heap
 *
 * Return:	Success:	Width of the doubling table
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5HF_get_dtable_width_test(const H5HF_t *fh)
{
    unsigned	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_dtable_width_test)

    /* Check arguments. */
    HDassert(fh);

    /* Return width of doubling table */
    ret_value = fh->hdr->man_dtable.cparam.width;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_dtable_width_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_dtable_max_drows_test
 *
 * Purpose:	Retrieve the max. # of direct block rows in any indirect block
 *
 * Return:	Success:	Max. # of direct block rows
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5HF_get_dtable_max_drows_test(const H5HF_t *fh)
{
    unsigned	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_dtable_max_drows_test)

    /* Check arguments. */
    HDassert(fh);

    /* Return max. # of direct blocks in any indirect block */
    ret_value = fh->hdr->man_dtable.max_direct_rows;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_dtable_max_drows_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_iblock_max_drows_test
 *
 * Purpose:	Retrieve the max. # of direct block rows in an indirect block
 *
 * Note:        POS is indexed from 1 and is only really working for the
 *              2nd-level indirect blocks (i.e. indirect blocks with
 *              only direct block children)
 *
 * Return:	Success:	Max. # of direct block rows
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
unsigned
H5HF_get_iblock_max_drows_test(const H5HF_t *fh, unsigned pos)
{
    unsigned	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_iblock_max_drows_test)

    /* Check arguments. */
    HDassert(fh);
    HDassert(pos);

    /* Return max. # of direct blocks in this indirect block row */
    ret_value = pos + (fh->hdr->man_dtable.max_direct_bits -
            fh->hdr->man_dtable.first_row_bits) + 1;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_iblock_max_drows_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_dblock_size_test
 *
 * Purpose:	Retrieve the size of a direct block for a given row
 *
 * Return:	Success:	Size of direct block
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5HF_get_dblock_size_test(const H5HF_t *fh, unsigned row)
{
    hsize_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_dblock_size_test)

    /* Check arguments. */
    HDassert(fh);

    /* Return direct block free space */
    ret_value = fh->hdr->man_dtable.row_block_size[row];

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_dblock_size_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_dblock_free_test
 *
 * Purpose:	Retrieve the size of direct block free space for a given
 *              direct or indirect block size
 *
 * Return:	Success:	Size of direct block free space
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5HF_get_dblock_free_test(const H5HF_t *fh, unsigned row)
{
    hsize_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_dblock_free_test)

    /* Check arguments. */
    HDassert(fh);

    /* Return direct block free space */
    ret_value = fh->hdr->man_dtable.row_tot_dblock_free[row];

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_dblock_free_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_id_off_test
 *
 * Purpose:	Retrieve the offset for a heap ID
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_get_id_off_test(const H5HF_t *fh, const void *_id, hsize_t *obj_off)
{
    const uint8_t *id = (const uint8_t *)_id;   /* Object ID */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_id_off_test)

    /* Check arguments. */
    HDassert(fh);
    HDassert(fh->hdr);
    HDassert(id);
    HDassert(obj_off);

    /* Get the offset for a heap ID */
    UINT64DECODE_VAR(id, *obj_off, fh->hdr->heap_off_size);                           \

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_get_id_off_test() */

