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
    cparam->addrmap = fh->hdr->addrmap;
    cparam->standalone_size = fh->hdr->standalone_size;
    HDmemcpy(&(cparam->managed), &(fh->hdr->man_dtable.cparam), sizeof(H5HF_dtable_cparam_t));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_get_cparam_test() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_dblock_overhead
 *
 * Purpose:	Retrieve the size of direct block overhead
 *
 * Return:	Success:	Size of direct block overhead
 *
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May  9, 2006
 *
 *-------------------------------------------------------------------------
 */
size_t
H5HF_get_dblock_overhead(const H5HF_t *fh)
{
    size_t	ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_get_dblock_overhead)

    /* Check arguments. */
    HDassert(fh);

    /* Return direct block overhead */
    ret_value = H5HF_MAN_ABS_DIRECT_OVERHEAD(fh->hdr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_get_dblock_overhead() */


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
    ret_value = fh->hdr->man_dtable.row_dblock_free[row];

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

