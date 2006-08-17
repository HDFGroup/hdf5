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
 *              Monday, March  6, 2006
 *
 * Purpose:	Fractal heap metadata statistics functions.
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


/********************/
/* Package Typedefs */
/********************/


/******************/
/* Local Typedefs */
/******************/


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
 * Function:	H5HF_stat_info
 *
 * Purpose:	Retrieve metadata statistics for the fractal heap
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_stat_info(const H5HF_t *fh, H5HF_stat_t *stats)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_stat_info)

    /* Check arguments. */
    HDassert(fh);
    HDassert(stats);

    /* Report statistics for fractal heap */
    stats->man_size = fh->hdr->man_size;
    stats->man_alloc_size = fh->hdr->man_alloc_size;
    stats->man_iter_off = fh->hdr->man_iter_off;
    stats->man_nobjs = fh->hdr->man_nobjs;
    stats->man_free_space = fh->hdr->total_man_free;
    stats->huge_size = fh->hdr->huge_size;
    stats->huge_nobjs = fh->hdr->huge_nobjs;
    stats->tiny_size = fh->hdr->tiny_size;
    stats->tiny_nobjs = fh->hdr->tiny_nobjs;
/* XXX: Add more metadata statistics for the heap */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_stat_info() */

