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
 *  Programmer: Bill Wendling <wendling@ncsa.uiuc.edu>
 *              25. August 2003
 *
 *  Dataset Filters
 *
 *      This is a filter, similar to the H5Z* filters. However, these
 *      transformations are applied to the data before a write or after a
 *      read.
 *
 *          - If applied before a write, a permanent change of the data
 *            may take place.
 *          - If applied after a read, the data is only changed
 *            in memory.
 *
 *      The transformations are transitory (not saved with the dataset).
 *      The transformations aren't automatically reversible, since this
 *      isn't possible in all cases and not possible if the original
 *      transformation was lost (it being transitory).
 */

/*
 * Pablo information
 * (Put before include files to avoid problems with inline functions)
 */
#define PABLO_MASK	H5DZ_mask

#include "H5private.h"          /* Generic Functions                    */


/*-------------------------------------------------------------------------
 * Function:    H5DZ_init_interface
 * Purpose:     Initializes the data filters layer.
 * Return:      Non-negative on success/Negative on failure
 * Programmer:  Bill Wendling
 *              25. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5DZ_init_interface(void)
{
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOINIT(H5DZ_init_interface)

    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5DZ_term_interface
 * Purpose:     Terminate the H5DZ layer.
 * Return:      0
 * Programmer:  Bill Wendling
 *              25. August 2003
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5DZ_term_interface(void)
{
    if (interface_initialize_g)
	interface_initialize_g = 0;

    return 0;
}
