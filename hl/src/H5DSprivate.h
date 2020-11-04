/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef H5DSprivate_H
#define H5DSprivate_H

/* High-level library internal header file */
#include "H5HLprivate2.h"

/* public LT prototypes			*/
#include "H5DSpublic.h"

/* attribute type of a DS dataset */
typedef struct ds_list_t {
#ifdef H5_DIMENSION_SCALES_WITH_NEW_REF
    H5R_ref_t    ref;
#else
    hobj_ref_t   ref;     /* object reference  */
#endif
    unsigned int dim_idx; /* dimension index of the dataset */
} ds_list_t;

/*-------------------------------------------------------------------------
 * private functions
 *-------------------------------------------------------------------------
 */

#endif
