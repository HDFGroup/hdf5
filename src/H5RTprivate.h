/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains private information about the r-tree module
 */
#ifndef H5RTprivate_H
#define H5RTprivate_H

#include "H5private.h"

/* The overall r-tree structure */
typedef struct H5RT_t H5RT_t;

/* Struct representing a leaf in the r-tree */
typedef struct H5RT_leaf_t {
    void *record;
    hsize_t min[H5S_MAX_RANK];
    hsize_t max[H5S_MAX_RANK];
    hsize_t mid[H5S_MAX_RANK];
    struct H5RT_leaf_t *next;
} H5RT_leaf_t;

H5RT_t *H5RT_create(int rank, H5RT_leaf_t *leaves, size_t count);
H5RT_leaf_t *H5RT_search(H5RT_t *rtree, hsize_t min[], hsize_t max[]);
void H5RT_free(H5RT_t *rtree);

#endif /* H5RTprivate_H */