/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef UC_3_H
#define UC_3_H

#include "hdf5.h"

#include "UC_1.h"
#include "UC_2.h"

/*
 * Definitions for VDS use case 3
 *
 * Datasets have a single unlimited dimension and one or two fixed
 * dimensions (they are reused from use cases 1 and 2). In this use case,
 * the datasets are mapped in the VDS with gaps between them.
 */

/* VDS dimensions
 * Height and width are large enough to encompass the
 * mapped source datasets with gaps.
 */
#define UC_31_VDS_HEIGHT    25  /* full height + 7 (gaps of 1)  */
#define UC_31_VDS_WIDTH     8   /* full width + 0 (no gaps)     */
#define UC_32_VDS_HEIGHT    13  /* full height + 5              */
#define UC_32_VDS_WIDTH     19  /* full width + 5               */
#define UC_31_GAP           1

/* VDS datatypes */
#define UC_31_VDS_DATATYPE       H5T_STD_I32LE
#define UC_32_VDS_DATATYPE       H5T_STD_I32LE

/* Starting size of virtual datasets */
static hsize_t UC_31_VDS_DIMS[RANK]  = {0, UC_31_VDS_HEIGHT, UC_31_VDS_WIDTH};
static hsize_t UC_32_VDS_DIMS[RANK]  = {0, UC_32_VDS_HEIGHT, UC_32_VDS_WIDTH};

/* Maximum size of virtual datasets */
static hsize_t UC_31_VDS_MAX_DIMS[RANK]  = {UC_1_N_MAX_PLANES, UC_31_VDS_HEIGHT, UC_31_VDS_WIDTH};
static hsize_t UC_32_VDS_MAX_DIMS[RANK]  = {UC_2_N_MAX_PLANES, UC_32_VDS_HEIGHT, UC_32_VDS_WIDTH};

/* Positions of mapped source datasets */
static hsize_t UC_32_POSITIONS[UC_2_N_SOURCES][RANK] = {
    /* A */ {0,  1,  1},
    /* B */ {0,  4,  0},
    /* C */ {0, 11,  4},
    /* D */ {0,  1,  9},
    /* E */ {0,  8, 12}
};

/* VDS file names */
#define UC_31_VDS_FILE_NAME     "3_1_vds.h5"
#define UC_32_VDS_FILE_NAME     "3_2_vds.h5"

/* Dataset name */
#define UC_3_VDS_DSET_NAME      "vds_dset"

/* Fill value */
static int UC_3_VDS_FILL_VALUE      = -9;

#endif /* UC_3_H */

