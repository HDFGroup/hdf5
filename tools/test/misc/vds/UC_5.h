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

#ifndef UC_5_H
#define UC_5_H

#include <hdf5.h>

#include "UC_common.h"

/*
 * Definitions for VDS use case 5
 *
 * Source datasets have one unlimited dimension and two fixed dimensions. In
 * this use case, the datasets are mapped in the VDS so that alternating
 * planes in the source are interleaved in the VDS.
 */

/* virtual dataset <---> source dataset mapping and sizes */

#define UC_5_N_SOURCES      3

/* Dataset dimensions */
#define UC_5_SRC_PLANES     3
#define UC_5_HEIGHT         4
#define UC_5_WIDTH          4

/* max number of planes for datasets */
#define UC_5_MAX_PLANES     H5S_UNLIMITED
#define UC_5_N_TEST_PLANES  9               /* number of planes in VDS */

/* Dataset datatypes */
#define UC_5_SOURCE_DATATYPE    H5T_STD_I32LE
#define UC_5_VDS_DATATYPE       H5T_STD_I32LE

/* Starting size of datasets, both source and VDS */
static hsize_t UC_5_SOURCE_DIMS[RANK] = {0, UC_5_HEIGHT, UC_5_WIDTH};
static hsize_t UC_5_VDS_DIMS[RANK]    = {0, UC_5_HEIGHT, UC_5_WIDTH};

/* Max size of datasets, both source and VDS */
static hsize_t UC_5_SOURCE_MAX_DIMS[RANK] = {UC_5_MAX_PLANES, UC_5_HEIGHT, UC_5_WIDTH};
static hsize_t UC_5_VDS_MAX_DIMS[RANK]    = {UC_5_MAX_PLANES, UC_5_HEIGHT, UC_5_WIDTH};

/* Planes (both source and VDS) */
static hsize_t UC_5_PLANE[RANK] = {1, UC_5_HEIGHT, UC_5_WIDTH};

/* File names for source datasets */
static char UC_5_FILE_NAMES[UC_5_N_SOURCES][NAME_LEN] = {
    {"5_a.h5"},
    {"5_b.h5"},
    {"5_c.h5"}
};

/* VDS file name */
static char UC_5_VDS_FILE_NAME[NAME_LEN] = "5_vds.h5";

/* Dataset names */
static char UC_5_SOURCE_DSET_NAME[NAME_LEN] = "source_dset";
static char UC_5_SOURCE_DSET_PATH[NAME_LEN] = "/source_dset";
static char UC_5_VDS_DSET_NAME[NAME_LEN]    = "vds_dset";

/* Fill values */
static int UC_5_FILL_VALUES[UC_5_N_SOURCES] = {
    -1,
    -2,
    -3
};
static int UC_5_VDS_FILL_VALUE = -9;

#endif /* UC_5_H */

