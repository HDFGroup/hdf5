/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef UC_4_H
#define UC_4_H

#include "hdf5.h"

#include "UC_common.h"

/*
 * Definitions for VDS use case 4
 *
 * Source datasets have three fixed dimensions. In this use case, the
 * datasets are mapped consecutively in the VDS along a single dimension with
 * no gaps between them. Datasets are automatically loaded using a
 * printf-like format string for the file name.
 */

/* virtual dataset <---> source dataset mapping and sizes */

#define UC_4_N_SOURCES      3

/* Dataset dimensions */
#define UC_4_SRC_PLANES     3
#define UC_4_HEIGHT         4
#define UC_4_WIDTH          4

/* max number of planes for VDS (sources are finite) */
#define UC_4_VDS_MAX_PLANES   H5S_UNLIMITED
#define UC_4_N_TEST_PLANES    9                 /* number of planes in the VDS */

/* Dataset datatypes */
#define UC_4_SOURCE_DATATYPE    H5T_STD_I32LE
#define UC_4_VDS_DATATYPE       H5T_STD_I32LE

/* Starting size of datasets, both source and VDS */
static hsize_t UC_4_SOURCE_DIMS[RANK] = {0, UC_4_HEIGHT, UC_4_WIDTH};
static hsize_t UC_4_VDS_DIMS[RANK] = {0, UC_4_HEIGHT, UC_4_WIDTH};

/* Max size of datasets, both source and VDS */
static hsize_t UC_4_SOURCE_MAX_DIMS[RANK] = {UC_4_SRC_PLANES, UC_4_HEIGHT, UC_4_WIDTH};
static hsize_t UC_4_VDS_MAX_DIMS[RANK] = {UC_4_VDS_MAX_PLANES, UC_4_HEIGHT, UC_4_WIDTH};

/* Planes (both source and VDS) */
static hsize_t UC_4_PLANE[RANK] = {1, UC_4_HEIGHT, UC_4_WIDTH};

/* File names for source datasets */
static char UC_4_FILE_NAMES[UC_4_N_SOURCES][NAME_LEN] = {
    {"4_0.h5"},
    {"4_1.h5"},
    {"4_2.h5"}
};
static char UC_4_MAPPING_FILE_NAME[NAME_LEN] = "4_%b.h5";

/* VDS file name */
static char UC_4_VDS_FILE_NAME[NAME_LEN] = "4_vds.h5";

/* Dataset names */
static char UC_4_SOURCE_DSET_NAME[NAME_LEN] = "source_dset";
static char UC_4_SOURCE_DSET_PATH[NAME_LEN] = "/source_dset";
static char UC_4_VDS_DSET_NAME[NAME_LEN]    = "vds_dset";

/* Fill values */
static int UC_4_FILL_VALUES[UC_4_N_SOURCES] = {
    -1,
    -2,
    -3
};
static int UC_4_VDS_FILL_VALUE = -9;

#endif /* UC_4_H */


