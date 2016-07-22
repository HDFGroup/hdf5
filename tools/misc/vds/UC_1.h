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

#ifndef UC_1_H
#define UC_1_H

#include "hdf5.h"

#include "UC_common.h"

/*
 * Definitions for VDS use case 1
 *
 * Datasets have a single unlimited dimension and two fixed dimensions. They
 * are mapped along a single dimension in the VDS with no gaps between them.
 */

/* virtual dataset <---> source dataset mapping and sizes

   *****************  --+
   *       A       *    K
   *****************  --+
   *               *    |
   *       B       *    N
   *               *    |
   *****************  --+
   *       C       *
   *****************
   *               *
   *       D       *
   *               *
   *****************
   *       E       *
   *****************
   *               *
   *       F       *
   *               *
   *****************

   |               |
   +-------M-------+
 

        dim[0]
       /
      /
     /
    -----> dim[2]
    |
    |
    |
   dim[1]

  */


#define UC_1_N_SOURCES      6

/* Dataset dimensions */
#define UC_1_SM_HEIGHT      2   /* K */
#define UC_1_LG_HEIGHT      4   /* N */
#define UC_1_SM_LG_HEIGHT   6   /* SM_HEIGHT + LG_HEIGHT */
#define UC_1_FULL_HEIGHT    18  /* (3 * K) + (3 * N) */
#define UC_1_HALF_HEIGHT    9
#define UC_1_WIDTH          8   /* M */
#define UC_1_HALF_WIDTH     4

#define UC_1_N_MAX_PLANES   H5S_UNLIMITED   /* max number of planes         */
#define UC_1_N_TEST_PLANES  5               /* number of planes we write    */

/* Dataset datatypes */
#define UC_1_SOURCE_DATATYPE    H5T_STD_I32LE
#define UC_1_VDS_DATATYPE       H5T_STD_I32LE

/* Starting size of datasets, both source and VDS */
static hsize_t UC_1_DIMS[UC_1_N_SOURCES][RANK] = {
    {0, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {0, UC_1_LG_HEIGHT, UC_1_WIDTH},
    {0, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {0, UC_1_LG_HEIGHT, UC_1_WIDTH},
    {0, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {0, UC_1_LG_HEIGHT, UC_1_WIDTH}
};

/* Maximum size of datasets, both source and VDS */
static hsize_t UC_1_MAX_DIMS[UC_1_N_SOURCES][RANK] = {
    {UC_1_N_MAX_PLANES, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {UC_1_N_MAX_PLANES, UC_1_LG_HEIGHT, UC_1_WIDTH},
    {UC_1_N_MAX_PLANES, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {UC_1_N_MAX_PLANES, UC_1_LG_HEIGHT, UC_1_WIDTH},
    {UC_1_N_MAX_PLANES, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {UC_1_N_MAX_PLANES, UC_1_LG_HEIGHT, UC_1_WIDTH}
};

/* File names for source datasets */
static char UC_1_FILE_NAMES[UC_1_N_SOURCES][NAME_LEN] = {
    {"1_a.h5"},
    {"1_b.h5"},
    {"1_c.h5"},
    {"1_d.h5"},
    {"1_e.h5"},
    {"1_f.h5"}
};

/* Dataset names */
static char UC_1_SOURCE_DSET_PATH[NAME_LEN] = "/source_dset";

#endif /* UC_1_H */

