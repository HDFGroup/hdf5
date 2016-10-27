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

#ifndef UC_2_H
#define UC_2_H

#include "hdf5.h"

/*
 * Definitions for VDS use case 2
 *
 * Datasets have a single unlimited dimension and two fixed dimensions. They
 * are mapped along two dimensions in the VDS with no gaps between them.
 */

/* virtual dataset <---> source dataset mapping and sizes

   **********************************
   *       A       *                *
   *****************        D       *
   *               *                *
   *       B       *                *
   *               ******************
   *****************        E       *
   *       C       *                *
   **********************************


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

#define UC_2_N_SOURCES      5

/* Dataset dimensions */
#define UC_2_A_HEIGHT       2
#define UC_2_B_HEIGHT       4
#define UC_2_AB_HEIGHT      6   /* For hyperslab start position */
#define UC_2_C_HEIGHT       2
#define UC_2_D_HEIGHT       5
#define UC_2_E_HEIGHT       3
#define UC_2_FULL_HEIGHT    8  /* A+B+C and D+E */
#define UC_2_WIDTH          7
#define UC_2_FULL_WIDTH     14 /* 2*width */

#define UC_2_N_PLANES_IN_SERIES 3   /* number of planes in a series of sub-images */
#define UC_2_N_MAX_PLANES       H5S_UNLIMITED   /* max number of planes */
#define UC_2_N_TEST_PLANES      6   /* number of planes we write */

/* Dataset datatypes */
#define UC_2_SOURCE_DATATYPE    H5T_STD_I32LE
#define UC_2_VDS_DATATYPE       H5T_STD_I32LE

/* Starting size of datasets, both source and VDS */
static hsize_t UC_2_DIMS[UC_2_N_SOURCES][RANK] = {
    {0, UC_2_A_HEIGHT, UC_2_WIDTH},
    {0, UC_2_B_HEIGHT, UC_2_WIDTH},
    {0, UC_2_C_HEIGHT, UC_2_WIDTH},
    {0, UC_2_D_HEIGHT, UC_2_WIDTH},
    {0, UC_2_E_HEIGHT, UC_2_WIDTH}
};

/* Maximum size of datasets, both source and VDS */
static hsize_t UC_2_MAX_DIMS[UC_2_N_SOURCES][RANK] = {
    {UC_2_N_MAX_PLANES, UC_2_A_HEIGHT, UC_2_WIDTH},
    {UC_2_N_MAX_PLANES, UC_2_B_HEIGHT, UC_2_WIDTH},
    {UC_2_N_MAX_PLANES, UC_2_C_HEIGHT, UC_2_WIDTH},
    {UC_2_N_MAX_PLANES, UC_2_D_HEIGHT, UC_2_WIDTH},
    {UC_2_N_MAX_PLANES, UC_2_E_HEIGHT, UC_2_WIDTH}
};

/* File names for source datasets */
static char UC_2_FILE_NAMES[UC_2_N_SOURCES][NAME_LEN] = {
    {"2_a.h5"},
    {"2_b.h5"},
    {"2_c.h5"},
    {"2_d.h5"},
    {"2_e.h5"}
};

/* VDS file name */
#define UC_2_VDS_FILE_NAME    "2_vds.h5"
    
/* Dataset names */
#define UC_2_SOURCE_DSET_NAME   "source_dset"
#define UC_2_SOURCE_DSET_PATH   "/source_dset"
#define UC_2_VDS_DSET_NAME      "vds_dset"

#endif /* UC_2_H */

