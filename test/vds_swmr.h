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

#ifndef VDS_SWMR_H
#define VDS_SWMR_H

#include <hdf5.h>

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


   NOTE: This use case also checks for varying numbers of written planes.
         Dataset A contains the full number of planes and each successive
         dataset contains one fewer plane, down to the last dataset, which
         contains zero planes. Each dataset is set to have an (unlimited
         dimension) extent equal to the number of planes written, so the
         "empty" regions will contain the VDS fill value.
*/


/* All datasets are 3D */
#define RANK                3

/* Lengths of string identifiers (file, dataset names, etc.) */
#define NAME_LEN            32

/* Compression level */
#define COMPRESSION_LEVEL   7

/* Number of source files */
#define N_SOURCES      6

/* Dataset dimensions */
#define SM_HEIGHT      2   /* K */
#define LG_HEIGHT      4   /* N */
#define SM_LG_HEIGHT   6   /* SM_HEIGHT + LG_HEIGHT */
#define FULL_HEIGHT    18  /* (3 * K) + (3 * N) */
#define HALF_HEIGHT    9
#define WIDTH          8   /* M */
#define HALF_WIDTH     4

/* Max number of planes in the dataset */
#define N_MAX_PLANES   H5S_UNLIMITED

/* Number of planes each writer will write */
#define N_PLANES_TO_WRITE   25

/* Dataset datatypes */
#define SOURCE_DATATYPE    H5T_STD_I32LE
#define VDS_DATATYPE       H5T_STD_I32LE

/* Starting size of datasets, both source and VDS */
static hsize_t DIMS[N_SOURCES][RANK] = {
    {0, SM_HEIGHT, WIDTH},
    {0, LG_HEIGHT, WIDTH},
    {0, SM_HEIGHT, WIDTH},
    {0, LG_HEIGHT, WIDTH},
    {0, SM_HEIGHT, WIDTH},
    {0, LG_HEIGHT, WIDTH}
};
static hsize_t VDS_DIMS[RANK] = {0, FULL_HEIGHT, WIDTH};

/* Maximum size of datasets, both source and VDS.
 * NOTE: Theoretical (i.e.: H5S_UNLIMITED), not the actual max
 * number of planes written out by the writers before they stop.
 * That number is specified separately.
 */
static hsize_t MAX_DIMS[N_SOURCES][RANK] = {
    {N_MAX_PLANES, SM_HEIGHT, WIDTH},
    {N_MAX_PLANES, LG_HEIGHT, WIDTH},
    {N_MAX_PLANES, SM_HEIGHT, WIDTH},
    {N_MAX_PLANES, LG_HEIGHT, WIDTH},
    {N_MAX_PLANES, SM_HEIGHT, WIDTH},
    {N_MAX_PLANES, LG_HEIGHT, WIDTH}
};
static hsize_t VDS_MAX_DIMS[RANK] = {N_MAX_PLANES, FULL_HEIGHT, WIDTH};

/* Planes */
static hsize_t PLANES[N_SOURCES][RANK] = {
    {1, SM_HEIGHT, WIDTH},
    {1, LG_HEIGHT, WIDTH},
    {1, SM_HEIGHT, WIDTH},
    {1, LG_HEIGHT, WIDTH},
    {1, SM_HEIGHT, WIDTH},
    {1, LG_HEIGHT, WIDTH}
};

/* File names for source datasets */
static char FILE_NAMES[N_SOURCES][NAME_LEN] = {
    {"vds_swmr_src_a.h5"},
    {"vds_swmr_src_b.h5"},
    {"vds_swmr_src_c.h5"},
    {"vds_swmr_src_d.h5"},
    {"vds_swmr_src_e.h5"},
    {"vds_swmr_src_f.h5"}
};

/* VDS file name */
static char VDS_FILE_NAME[NAME_LEN] = "vds_swmr.h5";

/* Dataset names */
static char SOURCE_DSET_NAME[NAME_LEN] = "source_dset";
static char SOURCE_DSET_PATH[NAME_LEN] = "/source_dset";
static char VDS_DSET_NAME[NAME_LEN]    = "vds_dset";

/* Fill values */
static int32_t FILL_VALUES[N_SOURCES] = {
    -1,
    -2,
    -3,
    -4,
    -5,
    -6
};
static int32_t VDS_FILL_VALUE = -9;

#endif /* VDS_SWMR_H */

