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
#define RANK 3

/* Lengths of string identifiers (file, dataset names, etc.) */
#define NAME_LEN 32

/* Compression level */
#define COMPRESSION_LEVEL 7

/* Number of source files */
#define N_SOURCES 6

/* Dataset dimensions */
#define SM_HEIGHT   2  /* K */
#define LG_HEIGHT   4  /* N */
#define FULL_HEIGHT 18 /* (3 * K) + (3 * N) */
#define WIDTH       8  /* M */

/* Number of planes each writer will write */
#define N_PLANES_TO_WRITE 25

/* Planes */
H5TEST_DLLVAR hsize_t PLANES[N_SOURCES][RANK];

/* File names for source datasets */
H5TEST_DLLVAR char FILE_NAMES[N_SOURCES][NAME_LEN];

/* VDS file name */
H5TEST_DLLVAR char VDS_FILE_NAME[NAME_LEN];

/* Dataset names */
H5TEST_DLLVAR char SOURCE_DSET_PATH[NAME_LEN];
H5TEST_DLLVAR char VDS_DSET_NAME[NAME_LEN];
#endif /* VDS_SWMR_H */
