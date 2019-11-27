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

#include "vds_swmr.h"

hsize_t PLANES[N_SOURCES][RANK] = {
    {1, SM_HEIGHT, WIDTH},
    {1, LG_HEIGHT, WIDTH},
    {1, SM_HEIGHT, WIDTH},
    {1, LG_HEIGHT, WIDTH},
    {1, SM_HEIGHT, WIDTH},
    {1, LG_HEIGHT, WIDTH}
};

char FILE_NAMES[N_SOURCES][NAME_LEN] = {
    {"vds_swmr_src_a.h5"},
    {"vds_swmr_src_b.h5"},
    {"vds_swmr_src_c.h5"},
    {"vds_swmr_src_d.h5"},
    {"vds_swmr_src_e.h5"},
    {"vds_swmr_src_f.h5"}
};

char VDS_FILE_NAME[NAME_LEN] = "vds_swmr.h5";
char SOURCE_DSET_PATH[NAME_LEN] = "/source_dset";
char VDS_DSET_NAME[NAME_LEN]    = "vds_dset";
