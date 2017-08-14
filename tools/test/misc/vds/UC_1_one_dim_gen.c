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

/*
 * File/dataset generator for VDS use case 1
 *
 * See the header file for a description.
 */

#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

#include "UC_common.h"
#include "UC_1.h"

static hsize_t UC_1_VDS_DIMS[RANK] = {0, UC_1_FULL_HEIGHT, UC_1_WIDTH};
static hsize_t UC_1_VDS_MAX_DIMS[RANK] = {UC_1_N_MAX_PLANES, UC_1_FULL_HEIGHT, UC_1_WIDTH};

/* Planes */
static hsize_t UC_1_PLANES[UC_1_N_SOURCES][RANK] = {
    {1, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {1, UC_1_LG_HEIGHT, UC_1_WIDTH},
    {1, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {1, UC_1_LG_HEIGHT, UC_1_WIDTH},
    {1, UC_1_SM_HEIGHT, UC_1_WIDTH},
    {1, UC_1_LG_HEIGHT, UC_1_WIDTH}
};

/* VDS file name */
static char UC_1_VDS_FILE_NAME[NAME_LEN] = "1_vds.h5";

/* Dataset names */
static char UC_1_SOURCE_DSET_NAME[NAME_LEN] = "source_dset";
static char UC_1_VDS_DSET_NAME[NAME_LEN]    = "vds_dset";
    
/* Fill values */
static int UC_1_FILL_VALUES[UC_1_N_SOURCES] = {
    -1,
    -2,
    -3,
    -4,
    -5,
    -6
};
static int UC_1_VDS_FILL_VALUE = -9;

int
main(void)
{
    hid_t src_sid       = -1;   /* source dataset's dataspace ID            */
    hid_t src_dcplid    = -1;   /* source dataset property list ID          */

    hid_t vds_sid       = -1;   /* VDS dataspace ID                         */
    hid_t vds_dcplid    = -1;   /* VDS dataset property list ID             */

    hid_t fid           = -1;   /* HDF5 file ID                             */
    hid_t did           = -1;   /* dataset ID                               */
    hid_t msid          = -1;   /* memory dataspace ID                      */
    hid_t fsid          = -1;   /* file dataspace ID                        */

    hsize_t extent[RANK];       /* dataset extents                          */
    hsize_t start[RANK];        /* starting point for hyperslab             */
    hsize_t map_start   = 0;    /* starting point in the VDS map            */

    int *buffer         = NULL; /* data buffer                              */
    hsize_t count       = 0;    /* number of elements in a plane            */
    int n_planes        = -1;   /* number of planes to write                */
    int value           = -1;   /* value written to datasets                */

    int i;                      /* iterator                                 */
    int j;                      /* iterator                                 */
    hsize_t k;                  /* iterator                                 */


    /* Start by creating the virtual dataset (VDS) dataspace and creation
     * property list. The individual source datasets are then created
     * and the VDS map (stored in the VDS property list) is updated.
     */

    /* Create VDS dcpl */
    if((vds_dcplid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        UC_ERROR
    if(H5Pset_fill_value(vds_dcplid, UC_1_VDS_DATATYPE,
                &UC_1_VDS_FILL_VALUE) < 0)
        UC_ERROR

    /* Create VDS dataspace */
    if((vds_sid = H5Screate_simple(RANK, UC_1_VDS_DIMS,
                    UC_1_VDS_MAX_DIMS)) < 0)
        UC_ERROR

    /************************************
     * Create source files and datasets *
     ************************************/

    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    map_start = 0;

    for(i = 0; i < UC_1_N_SOURCES; i++) {

        /* Create source dataset dcpl */
        if((src_dcplid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            UC_ERROR
        if(H5Pset_chunk(src_dcplid, RANK, UC_1_PLANES[i]) < 0)
            UC_ERROR
        if(H5Pset_fill_value(src_dcplid, UC_1_SOURCE_DATATYPE,
                    &UC_1_FILL_VALUES[i]) < 0)
            UC_ERROR
        if(0 != i % 2)
            if(H5Pset_deflate(src_dcplid, COMPRESSION_LEVEL) < 0)
                UC_ERROR

        /* Create source file, dataspace, and dataset */
        if((fid = H5Fcreate(UC_1_FILE_NAMES[i], H5F_ACC_TRUNC,
                        H5P_DEFAULT, H5P_DEFAULT)) < 0)
            UC_ERROR
        if((src_sid = H5Screate_simple(RANK, UC_1_DIMS[i],
                        UC_1_MAX_DIMS[i])) < 0)
            UC_ERROR
        if((did = H5Dcreate2(fid, UC_1_SOURCE_DSET_NAME,
                        UC_1_SOURCE_DATATYPE, src_sid,
                        H5P_DEFAULT, src_dcplid, H5P_DEFAULT)) < 0)
            UC_ERROR

        /* Set the dataset's extent (will eventually vary with i) */
        extent[0] = UC_1_N_TEST_PLANES;
        extent[1] = UC_1_PLANES[i][1];
        extent[2] = UC_1_PLANES[i][2];
        if(H5Dset_extent(did, extent) < 0)
            UC_ERROR

        /* Create a data buffer that represents a plane */
        count = UC_1_PLANES[i][1] * UC_1_PLANES[i][2];
        if(NULL == (buffer = (int *)malloc(count * sizeof(int))))
            UC_ERROR

        /* Create the memory dataspace */
        if((msid = H5Screate_simple(RANK, UC_1_PLANES[i], NULL)) < 0)
            UC_ERROR

        /* Get the file dataspace */
        if((fsid = H5Dget_space(did)) < 0)
            UC_ERROR

        /* Write planes to the dataset, number will eventually vary with i */
        n_planes = UC_1_N_TEST_PLANES;
        for(j = 0; j < n_planes; j++) {

            value = ((i + 1) * 10) + j;
            for(k = 0; k < count; k++)
               buffer[k] = value; 

            start[0] = (hsize_t)j;
            start[1] = 0;
            start[2] = 0;
            if(H5Sselect_hyperslab(fsid, H5S_SELECT_SET, start, NULL, UC_1_PLANES[i], NULL) < 0)
                UC_ERROR
            if(H5Dwrite(did, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, buffer) < 0)
                UC_ERROR

        } /* end for */

        /* set up hyperslabs for source and destination datasets */
        start[0] = 0;
        start[1] = 0;
        start[2] = 0;
        if(H5Sselect_hyperslab(src_sid, H5S_SELECT_SET, start, NULL,
                    UC_1_MAX_DIMS[i], NULL) < 0)
            UC_ERROR
        start[0] = 0;
        start[1] = map_start;
        start[2] = 0;
        if(H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, NULL,
                    UC_1_MAX_DIMS[i], NULL) < 0)
            UC_ERROR
        map_start += UC_1_PLANES[i][1];

        /* Add VDS mapping */
        if(H5Pset_virtual(vds_dcplid, vds_sid, UC_1_FILE_NAMES[i],
                    UC_1_SOURCE_DSET_PATH, src_sid) < 0)
            UC_ERROR

        /* close */
        if(H5Sclose(src_sid) < 0)
            UC_ERROR
        if(H5Pclose(src_dcplid) < 0)
            UC_ERROR
        if(H5Sclose(msid) < 0)
            UC_ERROR
        if(H5Sclose(fsid) < 0)
            UC_ERROR
        if(H5Dclose(did) < 0)
            UC_ERROR
        if(H5Fclose(fid) < 0)
            UC_ERROR
        free(buffer);

    } /* end for */


    /*******************
     * Create VDS file *
     *******************/

    /* file */
    if((fid = H5Fcreate(UC_1_VDS_FILE_NAME, H5F_ACC_TRUNC,
                    H5P_DEFAULT, H5P_DEFAULT)) < 0)
        UC_ERROR

    /* dataset */
    if((did = H5Dcreate2(fid, UC_1_VDS_DSET_NAME, UC_1_VDS_DATATYPE, vds_sid,
                    H5P_DEFAULT, vds_dcplid, H5P_DEFAULT)) < 0)
        UC_ERROR

    /* close */
    if(H5Pclose(vds_dcplid) < 0)
        UC_ERROR
    if(H5Sclose(vds_sid) < 0)
        UC_ERROR
    if(H5Dclose(did) < 0)
        UC_ERROR
    if(H5Fclose(fid) < 0)
        UC_ERROR

    return EXIT_SUCCESS;

error:

    H5E_BEGIN_TRY {
        if(src_sid >= 0)
            (void)H5Sclose(src_sid);
        if(src_dcplid >= 0)
            (void)H5Pclose(src_dcplid);
        if(vds_sid >= 0)
            (void)H5Sclose(vds_sid);
        if(vds_dcplid >= 0)
            (void)H5Pclose(vds_dcplid);
        if(fid >= 0)
            (void)H5Fclose(fid);
        if(did >= 0)
            (void)H5Dclose(did);
        if(msid >= 0)
            (void)H5Sclose(msid);
        if(fsid >= 0)
            (void)H5Sclose(fsid);
        if(buffer != NULL)
            free(buffer);
    } H5E_END_TRY

    return EXIT_FAILURE;

} /* end main */

