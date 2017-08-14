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
 * File/dataset generator for VDS use case 3
 *
 * See the header file for a description.
 */

#include <stdlib.h>
#include <string.h>

#include <hdf5.h>

#include "UC_common.h"
#include "UC_3.h"

/* Create the VDS that uses use case 1 files */
static herr_t
create_3_1_vds(void)
{
    hid_t src_sid       = -1;   /* source dataset's dataspace ID            */
    hid_t vds_sid       = -1;   /* VDS dataspace ID                         */
    hid_t vds_dcplid    = -1;   /* VDS dataset property list ID             */

    hid_t fid           = -1;   /* HDF5 file ID                             */
    hid_t did           = -1;   /* dataset ID                               */

    hsize_t start[RANK];        /* source starting point for hyperslab      */
    hsize_t position[RANK];     /* vds mapping positions                    */

    int i;                      /* iterator                                 */

    /* Create VDS dcpl */
    if((vds_dcplid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        UC_ERROR
    if(H5Pset_fill_value(vds_dcplid, UC_31_VDS_DATATYPE,
                &UC_3_VDS_FILL_VALUE) < 0)
        UC_ERROR

    /* Create VDS dataspace */
    if((vds_sid = H5Screate_simple(RANK, UC_31_VDS_DIMS,
                    UC_31_VDS_MAX_DIMS)) < 0)
        UC_ERROR

    /* Set starting positions */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;

    position[0] = 0;
    position[1] = UC_31_GAP;
    position[2] = 0;

    /******************************
     * Create source-VDS mappings *
     ******************************/
    for(i = 0; i < UC_1_N_SOURCES; i++) {

        if((src_sid = H5Screate_simple(RANK, UC_1_DIMS[i],
                    UC_1_MAX_DIMS[i])) < 0)
        UC_ERROR

        /* set up hyperslabs for source and destination datasets */
        if(H5Sselect_hyperslab(src_sid, H5S_SELECT_SET, start, NULL,
                    UC_1_MAX_DIMS[i], NULL) < 0)
            UC_ERROR
        if(H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, position,
                    NULL, UC_1_MAX_DIMS[i], NULL) < 0)
            UC_ERROR
        position[1] += UC_1_DIMS[i][1] + UC_31_GAP;

        /* Add VDS mapping */
        if(H5Pset_virtual(vds_dcplid, vds_sid, UC_1_FILE_NAMES[i],
                    UC_1_SOURCE_DSET_PATH, src_sid) < 0)
            UC_ERROR
        if(H5Sclose(src_sid) < 0)
            UC_ERROR

    } /* end for */

    /*******************************
     * Create VDS file and dataset *
     *******************************/

    /* file */
    if((fid = H5Fcreate(UC_31_VDS_FILE_NAME, H5F_ACC_TRUNC,
                    H5P_DEFAULT, H5P_DEFAULT)) < 0)
        UC_ERROR

    /* dataset */
    if((did = H5Dcreate2(fid, UC_3_VDS_DSET_NAME, UC_31_VDS_DATATYPE, vds_sid,
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

    return 0;

error:

    H5E_BEGIN_TRY {
        if(vds_sid >= 0)
            (void)H5Sclose(vds_sid);
        if(vds_dcplid >= 0)
            (void)H5Pclose(vds_dcplid);
        if(fid >= 0)
            (void)H5Fclose(fid);
        if(did >= 0)
            (void)H5Dclose(did);
    } H5E_END_TRY

    return -1;

} /* end create_3_1_vds() */

/* Create the VDS that uses use case 2 files */
static herr_t
create_3_2_vds(void)
{
    hid_t src_sid       = -1;   /* source dataset's dataspace ID            */
    hid_t vds_sid       = -1;   /* VDS dataspace ID                         */
    hid_t vds_dcplid    = -1;   /* VDS dataset property list ID             */

    hid_t fid           = -1;   /* HDF5 file ID                             */
    hid_t did           = -1;   /* dataset ID                               */

    hsize_t start[RANK];        /* source starting point for hyperslab      */

    int i;                      /* iterator                                 */

    /* Create VDS dcpl */
    if((vds_dcplid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        UC_ERROR
    if(H5Pset_fill_value(vds_dcplid, UC_32_VDS_DATATYPE,
                &UC_3_VDS_FILL_VALUE) < 0)
        UC_ERROR

    /* Create VDS dataspace */
    if((vds_sid = H5Screate_simple(RANK, UC_32_VDS_DIMS,
                    UC_32_VDS_MAX_DIMS)) < 0)
        UC_ERROR

    /* Set starting positions */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;

    /******************************
     * Create source-VDS mappings *
     ******************************/
    for(i = 0; i < UC_2_N_SOURCES; i++) {

        if((src_sid = H5Screate_simple(RANK, UC_2_DIMS[i],
                    UC_2_MAX_DIMS[i])) < 0)
        UC_ERROR

        /* set up hyperslabs for source and destination datasets */
        if(H5Sselect_hyperslab(src_sid, H5S_SELECT_SET, start, NULL,
                    UC_2_MAX_DIMS[i], NULL) < 0)
            UC_ERROR
        if(H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, UC_32_POSITIONS[i],
                    NULL, UC_2_MAX_DIMS[i], NULL) < 0)
            UC_ERROR

        /* Add VDS mapping */
        if(H5Pset_virtual(vds_dcplid, vds_sid, UC_2_FILE_NAMES[i],
                    UC_2_SOURCE_DSET_PATH, src_sid) < 0)
            UC_ERROR
        if(H5Sclose(src_sid) < 0)
            UC_ERROR

    } /* end for */

    /*******************************
     * Create VDS file and dataset *
     *******************************/

    /* file */
    if((fid = H5Fcreate(UC_32_VDS_FILE_NAME, H5F_ACC_TRUNC,
                    H5P_DEFAULT, H5P_DEFAULT)) < 0)
        UC_ERROR

    /* dataset */
    if((did = H5Dcreate2(fid, UC_3_VDS_DSET_NAME, UC_32_VDS_DATATYPE, vds_sid,
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

    return 0;

error:

    H5E_BEGIN_TRY {
        if(vds_sid >= 0)
            (void)H5Sclose(vds_sid);
        if(vds_dcplid >= 0)
            (void)H5Pclose(vds_dcplid);
        if(fid >= 0)
            (void)H5Fclose(fid);
        if(did >= 0)
            (void)H5Dclose(did);
    } H5E_END_TRY

    return -1;

} /* end create_3_2_vds() */

int
main(void)
{

    if(create_3_1_vds() < 0)
        UC_ERROR

    if(create_3_2_vds() < 0)
        UC_ERROR

    return EXIT_SUCCESS;

error:

    return EXIT_FAILURE;

} /* end main() */

