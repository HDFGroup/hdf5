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

#include "h5test.h"
#include "vds_swmr.h"

int
main(void)
{
    hid_t faplid        = -1;   /* file access property list ID (all files) */

    hid_t src_sid       = -1;   /* source dataset's dataspace ID            */
    hid_t src_dcplid    = -1;   /* source dataset property list ID          */

    hid_t vds_sid       = -1;   /* VDS dataspace ID                         */
    hid_t vds_dcplid    = -1;   /* VDS dataset property list ID             */

    hid_t fid           = -1;   /* HDF5 file ID                             */
    hid_t did           = -1;   /* dataset ID                               */

    hsize_t start[RANK];        /* starting point for hyperslab             */
    int map_start       = -1;   /* starting point in the VDS map            */

    int i;                      /* iterator                         */


    /* Start by creating the virtual dataset (VDS) dataspace and creation
     * property list. The individual source datasets are then created
     * and the VDS map (stored in the VDS property list) is updated.
     */

    /* Create VDS dcpl */
    if((vds_dcplid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(vds_dcplid, VDS_DATATYPE,
                &VDS_FILL_VALUE) < 0)
        TEST_ERROR

    /* Create VDS dataspace */
    if((vds_sid = H5Screate_simple(RANK, VDS_DIMS,
                    VDS_MAX_DIMS)) < 0)
        TEST_ERROR

    /************************************
     * Create source files and datasets *
     ************************************/

    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    map_start = 0;

    /* All SWMR files need to use the latest file format */
    if((faplid = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    if(H5Pset_libver_bounds(faplid, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    for(i = 0; i < N_SOURCES; i++) {

        /* source dataset dcpl */
        if((src_dcplid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR
        if(H5Pset_chunk(src_dcplid, RANK, PLANES[i]) < 0)
            TEST_ERROR
        if(H5Pset_fill_value(src_dcplid, SOURCE_DATATYPE,
                    &FILL_VALUES[i]) < 0)
            TEST_ERROR

        /* Use a mix of compressed and uncompressed datasets */
        if(0 != i % 2)
            if(H5Pset_deflate(src_dcplid, COMPRESSION_LEVEL) < 0)
                TEST_ERROR

        /* Create source file, dataspace, and dataset */
        if((fid = H5Fcreate(FILE_NAMES[i], H5F_ACC_TRUNC,
                        H5P_DEFAULT, faplid)) < 0)
            TEST_ERROR
        if((src_sid = H5Screate_simple(RANK, DIMS[i],
                        MAX_DIMS[i])) < 0)
            TEST_ERROR
        if((did = H5Dcreate2(fid, SOURCE_DSET_NAME,
                        SOURCE_DATATYPE, src_sid,
                        H5P_DEFAULT, src_dcplid, H5P_DEFAULT)) < 0)
            TEST_ERROR

        /* set up hyperslabs for source and destination datasets */
        start[1] = 0;
        if(H5Sselect_hyperslab(src_sid, H5S_SELECT_SET, start, NULL,
                    MAX_DIMS[i], NULL) < 0)
            TEST_ERROR
        start[1] = map_start;
        if(H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, start, NULL,
                    MAX_DIMS[i], NULL) < 0)
            TEST_ERROR
        map_start += PLANES[i][1];

        /* Add VDS mapping */
        if(H5Pset_virtual(vds_dcplid, vds_sid, FILE_NAMES[i],
                    SOURCE_DSET_PATH, src_sid) < 0)
            TEST_ERROR

        /* close */
        if(H5Sclose(src_sid) < 0)
            TEST_ERROR
        if(H5Pclose(src_dcplid) < 0)
            TEST_ERROR
        if(H5Dclose(did) < 0)
            TEST_ERROR
        if(H5Fclose(fid) < 0)
            TEST_ERROR

    } /* end for */


    /*******************
     * Create VDS file *
     *******************/

    /* file */
    if((fid = H5Fcreate(VDS_FILE_NAME, H5F_ACC_TRUNC,
                    H5P_DEFAULT, faplid)) < 0)
        TEST_ERROR

    /* dataset */
    if((did = H5Dcreate2(fid, VDS_DSET_NAME, VDS_DATATYPE, vds_sid,
                    H5P_DEFAULT, vds_dcplid, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* close */
    if(H5Pclose(faplid) < 0)
        TEST_ERROR
    if(H5Pclose(vds_dcplid) < 0)
        TEST_ERROR
    if(H5Sclose(vds_sid) < 0)
        TEST_ERROR
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    return EXIT_SUCCESS;

error:

    H5E_BEGIN_TRY {
        if(faplid >= 0)
            (void)H5Pclose(faplid);
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
    } H5E_END_TRY

    return EXIT_FAILURE;

} /* end main */

