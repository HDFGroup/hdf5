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
main(int argc, char *argv[])
{
    int file_number     = -1;   /* Source file number               */

    hid_t fid           = -1;   /* HDF5 file ID                     */
    hid_t faplid        = -1;   /* file access property list ID                */
    hid_t did           = -1;   /* dataset ID                       */
    hid_t msid          = -1;   /* memory dataspace ID              */
    hid_t fsid          = -1;   /* file dataspace ID                */

    hsize_t extent[RANK];       /* dataset extents                  */
    hsize_t start[RANK];        /* hyperslab start point            */

    int *buffer         = NULL; /* data buffer                      */
    int value           = -1;   /* value written to datasets        */

    hsize_t n_elements  = 0;    /* number of elements in a plane    */

    hsize_t i;                  /* iterator                         */
    hsize_t j;                  /* iterator                         */


    /******************************
     * Fill a source dataset file *
     ******************************/

    /* The file number is passed on the command line.
     * This is an integer index into the FILE_NAMES array. 
     */
    if(argc != 2) {
        HDfprintf(stderr, "ERROR: Must pass the source file number on the command line.\n");
        return EXIT_FAILURE;
    }

    file_number = HDatoi(argv[1]);
    if(file_number < 0 || file_number >= N_SOURCES)
        TEST_ERROR

    /* Open the source file and dataset */
    /* All SWMR files need to use the latest file format */
    if((faplid = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    if(H5Pset_libver_bounds(faplid, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR
    if((fid = H5Fopen(FILE_NAMES[file_number], H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, faplid)) < 0)
        TEST_ERROR
    if((did = H5Dopen2(fid, SOURCE_DSET_PATH, H5P_DEFAULT)) < 0)
        TEST_ERROR


    /* Create a data buffer that represents a plane */
    n_elements = PLANES[file_number][1] * PLANES[file_number][2];
    if(NULL == (buffer = (int *)HDmalloc(n_elements * sizeof(int))))
        TEST_ERROR

    /* Create the memory dataspace */
    if((msid = H5Screate_simple(RANK, PLANES[file_number], NULL)) < 0)
        TEST_ERROR

    /* Write planes to the dataset */
    for(i = 0; i < N_PLANES_TO_WRITE; i++) {

        unsigned delay;     /* Time interval between plane writes */

        /* Cork the dataset's metadata in the cache */
        if(H5Odisable_mdc_flushes(did) < 0)
            TEST_ERROR

        /* Set the dataset's extent. This is inefficient but that's ok here. */
        extent[0] = i + 1;
        extent[1] = PLANES[file_number][1];
        extent[2] = PLANES[file_number][2];
        if(H5Dset_extent(did, extent) < 0)
            TEST_ERROR

        /* Get the file dataspace */
        if((fsid = H5Dget_space(did)) < 0)
            TEST_ERROR

        /* Each plane is filled with the plane number as a data value. */
        value = (((int)i + 1) * 10) + (int)i;
        for(j = 0; j < n_elements; j++)
           buffer[j] = value;

        /* Set up the hyperslab for writing. */
        start[0] = i;
        start[1] = 0;
        start[2] = 0;
        if(H5Sselect_hyperslab(fsid, H5S_SELECT_SET, start, NULL, PLANES[file_number], NULL) < 0)
            TEST_ERROR

        /* Write the plane to the dataset. */
        if(H5Dwrite(did, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, buffer) < 0)
            TEST_ERROR

        /* Uncork the dataset's metadata from the cache */
        if(H5Oenable_mdc_flushes(did) < 0)
            TEST_ERROR

        /* Wait one second between writing planes */
        delay = HDtime(0) + 1;
        while(HDtime(0) < delay)
            ;

        /* Flush */
        if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            TEST_ERROR

    } /* end for */

    if(H5Pclose(faplid) < 0)
        TEST_ERROR
    if(H5Sclose(msid) < 0)
        TEST_ERROR
    if(H5Sclose(fsid) < 0)
        TEST_ERROR
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR
    HDfree(buffer);

    HDfprintf(stderr, "SWMR writer exited successfully\n");
    return EXIT_SUCCESS;

error:

    H5E_BEGIN_TRY {
        if(fid >= 0)
            (void)H5Fclose(fid);
        if(faplid >= 0)
            (void)H5Pclose(faplid);
        if(did >= 0)
            (void)H5Dclose(did);
        if(msid >= 0)
            (void)H5Sclose(msid);
        if(fsid >= 0)
            (void)H5Sclose(fsid);
        if(buffer != NULL)
            HDfree(buffer);
    } H5E_END_TRY

    HDfprintf(stderr, "ERROR: SWMR writer exited with errors\n");
    return EXIT_FAILURE;

} /* end main */

