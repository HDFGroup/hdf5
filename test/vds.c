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

/*
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              Monday, February 16, 2015
 *
 * Purpose:     Tests datasets with virtual layout.
 */
#include "h5test.h"
#include "H5srcdir.h"

const char *FILENAME[] = {
    "vds_1",
    "vds_2",
    "vds_3",
    "vds_4",
    NULL
};


/*-------------------------------------------------------------------------
 * Function:    test_api
 *
 * Purpose:     Tests API functions related to virtual datasets.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Monday, February 16, 2015
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_api(void)
{
    hid_t       dcpl = -1;      /* Dataset creation property list */
    hid_t       src_space[4] = {-1, -1, -1, -1}; /* Source dataspaces */
    hid_t       vspace[4] = {-1, -1, -1, -1}; /* Virtual dset dataspaces */
    char        *src_file[4] = {"src_file1", "src_file2.", "src_file3..", "src_file4..."}; /* Source file names (different lengths) */
    char        *src_dset[4] = {"src_dset1....", "src_dset2.....", "src_dset3......", "src_dset4......."}; /* Source dataset names (different lengths) */
    hsize_t     dims[2] = {10, 20}; /* Data space current size */
    hsize_t     start[2];       /* Hyperslab start */
    hsize_t     stride[2];      /* Hyperslab stride */
    hsize_t     count[2];       /* Hyperslab count */
    hsize_t     block[2];       /* Hyperslab block */
    size_t      size_out;
    ssize_t     ssize_out;
    hid_t       space_out = -1;
    char        name_out[32];
    unsigned    i;

    TESTING("virtual dataset API functions");

    /* Create DCPL */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /*
     * Test 1: All - all selection
     */
    /* Create source dataspace */
    if((src_space[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select all (should not be necessary, but just to be sure) */
    if(H5Sselect_all(src_space[0]) < 0)
        TEST_ERROR
    if(H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR

    /* Add virtual layout mapping */
    if(H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], src_space[0]) < 0)
        TEST_ERROR

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != 1)
        TEST_ERROR

    /* Test H5Pget_virtual_vspace */
    if((space_out = H5Pget_virtual_vspace(dcpl, 0)) < 0)
        TEST_ERROR
    if(H5Sget_select_type(space_out) != H5S_SEL_ALL)
        TEST_ERROR
    if(H5Sclose(space_out) < 0)
        TEST_ERROR
    space_out = -1;

    /* Test H5Pget_virtual_srcspace */
    if((space_out = H5Pget_virtual_srcspace(dcpl, 0)) < 0)
        TEST_ERROR
    if(H5Sget_select_type(space_out) != H5S_SEL_ALL)
        TEST_ERROR
    if(H5Sclose(space_out) < 0)
        TEST_ERROR
    space_out = -1;

    /* Test H5Pget_virtual_filename */
    if((ssize_out = H5Pget_virtual_filename(dcpl, 0, NULL, 0)) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_file[0]))
        TEST_ERROR
    HDassert((size_t)ssize_out < sizeof(name_out));
    if((ssize_out = H5Pget_virtual_filename(dcpl, 0, name_out, sizeof(name_out))) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_file[0]))
        TEST_ERROR
    if(HDstrncmp(name_out, src_file[0], (size_t)ssize_out + 1) != 0)
        TEST_ERROR

    /* Test H5Pget_virtual_dsetname */
    if((ssize_out = H5Pget_virtual_dsetname(dcpl, 0, NULL, 0)) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_dset[0]))
        TEST_ERROR
    HDassert((size_t)ssize_out < sizeof(name_out));
    if((ssize_out = H5Pget_virtual_dsetname(dcpl, 0, name_out, sizeof(name_out))) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_dset[0]))
        TEST_ERROR
    if(HDstrncmp(name_out, src_dset[0], (size_t)ssize_out + 1) != 0)
        TEST_ERROR

    /* Close */
    if(H5Sclose(src_space[0]) < 0)
        TEST_ERROR
    src_space[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;


    /* Close */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    dcpl = -1;

     PASSED();
     return 0;

error:
    H5E_BEGIN_TRY {
        for(i = 0; i < (sizeof(src_space) / sizeof(src_space[0])); i++) {
            if(src_space[i] >= 0)
                (void)H5Sclose(src_space[i]);
            if(vspace[i] >= 0)
                (void)H5Sclose(vspace[i]);
        } /* end for */
        if(space_out >= 0)
            (void)H5Sclose(space_out);
        if(dcpl >= 0)
            (void)H5Pclose(dcpl);
    } H5E_END_TRY;

     return 1;
} /* end test_api() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests datasets with virtual layout
 *
 * Return:      Success:        exit(0)
 *
 *              Failure:        exit(1)
 *
 * Programmer:  Neil Fortner
 *              Tuesday, February 17, 2015
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char filename[FILENAME_BUF_SIZE];
    hid_t file, grp, fapl;
    int nerrors = 0;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    nerrors += test_api();

    /* Verify symbol table messages are cached */
    //nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0); VDSINC

    if(nerrors)
        goto error;
    printf("All virtual dataset tests passed.\n");
    //h5_cleanup(FILENAME, fapl); VDSINC

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d VIRTUAL DATASET TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}    

