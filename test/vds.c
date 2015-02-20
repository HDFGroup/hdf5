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
#include "H5Dprivate.h" /* For H5D_VIRTUAL_DEF_LIST_SIZE */

typedef enum {
    TEST_API_BASIC,
    TEST_API_COPY_PLIST,
    TEST_API_CREATE_DSET,
    TEST_API_REOPEN_DSET,
    TEST_API_REOPEN_FILE,
    TEST_API_NTESTS
} test_api_config_t;

const char *FILENAME[] = {
    "vds_1",
    NULL
};

#define LIST_DOUBLE_SIZE (H5D_VIRTUAL_DEF_LIST_SIZE + 1)

#define FILENAME_BUF_SIZE       1024


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
/* Helper function to get DCPL for examination depending on config */
static int
test_api_get_ex_dcpl(test_api_config_t config, hid_t fapl, hid_t dcpl,
    hid_t *ex_dcpl, hid_t vspace, char *filename)
{
    hid_t       file = -1;      /* File */
    hid_t       dset = -1;      /* Virtual dataset */

    HDassert((config >= TEST_API_BASIC) && (config < TEST_API_NTESTS));
    HDassert(fapl >= 0);
    HDassert(dcpl >= 0);
    HDassert(ex_dcpl);
    HDassert(*ex_dcpl < 0);
    HDassert(vspace >= 0);
    HDassert(filename);

    /* Take different action depending on test configuration */
    if(config >= TEST_API_CREATE_DSET) {
        /* Create file and dataset */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
        if((dset = H5Dcreate2(file, "vdset", H5T_NATIVE_INT, vspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            TEST_ERROR

        /* Reopen dataset if requested */
        if(config >= TEST_API_REOPEN_DSET) {
            /* Close dataset */
            if(H5Dclose(dset) < 0)
                TEST_ERROR
            dset = -1;

            /* Reopen file if requested */
            if(config == TEST_API_REOPEN_FILE) {
                if(H5Fclose(file) < 0)
                    TEST_ERROR
                file = -1;
                if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
                    TEST_ERROR
            } /* end if */

            /* Open dataset */
            if((dset = H5Dopen2(file, "vdset", H5P_DEFAULT)) < 0)
                TEST_ERROR
        } /* end if */

        /* Get DCPL from dataset */
        if((*ex_dcpl = H5Dget_create_plist(dset)) < 0)
            TEST_ERROR

        /* Close dataset and file */
        if(H5Dclose(dset) < 0)
            TEST_ERROR
        dset = -1;
        if(H5Fclose(file) < 0)
            TEST_ERROR
        file = -1;
    } /* end if */
    else if(config == TEST_API_COPY_PLIST) {
        /* Copy property list */
        if((*ex_dcpl = H5Pcopy(dcpl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        /* Simply copy the id to ex_dcpl and increment the ref count so ex_dcpl
         * can be closed */
        if(H5Iinc_ref(dcpl) < 0)
            TEST_ERROR
        *ex_dcpl = dcpl;
    } /* end else */

    return 0;

error:
    H5E_BEGIN_TRY {
        if(file >= 0)
            (void)H5Fclose(file);
        if(dset >= 0)
            (void)H5Dclose(dset);
    } H5E_END_TRY;

    return -1;
} /* end test_api_get_ex_dcpl() */

/* Main test function */
static int
test_api(test_api_config_t config, hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       dcpl = -1;      /* Dataset creation property list */
    hid_t       ex_dcpl = -1;   /* Temporary dcpl for examination */
    hid_t       srcspace[4] = {-1, -1, -1, -1}; /* Source dataspaces */
    hid_t       vspace[4] = {-1, -1, -1, -1}; /* Virtual dset dataspaces */
    const char  *src_file[4] = {"src_file1", "src_file2.", "src_file3..", "src_file4..."}; /* Source file names (different lengths) */
    const char  *src_dset[4] = {"src_dset1....", "src_dset2.....", "src_dset3......", "src_dset4......."}; /* Source dataset names (different lengths) */
    char        tmp_filename[32];
    char        tmp_dsetname[32];
    hsize_t     dims[2] = {10, 20}; /* Data space current size */
    hsize_t     start[2];       /* Hyperslab start */
    hsize_t     stride[2];      /* Hyperslab stride */
    hsize_t     count[2];       /* Hyperslab count */
    hsize_t     block[2];       /* Hyperslab block */
    size_t      size_out;
    ssize_t     ssize_out;
    hid_t       space_out = -1;
    char        name_out[32];
    hsize_t     blocklist[4];
    unsigned    i;

    switch(config) {
        case TEST_API_BASIC:
            TESTING("virtual dataset API functions")
            break;
        case TEST_API_COPY_PLIST:
            TESTING("virtual dataset API functions with copied plists")
            break;
        case TEST_API_CREATE_DSET:
            TESTING("virtual dataset create")
            break;
        case TEST_API_REOPEN_DSET:
            TESTING("virtual dataset create with reopened dataset")
            break;
        case TEST_API_REOPEN_FILE:
            TESTING("virtual dataset create with reopened file")
            break;
        case TEST_API_NTESTS:
        default:
            TEST_ERROR
    } /* end switch */

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create DCPL */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /*
     * Test 1: All - all selection
     */
    /* Create source dataspace */
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select all (should not be necessary, but just to be sure) */
    if(H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR
    if(H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR

    /* Add virtual layout mapping */
    if(H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Close dataspaces */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != 1)
        TEST_ERROR

    /* Test H5Pget_virtual_vspace */
    if((space_out = H5Pget_virtual_vspace(ex_dcpl, 0)) < 0)
        TEST_ERROR
    if(H5Sget_select_type(space_out) != H5S_SEL_ALL)
        TEST_ERROR
    if(H5Sclose(space_out) < 0)
        TEST_ERROR
    space_out = -1;

    /* Test H5Pget_virtual_srcspace */
    if((space_out = H5Pget_virtual_srcspace(ex_dcpl, 0)) < 0)
        TEST_ERROR
    if(H5Sget_select_type(space_out) != H5S_SEL_ALL)
        TEST_ERROR
    if(H5Sclose(space_out) < 0)
        TEST_ERROR
    space_out = -1;

    /* Test H5Pget_virtual_filename */
    if((ssize_out = H5Pget_virtual_filename(ex_dcpl, 0, NULL, 0)) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_file[0]))
        TEST_ERROR
    HDassert((size_t)ssize_out < sizeof(name_out));
    if((ssize_out = H5Pget_virtual_filename(ex_dcpl, 0, name_out, sizeof(name_out))) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_file[0]))
        TEST_ERROR
    if(HDstrncmp(name_out, src_file[0], (size_t)ssize_out + 1) != 0)
        TEST_ERROR

    /* Test H5Pget_virtual_dsetname */
    if((ssize_out = H5Pget_virtual_dsetname(ex_dcpl, 0, NULL, 0)) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_dset[0]))
        TEST_ERROR
    HDassert((size_t)ssize_out < sizeof(name_out));
    if((ssize_out = H5Pget_virtual_dsetname(ex_dcpl, 0, name_out, sizeof(name_out))) < 0)
        TEST_ERROR
    if((size_t)ssize_out != HDstrlen(src_dset[0]))
        TEST_ERROR
    if(HDstrncmp(name_out, src_dset[0], (size_t)ssize_out + 1) != 0)
        TEST_ERROR

    /* Close */
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /*
     * Test X: Enough Selections to trigger doubling of mapping list
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create source dataspace */
    dims[0] = 1;
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select all in source space (should not be necessary, but just to be sure) */
    if(H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    dims[0] = LIST_DOUBLE_SIZE;
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Init hyperslab values */
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = 20;

    /* Build virtual layout */
    for(i = 0; i < LIST_DOUBLE_SIZE; i++) {
        /* Select row in virual dataspace */
        start[0] = (hsize_t)i;
        if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, block) < 0)
            TEST_ERROR

        /* Create file and dataset names */
        (void)HDsnprintf(tmp_filename, sizeof(tmp_filename), "src_file%u", i);
        tmp_filename[sizeof(tmp_filename) - 1] = '\0';
        (void)HDsnprintf(tmp_dsetname, sizeof(tmp_dsetname), "src_dset%u", i);
        tmp_dsetname[sizeof(tmp_dsetname) - 1] = '\0';

        /* Add virtual layout mapping */
        if(H5Pset_virtual(dcpl, vspace[0], tmp_filename, tmp_dsetname, srcspace[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Close dataspaces */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != LIST_DOUBLE_SIZE)
        TEST_ERROR

    /* Verify virtual layout */
    for(i = 0; i < LIST_DOUBLE_SIZE; i++) {
        /* Test H5Pget_virtual_vspace */
        if((space_out = H5Pget_virtual_vspace(ex_dcpl, i)) < 0)
            TEST_ERROR
        if(H5Sget_select_type(space_out) != H5S_SEL_HYPERSLABS)
            TEST_ERROR
        if((ssize_out = H5Sget_select_hyper_nblocks(space_out)) < 0)
            TEST_ERROR
        if(ssize_out != 1)
            TEST_ERROR
        if(H5Sget_select_hyper_blocklist(space_out, 0, 1, blocklist) < 0)
            TEST_ERROR
        if(blocklist[0] != (hsize_t)i)
            TEST_ERROR
        if(blocklist[1] != 0)
            TEST_ERROR
        if(blocklist[2] != (hsize_t)i)
            TEST_ERROR
        if(blocklist[3] != 19)
            TEST_ERROR
        if(H5Sclose(space_out) < 0)
            TEST_ERROR
        space_out = -1;

        /* Test H5Pget_virtual_srcspace */
        if((space_out = H5Pget_virtual_srcspace(ex_dcpl, i)) < 0)
            TEST_ERROR
        if(H5Sget_select_type(space_out) != H5S_SEL_ALL)
            TEST_ERROR
        if(H5Sclose(space_out) < 0)
            TEST_ERROR
        space_out = -1;

        /* Test H5Pget_virtual_filename */
        (void)HDsnprintf(tmp_filename, sizeof(tmp_filename), "src_file%u", i);
        tmp_filename[sizeof(tmp_filename) - 1] = '\0';
        if((ssize_out = H5Pget_virtual_filename(ex_dcpl, i, NULL, 0)) < 0)
            TEST_ERROR
        if((size_t)ssize_out != HDstrlen(tmp_filename))
            TEST_ERROR
        HDassert((size_t)ssize_out < sizeof(name_out));
        if((ssize_out = H5Pget_virtual_filename(ex_dcpl, i, name_out, sizeof(name_out))) < 0)
            TEST_ERROR
        if((size_t)ssize_out != HDstrlen(tmp_filename))
            TEST_ERROR
        if(HDstrncmp(name_out, tmp_filename, (size_t)ssize_out + 1) != 0)
            TEST_ERROR

        /* Test H5Pget_virtual_dsetname */
        (void)HDsnprintf(tmp_dsetname, sizeof(tmp_dsetname), "src_dset%u", i);
        tmp_dsetname[sizeof(tmp_dsetname) - 1] = '\0';
        if((ssize_out = H5Pget_virtual_dsetname(ex_dcpl, i, NULL, 0)) < 0)
            TEST_ERROR
        if((size_t)ssize_out != HDstrlen(tmp_dsetname))
            TEST_ERROR
        HDassert((size_t)ssize_out < sizeof(name_out));
        if((ssize_out = H5Pget_virtual_dsetname(ex_dcpl, i, name_out, sizeof(name_out))) < 0)
            TEST_ERROR
        if((size_t)ssize_out != HDstrlen(tmp_dsetname))
            TEST_ERROR
        if(HDstrncmp(name_out, tmp_dsetname, (size_t)ssize_out + 1) != 0)
            TEST_ERROR
    } /* end if */

    /* Close */
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /* Close */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    dcpl = -1;

     PASSED();
     return 0;

error:
    H5E_BEGIN_TRY {
        for(i = 0; i < (sizeof(srcspace) / sizeof(srcspace[0])); i++) {
            if(srcspace[i] >= 0)
                (void)H5Sclose(srcspace[i]);
            if(vspace[i] >= 0)
                (void)H5Sclose(vspace[i]);
        } /* end for */
        if(space_out >= 0)
            (void)H5Sclose(space_out);
        if(dcpl >= 0)
            (void)H5Pclose(dcpl);
        if(ex_dcpl >= 0)
            (void)H5Pclose(ex_dcpl);
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
    hid_t fapl;
    int test_api_config;
    int nerrors = 0;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    for(test_api_config = (int)TEST_API_BASIC; test_api_config < (int)TEST_API_NTESTS; test_api_config++)
        nerrors += test_api((test_api_config_t)test_api_config, fapl);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    printf("All virtual dataset tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d VIRTUAL DATASET TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}

