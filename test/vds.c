/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
    "vds_2",
    NULL
};

/* I/O test config flags */
#define TEST_IO_CLOSE_SRC       0x01u
#define TEST_IO_DIFFERENT_FILE  0x02u
#define TEST_IO_NTESTS          0x04u
//VDSINC add close source, virtual file

#define LIST_DOUBLE_SIZE (H5D_VIRTUAL_DEF_LIST_SIZE + 1)

#define FILENAME_BUF_SIZE       1024


/*-------------------------------------------------------------------------
 * Function:    vds_select_equal
 *
 * Purpose:     Helper function to check if the selections in the two
 *              provided dataspaces are the same.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Monday, March 2, 2015
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static htri_t
vds_select_equal(hid_t space1, hid_t space2)
{
    H5S_sel_type    type1;
    H5S_sel_type    type2;
    hsize_t         *buf1 = NULL;
    hsize_t         *buf2 = NULL;
    size_t          i;
    htri_t          ret_value = TRUE;

    /* Get and compare selection types */
    if((type1 = H5Sget_select_type(space1)) < 0)
        TEST_ERROR
    if((type2 = H5Sget_select_type(space2)) < 0)
        TEST_ERROR
    if(type1 != type2)
        return FALSE;

    /* Check selection type */
    switch(type1) {
        case H5S_SEL_NONE:
        case H5S_SEL_ALL:
            break;

        case H5S_SEL_POINTS:
            {
                int         rank1;
                int         rank2;
                hssize_t    npoints1;
                hssize_t    npoints2;

                /* Get and compare rank */
                if((rank1 = H5Sget_simple_extent_ndims(space1)) < 0)
                    TEST_ERROR
                if((rank2 = H5Sget_simple_extent_ndims(space2)) < 0)
                    TEST_ERROR
                if(rank1 != rank2)
                    return FALSE;

                /* Get and compare number of points */
                if((npoints1 = H5Sget_select_elem_npoints(space1)) < 0)
                    TEST_ERROR
                if((npoints2 = H5Sget_select_elem_npoints(space2)) < 0)
                    TEST_ERROR
                if(npoints1 != npoints2)
                    return FALSE;

                /* Allocate point lists.  Do not return directly afer
                 * allocating, to make sure buffers are freed. */
                if(NULL == (buf1 = (hsize_t *)HDmalloc((size_t)rank1 * (size_t)npoints1 * sizeof(hsize_t))))
                    TEST_ERROR
                if(NULL == (buf2 = (hsize_t *)HDmalloc((size_t)rank1 * (size_t)npoints1 * sizeof(hsize_t))))
                    TEST_ERROR

                /* Get and compare point lists */
                if(H5Sget_select_elem_pointlist(space1, (hsize_t)0, (hsize_t)npoints1, buf1) < 0)
                    TEST_ERROR
                if(H5Sget_select_elem_pointlist(space2, (hsize_t)0, (hsize_t)npoints1, buf2) < 0)
                    TEST_ERROR
                for(i = 0; i < ((size_t)rank1 * (size_t)npoints1); i++)
                    if(buf1[i] != buf2[i]) {
                        ret_value = FALSE;
                        break;
                    } /* end if */

                /* Free buffers */
                HDfree(buf1);
                buf1 = NULL;
                HDfree(buf2);
                buf2 = NULL;
            } /* end block */

            break;

        case H5S_SEL_HYPERSLABS:
            {
                int         rank1;
                int         rank2;
                hssize_t    nblocks1;
                hssize_t    nblocks2;

                /* Get and compare rank */
                if((rank1 = H5Sget_simple_extent_ndims(space1)) < 0)
                    TEST_ERROR
                if((rank2 = H5Sget_simple_extent_ndims(space2)) < 0)
                    TEST_ERROR
                if(rank1 != rank2)
                    return FALSE;

                /* Get and compare number of blocks */
                if((nblocks1 = H5Sget_select_hyper_nblocks(space1)) < 0)
                    TEST_ERROR
                if((nblocks2 = H5Sget_select_hyper_nblocks(space2)) < 0)
                    TEST_ERROR
                if(nblocks1 != nblocks2)
                    return FALSE;

                /* Allocate block lists.  Do not return directly afer
                 * allocating, to make sure buffers are freed. */
                if(NULL == (buf1 = (hsize_t *)HDmalloc((size_t)2 * (size_t)rank1 * (size_t)nblocks1 * sizeof(*buf1))))
                    TEST_ERROR
                if(NULL == (buf2 = (hsize_t *)HDmalloc((size_t)2 * (size_t)rank1 * (size_t)nblocks1 * sizeof(*buf2))))
                    TEST_ERROR

                /* Get and compare block lists */
                if(H5Sget_select_hyper_blocklist(space1, (hsize_t)0, (hsize_t)nblocks1, buf1) < 0)
                    TEST_ERROR
                if(H5Sget_select_hyper_blocklist(space2, (hsize_t)0, (hsize_t)nblocks1, buf2) < 0)
                    TEST_ERROR
                for(i = 0; i < ((size_t)2 * (size_t)rank1 * (size_t)nblocks1); i++)
                    if(buf1[i] != buf2[i]) {
                        ret_value = FALSE;
                        break;
                    } /* end if */

                /* Free buffers */
                HDfree(buf1);
                buf1 = NULL;
                HDfree(buf2);
                buf2 = NULL;
            } /* end block */

            break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
        default:
            TEST_ERROR
    } /* end switch */

    return ret_value;

error:
    if(buf1)
        HDfree(buf1);
    if(buf2)
        HDfree(buf2);

    return -1;
} /* end vds_select_equal() */


/*-------------------------------------------------------------------------
 * Function:    vds_check_mapping
 *
 * Purpose:     Helper function to check if the ith virtual mapping in the
 *              provided dcpl is the same as that described by the other
 *              parameters.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Monday, March 2, 2015
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
vds_check_mapping(hid_t dcpl, size_t i, hid_t vspace, hid_t srcspace,
    const char *filename, const char *dsetname)
{
    hid_t       space_out = -1;
    char        name_out[32];
    htri_t      tri_ret;
    ssize_t     str_len;

    HDassert(dcpl >= 0);
    HDassert(vspace >= 0);
    HDassert(srcspace >= 0);
    HDassert(filename);
    HDassert(dsetname);

    /* Check vspace */
    if((space_out = H5Pget_virtual_vspace(dcpl, i)) < 0)
        TEST_ERROR
    if((tri_ret = H5Sextent_equal(space_out, vspace)) < 0)
        TEST_ERROR
    if(!tri_ret)
        TEST_ERROR
    if((tri_ret = vds_select_equal(space_out, vspace)) < 0)
        TEST_ERROR
    if(!tri_ret)
        TEST_ERROR
    if(H5Sclose(space_out) < 0)
        TEST_ERROR
    space_out = -1;

    /* Check srcspace */
    if((space_out = H5Pget_virtual_srcspace(dcpl, i)) < 0)
        TEST_ERROR
    if((tri_ret = vds_select_equal(space_out, srcspace)) < 0)
        TEST_ERROR
    if(!tri_ret)
        TEST_ERROR
    if(H5Sclose(space_out) < 0)
        TEST_ERROR
    space_out = -1;

    /* Check filename */
    if((str_len = H5Pget_virtual_filename(dcpl, i, NULL, (size_t)0)) < 0)
        TEST_ERROR
    if((size_t)str_len != HDstrlen(filename))
        TEST_ERROR
    HDassert((size_t)str_len < sizeof(name_out));
    if((str_len = H5Pget_virtual_filename(dcpl, i, name_out, sizeof(name_out))) < 0)
        TEST_ERROR
    if((size_t)str_len != HDstrlen(filename))
        TEST_ERROR
    if(HDstrncmp(name_out, filename, (size_t)str_len + 1) != 0)
        TEST_ERROR

    /* Check dsetname */
    if((str_len = H5Pget_virtual_dsetname(dcpl, i, NULL, (size_t)0)) < 0)
        TEST_ERROR
    if((size_t)str_len != HDstrlen(dsetname))
        TEST_ERROR
    HDassert((size_t)str_len < sizeof(name_out));
    if((str_len = H5Pget_virtual_dsetname(dcpl, i, name_out, sizeof(name_out))) < 0)
        TEST_ERROR
    if((size_t)str_len != HDstrlen(dsetname))
        TEST_ERROR
    if(HDstrncmp(name_out, dsetname, (size_t)str_len + 1) != 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        if(space_out >= 0)
            (void)H5Sclose(space_out);
    } H5E_END_TRY

    return -1;
} /* end vds_check_mapping() */


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
    hid_t       vspace[LIST_DOUBLE_SIZE]; /* Virtual dset dataspaces */
    const char  *src_file[4] = {"src_file1", "src_file2.", "src_file3..", "src_file4..."}; /* Source file names (different lengths) */
    const char  *src_dset[4] = {"src_dset1....", "src_dset2.....", "src_dset3......", "src_dset4......."}; /* Source dataset names (different lengths) */
    char        tmp_filename[32];
    char        tmp_dsetname[32];
    hsize_t     dims[2] = {10, 20}; /* Data space current size */
    hsize_t     start[2];       /* Hyperslab start */
    hsize_t     stride[2];      /* Hyperslab stride */
    hsize_t     count[2];       /* Hyperslab count */
    hsize_t     block[2];       /* Hyperslab block */
    hsize_t     coord[10];      /* Point selection array */
    size_t      size_out;
    unsigned    i;

    /* Initialize vspace */
    for(i = 0; i < (unsigned)(sizeof(vspace) / sizeof(vspace[0])); i++)
        vspace[i] = -1;

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

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != (size_t)1)
        TEST_ERROR

    /* Check that the mapping in the DCPL is correct */
    if(vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR

    /* Close */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /*
     * Test 2: Hyper - hyper selection
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create source dataspace */
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select regular hyperslab in source space */
    start[0] = 2;
    start[1] = 1;
    stride[0] = 3;
    stride[1] = 5;
    count[0] = 2;
    count[1] = 3;
    block[0] = 2;
    block[1] = 4;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR

    /* Select composite hyperslab in virtual space */
    count[0] = 1;
    count[1] = 1;
    block[0] = 5;
    block[1] = 6;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR
    start[0] = 7;
    start[1] = 0;
    block[0] = 1;
    block[1] = 18;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, NULL, count, block) < 0)
        TEST_ERROR

    /* Add virtual layout mapping */
    if(H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != (size_t)1)
        TEST_ERROR

    /* Check that the mapping in the DCPL is correct */
    if(vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR

    /* Close */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /*
     * Test 3: Point - point selection
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create source dataspace */
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select points in source space */
    coord[0] = 5;
    coord[1] = 15;
    coord[2] = 7;
    coord[3] = 19;
    coord[4] = 8;
    coord[5] = 0;
    coord[6] = 2;
    coord[7] = 14;
    coord[8] = 8;
    coord[9] = 18;
    if(H5Sselect_elements(srcspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR

    /* Select points in virtual space */
    coord[0] = 3;
    coord[1] = 12;
    coord[2] = 7;
    coord[3] = 11;
    coord[4] = 4;
    coord[5] = 9;
    coord[6] = 7;
    coord[7] = 11;
    coord[8] = 5;
    coord[9] = 5;
    if(H5Sselect_elements(vspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR

    /* Add virtual layout mapping */
    if(H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != (size_t)1)
        TEST_ERROR

    /* Check that the mapping in the DCPL is correct */
    if(vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR

    /* Close */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /*
     * Test 4: Point - hyper selection
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create source dataspace */
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspace */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select hyperslab in source space */
    start[0] = 2;
    start[1] = 7;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = 5;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR

    /* Select points in virtual space */
    coord[0] = 1;
    coord[1] = 1;
    coord[2] = 4;
    coord[3] = 17;
    coord[4] = 3;
    coord[5] = 9;
    coord[6] = 5;
    coord[7] = 13;
    coord[8] = 7;
    coord[9] = 16;
    if(H5Sselect_elements(vspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR

    /* Add virtual layout mapping */
    if(H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != (size_t)1)
        TEST_ERROR

    /* Check that the mapping in the DCPL is correct */
    if(vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR

    /* Close */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /*
     * Test 5: All previous mappings together
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create dataspaces */
    for(i = 0; i < 4; i++) {
        /* Create source dataspace */
        if((srcspace[i] = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR

        /* Create virtual dataspace */
        if((vspace[i] = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR
    } /* end for */

    /* Select all (should not be necessary, but just to be sure) */
    if(H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR
    if(H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR

    /* Select regular hyperslab in source space */
    start[0] = 2;
    start[1] = 1;
    stride[0] = 3;
    stride[1] = 5;
    count[0] = 2;
    count[1] = 3;
    block[0] = 2;
    block[1] = 4;
    if(H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR

    /* Select composite hyperslab in virtual space */
    count[0] = 1;
    count[1] = 1;
    block[0] = 5;
    block[1] = 6;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR
    start[0] = 7;
    start[1] = 0;
    block[0] = 1;
    block[1] = 18;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, NULL, count, block) < 0)
        TEST_ERROR

    /* Select points in source space */
    coord[0] = 5;
    coord[1] = 15;
    coord[2] = 7;
    coord[3] = 19;
    coord[4] = 8;
    coord[5] = 0;
    coord[6] = 2;
    coord[7] = 14;
    coord[8] = 8;
    coord[9] = 18;
    if(H5Sselect_elements(srcspace[2], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR

    /* Select points in virtual space */
    coord[0] = 3;
    coord[1] = 12;
    coord[2] = 7;
    coord[3] = 11;
    coord[4] = 4;
    coord[5] = 9;
    coord[6] = 7;
    coord[7] = 11;
    coord[8] = 5;
    coord[9] = 5;
    if(H5Sselect_elements(vspace[2], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR

    /* Select hyperslab in source space */
    start[0] = 2;
    start[1] = 7;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = 5;
    if(H5Sselect_hyperslab(srcspace[3], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR

    /* Select points in virtual space */
    coord[0] = 1;
    coord[1] = 1;
    coord[2] = 4;
    coord[3] = 17;
    coord[4] = 3;
    coord[5] = 9;
    coord[6] = 5;
    coord[7] = 13;
    coord[8] = 7;
    coord[9] = 16;
    if(H5Sselect_elements(vspace[3], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    for(i = 0; i < 4; i++)
        if(H5Pset_virtual(dcpl, vspace[i], src_file[i], src_dset[i], srcspace[i]) < 0)
            TEST_ERROR

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != (size_t)4)
        TEST_ERROR

    /* Check that the mappings in the DCPL are correct */
    for(i = 0; i < 4; i++)
        if(vds_check_mapping(ex_dcpl, (size_t)i, vspace[i], srcspace[i], src_file[i], src_dset[i]) < 0)
            TEST_ERROR

    /* Close */
    for(i = 0; i < 4; i++) {
        if(H5Sclose(srcspace[i]) < 0)
            TEST_ERROR
        srcspace[i] = -1;
        if(H5Sclose(vspace[i]) < 0)
            TEST_ERROR
        vspace[i] = -1;
    } /* end for */
    if(H5Pclose(ex_dcpl) < 0)
        TEST_ERROR
    ex_dcpl = -1;


    /*
     * Test 6: Enough Selections to trigger doubling of mapping list
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

    /* Init virtual space extent */
    dims[0] = LIST_DOUBLE_SIZE;

    /* Init hyperslab values */
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = 20;

    /* Build virtual layout */
    for(i = 0; i < LIST_DOUBLE_SIZE; i++) {
        /* Create virtual dataspace */
        if((vspace[i] = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR

        /* Select row in virual dataspace */
        start[0] = (hsize_t)i;
        if(H5Sselect_hyperslab(vspace[i], H5S_SELECT_SET, start, NULL, count, block) < 0)
            TEST_ERROR

        /* Create file and dataset names */
        (void)HDsnprintf(tmp_filename, sizeof(tmp_filename), "src_file%u", i);
        tmp_filename[sizeof(tmp_filename) - 1] = '\0';
        (void)HDsnprintf(tmp_dsetname, sizeof(tmp_dsetname), "src_dset%u", i);
        tmp_dsetname[sizeof(tmp_dsetname) - 1] = '\0';

        /* Add virtual layout mapping */
        if(H5Pset_virtual(dcpl, vspace[i], tmp_filename, tmp_dsetname, srcspace[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Get examination DCPL */
    if(test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename) < 0)
        TEST_ERROR

    /* Test H5Pget_virtual_count */
    if(H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR
    if(size_out != (size_t)LIST_DOUBLE_SIZE)
        TEST_ERROR

    /* Verify virtual layout */
    for(i = 0; i < LIST_DOUBLE_SIZE; i++) {
        /* Generate source file name */
        (void)HDsnprintf(tmp_filename, sizeof(tmp_filename), "src_file%u", i);
        tmp_filename[sizeof(tmp_filename) - 1] = '\0';

        /* Generate source dset name */
        (void)HDsnprintf(tmp_dsetname, sizeof(tmp_dsetname), "src_dset%u", i);
        tmp_dsetname[sizeof(tmp_dsetname) - 1] = '\0';

        /* Check that the mapping in the DCPL is correct */
        if(vds_check_mapping(ex_dcpl, (size_t)i, vspace[i], srcspace[0], tmp_filename, tmp_dsetname) < 0)
            TEST_ERROR
    } /* end if */

    /* Close */
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    for(i = 0; i < LIST_DOUBLE_SIZE; i++) {
        if(H5Sclose(vspace[i]) < 0)
            TEST_ERROR
        vspace[i] = -1;
    } /* end for */
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
        } /* end for */
        for(i = 0; i < (sizeof(vspace) / sizeof(vspace[0])); i++) {
            if(vspace[i] >= 0)
                (void)H5Sclose(vspace[i]);
        } /* end for */
        if(dcpl >= 0)
            (void)H5Pclose(dcpl);
        if(ex_dcpl >= 0)
            (void)H5Pclose(ex_dcpl);
    } H5E_END_TRY;

     return 1;
} /* end test_api() */


/*-------------------------------------------------------------------------
 * Function:    test_basic_io
 *
 * Purpose:     Tests VDS I/O without unlimited selections or
 *              pattern-matching file/dataset strings
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Tuesday, March 3, 2015
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_basic_io(unsigned config, hid_t fapl)
{
    char        srcfilename[FILENAME_BUF_SIZE];
    char        vfilename[FILENAME_BUF_SIZE];
    hid_t       srcfile[4] = {-1, -1, -1, -1}; /* Files with source dsets */
    hid_t       vfile = -1;     /* File with virtual dset */
    hid_t       dcpl = -1;      /* Dataset creation property list */
    hid_t       srcspace[4] = {-1, -1, -1, -1}; /* Source dataspaces */
    hid_t       vspace[4] = {-1, -1, -1, -1}; /* Virtual dset dataspaces */
    hid_t       memspace = -1;   /* Memory dataspace */
    hid_t       srcdset[4] = {-1, -1, -1, -1}; /* Source datsets */
    hid_t       vdset = -1;     /* Virtual dataset */
    hsize_t     dims[2] = {10, 26}; /* Data space current size */
    hsize_t     start[4];       /* Hyperslab start */
    hsize_t     stride[4];      /* Hyperslab stride */
    hsize_t     count[4];       /* Hyperslab count */
    hsize_t     block[4];       /* Hyperslab block */
    hsize_t     coord[10];      /* Point selection array */
    int         buf[10][26];    /* Write and expected read buffer */
    int         rbuf[10][26];   /* Read buffer */
    int         evbuf[10][26];  /* Expected VDS "buffer" */
    int         erbuf[10][26];  /* Expected read buffer */
    int         i, j;

    TESTING("basic virtual dataset I/O")

    h5_fixname(FILENAME[0], fapl, vfilename, sizeof vfilename);
    h5_fixname(FILENAME[1], fapl, srcfilename, sizeof srcfilename);

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
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset", srcspace[0]) < 0)
        TEST_ERROR

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source dataset */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source dataset */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Close srcdset and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Read data through virtual dataset */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != buf[i][j])
                TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Reopen srcdset and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Read data directly from source dataset */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != buf[i][j])
                TEST_ERROR

    /* Close */
    if(H5Dclose(srcdset[0]) < 0)
        TEST_ERROR
    srcdset[0] = -1;
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(srcfile[0]) < 0)
        TEST_ERROR
    srcfile[0] = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;


    /*
     * Test 2: 2 Source datasets, hyperslab virtual mappings
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create source dataspace */
    dims[1] = 13;
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if(H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR
    start[1] = 13;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset2", srcspace[0]) < 0)
        TEST_ERROR

    /* Reset dims */
    dims[1] = 26;

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source datasets */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Close srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Read data through virtual dataset */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != buf[i][j])
                TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Reopen srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
        if((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Read data directly from source datasets */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR
    if(H5Dread(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != buf[i][j])
                TEST_ERROR

    /* Close */
    if(H5Dclose(srcdset[0]) < 0)
        TEST_ERROR
    srcdset[0] = -1;
    if(H5Dclose(srcdset[1]) < 0)
        TEST_ERROR
    srcdset[1] = -1;
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(srcfile[0]) < 0)
        TEST_ERROR
    srcfile[0] = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;


    /*
     * Test 3: 2 Source datasets, hyperslab virtual mappings on one mapping at a
     * time
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create source dataspace */
    dims[1] = 13;
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if(H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR
    start[1] = 13;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset2", srcspace[0]) < 0)
        TEST_ERROR

    /* Reset dims */
    dims[1] = 26;

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source datasets */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Close srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Read first source dataset through virtual dataset */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(vdset, H5T_NATIVE_INT, vspace[0], vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != (j < (int)(sizeof(buf[0]) / sizeof(buf[0][0]) / 2)
                    ? buf[i][j] : 0))
                TEST_ERROR

    /* Read second source dataset through virtual dataset */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(vdset, H5T_NATIVE_INT, vspace[1], vspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != (j < (int)(sizeof(buf[0]) / sizeof(buf[0][0]) / 2)
                    ? 0 : buf[i][j]))
                TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write first source dataset through virtual dataset */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, vspace[0], vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second source dataset through virtual dataset */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, vspace[1], vspace[1], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Reopen srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
        if((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Read data directly from source datasets */
    HDmemset(rbuf[0], 0, sizeof(rbuf));
    if(H5Dread(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR
    if(H5Dread(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != (j < (int)(sizeof(buf[0]) / sizeof(buf[0][0]) / 2)
                    ? (buf[i][j] - (int)(sizeof(buf) / sizeof(buf[0][0])))
                    : buf[i][j]))
                TEST_ERROR

    /* Close */
    if(H5Dclose(srcdset[0]) < 0)
        TEST_ERROR
    srcdset[0] = -1;
    if(H5Dclose(srcdset[1]) < 0)
        TEST_ERROR
    srcdset[1] = -1;
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(srcfile[0]) < 0)
        TEST_ERROR
    srcfile[0] = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;


    /*
     * Test 4: 2 Source datasets, hyperslab virtual mappings and selections
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create source dataspaces */
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((srcspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Select hyperslabs in source space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = 13;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    start[1] = 13;
    if(H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = 26;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    start[0] = 5;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset2", srcspace[1]) < 0)
        TEST_ERROR

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source datasets */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[1], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    /* Write first dataset */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update evbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            evbuf[i][j] = buf[2 * i][j];
        for(/* j = 13 */; j < 26; j++)
            evbuf[i][j] = buf[2 * i + 1][j - 13];
    } /* end for */

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second dataset */
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update evbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            evbuf[i + 5][j] = buf[2 * i][j + 13];
        for(/* j = 13 */; j < 26; j++)
            evbuf[i + 5][j] = buf[2 * i + 1][j];
    } /* end for */

    /* Close srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Read data through virtual dataset by hyperslab */
    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Read first slice */
    if(H5Dread(vdset, H5T_NATIVE_INT, vspace[0], srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            erbuf[i][j] = evbuf[2 * i][j];
        for(/* j = 13 */; j < 26; j++)
            erbuf[i][j] = evbuf[2 * i + 1][j - 13];
    } /* end for */

    /* Read second slice */
    if(H5Dread(vdset, H5T_NATIVE_INT, vspace[1], srcspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            erbuf[i + 5][j] = evbuf[2 * i][j + 13];
        for(/* j = 13 */; j < 26; j++)
            erbuf[i + 5][j] = evbuf[2 * i + 1][j];
    } /* end for */

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    /* Write first slice */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, vspace[0], srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update evbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            evbuf[2 * i][j] = buf[i][j];
        for(/* j = 13 */; j < 26; j++)
            evbuf[2 * i + 1][j - 13] = buf[i][j];
    } /* end for */

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second slice */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, vspace[1], srcspace[1], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update evbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            evbuf[2 * i][j + 13] = buf[i + 5][j];
        for(/* j = 13 */; j < 26; j++)
            evbuf[2 * i + 1][j] = buf[i + 5][j];
    } /* end for */

    /* Reopen srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
        if((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Read data directly from source datasets */
    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Read first dataset */
    if(H5Dread(srcdset[0], H5T_NATIVE_INT, srcspace[0], srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            erbuf[2 * i][j] = evbuf[i][j];
        for(/* j = 13 */; j < 26; j++)
            erbuf[2 * i + 1][j - 13] = evbuf[i][j];
    } /* end for */

    /* Read second dataset */
    if(H5Dread(srcdset[1], H5T_NATIVE_INT, srcspace[1], srcspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++) {
        for(j = 0; j < 13; j++)
            erbuf[2 * i][j + 13] = evbuf[i + 5][j];
        for(/* j = 13 */; j < 26; j++)
            erbuf[2 * i + 1][j] = evbuf[i + 5][j];
    } /* end for */

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close */
    if(H5Dclose(srcdset[0]) < 0)
        TEST_ERROR
    srcdset[0] = -1;
    if(H5Dclose(srcdset[1]) < 0)
        TEST_ERROR
    srcdset[1] = -1;
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(srcfile[0]) < 0)
        TEST_ERROR
    srcfile[0] = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(srcspace[1]) < 0)
        TEST_ERROR
    srcspace[1] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;


    /*
     * Test 5: 2 Source datasets, checkerboard/stripe pattern to trigger
     * sequence list refresh internally
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create memory dataspace */
    if((memspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    dims[1] = 52;
    if((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create source dataspace and file space for second operation (srcspace[1])
     */
    if((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((srcspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Reset dims */
    dims[1] = 26;

    /* Select hyperslabs (stripe) in source space and file space for second
     * operation (srcspace[1]) */
    start[0] = 0;
    start[1] = 0;
    stride[0] = 1;
    stride[1] = 2;
    count[0] = 1;
    count[1] = 26;
    block[0] = 10;
    block[1] = 1;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR
    start[1] = 1;
    if(H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR

    /* Select hyperslabs (checkerboard) in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    stride[0] = 2;
    stride[1] = 2;
    count[0] = 5;
    count[1] = 26;
    block[0] = 1;
    block[1] = 1;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR
    start[0] = 1;
    start[1] = 1;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR
    start[0] = 0;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR
    start[0] = 1;
    start[1] = 0;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset2", srcspace[0]) < 0)
        TEST_ERROR

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source datasets */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    /* Write first dataset */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i += 2)
        for(j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second dataset */
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 1; i < 10; i += 2)
        for(j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Read data through virtual dataset by hyperslab */
    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Read first stripe pattern */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Read second stripe pattern */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, srcspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i += 2)
        for(j = 0; j < 26; j++) {
            erbuf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));
            erbuf[i + 1][j] -= (int)(sizeof(buf) / sizeof(buf[0][0]));
        } /* end for */

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    /* Write first slice */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i += 2)
        for(j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second slice */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, memspace, srcspace[1], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 1; i < 10; i += 2)
        for(j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Reopen srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
        if((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Read data directly from source datasets */
    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Read first dataset */
    if(H5Dread(srcdset[0], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i += 2)
        for(j = 0; j < 26; j++) {
            erbuf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));
            erbuf[i + 1][j] -= (int)(sizeof(buf) / sizeof(buf[0][0]));
        } /* end for */

    /* Read second dataset */
    if(H5Dread(srcdset[1], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close */
    if(H5Dclose(srcdset[0]) < 0)
        TEST_ERROR
    srcdset[0] = -1;
    if(H5Dclose(srcdset[1]) < 0)
        TEST_ERROR
    srcdset[1] = -1;
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(srcfile[0]) < 0)
        TEST_ERROR
    srcfile[0] = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(srcspace[1]) < 0)
        TEST_ERROR
    srcspace[1] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;
    if(H5Sclose(memspace) < 0)
        TEST_ERROR
    memspace = -1;


    /*
     * Test 6: 1 Source dataset, two mappings, 4 dimensional virtual dataset
     * and 3 dimensional source dataset
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create memory dataspace */
    if((memspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    dims[0] = 3;
    dims[1] = 3;
    dims[2] = 3;
    dims[3] = 3;
    if((vspace[0] = H5Screate_simple(4, dims, NULL)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(4, dims, NULL)) < 0)
        TEST_ERROR

    /* Create source dataspaces */
    dims[0] = 2;
    dims[1] = 4;
    dims[2] = 4;
    if((srcspace[0] = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR
    if((srcspace[1] = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR

    /* Reset dims */
    dims[0] = 10;
    dims[1] = 26;

    /* Select hyperslabs (stripes) in source spaces */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    stride[0] = 1;
    stride[1] = 2;
    stride[2] = 1;
    count[0] = 1;
    count[1] = 2;
    count[2] = 1;
    block[0] = 2;
    block[1] = 1;
    block[2] = 4;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR
    start[1] = 1;
    if(H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR

    /* Select hyperslabs (corners) in first virtual space */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    start[3] = 0;
    stride[0] = 2;
    stride[1] = 2;
    stride[2] = 2;
    stride[3] = 2;
    count[0] = 2;
    count[1] = 2;
    count[2] = 2;
    count[3] = 2;
    block[0] = 1;
    block[1] = 1;
    block[2] = 1;
    block[3] = 1;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR

    /* Select hyperslabs ("+" pattern) in second virtual space */
    start[0] = 1;
    start[1] = 1;
    start[2] = 0;
    start[3] = 0;
    stride[0] = 2;
    stride[1] = 2;
    stride[2] = 2;
    stride[3] = 2;
    count[0] = 1;
    count[1] = 1;
    count[2] = 2;
    count[3] = 2;
    block[0] = 1;
    block[1] = 1;
    block[2] = 1;
    block[3] = 1;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR
    start[1] = 0;
    start[2] = 1;
    count[1] = 2;
    count[2] = 1;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR
    start[0] = 0;
    start[1] = 1;
    count[0] = 2;
    count[1] = 1;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR
    start[0] = 1;
    count[0] = 1;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR
    start[1] = 0;
    start[3] = 1;
    count[1] = 2;
    count[3] = 1;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[1]) < 0)
        TEST_ERROR

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source dataset */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 2;
    count[1] = 16;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Write data directly to source dataset */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR 

    /* Close srcdset and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Read data through virtual dataset */
    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 3;
    count[1] = 9;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Read data through virtual dataset by hyperslab */
    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Read first stripe pattern */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    HDmemset(erbuf, 0, sizeof(erbuf));
    erbuf[0][0] = buf[0][0];
    erbuf[0][2] = buf[0][1];
    erbuf[0][6] = buf[0][8];
    erbuf[0][8] = buf[0][9];
    erbuf[2][0] = buf[1][0];
    erbuf[2][2] = buf[1][1];
    erbuf[2][6] = buf[1][8];
    erbuf[2][8] = buf[1][9];
    erbuf[1][3] = buf[0][13];
    erbuf[1][5] = buf[0][14];

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 1;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Read second stripe pattern */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    HDmemset(erbuf, 0, sizeof(erbuf));
    erbuf[0][3] = buf[0][4];
    erbuf[0][5] = buf[0][5];
    erbuf[1][0] = buf[0][6];
    erbuf[1][1] = buf[0][7];
    erbuf[1][2] = buf[0][12];
    erbuf[1][3] = buf[0][15];
    erbuf[1][5] = buf[1][4];
    erbuf[1][6] = buf[1][7];
    erbuf[1][7] = buf[1][12];
    erbuf[1][8] = buf[1][13];
    erbuf[2][3] = buf[1][14];
    erbuf[2][5] = buf[1][15];

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 2;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Read third stripe pattern */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    HDmemset(erbuf, 0, sizeof(erbuf));
    erbuf[0][0] = buf[0][2];
    erbuf[0][2] = buf[0][3];
    erbuf[0][6] = buf[0][10];
    erbuf[0][8] = buf[0][11];
    erbuf[2][0] = buf[1][2];
    erbuf[2][2] = buf[1][3];
    erbuf[2][6] = buf[1][10];
    erbuf[2][8] = buf[1][11];
    erbuf[1][3] = buf[1][5];
    erbuf[1][5] = buf[1][6];

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset by hyperslab */
    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Write data through virtual dataset by hyperslab */
    /* Write first stripe pattern */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    HDmemset(erbuf, 0, sizeof(erbuf));
    erbuf[0][0] = buf[0][0];
    erbuf[0][1] = buf[0][2];
    erbuf[0][8] = buf[0][6];
    erbuf[0][9] = buf[0][8];
    erbuf[1][0] = buf[2][0];
    erbuf[1][1] = buf[2][2];
    erbuf[1][8] = buf[2][6];
    erbuf[1][9] = buf[2][8];
    erbuf[0][13] = buf[1][3];
    erbuf[0][14] = buf[1][5];

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 1;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Write second slice */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    erbuf[0][4] = buf[0][3];
    erbuf[0][5] = buf[0][5];
    erbuf[0][6] = buf[1][0];
    erbuf[0][7] = buf[1][1];
    erbuf[0][12] = buf[1][2];
    erbuf[0][15] = buf[1][3];
    erbuf[1][4] = buf[1][5];
    erbuf[1][7] = buf[1][6];
    erbuf[1][12] = buf[1][7];
    erbuf[1][13] = buf[1][8];
    erbuf[1][14] = buf[2][3];
    erbuf[1][15] = buf[2][5];

    /* Adjust write buffer */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 2;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Write third slice */
    if(H5Dwrite(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    erbuf[0][2] = buf[0][0];
    erbuf[0][3] = buf[0][2];
    erbuf[0][10] = buf[0][6];
    erbuf[0][11] = buf[0][8];
    erbuf[1][2] = buf[2][0];
    erbuf[1][3] = buf[2][2];
    erbuf[1][10] = buf[2][6];
    erbuf[1][11] = buf[2][8];
    erbuf[1][5] = buf[1][3];
    erbuf[1][6] = buf[1][5];

    /* Reopen srcdset and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Read data directly from source dataset */
    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 2;
    count[1] = 16;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL ,count, NULL) < 0)
        TEST_ERROR

    /* Reset rbuf */
    HDmemset(rbuf[0], 0, sizeof(rbuf));

    /* Read dataset */
    if(H5Dread(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for(j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close */
    if(H5Dclose(srcdset[0]) < 0)
        TEST_ERROR
    srcdset[0] = -1;
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(srcfile[0]) < 0)
        TEST_ERROR
    srcfile[0] = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(srcspace[1]) < 0)
        TEST_ERROR
    srcspace[1] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;
    if(H5Sclose(memspace) < 0)
        TEST_ERROR
    memspace = -1;


    /* Close */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    dcpl = -1;

     PASSED();
     return 0;

error:
    H5E_BEGIN_TRY {
        for(i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++) {
            if(srcdset[i] >= 0)
                (void)H5Dclose(srcdset[i]);
        } /* end for */
        if(vdset >= 0)
            (void)H5Dclose(vdset);
        for(i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++) {
            if(srcfile[i] >= 0)
                (void)H5Fclose(srcfile[i]);
        } /* end for */
        if(vfile >= 0)
            (void)H5Fclose(vfile);
        for(i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++) {
            if(srcspace[i] >= 0)
                (void)H5Sclose(srcspace[i]);
        } /* end for */
        for(i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++) {
            if(vspace[i] >= 0)
                (void)H5Sclose(vspace[i]);
        } /* end for */
        if(memspace >= 0)
            (void)H5Sclose(memspace);
        if(dcpl >= 0)
            (void)H5Pclose(dcpl);
    } H5E_END_TRY;

     return 1;
} /* end test_basic_io() */


/*-------------------------------------------------------------------------
 * Function:    test_unlim
 *
 * Purpose:     Tests VDS with unlimited selections
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Programmer:  Neil Fortner
 *              Thursday, April 30, 2015
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
test_unlim(unsigned config, hid_t fapl)
{
    char        srcfilename[FILENAME_BUF_SIZE];
    char        vfilename[FILENAME_BUF_SIZE];
    hid_t       srcfile[4] = {-1, -1, -1, -1}; /* Files with source dsets */
    hid_t       vfile = -1;     /* File with virtual dset */
    hid_t       dcpl = -1;      /* Dataset creation property list */
    hid_t       srcdcpl = -1;   /* DCPL for source dset */
    hid_t       dapl = -1;      /* Dataset access property list */
    hid_t       srcspace[4] = {-1, -1, -1, -1}; /* Source dataspaces */
    hid_t       vspace[4] = {-1, -1, -1, -1}; /* Virtual dset dataspaces */
    hid_t       memspace = -1;   /* Memory dataspace */
    hid_t       filespace = -1; /* File dataspace */
    hid_t       srcdset[4] = {-1, -1, -1, -1}; /* Source datsets */
    hid_t       vdset = -1;     /* Virtual dataset */
    hsize_t     dims[2] = {10, 10}; /* Data space current size */
    hsize_t     mdims[2] = {10, 20}; /* Data space maximum size */
    hsize_t     cdims[2] = {4, 4}; /* Chunk dimensions */
    hsize_t     start[4];       /* Hyperslab start */
    hsize_t     stride[4];      /* Hyperslab stride */
    hsize_t     count[4];       /* Hyperslab count */
    hsize_t     block[4];       /* Hyperslab block */
    hsize_t     coord[10];      /* Point selection array */
    int         buf[10][20];    /* Write and expected read buffer */
    int         rbuf[10][20];   /* Read buffer */
    int         erbuf[10][20];  /* Expected read buffer */
    int         ndims;          /* Number of dimensions */
    int         i, j;

    TESTING("virtual dataset I/O with unlimited selections")

    h5_fixname(FILENAME[0], fapl, vfilename, sizeof vfilename);
    h5_fixname(FILENAME[1], fapl, srcfilename, sizeof srcfilename);

    /* Create DCPLs */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if((srcdcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /* Create DAPL */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR

    /* Set chunk dimensions */
    if(H5Pset_chunk(srcdcpl, 2, cdims) < 0)
        TEST_ERROR

    /* Create memory space */
    if((memspace = H5Screate_simple(2, mdims, NULL)) < 0)
        TEST_ERROR


    /*
     * Test 1: 2 Source datasets, single unlimited hyperslab virtual mappings
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    if((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR

    /* Create source dataspace */
    dims[0] = 5;
    mdims[0] = 5;
    if((srcspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR
    mdims[0] = 10;

    /* Select hyperslab in source space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = H5S_UNLIMITED;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Select hyperslabs in virtual spaces */
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    start[0] = 5;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset2", srcspace[0]) < 0)
        TEST_ERROR

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source datasets */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = -1;

    /* Write data directly to source datasets */
    /* Select hyperslab in memory */
    start[0] = 0;
    count[1] = 10;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Write first dataset */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++)
        for(j = 0; j < 10; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write second dataset */
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++)
        for(j = 0; j < 10; j++)
            erbuf[i + 5][j] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 10)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[0] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    if(H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR
    if((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 10)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Reopen srcdset[0] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Extend srcdset[0] */
    dims[0] = 5;
    dims[1] = 15;
    if(H5Dset_extent(srcdset[0], dims) < 0)
        TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 5;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if((filespace = H5Dget_space(srcdset[0])) < 0)
        TEST_ERROR
    start[1] = 10;
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Close srcdset[0] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING and we only extended one source dataset the
     * dimensions will not have changed. */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 10)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    if(H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR
    if((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR

    /* Update erbuf to reflect new data that is now visible due to the change to
     * H5D_VDS_LAST_AVAILABLE */
    for(i = 0; i < 5; i++)
        for(j = 0; j < 5; j++)
            erbuf[i][j + 10] = buf[i][j];

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 15)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Reopen srcdset[1] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Extend srcdset[1] */
    dims[0] = 5;
    dims[1] = 20;
    if(H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 10;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR
    start[1] = 10;
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 5; i++)
        for(j = 0; j < 10; j++)
            erbuf[i + 5][j + 10] = buf[i][j];

    /* Close srcdset[1] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING and we only extended one source dataset the
     * dimensions will not have changed. */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 20)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    if(H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR
    if((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR

    /* Update erbuf to reflect new data that is no longer visible due to the
     * change to H5D_VDS_FIRST_MISSING */
    for(i = 5; i < 10; i++)
        for(j = 15; j < 20; j++)
            erbuf[i][j] = -1;

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 15)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close */
    if(!(config & TEST_IO_CLOSE_SRC)) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;
        if(H5Fclose(srcfile[0]) < 0)
            TEST_ERROR
        srcfile[0] = -1;
    } /* end if */
    else if(!(config & TEST_IO_DIFFERENT_FILE)) {
        if(H5Fclose(srcfile[0]) < 0)
            TEST_ERROR
        srcfile[0] = -1;
    } /* end if */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;


    /*
     * Test 2: 2 Source datasets, interleaved slices, single element wide
     */
    /* Clear virtual layout in DCPL */
    if(H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR

    /* Create virtual dataspaces */
    dims[0] = 10;
    dims[1] = 10;
    if((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR
    if((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR

    /* Create source dataspace */
    dims[1] = 5;
    mdims[1] = 10;
    if((srcspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR
    mdims[1] = 20;

    /* Select hyperslab in source space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = H5S_UNLIMITED;
    if(H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    stride[0] = 1;
    stride[1] = 2;
    count[0] = 1;
    count[1] = H5S_UNLIMITED;
    block[0] = 10;
    block[1] = 1;
    if(H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR
    start[1] = 1;
    if(H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR

    /* Add virtual layout mappings */
    if(H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset1", srcspace[0]) < 0)
        TEST_ERROR
    if(H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename : ".", "src_dset2", srcspace[0]) < 0)
        TEST_ERROR

    /* Create virtual file */
    if((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create source file if requested */
    if(config & TEST_IO_DIFFERENT_FILE) {
        if((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
    } /* end if */
    else {
        srcfile[0] = vfile;
        if(H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR
    } /* end if */

    /* Create source datasets */
    if((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create virtual dataset */
    if((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Populate write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = -1;

    /* Write data directly to source datasets */
    /* Select hyperslab in memory */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = 5;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR

    /* Write first dataset */
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i++)
        for(j = 0; j < 5; j++)
            erbuf[i][2 * j] = buf[i][j];

    /* Adjust write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write second dataset */
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i++)
        for(j = 0; j < 5; j++)
            erbuf[i][(2 * j) + 1] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 10)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    if(H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR
    if((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 10)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Reopen srcdset[0] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Extend srcdset[0] */
    dims[1] = 7;
    if(H5Dset_extent(srcdset[0], dims) < 0)
        TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 2;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if((filespace = H5Dget_space(srcdset[0])) < 0)
        TEST_ERROR
    start[1] = 5;
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if(H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf to reflect only new data that is now visible under
     * H5D_VDS_FIRST_MISSING (first slice) */
    for(i = 0; i < 10; i++)
        erbuf[i][10] = buf[i][0];

    /* Close srcdset[0] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING and we only extended one source dataset the
     * dimension will only changed to add one more slice. */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 11)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    if(H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR
    if((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR

    /* Update erbuf to reflect new data that is now visible due to the change to
     * H5D_VDS_LAST_AVAILABLE (second new slice) */
    for(i = 0; i < 10; i++)
            erbuf[i][12] = buf[i][1];

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 13)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Reopen srcdset[1] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(config & TEST_IO_DIFFERENT_FILE)
            if((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR
        if((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR
    } /* end if */

    /* Extend srcdset[1] */
    dims[1] = 10;
    if(H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR

    /* Adjust write buffer */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 5;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR
    start[1] = 5;
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR
    if(H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR

    /* Update erbuf */
    for(i = 0; i < 10; i++)
        for(j = 0; j < 5; j++)
            erbuf[i][(2 * j) + 11] = buf[i][j];

    /* Close srcdset[1] and srcfile if config option specified */
    if(config & TEST_IO_CLOSE_SRC) {
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;

        if(config & TEST_IO_DIFFERENT_FILE) {
            if(H5Fclose(srcfile[0]) < 0)
                TEST_ERROR
            srcfile[0] = -1;
        } /* end if */
    } /* end if */

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING and we only extended one source dataset the
     * dimensions will not have changed. */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 20)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    if(H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR
    if((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR

    /* Update erbuf to reflect new data that is no longer visible due to the
     * change to H5D_VDS_FIRST_MISSING */
    for(i = 0; i < 10; i++)
        for(j = 15; j < 20; j += 2)
            erbuf[i][j] = -1;

    /* Get VDS space */
    if((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR

    /* Get VDS space dimensions */
    if((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR
    if(ndims != 2)
        TEST_ERROR
    if(H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR
    if(dims[0] != 10)
        TEST_ERROR
    if(dims[1] != 14)
        TEST_ERROR
    if(mdims[0] != 10)
        TEST_ERROR
    if(mdims[1] != 20)
        TEST_ERROR

    /* Close filespace */
    if(H5Sclose(filespace) < 0)
        TEST_ERROR

    /* Read data through virtual dataset */
    /* Reset rbuf */
    //HDmemset(rbuf[0], 0, sizeof(rbuf)); VDSINC
    /* Initialize erbuf - used now instead of setting fill value because fill
     * values do not work VDSINC */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            rbuf[i][j] = -1;

    /* Select hyperslab in memory space */
    start[1] = 0;
    if(H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR

    /* Read data */
    if(H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR

    /* Verify read data */
    for(i = 0; i < (int)mdims[0]; i++)
        for(j = 0; j < (int)mdims[1]; j++)
            if(rbuf[i][j] != erbuf[i][j])
                TEST_ERROR

    /* Close */
    if(!(config & TEST_IO_CLOSE_SRC)) {
        if(H5Dclose(srcdset[0]) < 0)
            TEST_ERROR
        srcdset[0] = -1;
        if(H5Dclose(srcdset[1]) < 0)
            TEST_ERROR
        srcdset[1] = -1;
        if(H5Fclose(srcfile[0]) < 0)
            TEST_ERROR
        srcfile[0] = -1;
    } /* end if */
    else if(!(config & TEST_IO_DIFFERENT_FILE)) {
        if(H5Fclose(srcfile[0]) < 0)
            TEST_ERROR
        srcfile[0] = -1;
    } /* end if */
    if(H5Dclose(vdset) < 0)
        TEST_ERROR
    vdset = -1;
    if(H5Fclose(vfile) < 0)
        TEST_ERROR
    vfile = -1;
    if(H5Sclose(srcspace[0]) < 0)
        TEST_ERROR
    srcspace[0] = -1;
    if(H5Sclose(vspace[0]) < 0)
        TEST_ERROR
    vspace[0] = -1;
    if(H5Sclose(vspace[1]) < 0)
        TEST_ERROR
    vspace[1] = -1;


    /* Close */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    dcpl = -1;
    if(H5Pclose(srcdcpl) < 0)
        TEST_ERROR
    dcpl = -1;
    if(H5Pclose(dapl) < 0)
        TEST_ERROR
    dapl = -1;
    if(H5Sclose(memspace) < 0)
        TEST_ERROR
    memspace = -1;

     PASSED();
     return 0;

error:
    H5E_BEGIN_TRY {
        for(i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++) {
            if(srcdset[i] >= 0)
                (void)H5Dclose(srcdset[i]);
        } /* end for */
        if(vdset >= 0)
            (void)H5Dclose(vdset);
        for(i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++) {
            if(srcfile[i] >= 0)
                (void)H5Fclose(srcfile[i]);
        } /* end for */
        if(vfile >= 0)
            (void)H5Fclose(vfile);
        for(i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++) {
            if(srcspace[i] >= 0)
                (void)H5Sclose(srcspace[i]);
        } /* end for */
        for(i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++) {
            if(vspace[i] >= 0)
                (void)H5Sclose(vspace[i]);
        } /* end for */
        if(filespace >= 0)
            (void)H5Sclose(filespace);
        if(memspace >= 0)
            (void)H5Sclose(memspace);
        if(dcpl >= 0)
            (void)H5Pclose(dcpl);
        if(srcdcpl >= 0)
            (void)H5Pclose(srcdcpl);
        if(dapl >= 0)
            (void)H5Pclose(dapl);
    } H5E_END_TRY;

     return 1;
} /* end test_unlim() */


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
    unsigned bit_config;
    int nerrors = 0;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    for(test_api_config = (int)TEST_API_BASIC; test_api_config < (int)TEST_API_NTESTS; test_api_config++)
        nerrors += test_api((test_api_config_t)test_api_config, fapl);
    for(bit_config = 0; bit_config < TEST_IO_NTESTS; bit_config++) {
        printf("Config: %s%s\n", bit_config & TEST_IO_CLOSE_SRC ? "closed source dataset, " : "", bit_config & TEST_IO_DIFFERENT_FILE ? "different source file" : "same source file");
        nerrors += test_basic_io(bit_config, fapl);
        nerrors += test_unlim(bit_config, fapl);
    } /* end for */

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

