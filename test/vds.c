/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Tests datasets with virtual layout.
 */
#include "h5test.h"
#include "H5Dprivate.h" /* For H5D_VIRTUAL_DEF_LIST_SIZE */

typedef enum {
    TEST_API_BASIC,
    TEST_API_COPY_PLIST,
    TEST_API_ENCDEC_PLIST,
    TEST_API_CREATE_DSET,
    TEST_API_REOPEN_DSET,
    TEST_API_REOPEN_FILE,
    TEST_API_NTESTS
} test_api_config_t;

static const char *FILENAME[] = {"vds_virt_0", "vds_virt_1", "vds_src_0",  "vds_src_1", "vds%%_src",
                                 "vds_dapl",   "vds_virt_2", "vds_virt_3", "vds_src_2", "vds_src_3",
                                 "vds%%_src2", "vds_dapl2",  NULL};

/* Define to enable verbose test output */
/* #define VDS_TEST_VERBOSE 1 */

#ifdef VDS_TEST_VERBOSE

/* For verbose output just use standard error printing */
#define TESTING_2_SUPPRESSED(WHAT)                                                                           \
    do {                                                                                                     \
        TESTING_2(WHAT);                                                                                     \
    } while (0)
#define PASSED_SUPPRESSED()                                                                                  \
    do {                                                                                                     \
        PASSED();                                                                                            \
    } while (0)
#define TEST_ERROR_SUPPRESSED                                                                                \
    do {                                                                                                     \
        TEST_ERROR;                                                                                          \
    } while (0)

/* Print config directly to output */
#define PRINT_CONFIG(...)                                                                                    \
    do {                                                                                                     \
        printf("Config: " __VA_ARGS__);                                                                      \
        puts("");                                                                                            \
    } while (0)

#else /* VDS_TEST_VERBOSE */

/* Global strings for error output */
char vds_config_str_g[128] = "";
char vds_test_str_g[128]   = "";

/* Replacement for TESTING_2 for non-verbose-output */
#define TESTING_2_SUPPRESSED(WHAT)                                                                           \
    do {                                                                                                     \
        snprintf(vds_test_str_g, sizeof(vds_test_str_g), WHAT);                                              \
    } while (0)

/* Suppress output from PASSED() */
#define PASSED_SUPPRESSED()                                                                                  \
    do {                                                                                                     \
        ;                                                                                                    \
    } while (0)

/* Replacement for TEST_ERROR for non-verbose output */
#define TEST_ERROR_SUPPRESSED                                                                                \
    do {                                                                                                     \
        printf("Failed config: %s\nFailed test: %s\n", vds_config_str_g, vds_test_str_g);                    \
        TEST_ERROR;                                                                                          \
    } while (0)

/* Replacement for printf for printing configuration for non-verbose output */
#define PRINT_CONFIG(...)                                                                                    \
    do {                                                                                                     \
        snprintf(vds_config_str_g, sizeof(vds_config_str_g), __VA_ARGS__);                                   \
    } while (0)

#endif /* VDS_TEST_VERBOSE */

/* I/O test config flags */
#define TEST_IO_CLOSE_SRC      0x01U
#define TEST_IO_DIFFERENT_FILE 0x02U
#define TEST_IO_REOPEN_VIRT    0x04U
#define TEST_IO_FCLOSE_SEMI    0x08U
#define TEST_IO_FCLOSE_STRONG  0x10U
#define TEST_IO_NTESTS         0x20U

#define LIST_DOUBLE_SIZE (H5D_VIRTUAL_DEF_LIST_SIZE + 1)

#define FILENAME_BUF_SIZE 1024

#define TMPDIR "tmp_vds/"

/*-------------------------------------------------------------------------
 * Function:    vds_select_equal
 *
 * Purpose:     Helper function to check if the selections in the two
 *              provided dataspaces are the same.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static htri_t
vds_select_equal(hid_t space1, hid_t space2)
{
    H5S_sel_type type1;
    H5S_sel_type type2;
    hsize_t     *buf1 = NULL;
    hsize_t     *buf2 = NULL;
    size_t       i;
    htri_t       ret_value = true;

    /* Get and compare selection types */
    if ((type1 = H5Sget_select_type(space1)) < 0)
        TEST_ERROR;
    if ((type2 = H5Sget_select_type(space2)) < 0)
        TEST_ERROR;
    if (type1 != type2)
        return false;

    /* Check selection type */
    switch (type1) {
        case H5S_SEL_NONE:
        case H5S_SEL_ALL:
            break;

        case H5S_SEL_POINTS: {
            int      rank1;
            int      rank2;
            hssize_t npoints1;
            hssize_t npoints2;

            /* Get and compare rank */
            if ((rank1 = H5Sget_simple_extent_ndims(space1)) < 0)
                TEST_ERROR;
            if ((rank2 = H5Sget_simple_extent_ndims(space2)) < 0)
                TEST_ERROR;
            if (rank1 != rank2)
                return false;

            /* Get and compare number of points */
            if ((npoints1 = H5Sget_select_elem_npoints(space1)) < 0)
                TEST_ERROR;
            if ((npoints2 = H5Sget_select_elem_npoints(space2)) < 0)
                TEST_ERROR;
            if (npoints1 != npoints2)
                return false;

            /* Allocate point lists.  Do not return directly after
             * allocating, to make sure buffers are freed. */
            if (NULL == (buf1 = (hsize_t *)malloc((size_t)rank1 * (size_t)npoints1 * sizeof(hsize_t))))
                TEST_ERROR;
            if (NULL == (buf2 = (hsize_t *)malloc((size_t)rank1 * (size_t)npoints1 * sizeof(hsize_t))))
                TEST_ERROR;

            /* Get and compare point lists */
            if (H5Sget_select_elem_pointlist(space1, (hsize_t)0, (hsize_t)npoints1, buf1) < 0)
                TEST_ERROR;
            if (H5Sget_select_elem_pointlist(space2, (hsize_t)0, (hsize_t)npoints1, buf2) < 0)
                TEST_ERROR;
            for (i = 0; i < ((size_t)rank1 * (size_t)npoints1); i++)
                if (buf1[i] != buf2[i]) {
                    ret_value = false;
                    break;
                }

            /* Free buffers */
            free(buf1);
            buf1 = NULL;
            free(buf2);
            buf2 = NULL;
        } /* end block */

        break;

        case H5S_SEL_HYPERSLABS: {
            int      rank1;
            int      rank2;
            hssize_t nblocks1;
            hssize_t nblocks2;

            /* Get and compare rank */
            if ((rank1 = H5Sget_simple_extent_ndims(space1)) < 0)
                TEST_ERROR;
            if ((rank2 = H5Sget_simple_extent_ndims(space2)) < 0)
                TEST_ERROR;
            if (rank1 != rank2)
                return false;

            /* Get and compare number of blocks */
            if ((nblocks1 = H5Sget_select_hyper_nblocks(space1)) < 0)
                TEST_ERROR;
            if ((nblocks2 = H5Sget_select_hyper_nblocks(space2)) < 0)
                TEST_ERROR;
            if (nblocks1 != nblocks2)
                return false;

            /* Allocate block lists.  Do not return directly after
             * allocating, to make sure buffers are freed. */
            if (NULL ==
                (buf1 = (hsize_t *)malloc((size_t)2 * (size_t)rank1 * (size_t)nblocks1 * sizeof(*buf1))))
                TEST_ERROR;
            if (NULL ==
                (buf2 = (hsize_t *)malloc((size_t)2 * (size_t)rank1 * (size_t)nblocks1 * sizeof(*buf2))))
                TEST_ERROR;

            /* Get and compare block lists */
            if (H5Sget_select_hyper_blocklist(space1, (hsize_t)0, (hsize_t)nblocks1, buf1) < 0)
                TEST_ERROR;
            if (H5Sget_select_hyper_blocklist(space2, (hsize_t)0, (hsize_t)nblocks1, buf2) < 0)
                TEST_ERROR;
            for (i = 0; i < ((size_t)2 * (size_t)rank1 * (size_t)nblocks1); i++)
                if (buf1[i] != buf2[i]) {
                    ret_value = false;
                    break;
                }

            /* Free buffers */
            free(buf1);
            buf1 = NULL;
            free(buf2);
            buf2 = NULL;
        } /* end block */

        break;

        case H5S_SEL_ERROR:
        case H5S_SEL_N:
        default:
            TEST_ERROR;
    } /* end switch */

    return ret_value;

error:
    if (buf1)
        free(buf1);
    if (buf2)
        free(buf2);

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
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static int
vds_check_mapping(hid_t dcpl, size_t i, hid_t vspace, hid_t srcspace, const char *filename,
                  const char *dsetname)
{
    hid_t   space_out = H5I_INVALID_HID;
    char    name_out[32];
    htri_t  tri_ret;
    ssize_t str_len;

    assert(dcpl >= 0);
    assert(vspace >= 0);
    assert(srcspace >= 0);
    assert(filename);
    assert(dsetname);

    /* Check vspace */
    if ((space_out = H5Pget_virtual_vspace(dcpl, i)) < 0)
        TEST_ERROR;
    if ((tri_ret = H5Sextent_equal(space_out, vspace)) < 0)
        TEST_ERROR;
    if (!tri_ret)
        TEST_ERROR;
    if ((tri_ret = vds_select_equal(space_out, vspace)) < 0)
        TEST_ERROR;
    if (!tri_ret)
        TEST_ERROR;
    if (H5Sclose(space_out) < 0)
        TEST_ERROR;
    space_out = -1;

    /* Check srcspace */
    if ((space_out = H5Pget_virtual_srcspace(dcpl, i)) < 0)
        TEST_ERROR;
    if ((tri_ret = vds_select_equal(space_out, srcspace)) < 0)
        TEST_ERROR;
    if (!tri_ret)
        TEST_ERROR;
    if (H5Sclose(space_out) < 0)
        TEST_ERROR;
    space_out = -1;

    /* Check filename */
    if ((str_len = H5Pget_virtual_filename(dcpl, i, NULL, (size_t)0)) < 0)
        TEST_ERROR;
    if ((size_t)str_len != strlen(filename))
        TEST_ERROR;
    assert((size_t)str_len < sizeof(name_out));
    if ((str_len = H5Pget_virtual_filename(dcpl, i, name_out, sizeof(name_out))) < 0)
        TEST_ERROR;
    if ((size_t)str_len != strlen(filename))
        TEST_ERROR;
    if (strncmp(name_out, filename, (size_t)str_len + 1) != 0)
        TEST_ERROR;

    /* Check dsetname */
    if ((str_len = H5Pget_virtual_dsetname(dcpl, i, NULL, (size_t)0)) < 0)
        TEST_ERROR;
    if ((size_t)str_len != strlen(dsetname))
        TEST_ERROR;
    assert((size_t)str_len < sizeof(name_out));
    if ((str_len = H5Pget_virtual_dsetname(dcpl, i, name_out, sizeof(name_out))) < 0)
        TEST_ERROR;
    if ((size_t)str_len != strlen(dsetname))
        TEST_ERROR;
    if (strncmp(name_out, dsetname, (size_t)str_len + 1) != 0)
        TEST_ERROR;

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_out);
    }
    H5E_END_TRY

    return -1;
} /* end vds_check_mapping() */

/*-------------------------------------------------------------------------
 * Function:    test_api_get_ex_dcpl
 *
 * Purpose:     Tests API functions related to virtual datasets.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *-------------------------------------------------------------------------
 */
/* Helper function to get DCPL for examination depending on config */
static int
test_api_get_ex_dcpl(test_api_config_t config, hid_t fapl, hid_t dcpl, hid_t *ex_dcpl, hid_t vspace,
                     char *filename, hsize_t exp_meta_size)
{
    hid_t              file = H5I_INVALID_HID; /* File */
    hid_t              dset = H5I_INVALID_HID; /* Virtual dataset */
    H5D_space_status_t space_status;           /* Dataset space status */
    void              *plist_buf = NULL;       /* Serialized property list buffer */
    H5O_native_info_t  ninfo;                  /* Object info struct */
    htri_t             tri_ret;

    assert((config >= TEST_API_BASIC) && (config < TEST_API_NTESTS));
    assert(fapl >= 0);
    assert(dcpl >= 0);
    assert(ex_dcpl);
    assert(*ex_dcpl < 0);
    assert(vspace >= 0);
    assert(filename);

    /* Take different action depending on test configuration */
    if (config >= TEST_API_CREATE_DSET) {
        /* Create file and dataset */
        if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR;
        if ((dset = H5Dcreate2(file, "vdset", H5T_NATIVE_INT, vspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        /* Test H5Dget_space_status */
        if (H5Dget_space_status(dset, &space_status) < 0)
            TEST_ERROR;
        if (space_status != H5D_SPACE_STATUS_ALLOCATED)
            TEST_ERROR;

        /* Reopen dataset if requested */
        if (config >= TEST_API_REOPEN_DSET) {
            /* Close dataset */
            if (H5Dclose(dset) < 0)
                TEST_ERROR;
            dset = -1;

            /* Reopen file if requested */
            if (config == TEST_API_REOPEN_FILE) {
                if (H5Fclose(file) < 0)
                    TEST_ERROR;
                file = -1;
                if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                    TEST_ERROR;
            }

            /* Open dataset */
            if ((dset = H5Dopen2(file, "vdset", H5P_DEFAULT)) < 0)
                TEST_ERROR;
        }

        /* Get DCPL from dataset */
        if ((*ex_dcpl = H5Dget_create_plist(dset)) < 0)
            TEST_ERROR;

        /* Test H5Dget_offset() (just returns HADDR_UNDEF) */
        if (HADDR_UNDEF != H5Dget_offset(dset))
            TEST_ERROR;

        /* Test H5Oget_info returns correct metadata size */
        if (H5Oget_native_info(dset, &ninfo, H5O_NATIVE_INFO_META_SIZE) < 0)
            TEST_ERROR;
        if (ninfo.meta_size.obj.index_size != (hsize_t)0)
            TEST_ERROR;
        if (config == TEST_API_REOPEN_FILE) {
            if (ninfo.meta_size.obj.heap_size != exp_meta_size) {
                printf("VDS metadata size: %llu Expected: %llu\n",
                       (long long unsigned)ninfo.meta_size.obj.heap_size, (long long unsigned)exp_meta_size);
                TEST_ERROR;
            }
        }
        else if ((ninfo.meta_size.obj.heap_size != exp_meta_size) &&
                 (ninfo.meta_size.obj.heap_size != (hsize_t)0))
            TEST_ERROR;
        if (ninfo.meta_size.attr.index_size != (hsize_t)0)
            TEST_ERROR;
        if (ninfo.meta_size.attr.index_size != (hsize_t)0)
            TEST_ERROR;

        /* Test H5Dget_space_status */
        if (H5Dget_space_status(dset, &space_status) < 0)
            TEST_ERROR;
        if (space_status != H5D_SPACE_STATUS_ALLOCATED)
            TEST_ERROR;

        /* Close dataset */
        if (H5Dclose(dset) < 0)
            TEST_ERROR;
        dset = -1;

        /* Delete dataset */
        if (H5Ldelete(file, "vdset", H5P_DEFAULT) < 0)
            TEST_ERROR;

        /* Close file */
        if (H5Fclose(file) < 0)
            TEST_ERROR;
        file = -1;
    }
    else if (config == TEST_API_COPY_PLIST) {
        /* Copy property list */
        if ((*ex_dcpl = H5Pcopy(dcpl)) < 0)
            TEST_ERROR;
    }
    else if (config == TEST_API_ENCDEC_PLIST) {
        size_t plist_buf_size;

        /* Encode property list to plist_buf */
        if (H5Pencode2(dcpl, NULL, &plist_buf_size, fapl) < 0)
            TEST_ERROR;
        if (NULL == (plist_buf = malloc(plist_buf_size)))
            TEST_ERROR;
        if (H5Pencode2(dcpl, plist_buf, &plist_buf_size, fapl) < 0)
            TEST_ERROR;

        /* Decode serialized property list to *ex_dcpl */
        if ((*ex_dcpl = H5Pdecode(plist_buf)) < 0)
            TEST_ERROR;

        /* Free plist_buf */
        free(plist_buf);
        plist_buf = NULL;
    }
    else {
        /* Simply copy the id to ex_dcpl and increment the ref count so ex_dcpl
         * can be closed */
        if (H5Iinc_ref(dcpl) < 0)
            TEST_ERROR;
        *ex_dcpl = dcpl;
    }

    /* Verify examination DCPL is equal to original DCPL.  Do not compare the
     * plist to itself, and do not do the comparison if we reopened the file,
     * because in that case the extent of the source dset will not be current.
     */
    if ((*ex_dcpl != dcpl) && (config != TEST_API_REOPEN_FILE)) {
        if ((tri_ret = H5Pequal(dcpl, *ex_dcpl)) < 0)
            TEST_ERROR;
        if (!tri_ret)
            TEST_ERROR;
    }

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
        H5Dclose(dset);
    }
    H5E_END_TRY
    if (plist_buf)
        free(plist_buf);

    return -1;
} /* end test_api_get_ex_dcpl() */

/* Main test function */
static int
test_api(test_api_config_t config, hid_t fapl, H5F_libver_t low)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       dcpl        = H5I_INVALID_HID; /* Dataset creation property list */
    hid_t       ex_dcpl     = H5I_INVALID_HID; /* Temporary dcpl for examination */
    hid_t       srcspace[4] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                         H5I_INVALID_HID}; /* Source dataspaces */
    hid_t       vspace[LIST_DOUBLE_SIZE];        /* Virtual dset dataspaces */
    const char *src_file[4] = {"src_file1", "src_file2.", "src_file3..",
                               "src_file4..."}; /* Source file names (different lengths) */
    const char *src_dset[4] = {"src_dset1....", "src_dset2.....", "src_dset3......",
                               "src_dset4......."}; /* Source dataset names (different lengths) */
    char        tmp_filename[32];
    char        tmp_dsetname[32];
    hsize_t     dims[2] = {10, 20}; /* Data space current size */
    hsize_t     start[2];           /* Hyperslab start */
    hsize_t     stride[2];          /* Hyperslab stride */
    hsize_t     count[2];           /* Hyperslab count */
    hsize_t     block[2];           /* Hyperslab block */
    hsize_t     coord[10];          /* Point selection array */
    size_t      size_out;
    herr_t      ret;
    unsigned    i;

    /* Initialize vspace */
    for (i = 0; i < (unsigned)(sizeof(vspace) / sizeof(vspace[0])); i++)
        vspace[i] = -1;

    switch (config) {
        case TEST_API_BASIC:
            TESTING_2("virtual dataset API functions");
            break;
        case TEST_API_COPY_PLIST:
            TESTING_2("virtual dataset API functions with copied plists");
            break;
        case TEST_API_ENCDEC_PLIST:
            TESTING_2("virtual dataset API functions with encoded and decoded plists");
            break;
        case TEST_API_CREATE_DSET:
            TESTING_2("virtual dataset create");
            break;
        case TEST_API_REOPEN_DSET:
            TESTING_2("virtual dataset create with reopened dataset");
            break;
        case TEST_API_REOPEN_FILE:
            TESTING_2("virtual dataset create with reopened file");
            break;
        case TEST_API_NTESTS:
        default:
            TEST_ERROR;
    }

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Create DCPL */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /*
     * Test 1: All - all selection
     */
    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Select all (should not be necessary, but just to be sure) */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR;
    if (H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR;

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename, (hsize_t)69) < 0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)1)
        TEST_ERROR;

    /* Check that the mapping in the DCPL is correct */
    if (vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR;

    /* Close */
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR;
    vspace[0] = -1;
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

    /*
     * Test 2: Hyper - hyper selection
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Select regular hyperslab in source space */
    start[0]  = 2;
    start[1]  = 1;
    stride[0] = 3;
    stride[1] = 5;
    count[0]  = 2;
    count[1]  = 3;
    block[0]  = 2;
    block[1]  = 4;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    /* Select composite hyperslab in virtual space */
    count[0] = 1;
    count[1] = 1;
    block[0] = 5;
    block[1] = 6;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR;
    start[0] = 7;
    start[1] = 0;
    block[0] = 1;
    block[1] = 18;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, NULL, count, block) < 0)
        TEST_ERROR;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR;

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename,
                             (low >= H5F_LIBVER_V112) ? (hsize_t)99 : (low >= H5F_LIBVER_V110 ? 174 : 213)) <
        0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)1)
        TEST_ERROR;

    /* Check that the mapping in the DCPL is correct */
    if (vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR;

    /* Close */
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR;
    vspace[0] = -1;
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

#ifdef VDS_POINT_SELECTIONS /* VDS does not currently support point selections */
    /*
     * Test 3: Point - point selection
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(srcspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(vspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR;

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename, (hsize_t)0) < 0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)1)
        TEST_ERROR;

    /* Check that the mapping in the DCPL is correct */
    if (vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR;

    /* Close */
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR;
    vspace[0] = -1;
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

    /*
     * Test 4: Point - hyper selection
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Select hyperslab in source space */
    start[0] = 2;
    start[1] = 7;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = 5;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(vspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]) < 0)
        TEST_ERROR;

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename, (hsize_t)0) < 0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)1)
        TEST_ERROR;

    /* Check that the mapping in the DCPL is correct */
    if (vds_check_mapping(ex_dcpl, (size_t)0, vspace[0], srcspace[0], src_file[0], src_dset[0]) < 0)
        TEST_ERROR;

    /* Close */
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR;
    vspace[0] = -1;
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

    /*
     * Test 5: All previous mappings together
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create dataspaces */
    for (i = 0; i < 4; i++) {
        /* Create source dataspace */
        if ((srcspace[i] = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR;

        /* Create virtual dataspace */
        if ((vspace[i] = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR;
    }

    /* Select all (should not be necessary, but just to be sure) */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR;
    if (H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR;

    /* Select regular hyperslab in source space */
    start[0]  = 2;
    start[1]  = 1;
    stride[0] = 3;
    stride[1] = 5;
    count[0]  = 2;
    count[1]  = 3;
    block[0]  = 2;
    block[1]  = 4;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    /* Select composite hyperslab in virtual space */
    count[0] = 1;
    count[1] = 1;
    block[0] = 5;
    block[1] = 6;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR;
    start[0] = 7;
    start[1] = 0;
    block[0] = 1;
    block[1] = 18;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, NULL, count, block) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(srcspace[2], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(vspace[2], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

    /* Select hyperslab in source space */
    start[0] = 2;
    start[1] = 7;
    count[0] = 1;
    count[1] = 1;
    block[0] = 1;
    block[1] = 5;
    if (H5Sselect_hyperslab(srcspace[3], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(vspace[3], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

    /* Add virtual layout mappings */
    for (i = 0; i < 4; i++)
        if (H5Pset_virtual(dcpl, vspace[i], src_file[i], src_dset[i], srcspace[i]) < 0)
            TEST_ERROR;

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename, (hsize_t)0) < 0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)4)
        TEST_ERROR;

    /* Check that the mappings in the DCPL are correct */
    for (i = 0; i < 4; i++)
        if (vds_check_mapping(ex_dcpl, (size_t)i, vspace[i], srcspace[i], src_file[i], src_dset[i]) < 0)
            TEST_ERROR;

    /* Close */
    for (i = 0; i < 4; i++) {
        if (H5Sclose(srcspace[i]) < 0)
            TEST_ERROR;
        srcspace[i] = -1;
        if (H5Sclose(vspace[i]) < 0)
            TEST_ERROR;
        vspace[i] = -1;
    }
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

#else  /* VDS_POINT_SELECTIONS */

    /*
     * Test 3: Verify point selections fail
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(srcspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

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
    if (H5Sselect_elements(vspace[0], H5S_SELECT_SET, (size_t)5, coord) < 0)
        TEST_ERROR;

    /* Attempt to add virtual layout mapping */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_virtual(dcpl, vspace[0], src_file[0], src_dset[0], srcspace[0]);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
#endif /* VDS_POINT_SELECTIONS */

    /*
     * Test 6: Enough Selections to trigger doubling of mapping list
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create source dataspace */
    dims[0] = 1;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Select all in source space (should not be necessary, but just to be sure) */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR;

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
    for (i = 0; i < LIST_DOUBLE_SIZE; i++) {
        /* Create virtual dataspace */
        if ((vspace[i] = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR;

        /* Select row in virtual dataspace */
        start[0] = (hsize_t)i;
        if (H5Sselect_hyperslab(vspace[i], H5S_SELECT_SET, start, NULL, count, block) < 0)
            TEST_ERROR;

        /* Create file and dataset names */
        (void)snprintf(tmp_filename, sizeof(tmp_filename), "src_file%u", i);
        tmp_filename[sizeof(tmp_filename) - 1] = '\0';
        (void)snprintf(tmp_dsetname, sizeof(tmp_dsetname), "src_dset%u", i);
        tmp_dsetname[sizeof(tmp_dsetname) - 1] = '\0';

        /* Add virtual layout mapping */
        if (H5Pset_virtual(dcpl, vspace[i], tmp_filename, tmp_dsetname, srcspace[0]) < 0)
            TEST_ERROR;
    }

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename,
                             (low >= H5F_LIBVER_V112) ? (hsize_t)607 : (hsize_t)697) < 0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)LIST_DOUBLE_SIZE)
        TEST_ERROR;

    /* Verify virtual layout */
    for (i = 0; i < LIST_DOUBLE_SIZE; i++) {
        /* Generate source file name */
        (void)snprintf(tmp_filename, sizeof(tmp_filename), "src_file%u", i);
        tmp_filename[sizeof(tmp_filename) - 1] = '\0';

        /* Generate source dset name */
        (void)snprintf(tmp_dsetname, sizeof(tmp_dsetname), "src_dset%u", i);
        tmp_dsetname[sizeof(tmp_dsetname) - 1] = '\0';

        /* Check that the mapping in the DCPL is correct */
        if (vds_check_mapping(ex_dcpl, (size_t)i, vspace[i], srcspace[0], tmp_filename, tmp_dsetname) < 0)
            TEST_ERROR;
    }

    /* Close */
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR;
    srcspace[0] = -1;
    for (i = 0; i < LIST_DOUBLE_SIZE; i++) {
        if (H5Sclose(vspace[i]) < 0)
            TEST_ERROR;
        vspace[i] = -1;
    }
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

    /*
     * Test 7: Empty VDS
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Select all (should not be necessary, but just to be sure) */
    if (H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR;

    /* Get examination DCPL */
    if (test_api_get_ex_dcpl(config, fapl, dcpl, &ex_dcpl, vspace[0], filename, (hsize_t)0) < 0)
        TEST_ERROR;

    /* Test H5Pget_virtual_count */
    if (H5Pget_virtual_count(ex_dcpl, &size_out) < 0)
        TEST_ERROR;
    if (size_out != (size_t)0)
        TEST_ERROR;

    /* Close */
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR;
    vspace[0] = -1;
    if (H5Pclose(ex_dcpl) < 0)
        TEST_ERROR;
    ex_dcpl = -1;

    /* Close */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = -1;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (i = 0; i < (sizeof(srcspace) / sizeof(srcspace[0])); i++)
            H5Sclose(srcspace[i]);
        for (i = 0; i < (sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Pclose(dcpl);
        H5Pclose(ex_dcpl);
    }
    H5E_END_TRY

    return 1;
} /* end test_api() */

/*-------------------------------------------------------------------------
 * Function:    test_vds_prefix_first
 *
 * Purpose:     Set up vds link prefix via H5Pset_virtual_prefix() to be "tmp"
 *        Should be able to access the target source files in tmp directory via the prefix set
 *        by H5Pset_virtual_prefix()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static int
test_vds_prefix_first(unsigned config, hid_t vds_fapl, hid_t src_fapl)
{
    char       *srcfilename             = NULL;
    char       *srcfilename_map         = NULL;
    char       *vfilename               = NULL;
    char       *srcfilenamepct          = NULL;
    char       *srcfilenamepct_map      = NULL;
    const char *srcfilenamepct_map_orig = "vds%%%%_src";
    hid_t       srcfile[4]              = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Files with source dsets */
    hid_t       vfile                   = H5I_INVALID_HID;   /* File with virtual dset */
    hid_t       dcpl                    = H5I_INVALID_HID;   /* Dataset creation property list */
    hid_t       dapl                    = H5I_INVALID_HID;   /* Dataset access property list */
    hid_t       srcspace[4]             = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                         H5I_INVALID_HID}; /* Source dataspaces */
    hid_t       vspace[4]               = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                       H5I_INVALID_HID}; /* Virtual dset dataspaces */
    hid_t       memspace                = H5I_INVALID_HID;   /* Memory dataspace */
    hid_t       srcdset[4]              = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Source datasets */
    hid_t       vdset                   = H5I_INVALID_HID;   /* Virtual dataset */
    hsize_t     dims[4]                 = {10, 26, 0, 0};    /* Data space current size */
    int         buf[10][26];                                 /* Write and expected read buffer */
    int         rbuf[10][26];                                /* Read buffer */
    int         fill = -1;                                   /* Fill value */
    int         i, j;
    char        buffer[1024]; /* buffer to read vds_prefix       */

    TESTING_2_SUPPRESSED("basic virtual dataset I/O via H5Pset_vds_prefix(): all selection");

    if ((srcfilename = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilename_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((vfilename = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilenamepct = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilenamepct_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;

    h5_fixname(FILENAME[0], vds_fapl, vfilename, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[8], src_fapl, srcfilename, FILENAME_BUF_SIZE);
    h5_fixname_printf(FILENAME[8], src_fapl, srcfilename_map, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[10], src_fapl, srcfilenamepct, FILENAME_BUF_SIZE);
    h5_fixname_printf(srcfilenamepct_map_orig, src_fapl, srcfilenamepct_map, FILENAME_BUF_SIZE);

    /* create tmp directory and get current working directory path */
    if (HDmkdir(TMPDIR, (mode_t)0755) < 0 && errno != EEXIST)
        TEST_ERROR_SUPPRESSED;

    /* Create DCPL */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set fill value */
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Initialize VDS prefix items */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR_SUPPRESSED;

    if (H5Pset_virtual_prefix(dapl, TMPDIR) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pget_virtual_prefix(dapl, buffer, sizeof(buffer)) < 0)
        TEST_ERROR_SUPPRESSED;

    if (strcmp(buffer, TMPDIR) != 0)
        FAIL_PUTS_ERROR("vds prefix not set correctly");

    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all (should not be necessary, but just to be sure) */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if (NULL == HDgetcwd(buffer, 1024))
            TEST_ERROR_SUPPRESSED;
        if (HDchdir(TMPDIR) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (HDchdir(buffer) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source dataset */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++) {
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j]) {
                TEST_ERROR_SUPPRESSED;
            }
    }

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (NULL == HDgetcwd(buffer, 1024))
                TEST_ERROR_SUPPRESSED;
            if (HDchdir(TMPDIR) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if (HDchdir(buffer) < 0)
                TEST_ERROR_SUPPRESSED;
        }
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Pclose(dapl) < 0)
        TEST_ERROR_SUPPRESSED;
    dapl = -1;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    dcpl = -1;

    free(srcfilename);
    free(srcfilename_map);
    free(vfilename);
    free(srcfilenamepct);
    free(srcfilenamepct_map);

    PASSED_SUPPRESSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++)
            H5Dclose(srcdset[i]);
        H5Dclose(vdset);
        for (i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++)
            H5Fclose(srcfile[i]);
        H5Fclose(vfile);
        for (i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++)
            H5Sclose(srcspace[i]);
        for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Sclose(memspace);
        H5Pclose(dapl);
        H5Pclose(dcpl);
    }
    H5E_END_TRY

    if (HDsetenv("HDF5_VDS_PREFIX", "", 1) < 0)
        TEST_ERROR_SUPPRESSED;

    free(srcfilename);
    free(srcfilename_map);
    free(vfilename);
    free(srcfilenamepct);
    free(srcfilenamepct_map);

    return 1;
} /* end test_vds_prefix */

/*-------------------------------------------------------------------------
 * Function:    test_basic_io
 *
 * Purpose:     Tests VDS I/O without unlimited selections or
 *              pattern-matching file/dataset strings
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *-------------------------------------------------------------------------
 */
static int
test_basic_io(unsigned config, hid_t vds_fapl, hid_t src_fapl)
{
    char       *srcfilename             = NULL;
    char       *srcfilename_map         = NULL;
    char       *vfilename               = NULL;
    char       *vfilename2              = NULL;
    char       *srcfilenamepct          = NULL;
    char       *srcfilenamepct_map      = NULL;
    const char *srcfilenamepct_map_orig = "vds%%%%_src";
    hid_t       srcfile[4]              = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Files with source dsets */
    hid_t       vfile                   = H5I_INVALID_HID;   /* File with virtual dset */
    hid_t       vfile2                  = H5I_INVALID_HID;   /* File with copied virtual dset */
    hid_t       dcpl                    = H5I_INVALID_HID;   /* Dataset creation property list */
    hid_t       srcspace[4]             = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                         H5I_INVALID_HID}; /* Source dataspaces */
    hid_t       vspace[4]               = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                       H5I_INVALID_HID}; /* Virtual dset dataspaces */
    hid_t       memspace                = H5I_INVALID_HID;   /* Memory dataspace */
    hid_t       srcdset[4]              = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Source datasets */
    hid_t       vdset                   = H5I_INVALID_HID;   /* Virtual dataset */
    hsize_t     dims[4]                 = {10, 26, 0, 0};    /* Data space current size */
    hsize_t     start[4];                                    /* Hyperslab start */
    hsize_t     stride[4];                                   /* Hyperslab stride */
    hsize_t     count[4];                                    /* Hyperslab count */
    hsize_t     block[4];                                    /* Hyperslab block */
    hssize_t    offset[2] = {0, 0};                          /* Selection offset */
    int         buf[10][26];                                 /* Write and expected read buffer */
    int         rbuf[10][26];                                /* Read buffer */
    int         rbuf99[9][9];                                /* 9x9 Read buffer */
    int         evbuf[10][26];                               /* Expected VDS "buffer" */
    int         erbuf[10][26];                               /* Expected read buffer */
    int         fill = -1;                                   /* Fill value */
    herr_t      ret;                                         /* Generic return value */
    int         i, j, u, v;

    TESTING_2_SUPPRESSED("basic virtual dataset I/O");

    if ((srcfilename = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilename_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((vfilename = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((vfilename2 = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilenamepct = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilenamepct_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;

    h5_fixname(FILENAME[0], vds_fapl, vfilename, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[1], vds_fapl, vfilename2, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[2], src_fapl, srcfilename, FILENAME_BUF_SIZE);
    h5_fixname_printf(FILENAME[2], src_fapl, srcfilename_map, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[4], src_fapl, srcfilenamepct, FILENAME_BUF_SIZE);
    h5_fixname_printf(srcfilenamepct_map_orig, src_fapl, srcfilenamepct_map, FILENAME_BUF_SIZE);

    /* Create DCPL */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set fill value */
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill) < 0)
        TEST_ERROR_SUPPRESSED;

    /*
     * Test 1: All - all selection
     */
    /* Create source dataspace */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all (should not be necessary, but just to be sure) */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_all(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source dataset */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;

    /*
     * Test 2: 2 Source datasets, hyperslab virtual mappings, '%' in source
     * dataset name, also test H5Ocopy()
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 13;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 13;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "%%src_dset1", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset2%%", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[1] = 26;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "%src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2%", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "%src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2%", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source datasets */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dread(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Test H5Ocopy() to same file */
    /* Copy virtual dataset */
    if (H5Ocopy(vfile, "v_dset", vfile, "v_dset2", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close v_dset */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data directly to source datasets */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Open v_dset2 */
    if ((vdset = H5Dopen2(vfile, "v_dset2", H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through copied virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "%src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2%", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Only copy to a different file if the source datasets are in a different
     * file */
    if (config & TEST_IO_DIFFERENT_FILE) {
        /* Close v_dset2 */
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;

        /* Create file to copy virtual dataset to */
        if ((vfile2 = H5Fcreate(vfilename2, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Copy virtual dataset */
        if (H5Ocopy(vfile, "v_dset", vfile2, "v_dset3", H5P_DEFAULT, H5P_DEFAULT) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Adjust write buffer */
        for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
            for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
                buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

        /* Write data directly to source datasets */
        if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Close srcdsets and srcfile if config option specified */
        if (config & TEST_IO_CLOSE_SRC) {
            if (H5Dclose(srcdset[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[0] = -1;
            if (H5Dclose(srcdset[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[1] = -1;

            if (config & TEST_IO_DIFFERENT_FILE) {
                if (H5Fclose(srcfile[0]) < 0)
                    TEST_ERROR_SUPPRESSED;
                srcfile[0] = -1;
            }
        }

        /* Reopen copied virtual file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Fclose(vfile2) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile2 = -1;
            if ((vfile2 = H5Fopen(vfilename2, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Open v_dset3 */
        if ((vdset = H5Dopen2(vfile2, "v_dset3", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through copied virtual dataset */
        memset(rbuf[0], 0, sizeof(rbuf));
        if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
            for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
                if (rbuf[i][j] != buf[i][j])
                    TEST_ERROR_SUPPRESSED;

        /* Reopen srcdsets and srcfile if config option specified */
        if (config & TEST_IO_CLOSE_SRC) {
            if (config & TEST_IO_DIFFERENT_FILE)
                if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                    TEST_ERROR_SUPPRESSED;
            if ((srcdset[0] = H5Dopen2(srcfile[0], "%src_dset1", H5P_DEFAULT)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2%", H5P_DEFAULT)) < 0)
                TEST_ERROR_SUPPRESSED;
        }
    }

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(srcdset[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[1] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (vfile2 >= 0 && H5Fclose(vfile2) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile2 = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 3: 2 Source datasets, hyperslab virtual mappings with offsets
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 13;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 3;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    offset[1] = -3;
    if (H5Soffset_simple(vspace[0], offset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "%%src_dset1", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    offset[1] = 10;
    if (H5Soffset_simple(vspace[0], offset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset2%%", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[1] = 26;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "%src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2%", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    offset[1] = -3;
    if (H5Soffset_simple(vspace[0], offset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    offset[1] = 10;
    if (H5Soffset_simple(vspace[0], offset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "%src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2%", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source datasets */
    memset(rbuf[0], 0, sizeof(rbuf));
    offset[1] = -3;
    if (H5Soffset_simple(vspace[0], offset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    offset[1] = 10;
    if (H5Soffset_simple(vspace[0], offset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dread(srcdset[1], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(srcdset[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[1] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;

    /*
     * Test 4: 2 Source datasets, hyperslab virtual mappings on one mapping at a
     * time, '%' in source file name
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 13;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 13;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilenamepct_map : ".",
                       "src_dset1", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilenamepct_map : ".",
                       "src_dset2", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[1] = 26;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilenamepct, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read first source dataset through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, vspace[0], vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != (j < (int)(sizeof(buf[0]) / sizeof(buf[0][0]) / 2) ? buf[i][j] : 0))
                TEST_ERROR_SUPPRESSED;

    /* Read second source dataset through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, vspace[1], vspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != (j < (int)(sizeof(buf[0]) / sizeof(buf[0][0]) / 2) ? 0 : buf[i][j]))
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write first source dataset through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, vspace[0], vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second source dataset through virtual dataset */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, vspace[1], vspace[1], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilenamepct, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source datasets */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, vspace[0], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dread(srcdset[1], H5T_NATIVE_INT, vspace[1], H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != (j < (int)(sizeof(buf[0]) / sizeof(buf[0][0]) / 2)
                                   ? (buf[i][j] - (int)(sizeof(buf) / sizeof(buf[0][0])))
                                   : buf[i][j]))
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(srcdset[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[1] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 5: 2 Source datasets, hyperslab virtual mappings and selections
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspaces */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in source space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = 13;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 13;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = 26;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 5;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[1], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    /* Write first dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update evbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            evbuf[i][j] = buf[2 * i][j];
        for (/* j = 13 */; j < 26; j++)
            evbuf[i][j] = buf[2 * i + 1][j - 13];
    }

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update evbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            evbuf[i + 5][j] = buf[2 * i][j + 13];
        for (/* j = 13 */; j < 26; j++)
            evbuf[i + 5][j] = buf[2 * i + 1][j];
    }

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset by hyperslab */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read first slice */
    if (H5Dread(vdset, H5T_NATIVE_INT, vspace[0], srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            erbuf[i][j] = evbuf[2 * i][j];
        for (/* j = 13 */; j < 26; j++)
            erbuf[i][j] = evbuf[2 * i + 1][j - 13];
    }

    /* Read second slice */
    if (H5Dread(vdset, H5T_NATIVE_INT, vspace[1], srcspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            erbuf[i + 5][j] = evbuf[2 * i][j + 13];
        for (/* j = 13 */; j < 26; j++)
            erbuf[i + 5][j] = evbuf[2 * i + 1][j];
    }

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    /* Write first slice */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, vspace[0], srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update evbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            evbuf[2 * i][j] = buf[i][j];
        for (/* j = 13 */; j < 26; j++)
            evbuf[2 * i + 1][j - 13] = buf[i][j];
    }

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second slice */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, vspace[1], srcspace[1], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update evbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            evbuf[2 * i][j + 13] = buf[i + 5][j];
        for (/* j = 13 */; j < 26; j++)
            evbuf[2 * i + 1][j] = buf[i + 5][j];
    }

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source datasets */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read first dataset */
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, srcspace[0], srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            erbuf[2 * i][j] = evbuf[i][j];
        for (/* j = 13 */; j < 26; j++)
            erbuf[2 * i + 1][j - 13] = evbuf[i][j];
    }

    /* Read second dataset */
    if (H5Dread(srcdset[1], H5T_NATIVE_INT, srcspace[1], srcspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 13; j++)
            erbuf[2 * i][j + 13] = evbuf[i + 5][j];
        for (/* j = 13 */; j < 26; j++)
            erbuf[2 * i + 1][j] = evbuf[i + 5][j];
    }

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(srcdset[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[1] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[1] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 6: 2 Source datasets, checkerboard/stripe pattern to trigger
     * sequence list refresh internally
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory dataspace */
    if ((memspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[1] = 52;
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace and file space for second operation (srcspace[1])
     */
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[1] = 26;

    /* Select hyperslabs (stripe) in source space and file space for second
     * operation (srcspace[1]) */
    start[0]  = 0;
    start[1]  = 0;
    stride[0] = 1;
    stride[1] = 2;
    count[0]  = 1;
    count[1]  = 26;
    block[0]  = 10;
    block[1]  = 1;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 1;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs (checkerboard) in virtual spaces */
    start[0]  = 0;
    start[1]  = 0;
    stride[0] = 2;
    stride[1] = 2;
    count[0]  = 5;
    count[1]  = 26;
    block[0]  = 1;
    block[1]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 1;
    start[1] = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 1;
    start[1] = 0;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Write data directly to source datasets */
    /* Write first dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i += 2)
        for (j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 1; i < 10; i += 2)
        for (j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset by hyperslab */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read first stripe pattern */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Read second stripe pattern */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, srcspace[1], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i += 2)
        for (j = 0; j < 26; j++) {
            erbuf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));
            erbuf[i + 1][j] -= (int)(sizeof(buf) / sizeof(buf[0][0]));
        }

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset */
    /* Write first slice */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i += 2)
        for (j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write second slice */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, memspace, srcspace[1], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 1; i < 10; i += 2)
        for (j = 0; j < 26; j++)
            erbuf[i][j] = buf[i][j];

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source datasets */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read first dataset */
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i += 2)
        for (j = 0; j < 26; j++) {
            erbuf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));
            erbuf[i + 1][j] -= (int)(sizeof(buf) / sizeof(buf[0][0]));
        }

    /* Read second dataset */
    if (H5Dread(srcdset[1], H5T_NATIVE_INT, memspace, srcspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(srcdset[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[1] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[1] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    /*
     * Test 7: 1 Source dataset, two mappings, 4 dimensional virtual dataset
     * and 3 dimensional source dataset
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory dataspace */
    if ((memspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 3;
    dims[1] = 3;
    dims[2] = 3;
    dims[3] = 3;
    if ((vspace[0] = H5Screate_simple(4, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(4, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspaces */
    dims[0] = 2;
    dims[1] = 4;
    dims[2] = 4;
    if ((srcspace[0] = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcspace[1] = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[0] = 10;
    dims[1] = 26;

    /* Select hyperslabs (stripes) in source spaces */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 0;
    stride[0] = 1;
    stride[1] = 2;
    stride[2] = 1;
    count[0]  = 1;
    count[1]  = 2;
    count[2]  = 1;
    block[0]  = 2;
    block[1]  = 1;
    block[2]  = 4;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 1;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs (corners) in first virtual space */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 0;
    start[3]  = 0;
    stride[0] = 2;
    stride[1] = 2;
    stride[2] = 2;
    stride[3] = 2;
    count[0]  = 2;
    count[1]  = 2;
    count[2]  = 2;
    count[3]  = 2;
    block[0]  = 1;
    block[1]  = 1;
    block[2]  = 1;
    block[3]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs ("+" pattern) in second virtual space */
    start[0]  = 1;
    start[1]  = 1;
    start[2]  = 0;
    start[3]  = 0;
    stride[0] = 2;
    stride[1] = 2;
    stride[2] = 2;
    stride[3] = 2;
    count[0]  = 1;
    count[1]  = 1;
    count[2]  = 2;
    count[3]  = 2;
    block[0]  = 1;
    block[1]  = 1;
    block[2]  = 1;
    block[3]  = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    start[2] = 1;
    count[1] = 2;
    count[2] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;
    start[1] = 1;
    count[0] = 2;
    count[1] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 1;
    count[0] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    start[3] = 1;
    count[1] = 2;
    count[3] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_OR, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source dataset */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 2;
    count[1] = 16;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 9;
    count[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset by hyperslab */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read first stripe pattern */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    memset(erbuf, 0, sizeof(erbuf));
    for (i = 0; i < 9; i++)
        for (j = 0; j < 3; j++)
            erbuf[i][j] = fill;
    erbuf[0][0] = buf[0][0];
    erbuf[0][2] = buf[0][1];
    erbuf[2][0] = buf[0][8];
    erbuf[2][2] = buf[0][9];
    erbuf[6][0] = buf[1][0];
    erbuf[6][2] = buf[1][1];
    erbuf[8][0] = buf[1][8];
    erbuf[8][2] = buf[1][9];
    erbuf[4][0] = buf[0][13];
    erbuf[4][2] = buf[0][14];

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 1;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 3;
    count[0] = 9;
    count[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read second stripe pattern */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 9; i++)
        for (j = 3; j < 6; j++)
            erbuf[i][j] = fill;
    erbuf[1][3] = buf[0][4];
    erbuf[1][5] = buf[0][5];
    erbuf[3][3] = buf[0][6];
    erbuf[3][4] = buf[0][7];
    erbuf[3][5] = buf[0][12];
    erbuf[4][3] = buf[0][15];
    erbuf[4][5] = buf[1][4];
    erbuf[5][3] = buf[1][7];
    erbuf[5][4] = buf[1][12];
    erbuf[5][5] = buf[1][13];
    erbuf[7][3] = buf[1][14];
    erbuf[7][5] = buf[1][15];

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if ((j >= 3) && (j < 6)) {
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != 0)
                TEST_ERROR_SUPPRESSED;

    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select stripe */
    start[0] = 0;
    start[1] = 0;
    start[2] = 2;
    start[3] = 0;
    count[0] = 3;
    count[1] = 3;
    count[2] = 1;
    count[3] = 3;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 6;
    count[0] = 9;
    count[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read third stripe pattern */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 9; i++)
        for (j = 6; j < 9; j++)
            erbuf[i][j] = fill;
    erbuf[0][6] = buf[0][2];
    erbuf[0][8] = buf[0][3];
    erbuf[2][6] = buf[0][10];
    erbuf[2][8] = buf[0][11];
    erbuf[6][6] = buf[1][2];
    erbuf[6][8] = buf[1][3];
    erbuf[8][6] = buf[1][10];
    erbuf[8][8] = buf[1][11];
    erbuf[4][6] = buf[1][5];
    erbuf[4][8] = buf[1][6];

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if ((j >= 6) && (j < 9)) {
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != 0)
                TEST_ERROR_SUPPRESSED;

    /* Now read entire VDS */
    /* Set memory space extent to 9x9, select all in order to reach part of the
     * code in H5S_select_subtract() */
    dims[0] = 9;
    dims[1] = 9;
    if (H5Sset_extent_simple(memspace, 2, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_all(memspace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read third stripe pattern */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf99[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(rbuf99) / sizeof(rbuf99[0])); i++)
        for (j = 0; j < (int)(sizeof(rbuf99[0]) / sizeof(rbuf99[0][0])); j++)
            if (rbuf99[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Write data through virtual dataset by hyperslab */
    /* Select stripe (only select mapped elements) */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 0;
    start[3]  = 0;
    stride[0] = 2;
    stride[1] = 2;
    stride[2] = 1;
    stride[3] = 2;
    count[0]  = 2;
    count[1]  = 2;
    count[2]  = 1;
    count[3]  = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0]  = 1;
    start[1]  = 1;
    start[2]  = 0;
    start[3]  = 0;
    stride[0] = 1;
    stride[1] = 1;
    stride[2] = 1;
    stride[3] = 2;
    count[0]  = 1;
    count[1]  = 1;
    count[2]  = 1;
    count[3]  = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, stride, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset extent of memspace, select hyperslab */
    dims[0] = 10;
    dims[1] = 26;
    if (H5Sset_extent_simple(memspace, 2, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 10;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data through virtual dataset by hyperslab */
    /* Write first stripe pattern */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    memset(erbuf, 0, sizeof(erbuf));
    erbuf[0][0]  = buf[0][0];
    erbuf[0][1]  = buf[0][1];
    erbuf[0][8]  = buf[0][2];
    erbuf[0][9]  = buf[0][3];
    erbuf[1][0]  = buf[0][6];
    erbuf[1][1]  = buf[0][7];
    erbuf[1][8]  = buf[0][8];
    erbuf[1][9]  = buf[0][9];
    erbuf[0][13] = buf[0][4];
    erbuf[0][14] = buf[0][5];

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Select stripe (only select mapped elements) */
    start[0]  = 0;
    start[1]  = 1;
    start[2]  = 1;
    start[3]  = 0;
    stride[0] = 1;
    stride[1] = 1;
    stride[2] = 1;
    stride[3] = 2;
    count[0]  = 3;
    count[1]  = 1;
    count[2]  = 1;
    count[3]  = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0]  = 1;
    start[1]  = 0;
    start[2]  = 1;
    start[3]  = 0;
    stride[0] = 1;
    stride[1] = 2;
    stride[2] = 1;
    stride[3] = 1;
    count[0]  = 1;
    count[1]  = 2;
    count[2]  = 1;
    count[3]  = 3;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, stride, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 12;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write second slice */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    erbuf[0][4]  = buf[0][0];
    erbuf[0][5]  = buf[0][1];
    erbuf[0][6]  = buf[0][2];
    erbuf[0][7]  = buf[0][3];
    erbuf[0][12] = buf[0][4];
    erbuf[0][15] = buf[0][5];
    erbuf[1][4]  = buf[0][6];
    erbuf[1][7]  = buf[0][7];
    erbuf[1][12] = buf[0][8];
    erbuf[1][13] = buf[0][9];
    erbuf[1][14] = buf[0][10];
    erbuf[1][15] = buf[0][11];

    /* Adjust write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] += (int)(sizeof(buf) / sizeof(buf[0][0]));

    /* Select stripe (only select mapped elements) */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 2;
    start[3]  = 0;
    stride[0] = 2;
    stride[1] = 2;
    stride[2] = 1;
    stride[3] = 2;
    count[0]  = 2;
    count[1]  = 2;
    count[2]  = 1;
    count[3]  = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0]  = 1;
    start[1]  = 1;
    start[2]  = 2;
    start[3]  = 0;
    stride[0] = 1;
    stride[1] = 1;
    stride[2] = 1;
    stride[3] = 2;
    count[0]  = 1;
    count[1]  = 1;
    count[2]  = 1;
    count[3]  = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_OR, start, stride, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 10;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write third slice */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    erbuf[0][2]  = buf[0][0];
    erbuf[0][3]  = buf[0][1];
    erbuf[0][10] = buf[0][2];
    erbuf[0][11] = buf[0][3];
    erbuf[1][2]  = buf[0][6];
    erbuf[1][3]  = buf[0][7];
    erbuf[1][10] = buf[0][8];
    erbuf[1][11] = buf[0][9];
    erbuf[1][5]  = buf[0][4];
    erbuf[1][6]  = buf[0][5];

    /* Reopen srcdset and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDONLY, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read data directly from source dataset */
    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 2;
    count[1] = 16;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read dataset */
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Now try writing to whole VDS (should fail due to unmapped elements) */
    count[0] = 9;
    count[1] = 9;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    H5E_BEGIN_TRY
    {
        ret = H5Dwrite(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[1] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    /*
     * Test 8: For code coverage: Horizontal block virtual mappings, and file
     * selection, grid memory selection
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory dataspace */
    if ((memspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 8;
    dims[1] = 15;
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[0] = 4;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 4;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[0] = 10;
    dims[1] = 26;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset erbuf */
    memset(erbuf[0], 0, sizeof(rbuf));

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Select hyperslab in memory */
    start[0] = 0;
    start[1] = 0;
    count[0] = 4;
    count[1] = 15;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to first source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    u = 0;
    v = 0;
    for (i = 2; i < 4; i++)
        for (j = 0; j < 15; j++) {
            erbuf[u][v] = buf[i][j];
            v += 2;
            if (v >= 24) {
                u += 2;
                v = 0;
            }
        }

    /* Select hyperslab in memory */
    start[0] = 4;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to second source dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 4; i < 6; i++)
        for (j = 0; j < 15; j++) {
            erbuf[u][v] = buf[i][j];
            v += 2;
            if (v >= 24) {
                u += 2;
                v = 0;
            }
        }

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Select hyperslab in memory */
    start[0]  = 0;
    start[1]  = 0;
    stride[0] = 2;
    stride[1] = 2;
    count[0]  = 5;
    count[1]  = 12;
    block[0]  = 1;
    block[1]  = 1;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in file */
    start[0] = 2;
    start[1] = 0;
    count[0] = 4;
    count[1] = 15;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    /*
     * Test 9: For code coverage: Horizontal block virtual mappings, and file
     * selection, grid memory selection, 3 mappings, 3D memory space
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory dataspace */
    dims[1] = 13;
    dims[2] = 2;
    if ((memspace = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 6;
    dims[1] = 10;
    if ((vspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[2] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[0] = 2;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 2;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 4;
    if (H5Sselect_hyperslab(vspace[2], H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[2], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset3",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[0] = 10;
    dims[1] = 26;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[2] = H5Dcreate2(srcfile[0], "src_dset3", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset erbuf */
    memset(erbuf[0], 0, sizeof(rbuf));

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Select hyperslab in memory */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    count[0] = 2;
    count[1] = 5;
    count[2] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to first source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    u = 0;
    v = 0;
    for (i = 0; i < 2; i++)
        for (j = 0; j < 10; j++) {
            erbuf[u][v] = buf[i][j];
            if (++v == 6)
                v += 2;
            else if (v == 14) {
                u += 2;
                v = 0;
            }
        }

    /* Select hyperslab in memory */
    start[0] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to second source dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 2; i < 4; i++)
        for (j = 0; j < 10; j++) {
            erbuf[u][v] = buf[i][j];
            if (++v == 6)
                v += 2;
            else if (v == 14) {
                u += 2;
                v = 0;
            }
        }

    /* Select hyperslab in memory */
    start[0] = 4;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to third source dataset */
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 4; i < 6; i++)
        for (j = 0; j < 10; j++) {
            erbuf[u][v] = buf[i][j];
            if (++v == 6)
                v += 2;
            else if (v == 14) {
                u += 2;
                v = 0;
            }
        }

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Select hyperslab in memory */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 0;
    stride[0] = 2;
    stride[1] = 4;
    stride[2] = 1;
    count[0]  = 5;
    count[1]  = 2;
    count[2]  = 1;
    block[0]  = 1;
    block[1]  = 3;
    block[2]  = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;
    if (H5Sclose(vspace[2]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[2] = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    /*
     * Test 10: For code coverage: Vertical stripe virtual mappings, vertical
     * block file selection, block memory selection, 3D VDS
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory dataspace */
    if ((memspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 10;
    dims[1] = 9;
    dims[2] = 6;
    if ((vspace[0] = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(3, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 12;
    if ((srcspace[0] = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select all in source space (should not be necessary, but just to be sure)
     */
    if (H5Sselect_all(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[0]  = 0;
    start[1]  = 0;
    start[2]  = 0;
    count[0]  = 1;
    count[1]  = 4;
    count[2]  = 3;
    stride[0] = 1;
    stride[1] = 2;
    stride[2] = 2;
    block[0]  = 10;
    block[1]  = 1;
    block[2]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[2] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reset dims */
    dims[0] = 10;
    dims[1] = 26;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Initialize erbuf */
    memset(erbuf[0], 0, sizeof(rbuf));
    for (i = 0; i < 10; i++)
        for (j = 0; j < 24; j += 6) {
            erbuf[i][j]     = -1;
            erbuf[i][j + 1] = -1;
        }

    /* Populate write buffer */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            buf[i][j] = (i * (int)(sizeof(buf[0]) / sizeof(buf[0][0]))) + j;

    /* Select hyperslab in memory */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = 12;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to first source dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    u = 0;
    v = 0;
    for (i = 0; i < 10; i++)
        for (j = 0; j < 8; j++) {
            if (v == 0 || v == 12)
                erbuf[u][v] = buf[i][j];
            v += 2;
            if (!(v % 6))
                v += 6;
            if (v >= 28) {
                u++;
                v = 0;
            }
        }

    /* Select hyperslab in memory */
    start[1] = 8;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write data directly to second source dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    u = 0;
    v = 1;
    for (i = 0; i < 10; i++)
        for (j = 8; j < 16; j++) {
            if (v == 1 || v == 13)
                erbuf[u][v] = buf[i][j];
            v += 2;
            if (!((v - 1) % 6))
                v += 6;
            if (v >= 28) {
                u++;
                v = 1;
            }
        }

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Select hyperslab in memory */
    start[0]  = 0;
    start[1]  = 0;
    stride[0] = 1;
    stride[1] = 6;
    count[0]  = 1;
    count[1]  = 4;
    block[0]  = 10;
    block[1]  = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in file */
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    count[0] = 10;
    count[1] = 4;
    count[2] = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    memset(rbuf[0], 0, sizeof(rbuf));
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, vspace[0], H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)(sizeof(buf) / sizeof(buf[0])); i++)
        for (j = 0; j < (int)(sizeof(buf[0]) / sizeof(buf[0][0])); j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    /* Close */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    dcpl = -1;

    free(srcfilename);
    free(srcfilename_map);
    free(vfilename);
    free(vfilename2);
    free(srcfilenamepct);
    free(srcfilenamepct_map);

    PASSED_SUPPRESSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++)
            H5Dclose(srcdset[i]);
        H5Dclose(vdset);
        for (i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++)
            H5Fclose(srcfile[i]);
        H5Fclose(vfile);
        H5Fclose(vfile2);
        for (i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++)
            H5Sclose(srcspace[i]);
        for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Sclose(memspace);
        H5Pclose(dcpl);
    }
    H5E_END_TRY

    free(srcfilename);
    free(srcfilename_map);
    free(vfilename);
    free(vfilename2);
    free(srcfilenamepct);
    free(srcfilenamepct_map);

    return 1;
} /* end test_basic_io() */

/*-------------------------------------------------------------------------
 * Function:    test_unlim
 *
 * Purpose:     Tests VDS with unlimited selections
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *-------------------------------------------------------------------------
 */
static int
test_unlim(unsigned config, hid_t vds_fapl, hid_t src_fapl)
{
    char           srcfilename[FILENAME_BUF_SIZE];
    char           srcfilename_map[FILENAME_BUF_SIZE];
    char           vfilename[FILENAME_BUF_SIZE];
    hid_t          srcfile[4]  = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Files with source dsets */
    hid_t          vfile       = H5I_INVALID_HID;   /* File with virtual dset */
    hid_t          dcpl        = H5I_INVALID_HID;   /* Dataset creation property list */
    hid_t          srcdcpl     = H5I_INVALID_HID;   /* DCPL for source dset */
    hid_t          dapl        = H5I_INVALID_HID;   /* Dataset access property list */
    hid_t          srcspace[4] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                         H5I_INVALID_HID}; /* Source dataspaces */
    hid_t          vspace[4]   = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                       H5I_INVALID_HID}; /* Virtual dset dataspaces */
    hid_t          memspace    = H5I_INVALID_HID;   /* Memory dataspace */
    hid_t          filespace   = H5I_INVALID_HID;   /* File dataspace */
    hid_t          srcdset[4]  = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Source datasets */
    hid_t          vdset       = H5I_INVALID_HID;   /* Virtual dataset */
    hsize_t        dims[2]     = {10, 10};          /* Data space current size */
    hsize_t        mdims[2]    = {10, 20};          /* Data space maximum size */
    hsize_t        cdims[2]    = {4, 4};            /* Chunk dimensions */
    hsize_t        start[4];                        /* Hyperslab start */
    hsize_t        stride[4];                       /* Hyperslab stride */
    hsize_t        count[4];                        /* Hyperslab count */
    hsize_t        block[4];                        /* Hyperslab block */
    int            buf[10][20];                     /* Write and expected read buffer */
    int            rbuf[10][20];                    /* Read buffer */
    int            erbuf[10][20];                   /* Expected read buffer */
    int            ndims;                           /* Number of dimensions */
    int            fill = -1;                       /* Fill value */
    H5D_vds_view_t virtual_view;                    /* Virtual view property */
    int            i, j;

    TESTING_2_SUPPRESSED("virtual dataset I/O with unlimited selections");

    h5_fixname(FILENAME[0], vds_fapl, vfilename, sizeof vfilename);
    h5_fixname(FILENAME[2], src_fapl, srcfilename, sizeof srcfilename);
    h5_fixname_printf(FILENAME[2], src_fapl, srcfilename_map, sizeof srcfilename_map);

    /* Create DCPLs */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set fill value */
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set chunk dimensions */
    if (H5Pset_chunk(srcdcpl, 2, cdims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create DAPL */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory space */
    if ((memspace = H5Screate_simple(2, mdims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /*
     * Test 1: 2 Source datasets, single unlimited hyperslab virtual mappings
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[0]  = 5;
    mdims[0] = 5;
    if ((srcspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    mdims[0] = 10;

    /* Select hyperslab in source space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = H5S_UNLIMITED;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 5;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write data directly to source datasets */
    /* Select hyperslab in memory */
    start[0] = 0;
    count[1] = 10;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write first dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 10; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write second dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 10; j++)
            erbuf[i + 5][j] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Test H5Pget_virtual_view() */
    if (H5Pget_virtual_view(dapl, &virtual_view) < 0)
        TEST_ERROR_SUPPRESSED;
    if (virtual_view != H5D_VDS_LAST_AVAILABLE)
        TEST_ERROR_SUPPRESSED;

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Test H5Pget_virtual_view() */
    if (H5Pget_virtual_view(dapl, &virtual_view) < 0)
        TEST_ERROR_SUPPRESSED;
    if (virtual_view != H5D_VDS_FIRST_MISSING)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[0] */
    dims[0] = 5;
    dims[1] = 15;
    if (H5Dset_extent(srcdset[0], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[0])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 10;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING and we only extended one source dataset the
     * dimensions will not have changed. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf to reflect new data that is now visible due to the change to
     * H5D_VDS_LAST_AVAILABLE */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j + 10] = buf[i][j];

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 15)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[1] */
    dims[0] = 5;
    dims[1] = 20;
    if (H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 10;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 10;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 10; j++)
            erbuf[i + 5][j + 10] = buf[i][j];

    /* Close srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 20)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Now just read middle 2 rows */
    memset(rbuf[0], 0, sizeof(rbuf));
    start[0] = 4;
    count[0] = 2;
    count[1] = 20;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, memspace, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data - algorithmically check for only 2 middle rows being
     * read so we don't have to wipe out erbuf and then restore it afterwards */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            if ((i == 4) || (i == 5)) {
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != 0)
                TEST_ERROR_SUPPRESSED;

    /* Now test reopening virtual dataset without calling H5Dget_space, if
     * REOPEN_VIRT flag set */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        start[0] = 0;
        start[1] = 0;
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;

        /* Now try setting extent manually */
        /* Shrink to 18 */
        dims[1] = 18;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        start[0] = 0;
        start[1] = 0;
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Shrink to 15 */
        dims[1] = 15;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        start[0] = 0;
        start[1] = 0;
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
    }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 15)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Now test reopening virtual dataset without calling H5Dget_space, if
     * REOPEN_VIRT flag set */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        start[1] = 0;
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Now try setting extent manually */
        /* Grow to 18 */
        dims[1] = 18;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        start[0] = 0;
        start[1] = 0;
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Grow to 20 */
        dims[1] = 20;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        start[0] = 0;
        start[1] = 0;
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
    }

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 2: 2 Source datasets, interleaved slices, single element wide
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 10;
    dims[1] = 10;
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1]  = 5;
    mdims[1] = 10;
    if ((srcspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    mdims[1] = 20;

    /* Select hyperslab in source space */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = H5S_UNLIMITED;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    stride[0] = 1;
    stride[1] = 2;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 10;
    block[1]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write data directly to source datasets */
    /* Select hyperslab in memory */
    count[0] = 10;
    count[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write first dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][2 * j] = buf[i][j];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write second dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][(2 * j) + 1] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[0] */
    dims[1] = 7;
    if (H5Dset_extent(srcdset[0], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[0])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 5;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf to reflect only new data that is now visible under
     * H5D_VDS_FIRST_MISSING (first slice) */
    for (i = 0; i < 10; i++)
        erbuf[i][10] = buf[i][0];

    /* Close srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING and we only extended one source dataset the
     * dimension will only have changed to add one more slice. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 11)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf to reflect new data that is now visible due to the change to
     * H5D_VDS_LAST_AVAILABLE (second new slice) */
    for (i = 0; i < 10; i++)
        erbuf[i][12] = buf[i][1];

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 13)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[1] */
    dims[1] = 10;
    if (H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 5;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][(2 * j) + 11] = buf[i][j];

    /* Close srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 20)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Now just read middle 2 rows */
    memset(rbuf[0], 0, sizeof(rbuf));
    start[0] = 4;
    count[0] = 2;
    count[1] = 20;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, memspace, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data - algorithmically check for only 2 middle rows being
     * read so we don't have to wipe out erbuf and then restore it afterwards */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            if ((i == 4) || (i == 5)) {
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != 0)
                TEST_ERROR_SUPPRESSED;

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf to reflect new data that is no longer visible due to the
     * change to H5D_VDS_FIRST_MISSING */
    for (i = 0; i < 10; i++)
        for (j = 15; j < 20; j += 2)
            erbuf[i][j] = fill;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 14)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 3: 3 Source datasets, interleaved slices, two elements wide
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 10;
    dims[1] = 10;
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[2] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspaces */
    dims[1]  = 4;
    mdims[1] = 8;
    if ((srcspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    dims[1]  = 4;
    mdims[1] = 6;
    if ((srcspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    dims[1]  = 2;
    mdims[1] = 6;
    if ((srcspace[2] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    mdims[1] = 20;

    /* Select hyperslab in source spaces */
    start[0] = 0;
    start[1] = 0;
    count[0] = 10;
    count[1] = H5S_UNLIMITED;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_hyperslab(srcspace[2], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    stride[0] = 1;
    stride[1] = 6;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 10;
    block[1]  = 2;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 2;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 4;
    if (H5Sselect_hyperslab(vspace[2], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[2], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset3",
                       srcspace[2]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[1], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[2] = H5Dcreate2(srcfile[0], "src_dset3", H5T_NATIVE_INT, srcspace[2], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write data directly to source datasets */
    /* Select hyperslab in memory */
    count[0] = 10;
    count[1] = 4;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write first dataset */
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 2; j++) {
            erbuf[i][6 * j]       = buf[i][2 * j];
            erbuf[i][(6 * j) + 1] = buf[i][(2 * j) + 1];
        }

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write second dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 2; j++) {
            erbuf[i][(6 * j) + 2] = buf[i][2 * j];
            erbuf[i][(6 * j) + 3] = buf[i][(2 * j) + 1];
        }

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Select hyperslab in memory */
    count[0] = 10;
    count[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write third dataset */
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++) {
        erbuf[i][4] = buf[i][0];
        erbuf[i][5] = buf[i][1];
    }

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[0] */
    dims[1] = 7;
    if (H5Dset_extent(srcdset[0], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[0])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 4;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Note that since we are using
     * H5D_VDS_FIRST_MISSING the size will not have changed. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf to reflect new data that is now visible due to the change to
     * H5D_VDS_LAST_AVAILABLE */
    for (i = 0; i < 10; i++) {
        erbuf[i][12] = buf[i][0];
        erbuf[i][13] = buf[i][1];
        erbuf[i][18] = buf[i][2];
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 19)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[2] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[2] = H5Dopen2(srcfile[0], "src_dset3", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[2] */
    dims[1] = 5;
    if (H5Dset_extent(srcdset[2], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[2])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 2;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++) {
        erbuf[i][10] = buf[i][0];
        erbuf[i][11] = buf[i][1];
        erbuf[i][16] = buf[i][2];
    }

    /* Close srcdset[2] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Note that the dimensions will not have
     * changed. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 19)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 14)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[1] */
    dims[1] = 6;
    if (H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset */
    count[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 4;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++) {
        erbuf[i][14] = buf[i][0];
        erbuf[i][15] = buf[i][1];
    }

    /* Close srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 17)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 19)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Now just read middle 2 rows */
    memset(rbuf[0], 0, sizeof(rbuf));
    start[0] = 4;
    count[0] = 2;
    count[1] = 19;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data - algorithmically check for only 2 middle rows being
     * read */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if ((i == 4) || (i == 5)) {
                if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != 0)
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[1] = -1;
    if (H5Sclose(srcspace[2]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[2] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 4: 2 Source datasets, offset starts
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspaces */
    dims[0]  = 5;
    dims[1]  = 0;
    mdims[0] = 5;
    if ((srcspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    dims[1] = 5;
    if ((srcspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    mdims[0] = 10;

    /* Select hyperslab in source spaces */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = H5S_UNLIMITED;
    if (H5Sselect_hyperslab(srcspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    start[1] = 10;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 5;
    start[1] = 0;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset1",
                       srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset2",
                       srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace[1], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write data directly to second source dataset */
    /* Select hyperslab in memory */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Write second dataset */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            erbuf[i + 5][j] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 5)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 5)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[0] */
    dims[0] = 5;
    dims[1] = 5;
    if (H5Dset_extent(srcdset[0], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[0] */
    start[0] = 0;
    start[1] = 0;
    count[0] = 5;
    count[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j + 10] = buf[i][j];

    /* Close srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 5)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 15)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[0] = -1;
    if (H5Sclose(srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace[1] = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /* Close */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    dcpl = -1;
    if (H5Pclose(srcdcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    dcpl = -1;
    if (H5Pclose(dapl) < 0)
        TEST_ERROR_SUPPRESSED;
    dapl = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    PASSED_SUPPRESSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++)
            H5Dclose(srcdset[i]);
        H5Dclose(vdset);
        for (i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++)
            H5Fclose(srcfile[i]);
        H5Fclose(vfile);
        for (i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++)
            H5Sclose(srcspace[i]);
        for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Sclose(filespace);
        H5Sclose(memspace);
        H5Pclose(dcpl);
        H5Pclose(srcdcpl);
        H5Pclose(dapl);
    }
    H5E_END_TRY

    return 1;
} /* end test_unlim() */

/*-------------------------------------------------------------------------
 * Function:    test_printf
 *
 * Purpose:     Tests VDS with unlimited selections and printf style
 *              source dataset resolution
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *-------------------------------------------------------------------------
 */
static int
test_printf(unsigned config, hid_t vds_fapl, hid_t src_fapl)
{
    char       *srcfilename                 = NULL;
    char       *srcfilename_map             = NULL;
    char       *srcfilename2                = NULL;
    char       *srcfilename2_map            = NULL;
    char       *vfilename                   = NULL;
    char       *printf_srcfilename_map      = NULL;
    char       *srcfilenamepct              = NULL;
    char       *srcfilenamepct_map          = NULL;
    const char *printf_srcfilename_map_orig = "vds_src_%b";
    const char *srcfilenamepct_map_orig     = "vds%%%%_src";
    hid_t       srcfile[4]                  = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Files with source dsets */
    hid_t       vfile                       = H5I_INVALID_HID;   /* File with virtual dset */
    hid_t       dcpl                        = H5I_INVALID_HID;   /* Dataset creation property list */
    hid_t       dapl                        = H5I_INVALID_HID;   /* Dataset access property list */
    hid_t       srcspace                    = H5I_INVALID_HID;   /* Source dataspace */
    hid_t       vspace[2]  = {H5I_INVALID_HID, H5I_INVALID_HID}; /* Virtual dset dataspaces */
    hid_t       memspace   = H5I_INVALID_HID;                    /* Memory dataspace */
    hid_t       filespace  = H5I_INVALID_HID;                    /* File dataspace */
    hid_t       srcdset[6] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID}; /* Source datasets */
    hid_t       vdset      = H5I_INVALID_HID;                                     /* Virtual dataset */
    hsize_t     dims[2]    = {10, 0};  /* Data space current size */
    hsize_t     mdims[2]   = {10, 20}; /* Data space maximum size */
    hsize_t     start[2]   = {0, 0};   /* Hyperslab start */
    hsize_t     stride[2];             /* Hyperslab stride */
    hsize_t     count[2];              /* Hyperslab count */
    hsize_t     block[2];              /* Hyperslab block */
    int         buf[10][20];           /* Write and expected read buffer */
    int         rbuf[10][20];          /* Read buffer */
    int         erbuf[10][20];         /* Expected read buffer */
    int         ndims;                 /* Number of dimensions */
    int         fill = -1;             /* Fill value */
    hsize_t     gap_size;              /* Gap size property */
    int         i, j;

    TESTING_2_SUPPRESSED("virtual dataset I/O with printf source");

    if ((srcfilename = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilename_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilename2 = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilename2_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((vfilename = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((printf_srcfilename_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilenamepct = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;
    if ((srcfilenamepct_map = (char *)calloc(FILENAME_BUF_SIZE, sizeof(char))) == NULL)
        TEST_ERROR_SUPPRESSED;

    h5_fixname(FILENAME[0], vds_fapl, vfilename, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[2], src_fapl, srcfilename, FILENAME_BUF_SIZE);
    h5_fixname_printf(FILENAME[2], src_fapl, srcfilename_map, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[3], src_fapl, srcfilename2, FILENAME_BUF_SIZE);
    h5_fixname_printf(FILENAME[2], src_fapl, srcfilename2_map, FILENAME_BUF_SIZE);
    h5_fixname_printf(printf_srcfilename_map_orig, src_fapl, printf_srcfilename_map, FILENAME_BUF_SIZE);
    h5_fixname(FILENAME[4], src_fapl, srcfilenamepct, FILENAME_BUF_SIZE);
    h5_fixname_printf(srcfilenamepct_map_orig, src_fapl, srcfilenamepct_map, FILENAME_BUF_SIZE);

    /* Create DCPL */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set fill value */
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create DAPL */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory space */
    if ((memspace = H5Screate_simple(2, mdims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /*
     * Test 1: 1 Source dataset mapping, 10x5 blocks
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspace */
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 5;
    if ((srcspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual space */
    stride[0] = 1;
    stride[1] = 5;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 10;
    block[1]  = 5;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".", "src_dset%b",
                       srcspace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 0)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

    /* Create 2 source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset0", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write to srcdset[0] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[1] */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j + 5] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create 3rd source dataset */
    if ((srcdset[2] = H5Dcreate2(srcfile[0], "src_dset2", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[2] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j + 10] = buf[i][j];

    /* Close srcdset[2] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 15)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Now try with different selections */
    count[0] = 10;
    for (start[1] = (hsize_t)0; start[1] < (hsize_t)5; start[1]++)
        for (count[1] = (hsize_t)1; count[1] < (hsize_t)11; count[1]++) {
            /* Reset rbuf */
            memset(rbuf[0], 0, sizeof(rbuf));

            /* Select hyperslab in memory space */
            if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
                TEST_ERROR_SUPPRESSED;

            /* Select hyperslab in file space */
            if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
                TEST_ERROR_SUPPRESSED;

            /* Read data */
            if (H5Dread(vdset, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, rbuf[0]) < 0)
                TEST_ERROR_SUPPRESSED;

            /* Verify read data */
            for (i = 0; i < (int)mdims[0]; i++)
                for (j = 0; j < (int)mdims[1]; j++) {
                    if ((j < (int)start[1]) || (j >= (int)(start[1] + count[1]))) {
                        if (rbuf[i][j] != 0)
                            TEST_ERROR_SUPPRESSED;
                    }
                    else if (rbuf[i][j] != erbuf[i][j])
                        TEST_ERROR_SUPPRESSED;
                }
        }
    start[1] = 0;

    /* Now try writing through VDS */
    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in file space */
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write data through VDS */
    if (H5Dwrite(vdset, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dopen2(srcfile[0], "src_dset0", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile[0], "src_dset1", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[2] = H5Dopen2(srcfile[0], "src_dset2", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Read srcdset[0] */
    count[0] = 10;
    count[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dread(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            if (rbuf[i][j] != buf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Read srcdset[1] */
    if (H5Dread(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            if (rbuf[i][j] != buf[i][j + 5])
                TEST_ERROR_SUPPRESSED;

    /* Read srcdset[2] */
    if (H5Dread(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 5; j++)
            if (rbuf[i][j] != buf[i][j + 10])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (H5Dclose(srcdset[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[0] = -1;
    if (H5Dclose(srcdset[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[1] = -1;
    if (H5Dclose(srcdset[2]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdset[2] = -1;
    if (H5Fclose(srcfile[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    srcfile[0] = -1;
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;

    /*
     * Test 2: 1 Source dataset mapping, 10x1 blocks, test printf gap setting,
     * '%' in source file name
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 1;
    if ((srcspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual space */
    stride[0] = 1;
    stride[1] = 1;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 10;
    block[1]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilenamepct_map : ".",
                       "src_dset%b", srcspace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilenamepct, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 0)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilenamepct, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

    /* Create source datasets in a pattern with increasing gaps:
     * XX-X--X---X----X */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset0", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset1", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[2] = H5Dcreate2(srcfile[0], "src_dset3", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[3] = H5Dcreate2(srcfile[0], "src_dset6", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[4] = H5Dcreate2(srcfile[0], "src_dset10", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[5] = H5Dcreate2(srcfile[0], "src_dset15", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write to srcdset[0] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][0] = buf[i][0];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[1] */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][1] = buf[i][0];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[2] */
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][3] = buf[i][0];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[3] */
    if (H5Dwrite(srcdset[3], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][6] = buf[i][0];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[4] */
    if (H5Dwrite(srcdset[4], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][10] = buf[i][0];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[5] */
    if (H5Dwrite(srcdset[5], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][15] = buf[i][0];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        for (i = 0; i < 6; i++) {
            if (H5Dclose(srcdset[i]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[i] = -1;
        }

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 2)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Test H5Pget_virtual_printf_gap() */
    if (H5Pget_virtual_printf_gap(dapl, &gap_size) < 0)
        TEST_ERROR_SUPPRESSED;
    if (gap_size != (hsize_t)0)
        TEST_ERROR_SUPPRESSED;

    /* Close VDS and reopen with printf gap set to 1, reopen file as well if
     * config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)1) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Test H5Pget_virtual_printf_gap() */
    if (H5Pget_virtual_printf_gap(dapl, &gap_size) < 0)
        TEST_ERROR_SUPPRESSED;
    if (gap_size != (hsize_t)1)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 4)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with printf gap set to 2, reopen file as well if
     * config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)2) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Test H5Pget_virtual_printf_gap() */
    if (H5Pget_virtual_printf_gap(dapl, &gap_size) < 0)
        TEST_ERROR_SUPPRESSED;
    if (gap_size != (hsize_t)2)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 7)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with printf gap set to 3, reopen file as well if
     * config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)3) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Test H5Pget_virtual_printf_gap() */
    if (H5Pget_virtual_printf_gap(dapl, &gap_size) < 0)
        TEST_ERROR_SUPPRESSED;
    if (gap_size != (hsize_t)3)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 11)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with printf gap set to 4, reopen file as well if
     * config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)4) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Test H5Pget_virtual_printf_gap() */
    if (H5Pget_virtual_printf_gap(dapl, &gap_size) < 0)
        TEST_ERROR_SUPPRESSED;
    if (gap_size != (hsize_t)4)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 16)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 2)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reset dapl */
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)0) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        for (i = 0; i < 6; i++) {
            if (H5Dclose(srcdset[i]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[i] = -1;
        }
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;

    /* Next 2 tests are always run with a different source file, so only run if
     * that config option is set (so they're not run twice with the same
     * configuration) */
    if (config & TEST_IO_DIFFERENT_FILE) {
        /*
         * Test 3: 1 Source dataset mapping, 10x5 blocks, printf source file
         */
        /* Clean up files so the source files do not exist yet */
        H5Iinc_ref(vds_fapl); /* Prevent FAPL from being closed */
        h5_clean_files(FILENAME, vds_fapl);

        /* Clear virtual layout in DCPL */
        if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create virtual dataspaces */
        if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create source dataspace */
        dims[1] = 5;
        if ((srcspace = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Select hyperslabs in virtual space */
        stride[0] = 1;
        stride[1] = 5;
        count[0]  = 1;
        count[1]  = H5S_UNLIMITED;
        block[0]  = 10;
        block[1]  = 5;
        if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Add virtual layout mapping */
        if (H5Pset_virtual(dcpl, vspace[0], printf_srcfilename_map, "src_dset", srcspace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create virtual file */
        if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create virtual dataset */
        if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Reopen virtual dataset and file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Dclose(vdset) < 0)
                TEST_ERROR_SUPPRESSED;
            vdset = -1;
            if (H5Fclose(vfile) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile = -1;
            if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Get VDS space */
        if ((filespace = H5Dget_space(vdset)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Get VDS space dimensions */
        if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (ndims != 2)
            TEST_ERROR_SUPPRESSED;
        if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
            TEST_ERROR_SUPPRESSED;
        if (dims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (dims[1] != 0)
            TEST_ERROR_SUPPRESSED;
        if (mdims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[1] != 20)
            TEST_ERROR_SUPPRESSED;

        /* Close filespace */
        if (H5Sclose(filespace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create 2 source files, one source dataset */
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                     H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcfile[1] = H5Fcreate(srcfilename2, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Populate write buffer */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                buf[i][j] = (i * (int)mdims[1]) + j;

        /* Initialize erbuf */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                erbuf[i][j] = fill;

        /* Write to srcdset[0] */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
            TEST_ERROR_SUPPRESSED;
        if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Update erbuf */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 5; j++)
                erbuf[i][j] = buf[i][j];

        /* Close srcdset and srcfiles if config option specified */
        if (config & TEST_IO_CLOSE_SRC) {
            if (H5Dclose(srcdset[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[0] = -1;
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
            if (H5Fclose(srcfile[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[1] = -1;
        }

        /* Reopen virtual dataset and file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Dclose(vdset) < 0)
                TEST_ERROR_SUPPRESSED;
            vdset = -1;
            if (H5Fclose(vfile) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile = -1;
            if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Get VDS space */
        if ((filespace = H5Dget_space(vdset)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Get VDS space dimensions */
        if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (ndims != 2)
            TEST_ERROR_SUPPRESSED;
        if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
            TEST_ERROR_SUPPRESSED;
        if (dims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (dims[1] != 5)
            TEST_ERROR_SUPPRESSED;
        if (mdims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[1] != 20)
            TEST_ERROR_SUPPRESSED;

        /* Close filespace */
        if (H5Sclose(filespace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Reopen srcfile[1] if config option specified */
        if (config & TEST_IO_CLOSE_SRC)
            if ((srcfile[1] = H5Fopen(srcfilename2, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

        /* Create 2nd source dataset */
        if ((srcdset[1] = H5Dcreate2(srcfile[1], "src_dset", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                     H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Adjust write buffer */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                buf[i][j] += (int)mdims[0] * (int)mdims[1];

        /* Write to srcdset[1] */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
            TEST_ERROR_SUPPRESSED;
        if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Update erbuf */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 5; j++)
                erbuf[i][j + 5] = buf[i][j];

        /* Close srcdset[1] and srcfile[1] if config option specified */
        if (config & TEST_IO_CLOSE_SRC) {
            if (H5Dclose(srcdset[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[1] = -1;
            if (H5Fclose(srcfile[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[1] = -1;
        }

        /* Reopen virtual dataset and file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Dclose(vdset) < 0)
                TEST_ERROR_SUPPRESSED;
            vdset = -1;
            if (H5Fclose(vfile) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile = -1;
            if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Get VDS space */
        if ((filespace = H5Dget_space(vdset)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Get VDS space dimensions */
        if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (ndims != 2)
            TEST_ERROR_SUPPRESSED;
        if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
            TEST_ERROR_SUPPRESSED;
        if (dims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (dims[1] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[1] != 20)
            TEST_ERROR_SUPPRESSED;

        /* Close filespace */
        if (H5Sclose(filespace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Close */
        if (!(config & TEST_IO_CLOSE_SRC)) {
            if (H5Dclose(srcdset[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[0] = -1;
            if (H5Dclose(srcdset[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[1] = -1;
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
            if (H5Fclose(srcfile[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[1] = -1;
        }
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if (H5Sclose(srcspace) < 0)
            TEST_ERROR_SUPPRESSED;
        srcspace = -1;
        if (H5Sclose(vspace[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        vspace[0] = -1;

        /*
         * Test 4: 1 Source dataset mapping, 10x5 blocks, printf source file and
         * source dset, extra %%s in source dataset name
         */
        /* Clean up files so the source files do not exist yet */
        H5Iinc_ref(vds_fapl); /* Prevent FAPL from being closed */
        h5_clean_files(FILENAME, vds_fapl);

        /* Clear virtual layout in DCPL */
        if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create virtual dataspaces */
        if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create source dataspace */
        dims[1] = 5;
        if ((srcspace = H5Screate_simple(2, dims, NULL)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Select hyperslabs in virtual space */
        stride[0] = 1;
        stride[1] = 5;
        count[0]  = 1;
        count[1]  = H5S_UNLIMITED;
        block[0]  = 10;
        block[1]  = 5;
        if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Add virtual layout mapping */
        if (H5Pset_virtual(dcpl, vspace[0], printf_srcfilename_map, "%%src%%_dset%%%b", srcspace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create virtual file */
        if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create virtual dataset */
        if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Reopen virtual dataset and file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Dclose(vdset) < 0)
                TEST_ERROR_SUPPRESSED;
            vdset = -1;
            if (H5Fclose(vfile) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile = -1;
            if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Get VDS space */
        if ((filespace = H5Dget_space(vdset)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Get VDS space dimensions */
        if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (ndims != 2)
            TEST_ERROR_SUPPRESSED;
        if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
            TEST_ERROR_SUPPRESSED;
        if (dims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (dims[1] != 0)
            TEST_ERROR_SUPPRESSED;
        if (mdims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[1] != 20)
            TEST_ERROR_SUPPRESSED;

        /* Close filespace */
        if (H5Sclose(filespace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Create 2 source files, one source dataset */
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcdset[0] = H5Dcreate2(srcfile[0], "%src%_dset%0", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                     H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((srcfile[1] = H5Fcreate(srcfilename2, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Populate write buffer */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                buf[i][j] = (i * (int)mdims[1]) + j;

        /* Initialize erbuf */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                erbuf[i][j] = fill;

        /* Write to srcdset[0] */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
            TEST_ERROR_SUPPRESSED;
        if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Update erbuf */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 5; j++)
                erbuf[i][j] = buf[i][j];

        /* Close srcdset and srcfiles if config option specified */
        if (config & TEST_IO_CLOSE_SRC) {
            if (H5Dclose(srcdset[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[0] = -1;
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
            if (H5Fclose(srcfile[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[1] = -1;
        }

        /* Reopen virtual dataset and file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Dclose(vdset) < 0)
                TEST_ERROR_SUPPRESSED;
            vdset = -1;
            if (H5Fclose(vfile) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile = -1;
            if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Get VDS space */
        if ((filespace = H5Dget_space(vdset)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Get VDS space dimensions */
        if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (ndims != 2)
            TEST_ERROR_SUPPRESSED;
        if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
            TEST_ERROR_SUPPRESSED;
        if (dims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (dims[1] != 5)
            TEST_ERROR_SUPPRESSED;
        if (mdims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[1] != 20)
            TEST_ERROR_SUPPRESSED;

        /* Close filespace */
        if (H5Sclose(filespace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Reopen srcfile[1] if config option specified */
        if (config & TEST_IO_CLOSE_SRC)
            if ((srcfile[1] = H5Fopen(srcfilename2, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

        /* Create 2nd source dataset */
        if ((srcdset[1] = H5Dcreate2(srcfile[1], "%src%_dset%1", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                     H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Adjust write buffer */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++)
                buf[i][j] += (int)mdims[0] * (int)mdims[1];

        /* Write to srcdset[1] */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
            TEST_ERROR_SUPPRESSED;
        if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Update erbuf */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 5; j++)
                erbuf[i][j + 5] = buf[i][j];

        /* Close srcdset[1] and srcfile[1] if config option specified */
        if (config & TEST_IO_CLOSE_SRC) {
            if (H5Dclose(srcdset[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[1] = -1;
            if (H5Fclose(srcfile[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[1] = -1;
        }

        /* Reopen virtual dataset and file if config option specified */
        if (config & TEST_IO_REOPEN_VIRT) {
            if (H5Dclose(vdset) < 0)
                TEST_ERROR_SUPPRESSED;
            vdset = -1;
            if (H5Fclose(vfile) < 0)
                TEST_ERROR_SUPPRESSED;
            vfile = -1;
            if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
            if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        }

        /* Get VDS space */
        if ((filespace = H5Dget_space(vdset)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Get VDS space dimensions */
        if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
            TEST_ERROR_SUPPRESSED;
        if (ndims != 2)
            TEST_ERROR_SUPPRESSED;
        if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
            TEST_ERROR_SUPPRESSED;
        if (dims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (dims[1] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[0] != 10)
            TEST_ERROR_SUPPRESSED;
        if (mdims[1] != 20)
            TEST_ERROR_SUPPRESSED;

        /* Close filespace */
        if (H5Sclose(filespace) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Close */
        if (!(config & TEST_IO_CLOSE_SRC)) {
            if (H5Dclose(srcdset[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[0] = -1;
            if (H5Dclose(srcdset[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[1] = -1;
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
            if (H5Fclose(srcfile[1]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[1] = -1;
        }
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if (H5Sclose(srcspace) < 0)
            TEST_ERROR_SUPPRESSED;
        srcspace = -1;
        if (H5Sclose(vspace[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        vspace[0] = -1;
    }

    /*
     * Test 5: 2 Source mappings, interleaved slices, single element wide,
     * hyperslab selection in source, extra %%s in source dataset names
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 10;
    dims[1] = 10;
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace (2 elements wide) */
    dims[1] = 2;
    if ((srcspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in source space */
    count[0] = 10;
    count[1] = 1;
    if (H5Sselect_hyperslab(srcspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    stride[0] = 1;
    stride[1] = 2;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 10;
    block[1]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 1;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[1] = 0;

    /* Add virtual layout mappings */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "%bsrc_dset_a%b%%", srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset_b%b%%%%", srcspace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 0)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

    /* Create 2 source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "0src_dset_a0%", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset_b0%%", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write to srcdset[0] */
    block[0] = 10;
    block[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][0] = buf[i][0];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[1] */
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][1] = buf[i][0];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 2)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create 3rd source dataset */
    if ((srcdset[2] = H5Dcreate2(srcfile[0], "src_dset_b1%%", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[2] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][3] = buf[i][0];

    /* Close srcdset[2] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 4)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Make sure that the 4th slice is no longer
     * visible due to the change to H5D_VDS_FIRST_MISSING. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 2)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create 4th source dataset */
    if ((srcdset[3] = H5Dcreate2(srcfile[0], "2src_dset_a2%", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[3] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[3], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][4] = buf[i][0];

    /* Close srcdset[3] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[3]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[3] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 2)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Make sure that the 4th slice is now visible
     * due to the change to H5D_VDS_LAST_AVAILABLE. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 4)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with printf_gap set to 1, reopen file as well if
     * config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)1) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Make sure that the 6th slice is now visible
     * due to the change to printf_gap. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 5)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reset dapl */
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)0) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        for (i = 0; i < 4; i++) {
            if (H5Dclose(srcdset[i]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[i] = -1;
        }
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 6: 2 Source mappings, side-by-side, 5x5 and 5x10 blocks
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    dims[0] = 10;
    dims[1] = 10;
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace (1 dimensional) */
    dims[0] = 50;
    if ((srcspace = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslab in source space */
    count[0] = 25;
    if (H5Sselect_hyperslab(srcspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual spaces */
    stride[0] = 1;
    stride[1] = 5;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 5;
    block[1]  = 5;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0]  = 5;
    stride[1] = 10;
    block[1]  = 10;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;

    /* Add virtual layout mappings (select ALL in source space for second
     * mapping) */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset_a%b", srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_all(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset_b%b", srcspace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 0)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

    /* Create 2 source datasets */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset_a0", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdset[1] = H5Dcreate2(srcfile[0], "src_dset_b0", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write to srcdset[0] */
    block[0] = 5;
    block[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    count[0] = 25;
    if (H5Sselect_hyperslab(srcspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, srcspace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j] = buf[i][j];

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[1] */
    block[1] = 10;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_all(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, srcspace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 10; j++)
            erbuf[i + 5][j] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  Make sure that the 4th slice is no longer
     * visible due to the change to H5D_VDS_FIRST_MISSING. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 5)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create 3rd source dataset */
    if ((srcdset[2] = H5Dcreate2(srcfile[0], "src_dset_a1", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[2] */
    block[1] = 5;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sselect_hyperslab(srcspace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, srcspace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j + 5] = buf[i][j];

    /* Close srcdset[2] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with view set to H5D_VDS_LAST_AVAILABLE, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions.  There should be no change. */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile[0] = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create 4th source dataset */
    if ((srcdset[3] = H5Dcreate2(srcfile[0], "src_dset_a2", H5T_NATIVE_INT, srcspace, H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[3] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[3], H5T_NATIVE_INT, memspace, srcspace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++)
            erbuf[i][j + 10] = buf[i][j];

    /* Close srcdset[3] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[3]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[3] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 15)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Now test reopening virtual dataset without calling H5Dget_space, if
     * REOPEN_VIRT flag set */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Now try setting extent manually */
        /* Shrink to 12 */
        dims[1] = 12;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Shrink to 10 */
        dims[1] = 12;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
    }

    /* Close VDS and reopen with view set to H5D_VDS_FIRST_MISSING, reopen file
     * as well if config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_view(dapl, H5D_VDS_FIRST_MISSING) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Now test reopening virtual dataset without calling H5Dget_space, if
     * REOPEN_VIRT flag set */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Now try setting extent manually */
        /* Grow to 12 */
        dims[1] = 12;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }

        /* Grow to 15 */
        dims[1] = 15;
        if (H5Dset_extent(vdset, dims) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data through virtual dataset */
        /* Reset rbuf */
        memset(rbuf[0], 0, sizeof(rbuf));

        /* Select hyperslab in memory space */
        if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Read data */
        if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
            TEST_ERROR_SUPPRESSED;

        /* Verify read data */
        for (i = 0; i < (int)mdims[0]; i++)
            for (j = 0; j < (int)mdims[1]; j++) {
                if (j >= (int)dims[1]) {
                    if (rbuf[i][j] != 0)
                        TEST_ERROR_SUPPRESSED;
                }
                else if (rbuf[i][j] != erbuf[i][j])
                    TEST_ERROR_SUPPRESSED;
            }
    }

    /* Reset dapl */
    if (H5Pset_virtual_view(dapl, H5D_VDS_LAST_AVAILABLE) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        for (i = 0; i < 4; i++) {
            if (H5Dclose(srcdset[i]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[i] = -1;
        }
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;
    if (H5Sclose(vspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[1] = -1;

    /*
     * Test 7: 1 Source dataset mapping, 10x1 blocks, test reallocating sub_dset
     * array
     */
    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual dataspaces */
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source dataspace */
    dims[1] = 1;
    if ((srcspace = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Select hyperslabs in virtual space */
    stride[0] = 1;
    stride[1] = 1;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 10;
    block[1]  = 1;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Add virtual layout mapping */
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilenamepct_map : ".",
                       "src_dset%b", srcspace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile[0] = H5Fcreate(srcfilenamepct, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile[0] = vfile;
        if (H5Iinc_ref(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 0)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile[0] = H5Fopen(srcfilenamepct, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

    /* Create 1 source dataset */
    if ((srcdset[0] = H5Dcreate2(srcfile[0], "src_dset0", H5T_NATIVE_INT, srcspace, H5P_DEFAULT, H5P_DEFAULT,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write to srcdset[0] */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 10; i++)
        erbuf[i][0] = buf[i][0];

    /* Close srcdset[0] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile[0]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile[0] = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 1)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Close VDS and reopen with printf gap set to 127, reopen file as well if
     * config option specified */
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)127) < 0)
        TEST_ERROR_SUPPRESSED;
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    if ((vdset = H5Dopen2(vfile, "v_dset", dapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 1)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 20)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reset dapl */
    if (H5Pset_virtual_printf_gap(dapl, (hsize_t)0) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        if (H5Dclose(srcdset[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[0] = -1;
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile[0]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile[0] = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    if (H5Sclose(srcspace) < 0)
        TEST_ERROR_SUPPRESSED;
    srcspace = -1;
    if (H5Sclose(vspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    vspace[0] = -1;

    /* Close */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    dcpl = -1;
    if (H5Pclose(dapl) < 0)
        TEST_ERROR_SUPPRESSED;
    dapl = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    free(srcfilename);
    free(srcfilename_map);
    free(srcfilename2);
    free(srcfilename2_map);
    free(vfilename);
    free(printf_srcfilename_map);
    free(srcfilenamepct);
    free(srcfilenamepct_map);

    PASSED_SUPPRESSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++)
            H5Dclose(srcdset[i]);
        H5Dclose(vdset);
        for (i = 0; i < (int)(sizeof(srcfile) / sizeof(srcfile[0])); i++)
            H5Fclose(srcfile[i]);
        H5Fclose(vfile);
        H5Sclose(srcspace);
        for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Sclose(filespace);
        H5Sclose(memspace);
        H5Pclose(dcpl);
        H5Pclose(dapl);
    }
    H5E_END_TRY

    free(srcfilename);
    free(srcfilename_map);
    free(srcfilename2);
    free(srcfilename2_map);
    free(vfilename);
    free(printf_srcfilename_map);
    free(srcfilenamepct);
    free(srcfilenamepct_map);

    return 1;
} /* end test_printf() */

/*-------------------------------------------------------------------------
 * Function:    test_all
 *
 * Purpose:     Tests fixed, unlimited, and printf selections in the same
 *              VDS
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *-------------------------------------------------------------------------
 */
static int
test_all(unsigned config, hid_t vds_fapl, hid_t src_fapl)
{
    char    vfilename[FILENAME_BUF_SIZE];
    char    srcfilename[FILENAME_BUF_SIZE];
    char    srcfilename_map[FILENAME_BUF_SIZE];
    hid_t   srcfile     = H5I_INVALID_HID; /* File with source dsets */
    hid_t   vfile       = H5I_INVALID_HID; /* File with virtual dset */
    hid_t   dcpl        = H5I_INVALID_HID; /* Dataset creation property list */
    hid_t   srcdcpl     = H5I_INVALID_HID; /* DCPL for source dset */
    hid_t   srcspace[3] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID}; /* Source dataspaces */
    hid_t   vspace[3]   = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID}; /* Virtual dset dataspaces */
    hid_t   memspace    = H5I_INVALID_HID;                                     /* Memory dataspace */
    hid_t   filespace   = H5I_INVALID_HID;                                     /* File dataspace */
    hid_t   srcdset[5]  = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                        H5I_INVALID_HID}; /* Source datasets */
    hid_t   vdset       = H5I_INVALID_HID;   /* Virtual dataset */
    hsize_t dims[2]     = {6, 6};            /* Data space current size */
    hsize_t mdims[2]    = {10, 10};          /* Data space maximum size */
    hsize_t cdims[2]    = {2, 2};            /* Chunk dimensions */
    hsize_t start[2];                        /* Hyperslab start */
    hsize_t stride[2];                       /* Hyperslab stride */
    hsize_t count[2];                        /* Hyperslab count */
    hsize_t block[2];                        /* Hyperslab block */
    int     buf[10][10];                     /* Write and expected read buffer */
    int     rbuf[10][10];                    /* Read buffer */
    int     erbuf[10][10];                   /* Expected read buffer */
    int     ndims;                           /* Number of dimensions */
    int     fill = -1;                       /* Fill value */
    int     i, j;

    TESTING_2_SUPPRESSED("virtual dataset I/O with mixed selection types");

    h5_fixname(FILENAME[0], vds_fapl, vfilename, sizeof vfilename);
    h5_fixname(FILENAME[2], src_fapl, srcfilename, sizeof srcfilename);
    h5_fixname_printf(FILENAME[2], src_fapl, srcfilename_map, sizeof srcfilename_map);

    /* Create DCPLs */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcdcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set fill value */
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fill) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Set chunk dimensions */
    if (H5Pset_chunk(srcdcpl, 2, cdims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create memory space */
    if ((memspace = H5Screate_simple(2, mdims, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Clear virtual layout in DCPL */
    if (H5Pset_layout(dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create fixed mapping */
    if ((vspace[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 3;
    start[1] = 3;
    count[0] = 3;
    count[1] = 3;
    if (H5Sselect_hyperslab(vspace[0], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcspace[0] = H5Screate_simple(2, count, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[0], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset_fixed", srcspace[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create unlimited mapping */
    if ((vspace[1] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 3;
    start[1] = 0;
    count[0] = 1;
    count[1] = 1;
    block[0] = H5S_UNLIMITED;
    block[1] = 3;
    if (H5Sselect_hyperslab(vspace[1], H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    dims[0] = 0;
    dims[1] = 3;
    if ((srcspace[1] = H5Screate_simple(2, dims, block)) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 0;
    if (H5Sselect_hyperslab(srcspace[1], H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[1], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset_unlim", srcspace[1]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create printf mapping */
    if ((vspace[2] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0]  = 0;
    start[1]  = 2;
    stride[0] = 1;
    stride[1] = 3;
    count[0]  = 1;
    count[1]  = H5S_UNLIMITED;
    block[0]  = 3;
    block[1]  = 2;
    if (H5Sselect_hyperslab(vspace[2], H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((srcspace[2] = H5Screate_simple(2, block, NULL)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Pset_virtual(dcpl, vspace[2], config & TEST_IO_DIFFERENT_FILE ? srcfilename_map : ".",
                       "src_dset_printf_%b", srcspace[2]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create virtual file */
    if ((vfile = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create source file if requested */
    if (config & TEST_IO_DIFFERENT_FILE) {
        if ((srcfile = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
    }
    else {
        srcfile = vfile;
        if (H5Iinc_ref(srcfile) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Create virtual dataset */
    if ((vdset = H5Dcreate2(vfile, "v_dset", H5T_NATIVE_INT, vspace[0], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Close srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 6)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 6)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Reopen srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC)
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;

    /* Create fixed source dataset */
    if ((srcdset[0] = H5Dcreate2(srcfile, "src_dset_fixed", H5T_NATIVE_INT, srcspace[0], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Create unlimited source_dataset */
    if ((srcdset[1] = H5Dcreate2(srcfile, "src_dset_unlim", H5T_NATIVE_INT, srcspace[1], H5P_DEFAULT, srcdcpl,
                                 H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Populate write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] = (i * (int)mdims[1]) + j;

    /* Initialize erbuf */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            erbuf[i][j] = fill;

    /* Write to srcdset[0] */
    start[0] = 0;
    start[1] = 0;
    block[0] = 3;
    block[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[0], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            erbuf[i + 3][j + 3] = buf[i][j];

    /* Close srcdsets and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        for (i = 0; i < 2; i++) {
            if (H5Dclose(srcdset[i]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[i] = -1;
        }

        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 6)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 6)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if ((i >= (int)dims[0]) || (j >= (int)dims[1])) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile, "src_dset_unlim", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[1] */
    dims[0] = 2;
    dims[1] = 3;
    if (H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[1] */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 2; i++)
        for (j = 0; j < 3; j++)
            erbuf[i + 3][j] = buf[i][j];

    /* Close srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 6)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 6)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if ((i >= (int)dims[0]) || (j >= (int)dims[1])) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create first printf source dataset */
    if ((srcdset[2] = H5Dcreate2(srcfile, "src_dset_printf_0", H5T_NATIVE_INT, srcspace[2], H5P_DEFAULT,
                                 srcdcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[2] */
    start[0] = 0;
    start[1] = 0;
    block[0] = 3;
    block[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[2], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            erbuf[i][j + 2] = buf[i][j];

    /* Close srcdset[2] srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[2]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[2] = -1;
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 6)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 6)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if ((i >= (int)dims[0]) || (j >= (int)dims[1])) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile, "src_dset_unlim", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[1] */
    dims[0] = 3;
    dims[1] = 3;
    if (H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset[1] */
    start[0] = 0;
    start[1] = 0;
    block[0] = 1;
    block[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 2;
    start[1] = 0;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 3; i++)
        erbuf[5][i] = buf[0][i];

    /* Close srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 6)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 6)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if ((i >= (int)dims[0]) || (j >= (int)dims[1])) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create second printf source dataset, this time without using srcdcpl */
    if ((srcdset[3] = H5Dcreate2(srcfile, "src_dset_printf_1", H5T_NATIVE_INT, srcspace[2], H5P_DEFAULT,
                                 H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[3] */
    start[0] = 0;
    start[1] = 0;
    block[0] = 3;
    block[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[3], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            erbuf[i][j + 5] = buf[i][j];

    /* Close srcdset[3] srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[3]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[3] = -1;
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 6)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 7)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if ((i >= (int)dims[0]) || (j >= (int)dims[1])) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (config & TEST_IO_DIFFERENT_FILE)
            if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
                TEST_ERROR_SUPPRESSED;
        if ((srcdset[1] = H5Dopen2(srcfile, "src_dset_unlim", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Extend srcdset[1] */
    dims[0] = 7;
    dims[1] = 3;
    if (H5Dset_extent(srcdset[1], dims) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to new area of srcdset[1] */
    start[0] = 0;
    start[1] = 0;
    block[0] = 4;
    block[1] = 3;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if ((filespace = H5Dget_space(srcdset[1])) < 0)
        TEST_ERROR_SUPPRESSED;
    start[0] = 3;
    start[1] = 0;
    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[1], H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 4; i++)
        for (j = 0; j < 3; j++)
            erbuf[i + 6][j] = buf[i][j];

    /* Close srcdset[1] and srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[1]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[1] = -1;
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 7)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++) {
            if (j >= (int)dims[1]) {
                if (rbuf[i][j] != 0)
                    TEST_ERROR_SUPPRESSED;
            }
            else if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;
        }

    /* Reopen srcfile if config option specified */
    if ((config & TEST_IO_CLOSE_SRC) && (config & TEST_IO_DIFFERENT_FILE))
        if ((srcfile = H5Fopen(srcfilename, H5F_ACC_RDWR, src_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;

    /* Create third printf source dataset */
    if ((srcdset[4] = H5Dcreate2(srcfile, "src_dset_printf_2", H5T_NATIVE_INT, srcspace[2], H5P_DEFAULT,
                                 srcdcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Adjust write buffer */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            buf[i][j] += (int)mdims[0] * (int)mdims[1];

    /* Write to srcdset[4] */
    start[0] = 0;
    start[1] = 0;
    block[0] = 3;
    block[1] = 2;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, block, NULL) < 0)
        TEST_ERROR_SUPPRESSED;
    if (H5Dwrite(srcdset[4], H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, buf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Update erbuf */
    for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
            erbuf[i][j + 8] = buf[i][j];

    /* Close srcdset[4] srcfile if config option specified */
    if (config & TEST_IO_CLOSE_SRC) {
        if (H5Dclose(srcdset[4]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcdset[4] = -1;
        if (config & TEST_IO_DIFFERENT_FILE) {
            if (H5Fclose(srcfile) < 0)
                TEST_ERROR_SUPPRESSED;
            srcfile = -1;
        }
    }

    /* Reopen virtual dataset and file if config option specified */
    if (config & TEST_IO_REOPEN_VIRT) {
        if (H5Dclose(vdset) < 0)
            TEST_ERROR_SUPPRESSED;
        vdset = -1;
        if (H5Fclose(vfile) < 0)
            TEST_ERROR_SUPPRESSED;
        vfile = -1;
        if ((vfile = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
            TEST_ERROR_SUPPRESSED;
        if ((vdset = H5Dopen2(vfile, "v_dset", H5P_DEFAULT)) < 0)
            TEST_ERROR_SUPPRESSED;
    }

    /* Get VDS space */
    if ((filespace = H5Dget_space(vdset)) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Get VDS space dimensions */
    if ((ndims = H5Sget_simple_extent_ndims(filespace)) < 0)
        TEST_ERROR_SUPPRESSED;
    if (ndims != 2)
        TEST_ERROR_SUPPRESSED;
    if (H5Sget_simple_extent_dims(filespace, dims, mdims) < 0)
        TEST_ERROR_SUPPRESSED;
    if (dims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (dims[1] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[0] != 10)
        TEST_ERROR_SUPPRESSED;
    if (mdims[1] != 10)
        TEST_ERROR_SUPPRESSED;

    /* Close filespace */
    if (H5Sclose(filespace) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data through virtual dataset */
    /* Reset rbuf */
    memset(rbuf[0], 0, sizeof(rbuf));

    /* Select hyperslab in memory space */
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(memspace, H5S_SELECT_SET, start, NULL, dims, NULL) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Read data */
    if (H5Dread(vdset, H5T_NATIVE_INT, memspace, H5S_ALL, H5P_DEFAULT, rbuf[0]) < 0)
        TEST_ERROR_SUPPRESSED;

    /* Verify read data */
    for (i = 0; i < (int)mdims[0]; i++)
        for (j = 0; j < (int)mdims[1]; j++)
            if (rbuf[i][j] != erbuf[i][j])
                TEST_ERROR_SUPPRESSED;

    /* Close */
    if (!(config & TEST_IO_CLOSE_SRC)) {
        for (i = 0; i < 5; i++) {
            if (H5Dclose(srcdset[i]) < 0)
                TEST_ERROR_SUPPRESSED;
            srcdset[i] = -1;
        }
        if (H5Fclose(srcfile) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile = -1;
    }
    else if (!(config & TEST_IO_DIFFERENT_FILE)) {
        if (H5Fclose(srcfile) < 0)
            TEST_ERROR_SUPPRESSED;
        srcfile = -1;
    }
    if (H5Dclose(vdset) < 0)
        TEST_ERROR_SUPPRESSED;
    vdset = -1;
    if (H5Fclose(vfile) < 0)
        TEST_ERROR_SUPPRESSED;
    vfile = -1;
    for (i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++) {
        if (H5Sclose(srcspace[i]) < 0)
            TEST_ERROR_SUPPRESSED;
        srcspace[i] = -1;
    }
    for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++) {
        if (H5Sclose(vspace[i]) < 0)
            TEST_ERROR_SUPPRESSED;
        vspace[i] = -1;
    }
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    dcpl = -1;
    if (H5Pclose(srcdcpl) < 0)
        TEST_ERROR_SUPPRESSED;
    srcdcpl = -1;
    if (H5Sclose(memspace) < 0)
        TEST_ERROR_SUPPRESSED;
    memspace = -1;

    PASSED_SUPPRESSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (i = 0; i < (int)(sizeof(srcdset) / sizeof(srcdset[0])); i++)
            H5Dclose(srcdset[i]);
        H5Dclose(vdset);
        H5Fclose(srcfile);
        H5Fclose(vfile);
        for (i = 0; i < (int)(sizeof(srcspace) / sizeof(srcspace[0])); i++)
            H5Sclose(srcspace[i]);
        for (i = 0; i < (int)(sizeof(vspace) / sizeof(vspace[0])); i++)
            H5Sclose(vspace[i]);
        H5Sclose(filespace);
        H5Sclose(memspace);
        H5Pclose(dcpl);
        H5Pclose(srcdcpl);
    }
    H5E_END_TRY

    return 1;
} /* end test_all() */

/*-------------------------------------------------------------------------
 * Function:    test_dapl_values
 *
 * Purpose:     Ensure that H5Dget_access_plist returns correct values.
 *
 * Return:      Success:    0
 *              Failure:    1
 *-------------------------------------------------------------------------
 */
static int
test_dapl_values(hid_t fapl_id)
{
    hid_t          fid      = H5I_INVALID_HID; /* file to write to                     */
    hid_t          dcpl_id  = H5I_INVALID_HID; /* dataset creation properties          */
    hid_t          dapl_id1 = H5I_INVALID_HID; /* dataset access properties            */
    hid_t          dapl_id2 = H5I_INVALID_HID; /* dataset access properties            */
    hid_t          vds_sid  = H5I_INVALID_HID; /* vds data space                       */
    hid_t          src_sid  = H5I_INVALID_HID; /* source data space                    */
    hid_t          did1     = H5I_INVALID_HID; /* dataset                              */
    hid_t          did2     = H5I_INVALID_HID; /* dataset                              */
    hsize_t        start;                      /* hyperslab start                      */
    hsize_t        stride;                     /* hyperslab count                      */
    hsize_t        count;                      /* hyperslab count                      */
    hsize_t        block;                      /* hyperslab count                      */
    hsize_t        dims;                       /* dataset size                         */
    hsize_t        max_dims;                   /* dataset max size                     */
    H5D_vds_view_t view;                       /* view from dapl                       */
    hsize_t        gap_size;                   /* gap size from dapl                   */
    char           filename[1024];             /* file names                           */

    TESTING_2("H5Dget_access_plist() returns dapl w/ correct values");

    /* Create the file */
    h5_fixname(FILENAME[5], fapl_id, filename, sizeof(filename));
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        FAIL_STACK_ERROR;

    /* Create the dcpl and set up VDS mapping */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    /* source */
    dims = 42;
    if ((src_sid = H5Screate_simple(1, &dims, NULL)) < 0)
        FAIL_STACK_ERROR;
    /* vds */
    dims     = 0;
    max_dims = H5S_UNLIMITED;
    if ((vds_sid = H5Screate_simple(1, &dims, &max_dims)) < 0)
        FAIL_STACK_ERROR;
    start  = 0;
    stride = 42;
    count  = H5S_UNLIMITED;
    block  = 42;
    if (H5Sselect_hyperslab(vds_sid, H5S_SELECT_SET, &start, &stride, &count, &block) < 0)
        FAIL_STACK_ERROR;
    /* map */
    if (H5Pset_virtual(dcpl_id, vds_sid, "f-%b.h5", "/dset1", src_sid) < 0)
        FAIL_STACK_ERROR;

    /* Create the dapls and set values
     * There are two of them. The reason for this is that the only way
     * to set the printf gap is to use the default view and using the
     * default isn't the best way to test setting and getting the view.
     */
    /* dapl 1 */
    if ((dapl_id1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_virtual_view(dapl_id1, H5D_VDS_FIRST_MISSING) < 0)
        FAIL_STACK_ERROR;
    /* dapl 2 */
    if ((dapl_id2 = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    /* default but we set it explicitly to be sure */
    if (H5Pset_virtual_view(dapl_id2, H5D_VDS_LAST_AVAILABLE) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_virtual_printf_gap(dapl_id2, 123) < 0)
        FAIL_STACK_ERROR;

    /* Create the datasets */
    if ((did1 = H5Dcreate2(fid, "dset1", H5T_NATIVE_INT, vds_sid, H5P_DEFAULT, dcpl_id, dapl_id1)) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dcreate2(fid, "dset2", H5T_NATIVE_INT, vds_sid, H5P_DEFAULT, dcpl_id, dapl_id2)) < 0)
        FAIL_STACK_ERROR;

    /* Close the dapls */
    if (H5Pclose(dapl_id1) < 0)
        FAIL_STACK_ERROR;
    dapl_id1 = -1;
    if (H5Pclose(dapl_id2) < 0)
        FAIL_STACK_ERROR;
    dapl_id2 = -1;

    /* Get a data access property lists from the dataset */
    if ((dapl_id1 = H5Dget_access_plist(did1)) < 0)
        FAIL_STACK_ERROR;
    if ((dapl_id2 = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    /* Check the values from the dapls */
    /* dapl 1 */
    if (H5Pget_virtual_view(dapl_id1, &view) < 0)
        FAIL_STACK_ERROR;
    if (H5D_VDS_FIRST_MISSING != view)
        TEST_ERROR;
    /* dapl 2 */
    if (H5Pget_virtual_view(dapl_id2, &view) < 0)
        FAIL_STACK_ERROR;
    if (H5D_VDS_LAST_AVAILABLE != view)
        TEST_ERROR;
    if (H5Pget_virtual_printf_gap(dapl_id2, &gap_size) < 0)
        FAIL_STACK_ERROR;
    if (gap_size != 123)
        TEST_ERROR;

    /* Close everything */
    if (H5Sclose(vds_sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(src_sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl_id1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl_id2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did1);
        H5Dclose(did2);
        H5Pclose(dapl_id1);
        H5Pclose(dapl_id2);
        H5Pclose(dcpl_id);
        H5Sclose(vds_sid);
        H5Sclose(src_sid);
        H5Fclose(fid);
    }
    H5E_END_TRY
    return 1;
} /* end test_dapl_values() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests datasets with virtual layout
 *
 * Note:
 *  Tests are modified to test with the low/high bounds combination
 *  set in fapl.
 *  Please see RFC for "H5Sencode/H5Sdecode Format Change".
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char         filename[FILENAME_BUF_SIZE];
    hid_t        fapl;
    hid_t        vds_fapl = H5I_INVALID_HID; /* File access property list */
    hid_t        src_fapl = H5I_INVALID_HID; /* File access property list */
    int          test_api_config;
    unsigned     bit_config;
    H5F_libver_t low, high;   /* Low and high bounds */
    const char  *driver_name; /* File Driver value from environment */
    bool         driver_is_parallel;
    int          nerrors = 0;

    driver_name = h5_get_test_driver_name();

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    if (h5_using_parallel_driver(fapl, &driver_is_parallel) < 0)
        TEST_ERROR;

    /*
     * Skip VDS tests for parallel-enabled and splitter VFDs. VDS currently
     * doesn't support parallel reads and the splitter VFD has external
     * link-related bugs.
     */
    if (driver_is_parallel || !strcmp(driver_name, "splitter")) {
        puts(" -- SKIPPED for incompatible VFD --");
        exit(EXIT_SUCCESS);
    }

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create FAPLs for VDS and source files */
    if ((vds_fapl = H5Pcopy(fapl)) < 0)
        TEST_ERROR;
    if ((src_fapl = H5Pcopy(fapl)) < 0)
        TEST_ERROR;

    /* Loop through all the combinations of low/high version bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            char        msg[128];    /* Message for file version bounds */
            const char *low_string;  /* The low bound string */
            const char *high_string; /* The high bound string */
#ifndef VDS_TEST_VERBOSE
            int tmp_nerrors;
#endif /* VDS_TEST_VERBOSE */

            /* Invalid combinations, just continue */
            if (high == H5F_LIBVER_EARLIEST || high < low)
                continue;

            /* Test virtual dataset only for V110 and above */
            if (high < H5F_LIBVER_V110)
                continue;

            /* Set the low/high version bounds */
            if (H5Pset_libver_bounds(vds_fapl, low, high) < 0)
                TEST_ERROR;
            if (H5Pset_libver_bounds(src_fapl, low, high) < 0)
                TEST_ERROR;

            /* Display testing info */
            low_string  = h5_get_version_string(low);
            high_string = h5_get_version_string(high);
            snprintf(msg, sizeof(msg),
                     "Testing virtual dataset I/O with file version bounds: (%s, %s):", low_string,
                     high_string);
            puts(msg);

            for (test_api_config = (int)TEST_API_BASIC; test_api_config < (int)TEST_API_NTESTS;
                 test_api_config++)
                nerrors += test_api((test_api_config_t)test_api_config, vds_fapl, low);

            TESTING_2("Virtual dataset I/O");
#ifdef VDS_TEST_VERBOSE
            puts("");
#else  /* VDS_TEST_VERBOSE */
            tmp_nerrors = nerrors;
#endif /* VDS_TEST_VERBOSE */

            for (bit_config = 0; bit_config < TEST_IO_NTESTS; bit_config++) {
                /* Skip invalid configurations */
                if ((bit_config & TEST_IO_FCLOSE_SEMI) && (bit_config & TEST_IO_FCLOSE_STRONG))
                    continue;

                /* Print message */
                PRINT_CONFIG(
                    "%s%s%s, %s file close degree",
                    bit_config & TEST_IO_CLOSE_SRC ? "closed source dataset, " : "",
                    bit_config & TEST_IO_DIFFERENT_FILE ? "different source file" : "same source file",
                    bit_config & TEST_IO_REOPEN_VIRT ? ", reopen virtual file" : "",
                    bit_config & TEST_IO_FCLOSE_SEMI
                        ? "H5F_CLOSE_SEMI"
                        : (bit_config & TEST_IO_FCLOSE_STRONG ? "H5F_CLOSE_STRONG" : "H5F_CLOSE_WEAK"));

                /* Set file close degree */
                if (bit_config & TEST_IO_FCLOSE_SEMI) {
                    if (H5Pset_fclose_degree(vds_fapl, H5F_CLOSE_SEMI) < 0)
                        TEST_ERROR;
                }
                else if (bit_config & TEST_IO_FCLOSE_STRONG) {
                    if (H5Pset_fclose_degree(vds_fapl, H5F_CLOSE_STRONG) < 0)
                        TEST_ERROR;
                }
                else {
                    if (H5Pset_fclose_degree(vds_fapl, H5F_CLOSE_WEAK) < 0)
                        TEST_ERROR;
                }

                /* Run tests */
                nerrors += test_basic_io(bit_config, vds_fapl, src_fapl);
                nerrors += test_vds_prefix_first(bit_config, vds_fapl, src_fapl);
                nerrors += test_unlim(bit_config, vds_fapl, src_fapl);
                nerrors += test_printf(bit_config, vds_fapl, src_fapl);
                nerrors += test_all(bit_config, vds_fapl, src_fapl);
            }

#ifndef VDS_TEST_VERBOSE
            if (tmp_nerrors == nerrors)
                PASSED();
#endif /* VDS_TEST_VERBOSE */

            nerrors += test_dapl_values(vds_fapl);

            /* Verify symbol table messages are cached */
            nerrors += (h5_verify_cached_stabs(FILENAME, vds_fapl) < 0 ? 1 : 0);

        } /* end for high */
    }     /* end for low */

    if (H5Pclose(vds_fapl) < 0)
        TEST_ERROR;
    if (H5Pclose(src_fapl) < 0)
        TEST_ERROR;

    if (nerrors)
        goto error;
    printf("All virtual dataset tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    return EXIT_SUCCESS;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d VIRTUAL DATASET TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    return EXIT_FAILURE;
} /* end main() */
