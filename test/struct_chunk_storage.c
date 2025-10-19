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
 * Purpose:    Tests sparse storage based on the concept of structured chunk
 */
#define H5D_FRIEND /*suppress error about including H5Dpkg      */
#define H5D_TESTING
#define H5FD_FRIEND /*suppress error about including H5FDpkg      */
#define H5FD_TESTING

#define H5Z_FRIEND /*suppress error about including H5Zpkg      */

#include "testhdf5.h"
#include "H5srcdir.h"

#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Iprivate.h"
#include "H5Pprivate.h"

#define H5F_FRIEND /*suppress error about including H5Fpkg */
#define H5F_TESTING
#include "H5Fpkg.h" /* File access                          */

#define H5S_FRIEND  /*suppress error about including H5Spkg */
#include "H5Spkg.h" /* Dataspace                            */

#define H5T_FRIEND  /*suppress error about including H5Tpkg */
#include "H5Tpkg.h" /* Datatype                             */

#define H5A_FRIEND  /*suppress error about including H5Apkg     */
#include "H5Apkg.h" /* Attributes                   */

/* Use in version bound test */
#define H5O_FRIEND  /*suppress error about including H5Opkg */
#include "H5Opkg.h" /* Object headers                       */

#include "H5Dpkg.h"
#include "H5FDpkg.h"
#include "H5VMprivate.h"
#include "H5Zpkg.h"

static const char *FILENAME[] = {"struct_chunk_api",             /* 0 */
                                 "struct_chunk_1d",              /* 1 */
                                 "struct_chunk_2d",              /* 2 */
                                 "struct_chunk_filter_1d",       /* 3 */
                                 "struct_chunk_filter_2d",       /* 4 */
                                 "struct_chunk_filter_register", /* 5 */
                                 NULL};

#ifdef TBD
static const char *FILENAME_TBD[] = {"sparse",                    /* 0 */
                                     "sparse_direct_chunk",       /* 1 */
                                     "sparse_query_direct_chunk", /* 2 */
                                     "sparse_dense_api",          /* 3 */
                                     NULL};
#endif

#define FILENAME_BUF_SIZE 1024

#define EXT1_SPARSE_DSET "ext1_sparse_dset"
#define EXT2_SPARSE_DSET "ext2_sparse_dset"

#define SPARSE_DSET        "sparse_dset"
#define SPARSE_DSET        "sparse_dset"
#define SPARSE_DSET2       "sparse_dset2"
#define SPARSE_FILTER_DSET "sparse_filter_dset"

#define CHUNKED_DSET "chunked_dset"

#define RANK     2
#define NX       10
#define NY       10
#define CHUNK_NX 5
#define CHUNK_NY 5

#define CHK_SINGLE 1
#define CHK_FA     2
#define CHK_EA     3

/* Size of a chunk */
#define CHK_SIZE (CHUNK_NX * CHUNK_NY * sizeof(int))

static herr_t test_struct_chunk_info_1d(hid_t fcpl, hid_t fapl, bool filtered, bool early, unsigned chk_type);
static herr_t test_struct_chunk_info_2d_bt2(hid_t fcpl, hid_t fapl, bool filtered, bool early);
static herr_t test_struct_chunk_extent_1d(hid_t fcpl, hid_t fapl, bool filtered, bool early);
static herr_t test_struct_chunk_extent_2d(hid_t fcpl, hid_t fapl, bool filtered, bool early, bool expand);

static herr_t test_struct_chunk_api(hid_t fcpl, hid_t fapl);
static herr_t test_struct_chunk_1d_single(hid_t fcpl, hid_t fapl, bool filtered, bool early);
static herr_t test_struct_chunk_2d_bt2(hid_t fcpl, hid_t fapl, bool filtered, bool early);
static herr_t test_struct_chunk_1d_fa(hid_t fcpl, hid_t fapl, bool filtered, bool early);
static herr_t test_struct_chunk_2d_ea(hid_t fcpl, hid_t fapl, bool filtered, bool early);
static herr_t test_struct_chunk_filter_register(hid_t fcpl, hid_t fapl);

static herr_t filter_class3_set_local(hid_t dcpl_id, hid_t type_id, hid_t H5_ATTR_UNUSED space_id,
                                      H5_section_type_t sec_type);
static size_t filter_class3(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                            size_t nbytes, size_t *buf_size, void **buf);

#define H5Z_FILTER_CLASS3 305
#define FILTER_PARAM      9 /* No particular meaning, just for checking */
#define FILTER_PARAM_MOD  3 /* No particular meaning, just for checking */

size_t filter_bytes_read    = 0;
size_t filter_bytes_written = 0;

/* This message derives from H5Z */
const H5Z_class3_t H5Z_TEST_CLASS3[1] = {{
    H5Z_CLASS_T_VERS, H5Z_FILTER_CLASS3, /* Filter id number        */
    1, 1, "test_class3",                 /* Filter name for debugging    */
    NULL,                                /* The "can apply" callback     */
    filter_class3_set_local,             /* The "set local" callback     */
    filter_class3,                       /* The actual filter function    */
}};

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_info_1d
 *
 * Purpose:     Verify H5Oget_native_info() for 1d dataset with
 *              fixed array or extensible array chunk index
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_info_1d(hid_t fcpl, hid_t fapl, bool filtered, bool early, unsigned chk_type)
{
    char  filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t fid  = H5I_INVALID_HID;
    hid_t sid  = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t did  = H5I_INVALID_HID;

    hsize_t sg_dim[1]       = {30}; /* 1-d dataspace */
    hsize_t sg_chunk_dim[1] = {30}; /* Chunk size */

    hsize_t fa_dim[1]     = {30}; /* 1-d dataspace */
    hsize_t fa_max_dim[1] = {50};

    hsize_t ea_dim[1]     = {30}; /* 1-d dataspace */
    hsize_t ea_max_dim[1] = {H5S_UNLIMITED};

    hsize_t chunk_dim[1] = {5}; /* Chunk size */

    H5D_chunk_index_t idx_type; /* dataset chunk index type */

    int wbuf[30]; /* Write buffer */

    hsize_t start[1];
    hsize_t stride[1];
    hsize_t count[1];
    hsize_t block[1];

    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    int      nfilters;
    unsigned options;

    H5O_native_info_t nat_info;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk with H5Oget_native_info() on 1d dataset with Single/Fixed/Extensible array "
            "chunk index type");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create property list for dataset creation */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, (chk_type == CHK_SINGLE ? sg_chunk_dim : chunk_dim), H5D_SPARSE_CHUNK) <
        0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_FIXED, &nfilters) < 0)
            TEST_ERROR;

        if (nfilters != 1)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    /* Create dataspace */
    if (chk_type == CHK_SINGLE) {
        if ((sid = H5Screate_simple(1, sg_dim, NULL)) < 0)
            TEST_ERROR;
    }
    else if (chk_type == CHK_FA) {
        if ((sid = H5Screate_simple(1, fa_dim, fa_max_dim)) < 0)
            TEST_ERROR;
    }
    else if (chk_type == CHK_EA) {
        if ((sid = H5Screate_simple(1, ea_dim, ea_max_dim)) < 0)
            TEST_ERROR;
    }

    /* Create dataset */

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    if (idx_type != (chk_type == CHK_SINGLE
                         ? H5D_CHUNK_IDX_SINGLE
                         : (chk_type == CHK_FA ? H5D_CHUNK_IDX_FARRAY : H5D_CHUNK_IDX_EARRAY)))
        FAIL_PUTS_ERROR("should be using the expected array chunk index");

    /* Starting at 4, select 3 blocks of size 2 each */
    /* Selection is across chunks and within the chunk */
    start[0]  = 4;
    stride[0] = 6;
    count[0]  = 3;
    block[0]  = 2;
    H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block);

    memset(wbuf, 0, sizeof(wbuf));

    /* Starting at 4, initialize 3 blocks of size 2 to the dataset */
    wbuf[4]  = 4;
    wbuf[5]  = 5;
    wbuf[10] = 10;
    wbuf[11] = 11;
    wbuf[16] = 16;
    wbuf[17] = 17;

    /* Starting at 1, select 3 blocks of size 1 each */
    start[0]  = 1;
    stride[0] = 6;
    count[0]  = 3;
    block[0]  = 1;
    H5Sselect_hyperslab(sid, H5S_SELECT_OR, start, stride, count, block);

    wbuf[1]  = 1;
    wbuf[7]  = 7;
    wbuf[13] = 13;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl, 1, (chk_type == CHK_SINGLE ? sg_chunk_dim : chunk_dim)) < 0)

        if (early) {
            if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
                TEST_ERROR;
        }

    if (filtered) {
        if (H5Pset_deflate(dcpl, 9) < 0)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;
    }

    /* Create dataspace */
    if (chk_type == CHK_SINGLE) {
        if ((sid = H5Screate_simple(1, sg_dim, NULL)) < 0)
            TEST_ERROR;
    }
    else if (chk_type == CHK_FA) {
        if ((sid = H5Screate_simple(1, fa_dim, fa_max_dim)) < 0)
            TEST_ERROR;
    }
    else if (chk_type == CHK_EA) {
        if ((sid = H5Screate_simple(1, ea_dim, ea_max_dim)) < 0)
            TEST_ERROR;
    }

    /*
     * Create legacy chunked dataset
     * This is done just to compare the meta data size
     * between structured chunk and legacy chunked datasets
     */
    if ((did = H5Dcreate2(fid, CHUNKED_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Obtain the correct chunk indexing type */
    /* This may be v1-btree chunk index */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    /* Starting at 4, select 3 blocks of size 2 each */
    start[0]  = 4;
    stride[0] = 6;
    block[0]  = 2;
    H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block);

    memset(wbuf, 0, sizeof(wbuf));

    /* Starting at 1, select 1 blocks of size 1 each */
    start[0]  = 1;
    stride[0] = 6;
    count[0]  = 1;
    block[0]  = 1;
    H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block);

    wbuf[1] = 1;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Obtain the correct chunk indexing type */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    memset(&nat_info, 0, sizeof(nat_info));
    if (H5Oget_native_info(did, &nat_info, H5O_NATIVE_INFO_META_SIZE) < 0)
        TEST_ERROR;

    /* Verify the size of meta data */
    /* This may change as the implementation of structured chunk is still in progress */
    if (chk_type == CHK_SINGLE) {
        if (nat_info.meta_size.obj.index_size != 0)
            TEST_ERROR;
    }
    else if (chk_type == CHK_FA) {
        if (nat_info.meta_size.obj.index_size != (filtered ? 466 : 226))
            TEST_ERROR;
    }
    else if (chk_type == CHK_EA) {
        if (nat_info.meta_size.obj.index_size != (filtered ? 274 : 178))
            TEST_ERROR;
    }

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, CHUNKED_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Obtain the correct chunk indexing type */
    /* This may be v1-btree chunk index */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    memset(&nat_info, 0, sizeof(nat_info));
    if (H5Oget_native_info(did, &nat_info, H5O_NATIVE_INFO_META_SIZE) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* end test_struct_chunk_info_1d() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_info_2d_bt2
 *
 * Purpose:     Verify H5Oget_native_info() for 2d dataset with
 *              v2-btree chunk index
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_info_2d_bt2(hid_t fcpl, hid_t fapl, bool filtered, bool early)
{
    char              filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t             fid          = H5I_INVALID_HID; /* File ID */
    hid_t             sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t             did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t             dcpl         = H5I_INVALID_HID; /* Creation plist */
    hsize_t           dim[2]       = {10, 19};        /* 2-d dataspace (contains partial edge chunk) */
    hsize_t           dmax[2]      = {H5S_UNLIMITED, H5S_UNLIMITED}; /* maximum dimension */
    hsize_t           chunk_dim[2] = {5, 5};                         /* Chunk size */
    H5D_chunk_index_t idx_type;                                      /* dataset chunk index type     */

    int          wbuf[190]; /* Write buffer */
    hsize_t      start[2];
    hsize_t      stride[2];
    hsize_t      count[2];
    hsize_t      block[2];
    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    int      nfilters;
    unsigned options;

    H5O_native_info_t nat_info;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk with H5Oget_native_info() on 2d dataset with bt2 chunk index");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 2, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_FIXED, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 1)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dim, dmax)) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_FILTER_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using version 2 btree chunk index");

    /* Starting at [3, 3], select 2 blocks of size 3x3 each */
    start[0]  = 3;
    start[1]  = 3;
    stride[0] = 4;
    stride[1] = 12;
    count[0]  = 1;
    count[1]  = 2;
    block[0]  = 3;
    block[1]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize 2 3x3 blocks */
    wbuf[60] = 60;
    wbuf[61] = 61;
    wbuf[62] = 62;

    wbuf[72] = 72;
    wbuf[73] = 73;
    wbuf[74] = 74;

    wbuf[79] = 79;
    wbuf[80] = 80;
    wbuf[81] = 81;

    wbuf[91] = 91;
    wbuf[92] = 92;
    wbuf[93] = 93;

    wbuf[98]  = 98;
    wbuf[99]  = 99;
    wbuf[100] = 100;

    wbuf[110] = 110;
    wbuf[111] = 111;
    wbuf[112] = 112;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /*
     * Create legacy chunked dataset
     */

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl, 2, chunk_dim) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_deflate(dcpl, 9) < 0)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;
    }

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dim, dmax)) < 0)
        TEST_ERROR;

    /*
     * Create legacy chunked dataset
     * This is done just to compare the meta data size
     * between structured chunk and legacy chunked datasets
     */
    if ((did = H5Dcreate2(fid, CHUNKED_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Retrieve the chunk indexing type */
    /* This may be v1-btree or v2-btree chunk index */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    /* Starting at [3, 3], select 2 blocks of size 3x3 each */
    start[0]  = 3;
    start[1]  = 3;
    stride[0] = 4;
    stride[1] = 12;
    count[0]  = 1;
    count[1]  = 2;
    block[0]  = 3;
    block[1]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize 2 3x3 blocks */
    wbuf[60] = 60;
    wbuf[61] = 61;
    wbuf[62] = 62;

    wbuf[72] = 72;
    wbuf[73] = 73;
    wbuf[74] = 74;

    wbuf[79] = 79;
    wbuf[80] = 80;
    wbuf[81] = 81;

    wbuf[91] = 91;
    wbuf[92] = 92;
    wbuf[93] = 93;

    wbuf[98]  = 98;
    wbuf[99]  = 99;
    wbuf[100] = 100;

    wbuf[110] = 110;
    wbuf[111] = 111;
    wbuf[112] = 112;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Open the structured chunk dataset */
    if ((did = H5Dopen2(fid, SPARSE_FILTER_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Retrieve the chunk indexing type */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    memset(&nat_info, 0, sizeof(nat_info));
    if (H5Oget_native_info(did, &nat_info, H5O_NATIVE_INFO_META_SIZE) < 0)
        TEST_ERROR;

    if (nat_info.meta_size.obj.index_size != 2086)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Open the legacy chunked dataset */
    if ((did = H5Dopen2(fid, CHUNKED_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Retrieve the chunk indexing type */
    /* This may be v1-btree or v2-btree chunk index */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;

    memset(&nat_info, 0, sizeof(nat_info));
    if (H5Oget_native_info(did, &nat_info, H5O_NATIVE_INFO_META_SIZE) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* test_struct_chunk_info_2d_bt2() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_extent_1d
 *
 * Purpose:     Verify H5Dset_extent() for:
 *                  --1d dataset with fixed array and extensible array chunk index
 *                  --Chunks were not written before H5Dset_extent()
 *              Expand or shrink for H5Dset_extent should succeed
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_extent_1d(hid_t fcpl, hid_t fapl, bool filtered, bool early)
{
    char  filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t fid     = H5I_INVALID_HID;
    hid_t sid     = H5I_INVALID_HID;
    hid_t dcpl    = H5I_INVALID_HID;
    hid_t did     = H5I_INVALID_HID;
    hid_t new_sid = H5I_INVALID_HID;
    hid_t msid    = H5I_INVALID_HID;

    hsize_t fa_dim[1]     = {20}; /* 1-d dataspace */
    hsize_t fa_max_dim[1] = {50};

    hsize_t chunk_dim[1] = {5}; /* Chunk size */

    hsize_t ea_dim[1]     = {20}; /* 1-d dataspace */
    hsize_t ea_max_dim[1] = {H5S_UNLIMITED};
    int     status;

    H5D_chunk_index_t idx_type; /* dataset chunk index type */

    hsize_t shrink_dim[1] = {10};
    hsize_t expand_dim[1] = {50};

    int wbuf1[30]; /* Write buffer */
    int wbuf2[30]; /* Write buffer */
    int rbuf[30];  /* Read buffer */

    hsize_t start[1];
    hsize_t stride[1];
    hsize_t count[1];
    hsize_t block[1];

    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    int      nfilters;
    unsigned options;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk with HD5set_extent() on 1d dataset");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, fa_dim, fa_max_dim)) < 0)
        TEST_ERROR;

    /* Create property list for dataset creation */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, (size_t)0,
                           NULL) < 0)
            TEST_ERROR;

        if (H5Pset_filter2(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 2)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_FIXED, &nfilters) < 0)
            TEST_ERROR;

        if (nfilters != 1)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    /* Create 1st dataset */

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, EXT1_SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("should be using fixed array chunk index");

    /* Expand case */
    H5E_BEGIN_TRY
    {
        status = H5Dset_extent(did, expand_dim);
    }
    H5E_END_TRY

    if (early) {
        if (status >= 0)
            TEST_ERROR;
    }
    else {
        if (status < 0)
            TEST_ERROR;
    }

    if ((new_sid = H5Dget_space(did)) < 0)
        TEST_ERROR;

    if (!early) {

        /* Starting at 20, select 1 blocks of size 3 each */
        start[0]  = 20;
        stride[0] = 1;
        count[0]  = 3;
        block[0]  = 1;
        H5Sselect_hyperslab(new_sid, H5S_SELECT_OR, start, stride, count, block);

        memset(wbuf1, 0, sizeof(wbuf1));
        wbuf1[20] = 20;
        wbuf1[21] = 21;
        wbuf1[22] = 22;

        if (H5Dwrite(did, H5T_NATIVE_INT, new_sid, new_sid, H5P_DEFAULT, wbuf1) < 0)
            TEST_ERROR;
    }

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Sclose(new_sid) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /* Create 2nd dataset */

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, ea_dim, ea_max_dim)) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, EXT2_SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array chunk index");

    /* Shrink case */
    H5E_BEGIN_TRY
    {
        status = H5Dset_extent(did, shrink_dim);
    }
    H5E_END_TRY

    if (early) {
        if (status >= 0)
            TEST_ERROR;
    }
    else {
        if (status < 0)
            TEST_ERROR;
    }

    if ((new_sid = H5Dget_space(did)) < 0)
        TEST_ERROR;

    if (!early) {

        /* Starting at 1, select 1 block of size 3 each */
        start[0]  = 1;
        stride[0] = 1;
        count[0]  = 3;
        block[0]  = 1;
        H5Sselect_hyperslab(new_sid, H5S_SELECT_SET, start, stride, count, block);

        memset(wbuf2, 0, sizeof(wbuf2));
        wbuf2[1] = 1;
        wbuf2[2] = 2;
        wbuf2[3] = 3;

        if (H5Dwrite(did, H5T_NATIVE_INT, new_sid, new_sid, H5P_DEFAULT, wbuf2) < 0)
            TEST_ERROR;
    }

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Sclose(new_sid) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (!early) {
        if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            TEST_ERROR;

        if ((did = H5Dopen2(fid, EXT1_SPARSE_DSET, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        if ((new_sid = H5Dget_space(did)) < 0)
            TEST_ERROR;

        /* Just read the selected chunk, otherwise H5SC_read didn't handle the case
           properly when reading in all chunks (which may or may not be allocated) */
        if ((msid = H5Screate_simple(1, count, NULL)) < 0)
            TEST_ERROR;
        start[0]  = 20;
        stride[0] = 1;
        count[0]  = 3;
        block[0]  = 1;
        H5Sselect_hyperslab(new_sid, H5S_SELECT_SET, start, stride, count, block);

        memset(rbuf, 0, sizeof(rbuf));

        if (H5Dread(did, H5T_NATIVE_INT, msid, new_sid, H5P_DEFAULT, rbuf) < 0)
            TEST_ERROR;

        /* Verify data read */
        if (rbuf[0] != 20 || rbuf[1] != 21 || rbuf[2] != 22)
            TEST_ERROR;

        if (H5Dclose(did) < 0)
            TEST_ERROR;

        if (H5Sclose(msid) < 0)
            TEST_ERROR;

        if (H5Sclose(new_sid) < 0)
            TEST_ERROR;

        if ((did = H5Dopen2(fid, EXT2_SPARSE_DSET, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        if ((new_sid = H5Dget_space(did)) < 0)
            TEST_ERROR;

        /* Just read the selected chunk, otherwise H5SC_read didn't handle the case
           properly when reading in all chunks (which may or may not be allocated) */
        if ((msid = H5Screate_simple(1, count, NULL)) < 0)
            TEST_ERROR;
        start[0]  = 1;
        stride[0] = 1;
        count[0]  = 3;
        block[0]  = 1;
        H5Sselect_hyperslab(new_sid, H5S_SELECT_SET, start, stride, count, block);

        memset(rbuf, 0, sizeof(rbuf));

        if (H5Dread(did, H5T_NATIVE_INT, msid, new_sid, H5P_DEFAULT, rbuf) < 0)
            TEST_ERROR;

        /* Verify data read */
        if (rbuf[0] != 1 || rbuf[1] != 2 || rbuf[2] != 3)
            TEST_ERROR;

        if (H5Dclose(did) < 0)
            TEST_ERROR;

        if (H5Sclose(new_sid) < 0)
            TEST_ERROR;

        if (H5Sclose(msid) < 0)
            TEST_ERROR;

        if (H5Fclose(fid) < 0)
            TEST_ERROR;
    }

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(new_sid);
        H5Sclose(msid);
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* end test_struct_chunk_extent_1d() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_extent_2d
 *
 * Purpose:     Verify H5Dset_extent() for 2d dataset with v2-btree chunk index;
 *              there is a write before set_extent()
 *
 *              Expand or shrink for H5Dset_extent should fail for now
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_extent_2d(hid_t fcpl, hid_t fapl, bool filtered, bool early, bool expand)
{
    char              filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t             fid          = H5I_INVALID_HID; /* File ID */
    hid_t             sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t             did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t             dcpl         = H5I_INVALID_HID; /* Creation plist */
    hsize_t           dim[2]       = {10, 19};        /* 2-d dataspace (contains partial edge chunk) */
    hsize_t           dmax[2]      = {H5S_UNLIMITED, H5S_UNLIMITED}; /* maximum dimension */
    hsize_t           chunk_dim[2] = {5, 5};                         /* Chunk size */
    H5D_chunk_index_t idx_type;                                      /* dataset chunk index type     */

    hsize_t expand_dim[2] = {20, 29}; /* Chunk size */
    hsize_t shrink_dim[2] = {5, 9};   /* Chunk size */
    int     status;

    int          wbuf[190]; /* Write buffer */
    hsize_t      start[2];
    hsize_t      stride[2];
    hsize_t      count[2];
    hsize_t      block[2];
    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    int      nfilters;
    unsigned options;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk with HD5set_extent() on 2d dataset");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dim, dmax)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 2, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, (size_t)0,
                           NULL) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 2)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_FILTER_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using version 2 btree chunk index");

    /* Starting at [3, 3], select 2 blocks of size 3x3 each */
    start[0]  = 3;
    start[1]  = 3;
    stride[0] = 4;
    stride[1] = 12;
    count[0]  = 1;
    count[1]  = 2;
    block[0]  = 3;
    block[1]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize 2 3x3 blocks */
    wbuf[60] = 60;
    wbuf[61] = 61;
    wbuf[62] = 62;

    wbuf[72] = 72;
    wbuf[73] = 73;
    wbuf[74] = 74;

    wbuf[79] = 79;
    wbuf[80] = 80;
    wbuf[81] = 81;

    wbuf[91] = 91;
    wbuf[92] = 92;
    wbuf[93] = 93;

    wbuf[98]  = 98;
    wbuf[99]  = 99;
    wbuf[100] = 100;

    wbuf[110] = 110;
    wbuf[111] = 111;
    wbuf[112] = 112;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    /* Expand case */
    H5E_BEGIN_TRY
    {
        status = H5Dset_extent(did, expand ? expand_dim : shrink_dim);
    }
    H5E_END_TRY

    if (status >= 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* test_struct_chunk_extent_2d() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_api
 *
 * Purpose:     Verify APIs for structured chunk layout:
 *              --H5Dget_create_plist()
 *              --H5Pget/set_layout()
 *              --H5Pget/set_struct_chunk()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_api(hid_t fcpl, hid_t fapl)
{
    char         filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t        fid              = H5I_INVALID_HID;
    hid_t        sid              = H5I_INVALID_HID;
    hid_t        sid2             = H5I_INVALID_HID;
    hid_t        dcpl             = H5I_INVALID_HID;
    hid_t        dcpl2            = H5I_INVALID_HID;
    hid_t        did              = H5I_INVALID_HID;
    hid_t        did2             = H5I_INVALID_HID;
    hsize_t      dim[1]           = {50};      /* 1-d dataspace */
    hsize_t      chunk_dim[1]     = {5};       /* 1-d Chunk size */
    hsize_t      dim2[2]          = {50, 100}; /* 2-d dataspace */
    hsize_t      chunk_dim2[2]    = {5, 10};   /* 2-d Chunk size */
    hsize_t      my_chunk_dim[2]  = {0, 0};
    hsize_t      my_chunk_dim2[2] = {0, 0};
    unsigned     my_flag;
    int          my_rank;
    H5D_layout_t my_layout;
    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk APIs");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create a file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create 1d dataspace */
    if ((sid = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR;

    /* Create property list for compact dataset creation */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;
    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    /* Reopen dataset */
    if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    if ((my_rank = H5Pget_struct_chunk(dcpl, 2, my_chunk_dim, &my_flag)) != 1)
        TEST_ERROR;
    if (my_flag != H5D_SPARSE_CHUNK)
        TEST_ERROR;
    if (my_chunk_dim[0] != chunk_dim[0])
        TEST_ERROR;

    if ((my_layout = H5Pget_layout(dcpl)) != H5D_STRUCT_CHUNK)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    /* Create 2d dataspace */
    if ((sid2 = H5Screate_simple(2, dim2, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl2, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl2, 2, chunk_dim2, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if ((did2 = H5Dcreate2(fid, SPARSE_DSET2, H5T_NATIVE_INT, sid2, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dclose(did2) < 0)
        TEST_ERROR;

    if (H5Sclose(sid2) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl2) < 0)
        TEST_ERROR;

    if ((did2 = H5Dopen2(fid, SPARSE_DSET2, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((dcpl2 = H5Dget_create_plist(did2)) < 0)
        TEST_ERROR;

    if ((my_rank = H5Pget_struct_chunk(dcpl2, 2, my_chunk_dim2, &my_flag)) != 2)
        TEST_ERROR;
    if (my_flag != H5D_SPARSE_CHUNK)
        TEST_ERROR;
    if (my_chunk_dim2[0] != chunk_dim2[0] || my_chunk_dim2[1] != chunk_dim2[1])
        TEST_ERROR;

    if ((my_layout = H5Pget_layout(dcpl2)) != H5D_STRUCT_CHUNK)
        TEST_ERROR;

    if (H5Dclose(did2) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl2) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(sid2);
        H5Pclose(dcpl);
        H5Pclose(dcpl2);
        H5Dclose(did);
        H5Dclose(did2);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }
    return FAIL;
} /* end test_struct_chunk_api() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_1d_single
 *
 * Purpose:     Verify writing and reading hyperslab selection to a
 *              structured chunk dataset, using single chunk index
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_1d_single(hid_t fcpl, hid_t fapl, bool filtered, bool early)
{
    char  filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t fid  = H5I_INVALID_HID;
    hid_t sid  = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t did  = H5I_INVALID_HID;

    hsize_t           dim[1]       = {20}; /* 1-d dataspace */
    hsize_t           chunk_dim[1] = {20}; /* Chunk size */
    H5D_chunk_index_t idx_type;            /* dataset chunk index type */

    int wbuf[20]; /* Write buffer */
    int rbuf[20]; /* Read buffer */

    hsize_t start[1];
    hsize_t stride[1];
    hsize_t count[1];
    hsize_t block[1];

    unsigned     i;
    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    size_t       my_cd_nelmts = 1;
    unsigned int my_cd_value  = 0;

    int          nfilters;
    H5Z_filter_t filter_id;
    unsigned int flags;
    unsigned     options;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk 1d dataset with single chunk index");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR;

    /* Create property list for dataset creation */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, (size_t)0,
                           NULL) < 0)
            TEST_ERROR;

        if (H5Pset_filter2(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 2)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_FIXED, &nfilters) < 0)
            TEST_ERROR;

        if (nfilters != 1)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_SINGLE)
        FAIL_PUTS_ERROR("should be using single chunk index");

    /* Starting at 4, select 3 blocks of size 2 each */
    /* Selection is across chunks and within the chunk */
    start[0]  = 4;
    stride[0] = 6;
    count[0]  = 3;
    block[0]  = 2;
    H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block);

    memset(wbuf, 0, sizeof(wbuf));

    /* Starting at 4, initialize 3 blocks of size 2 to the dataset */
    wbuf[4]  = 4;
    wbuf[5]  = 5;
    wbuf[10] = 10;
    wbuf[11] = 11;
    wbuf[16] = 16;
    wbuf[17] = 17;

    /* Starting at 1, select 3 blocks of size 1 each */
    start[0]  = 1;
    stride[0] = 6;
    count[0]  = 3;
    block[0]  = 1;
    H5Sselect_hyperslab(sid, H5S_SELECT_OR, start, stride, count, block);

    /* Starting at 1, initialize 3 blocks of size 1 to the dataset */
    wbuf[1]  = 1;
    wbuf[7]  = 7;
    wbuf[13] = 13;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_SINGLE)
        FAIL_PUTS_ERROR("should be using single chunk index");

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    if (filtered) {
        /* Get filter info for section "selection", filter number 0 */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 0, NULL, &my_cd_nelmts, &my_cd_value,
                                        (size_t)0, NULL, NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_DEFLATE)
            TEST_ERROR;
        if (my_cd_nelmts != 1)
            TEST_ERROR;
        if (my_cd_value != 9)
            TEST_ERROR;

        /* Get filter info by filter number 1 for section "selection" */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 1, &flags, NULL, NULL, (size_t)0, NULL,
                                        NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_SHUFFLE)
            TEST_ERROR;
        if (flags != H5Z_FLAG_OPTIONAL)
            TEST_ERROR;

        /* Get filter info by filter id for section "fixed data" */
        if (H5Pget_filter_by_id3(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, &flags, &my_cd_nelmts,
                                 &my_cd_value, (size_t)0, NULL, NULL) < 0)
            TEST_ERROR;
        if (my_cd_nelmts != 1)
            TEST_ERROR;
        if (my_cd_value != 9)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    memset(rbuf, 0, sizeof(rbuf));
    if (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < 20; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* end test_struct_chunk_1d() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_2d_bt2
 *
 * Purpose:     Verify writing and reading hyperslab selection to a
 *              structured chunk dataset, using v2-btree chunk index
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_2d_bt2(hid_t fcpl, hid_t fapl, bool filtered, bool early)
{
    char              filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t             fid          = H5I_INVALID_HID; /* File ID */
    hid_t             sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t             did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t             dcpl         = H5I_INVALID_HID; /* Creation plist */
    hsize_t           dim[2]       = {10, 19};        /* 2-d dataspace (contains partial edge chunk) */
    hsize_t           dmax[2]      = {H5S_UNLIMITED, H5S_UNLIMITED}; /* maximum dimension */
    hsize_t           chunk_dim[2] = {5, 5};                         /* Chunk size */
    H5D_chunk_index_t idx_type;                                      /* dataset chunk index type     */

    int          wbuf[190]; /* Write buffer */
    int          rbuf[190]; /* Read buffer */
    hsize_t      start[2];
    hsize_t      stride[2];
    hsize_t      count[2];
    hsize_t      block[2];
    unsigned     i;
    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    size_t       my_cd_nelmts = 1;
    unsigned int my_cd_value  = 0;

    int          nfilters;
    H5Z_filter_t filter_id;
    unsigned int flags;
    unsigned     options;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk 2d dataset with bt2 chunk index");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dim, dmax)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 2, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, (size_t)0,
                           NULL) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 2)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_FILTER_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using version 2 btree chunk index");

    /* Starting at [3, 3], select 2 blocks of size 3x3 each */
    start[0]  = 3;
    start[1]  = 3;
    stride[0] = 4;
    stride[1] = 12;
    count[0]  = 1;
    count[1]  = 2;
    block[0]  = 3;
    block[1]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize 2 3x3 blocks */
    wbuf[60] = 60;
    wbuf[61] = 61;
    wbuf[62] = 62;

    wbuf[72] = 72;
    wbuf[73] = 73;
    wbuf[74] = 74;

    wbuf[79] = 79;
    wbuf[80] = 80;
    wbuf[81] = 81;

    wbuf[91] = 91;
    wbuf[92] = 92;
    wbuf[93] = 93;

    wbuf[98]  = 98;
    wbuf[99]  = 99;
    wbuf[100] = 100;

    wbuf[110] = 110;
    wbuf[111] = 111;
    wbuf[112] = 112;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_FILTER_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using v2 btree chunk index");

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    if (filtered) {
        /* Get filter info by filter number 0 for section "selection" */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 0, NULL, &my_cd_nelmts, &my_cd_value,
                                        (size_t)0, NULL, NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_DEFLATE)
            TEST_ERROR;
        if (my_cd_nelmts != 1)
            TEST_ERROR;
        if (my_cd_value != 9)
            TEST_ERROR;

        /* Get filter info by filter number 1 for section "selection" */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 1, &flags, NULL, NULL, (size_t)0, NULL,
                                        NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_SHUFFLE)
            TEST_ERROR;
        if (flags != H5Z_FLAG_OPTIONAL)
            TEST_ERROR;

        H5E_BEGIN_TRY
        {
            /* Get filter info by filter id for section "fixed data" */
            filter_id = H5Pget_filter_by_id3(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, &flags,
                                             &my_cd_nelmts, &my_cd_value, (size_t)0, NULL, NULL);
        }
        H5E_END_TRY
        /* No filter for section "fixed data" */
        if (filter_id >= 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    memset(rbuf, 0, sizeof(rbuf));
    if (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < 190; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* test_struct_chunk_2d() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_1d_fa()
 *
 * Purpose:     Verify writing and reading hyperslab selection to a
 *              structured chunk dataset, using fixed array chunk index
 *              Also verify the following APIs for structured chunk with filter:
 *              --H5Pset_filter2()
 *              --H5Pget_nfilters2()
 *              --H5Pget_filters3()
 *              --H5Pget_filter_by_id3()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_1d_fa(hid_t fcpl, hid_t fapl, bool filtered, bool early)
{
    char              filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t             fid          = H5I_INVALID_HID; /* File ID */
    hid_t             sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t             did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t             dcpl         = H5I_INVALID_HID; /* Creation plist */
    hsize_t           dim[1]       = {19};            /* 1-d dataspace (contains partial edge chunk) */
    hsize_t           chunk_dim[1] = {5};             /* Chunk size */
    H5D_chunk_index_t idx_type;                       /* dataset chunk index type */

    int          wbuf[19]; /* Write buffer */
    int          rbuf[19]; /* Read buffer */
    hsize_t      start[1];
    hsize_t      stride[1];
    hsize_t      count[1];
    hsize_t      block[1];
    unsigned     i;
    unsigned int level        = 9;
    unsigned int cd_values[1] = {level};
    size_t       cd_nelmts    = 1;

    size_t       my_cd_nelmts = 1;
    unsigned int my_cd_value  = 0;

    int          nfilters;
    H5Z_filter_t filter_id;
    unsigned int flags;
    unsigned     options;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk 1d dataset with fixed array chunk index");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, (size_t)0,
                           NULL) < 0)
            TEST_ERROR;

        if (H5Pset_filter2(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 2)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_FIXED, &nfilters) < 0)
            TEST_ERROR;

        if (nfilters != 1)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_FILTER_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("should be using fixed array chunk index");

    /* Starting at 3, select 3 blocks of size 3 each */
    start[0]  = 3;
    stride[0] = 6;
    count[0]  = 3;
    block[0]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    /* Write sparse data to the dataset */
    memset(wbuf, 0, sizeof(wbuf));

    /* Starting at 3, initialize 3 blocks of size 3 and write to the dataset */
    wbuf[3] = 3;
    wbuf[4] = 4;
    wbuf[5] = 5;

    wbuf[9]  = 9;
    wbuf[10] = 10;
    wbuf[11] = 11;

    wbuf[15] = 15;
    wbuf[16] = 16;
    wbuf[17] = 17;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_FILTER_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("should be using fixed array chunk index");

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    if (filtered) {
        /* Get filter info for section "selection", filter number 0 */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 0, NULL, &my_cd_nelmts, &my_cd_value,
                                        (size_t)0, NULL, NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_DEFLATE)
            TEST_ERROR;
        if (my_cd_nelmts != 1)
            TEST_ERROR;
        if (my_cd_value != 9)
            TEST_ERROR;

        /* Get filter info by filter number 1 for section "selection" */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 1, &flags, NULL, NULL, (size_t)0, NULL,
                                        NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_SHUFFLE)
            TEST_ERROR;
        if (flags != H5Z_FLAG_OPTIONAL)
            TEST_ERROR;

        /* Get filter info by filter id for section "fixed data" */
        if (H5Pget_filter_by_id3(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, &flags, &my_cd_nelmts,
                                 &my_cd_value, (size_t)0, NULL, NULL) < 0)
            TEST_ERROR;
        if (my_cd_nelmts != 1)
            TEST_ERROR;
        if (my_cd_value != 9)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    memset(rbuf, 0, sizeof(rbuf));
    if (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < 19; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* test_struct_chunk_1d_fa() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_2d_ea()
 *
 * Purpose:     Verify writing and reading hyperslab selection to a
 *              structured chunk dataset, using extensible array chunk index
 *              Also verify the following APIs for structured chunk with filter:
 *              --H5Pset_filter2()
 *              --H5Pget_nfilters2()
 *              --H5Pget_filters3()
 *              --H5Pget_filter_by_id3()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_2d_ea(hid_t fcpl, hid_t fapl, bool filtered, bool early)
{
    char              filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t             fid          = H5I_INVALID_HID; /* File ID */
    hid_t             sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t             did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t             dcpl         = H5I_INVALID_HID; /* Creation plist */
    hsize_t           dim[2]       = {10, 19};        /* 2-d dataspace (contains partial edge chunk) */
    hsize_t           dmax[2]      = {10, H5S_UNLIMITED};
    hsize_t           chunk_dim[2] = {5, 5}; /* Chunk size */
    H5D_chunk_index_t idx_type;              /* dataset chunk index type */
    int               wbuf[190];             /* Write buffer */
    int               rbuf[190];             /* Read buffer */
    hsize_t           start[2];
    hsize_t           stride[2];
    hsize_t           count[2];
    hsize_t           block[2];
    unsigned          i;
    unsigned int      level        = 9;
    unsigned int      cd_values[1] = {level};
    size_t            cd_nelmts    = 1;

    size_t       my_cd_nelmts = 1;
    unsigned int my_cd_value  = 0;

    int          nfilters;
    H5Z_filter_t filter_id;
    unsigned int flags;
    unsigned     options;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk 2d dataset with extensible array chunk index");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(2, dim, dmax)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 2, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if (early) {
        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            TEST_ERROR;
    }

    if (filtered) {
        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, cd_nelmts,
                           cd_values) < 0)
            TEST_ERROR;

        if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, (size_t)0,
                           NULL) < 0)
            TEST_ERROR;

        if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
            TEST_ERROR;
        if (nfilters != 2)
            TEST_ERROR;

        if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_FILTER_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array chunk index");

    /* Starting at [3, 3], select 2 blocks of size 3x3 each */
    start[0]  = 3;
    start[1]  = 3;
    stride[0] = 4;
    stride[1] = 12;
    count[0]  = 1;
    count[1]  = 2;
    block[0]  = 3;
    block[1]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize 2 3x3 blocks */
    wbuf[60] = 60;
    wbuf[61] = 61;
    wbuf[62] = 62;

    wbuf[72] = 72;
    wbuf[73] = 73;
    wbuf[74] = 74;

    wbuf[79] = 79;
    wbuf[80] = 80;
    wbuf[81] = 81;

    wbuf[91] = 91;
    wbuf[92] = 92;
    wbuf[93] = 93;

    wbuf[98]  = 98;
    wbuf[99]  = 99;
    wbuf[100] = 100;

    wbuf[110] = 110;
    wbuf[111] = 111;
    wbuf[112] = 112;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_FILTER_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(did, &idx_type) < 0)
        TEST_ERROR;
    if (idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array chunk index");

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    if (filtered) {
        /* Get filter info by filter number 0 for section "selection" */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 0, NULL, &my_cd_nelmts, &my_cd_value,
                                        (size_t)0, NULL, NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_DEFLATE)
            TEST_ERROR;
        if (my_cd_nelmts != 1)
            TEST_ERROR;
        if (my_cd_value != 9)
            TEST_ERROR;

        /* Get filter info by filter number 1 for section "selection" */
        if ((filter_id = H5Pget_filter3(dcpl, H5_SECTION_SELECTION, 1, &flags, NULL, NULL, (size_t)0, NULL,
                                        NULL)) < 0)
            TEST_ERROR;
        if (filter_id != H5Z_FILTER_SHUFFLE)
            TEST_ERROR;
        if (flags != H5Z_FLAG_OPTIONAL)
            TEST_ERROR;

        H5E_BEGIN_TRY
        {
            /* Get filter info by filter id for section "fixed data" */
            filter_id = H5Pget_filter_by_id3(dcpl, H5_SECTION_FIXED, H5Z_FILTER_DEFLATE, &flags,
                                             &my_cd_nelmts, &my_cd_value, (size_t)0, NULL, NULL);
        }
        H5E_END_TRY
        /* No filter for section "fixed data" */
        if (filter_id >= 0)
            TEST_ERROR;

        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options != H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }
    else {
        if (H5Pget_chunk_opts(dcpl, &options) < 0)
            TEST_ERROR;

        if (options == H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
            TEST_ERROR;
    }

    memset(rbuf, 0, sizeof(rbuf));
    if (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < 190; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* test_struct_chunk_2d_ea() */

/*-------------------------------------------------------------------------
 *  Function:    filter_class3_set_local
 *
 *  Purpose:     Set_local callback for H5Z_FiLTER_CLASS3 filter.
 *
 *  Return:      Success:        Data chunk size
 *              Failure:        0
 *-------------------------------------------------------------------------
 */
static herr_t
filter_class3_set_local(hid_t dcpl_id, hid_t H5_ATTR_UNUSED type_id, hid_t H5_ATTR_UNUSED space_id,
                        H5_section_type_t sec_type)
{
    unsigned flags;         /* Filter flags */
    size_t   cd_nelmts = 1; /* Number of filter parameters */
    unsigned cd_values[1];  /* Filter parameters */

    /* Get the filter's current parameters */
    if (H5Pget_filter_by_id3(dcpl_id, sec_type, H5Z_FILTER_CLASS3, &flags, &cd_nelmts, cd_values, (size_t)0,
                             NULL, NULL) < 0)
        return (FAIL);

    /* Check that the parameter values were passed along correctly */
    cd_values[0] = FILTER_PARAM_MOD;

    /* Modify the filter's parameters for this dataset */
    if (H5Pmodify_filter2(dcpl_id, sec_type, H5Z_FILTER_CLASS3, flags, cd_nelmts, cd_values) < 0)
        return (FAIL);

    return (SUCCEED);
} /* filter_class3_set_local() */

/*-------------------------------------------------------------------------
 *  Function:    filter_class3
 *
 *  Purpose:     This filter counts the number of bytes read and written,
 *               incrementing count_nbytes_read or count_nbytes_written as
 *               appropriate.
 *
 *  Return:      Success:        Data chunk size
 *              Failure:        0
 *-------------------------------------------------------------------------
 */
static size_t
filter_class3(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
              const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes, size_t H5_ATTR_UNUSED *buf_size,
              void H5_ATTR_UNUSED **buf)
{
    if (flags & H5Z_FLAG_REVERSE)
        filter_bytes_read += nbytes;
    else
        filter_bytes_written += nbytes;

    return nbytes;
} /* filter_class3() */

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_filter_register()
 *
 * Purpose:     Verify H5Zregister with H5Z_class3_t
 *              Also verify APIs:
 *              --H5Pmodify_filter2()
 *              --H5Premove_filter2()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_filter_register(hid_t fcpl, hid_t fapl)
{
    char    filename[FILENAME_BUF_SIZE];    /* File name */
    hid_t   fid          = H5I_INVALID_HID; /* File ID */
    hid_t   sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t   did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t   dcpl         = H5I_INVALID_HID; /* Creation plist */
    hsize_t dim[1]       = {10};            /* 1-d dataspace */
    hsize_t chunk_dim[1] = {5};             /* Chunk size */
    int     wbuf[10];                       /* Write buffer */
    int     rbuf[10];                       /* Read buffer */
    hsize_t start[1];
    hsize_t stride[1];
    hsize_t count[1];
    hsize_t block[1];

    unsigned int cd_values[1] = {FILTER_PARAM};
    size_t       cd_nelmts    = 1;
    int          nfilters;

    H5F_libver_t low, high; /* File format bound */
    bool         fail_as_expected = false;

    TESTING("structured chunk dataset with filter register");

    if (H5Pget_libver_bounds(fapl, &low, &high) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Zregister(H5Z_TEST_CLASS3) < 0)
        TEST_ERROR;

    if (H5Pset_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_CLASS3, H5Z_FLAG_OPTIONAL, cd_nelmts,
                       cd_values) < 0)
        TEST_ERROR;

    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(fid, SPARSE_FILTER_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    }
    H5E_END_TRY

    /* Should fail for high bound < latest format */
    if (high < H5F_LIBVER_LATEST) {
        if (did >= 0)
            TEST_ERROR;
        else {
            /* Fail as expected: clean up and return succeed */
            fail_as_expected = true;
            goto done;
        }
    }
    else if (did < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    if (H5Pget_filter_by_id3(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_CLASS3, NULL, &cd_nelmts, cd_values,
                             (size_t)0, NULL, NULL) < 0)
        TEST_ERROR;

    if (cd_nelmts != 1)
        TEST_ERROR;
    if (cd_values[0] != FILTER_PARAM_MOD)
        TEST_ERROR;

    /* Starting at 3, select 1 block of size 3 */
    /* Selection is across 2 chunks */
    start[0]  = 3;
    stride[0] = 6;
    count[0]  = 1;
    block[0]  = 3;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    /* Write sparse data to the dataset */
    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize and write sparse data to the dataset */
    wbuf[3] = 1;
    wbuf[4] = 2;
    wbuf[5] = 3;

    if (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (!filter_bytes_written)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_FILTER_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Dget_create_plist(did)) < 0)
        TEST_ERROR;

    memset(rbuf, 0, sizeof(rbuf));
    if (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    if (!filter_bytes_read)
        TEST_ERROR;

    if (rbuf[3] != wbuf[3] || rbuf[4] != wbuf[4] || rbuf[5] != wbuf[5])
        TEST_ERROR;

    if (H5Premove_filter2(dcpl, H5_SECTION_SELECTION, H5Z_FILTER_CLASS3) < 0)
        TEST_ERROR;

    if (H5Pget_nfilters2(dcpl, H5_SECTION_SELECTION, &nfilters) < 0)
        TEST_ERROR;

    if (nfilters)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

done:
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    if (fail_as_expected) {
        PASSED();
        return SUCCEED;
    }

    return FAIL;
} /* test_struct_chunk_filter_register() */

#ifdef TBD

/*-------------------------------------------------------------------------
 * Function:    test_struct_chunk_api_defined_erase
 *
 * Purpose:     Verify APIs for handling sparse data:
 *              --H5Dget_defined()
 *              --H5Derase()
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_struct_chunk_api_defined_erase(hid_t fapl)
{
    char    filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t   fid          = H5I_INVALID_HID;
    hid_t   sid          = H5I_INVALID_HID;
    hid_t   sid1         = H5I_INVALID_HID;
    hid_t   sid2         = H5I_INVALID_HID;
    hid_t   dcpl         = H5I_INVALID_HID;
    hid_t   did          = H5I_INVALID_HID;
    hsize_t dim[1]       = {50}; /* 1-d dataspace */
    hsize_t chunk_dim[1] = {5};  /* Chunk size */
    int     wbuf[50];            /* Write buffer */
    herr_t  ret;

    TESTING("APIs for handling sparse data");

    /* Create a file */
    h5_fixname(FILENAME_TBD[0], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR;

    /* Create property list for compact dataset creation */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_STRUCT_CHUNK) < 0)
        TEST_ERROR;

    if (H5Pset_struct_chunk(dcpl, 1, chunk_dim, H5D_SPARSE_CHUNK) < 0)
        TEST_ERROR;

    if ((did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write sparse data to the dataset */
    memset(wbuf, 0, sizeof(wbuf));

    /* Initialize and write sparse data to the dataset */
    wbuf[1]  = 1;
    wbuf[12] = 12;
    wbuf[13] = 13;
    wbuf[14] = 14;
    wbuf[22] = 22;
    wbuf[23] = 23;
    wbuf[24] = 24;
    wbuf[48] = 48;
    wbuf[49] = 49;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    /* Get defined elements */
    /* TBD: Verify that dataset with H5D_SPARSE_CHUNK layout will succeed; otherwise fail */
    if ((sid1 = H5Dget_defined(did, H5S_ALL, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* TBD: Verify defined elements in sid1 are as expected */

    /* Erase all defined elements */
    /* TBD: Verify that dataset with H5D_SPARSE_CHUNK layout will succeed; otherwise fail */
    /* Since it is not supported yet, it is expected to fail */
    H5E_BEGIN_TRY
    {
        ret = H5Derase(did, sid1, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Call H5Dget_defined() again after H5Derase() */
    if ((sid2 = H5Dget_defined(did, H5S_ALL, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* TBD: Verify nothing is defined in sid2 */

    if (H5Sclose(sid1) < 0)
        TEST_ERROR;
    if (H5Sclose(sid2) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Pclose(dcpl);
        H5Dclose(did);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_struct_chunk_api_defined_erase() */

/*-------------------------------------------------------------------------
 * Function:    test_sparse_direct_chunk
 *
 * Purpose:     Verify APIs for direct chunk I/O on structured chunk:
 *                  --H5Dwrite_struct_chunk()
 *                  --H5Dread_struct_chunk()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sparse_direct_chunk(hid_t fapl)
{
    char  filename[FILENAME_BUF_SIZE]; /* File name */
    hid_t fid  = H5I_INVALID_HID;
    hid_t did  = H5I_INVALID_HID;
    hid_t sid  = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;

    hsize_t dims[2]       = {NX, NY};
    hsize_t maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t chunk_dims[2] = {CHUNK_NX, CHUNK_NY};

    int     buf[NX][NY];
    size_t  encode_size;
    hsize_t start[2], block[2], count[2];

    hsize_t                 wr_offset[2] = {0, 0};
    H5D_struct_chunk_info_t wr_chk_info;
    uint16_t                wr_filter_mask[2] = {0, 0};
    size_t                  wr_section_size[2];
    void                   *wr_buf[2];
    unsigned char          *wr_buf0;
    int                    *wr_buf1;

    hsize_t                 rd_offset[2] = {5, 5};
    H5D_struct_chunk_info_t rd_chk_info;
    uint16_t                rd_filter_mask[2] = {0, 0};
    size_t                  rd_section_size[2];
    void                   *rd_buf[2];
    unsigned char          *rd_buf0;
    int                    *rd_buf1;

    TESTING("APIs for direct chunk I/O on structured chunks");

    SKIPPED();
    return 0;

    /* Create a file */
    h5_fixname(FILENAME_TBD[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /*
     * Create the data space with unlimited dimensions.
     */
    if ((sid = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /*
     * Create a new dataset within the file using dcpl
     */
    if ((did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    start[0] = 3;
    start[1] = 2;
    block[0] = 2;
    block[1] = 3;
    count[0] = count[1] = 1;
    /* Select the 2x3 block in chunk index 0 for writing */
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR;

    /* Get the encoded size for the selection */
    if (H5Sencode2(sid, NULL, &encode_size, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Set up section size for section 0 and section 1 */
    wr_section_size[0] = encode_size;
    wr_section_size[1] = block[0] * block[1] * sizeof(int);

    /* Allocate buffers for section 0 (encoded selection) and section 1 (data) */
    if ((wr_buf0 = (unsigned char *)calloc((size_t)1, encode_size)) == NULL)
        TEST_ERROR;
    if ((wr_buf1 = (int *)calloc((size_t)1, wr_section_size[1])) == NULL)
        TEST_ERROR;

    /* Encode selection into the buffer for section 0 */
    if (H5Sencode2(sid, wr_buf0, &encode_size, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Set up data into the buffer for section 1 */
    wr_buf1[0] = 32;
    wr_buf1[1] = 33;
    wr_buf1[2] = 34;
    wr_buf1[3] = 42;
    wr_buf1[4] = 43;
    wr_buf1[5] = 44;

    /* Set up the buffer for H5D_write_struct_chunk() */
    wr_buf[0] = wr_buf0;
    wr_buf[1] = wr_buf1;

    wr_chk_info.type              = 4; /* should be H5D_SPARSE_CHUNK */
    wr_chk_info.num_sections      = 2;
    wr_chk_info.filter_mask       = wr_filter_mask;
    wr_chk_info.section_size      = wr_section_size;
    wr_chk_info.section_orig_size = wr_section_size;

    /* Write the structured chunk at offset [0,0]: chunk index 0 */
    if (H5Dwrite_struct_chunk(did, H5P_DEFAULT, wr_offset, &wr_chk_info, wr_buf) < 0)
        TEST_ERROR;

    /* Read the whole dataset */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;
    /* TBD: Verify buf read has data as in wr_buf1[] at location wr_buf0[] */

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if ((did = H5Dopen2(fid, SPARSE_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    if ((sid = H5Dget_space(did)) == H5I_INVALID_HID)
        TEST_ERROR;

    /* Select the 2x1 block in chunk index 3 for reading */
    start[0] = 5;
    start[1] = 5;
    block[0] = 2;
    block[1] = 1;
    count[0] = count[1] = 1;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, block) < 0)
        TEST_ERROR;

    if (H5Sencode2(sid, NULL, &encode_size, H5P_DEFAULT) < 0)
        TEST_ERROR;

    rd_section_size[0] = encode_size;
    rd_section_size[1] = block[0] * block[1] * sizeof(int);

    /* Allocate buffers for section 0 (encoded selection) and section 1 (data) */
    if ((rd_buf0 = (unsigned char *)calloc((size_t)1, encode_size)) == NULL)
        TEST_ERROR;
    if ((rd_buf1 = (int *)calloc((size_t)1, rd_section_size[1])) == NULL)
        TEST_ERROR;

    rd_buf[0] = rd_buf0;
    rd_buf[1] = rd_buf1;

    rd_chk_info.type              = 4; /* should be H5D_SPARSE_CHUNK */
    rd_chk_info.num_sections      = 2;
    rd_chk_info.filter_mask       = rd_filter_mask;
    rd_chk_info.section_size      = rd_section_size;
    rd_chk_info.section_orig_size = rd_section_size;

    /* Read the structured chunk at offset [5,5] */
    if (H5Dread_struct_chunk(did, H5P_DEFAULT, rd_offset, &rd_chk_info, rd_buf) < 0)
        TEST_ERROR;
    /* Verify rd_chk_info and rd_buf are the same as wr_chk_info and wr_buf */

    /*
     * Close/release resources.
     */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    H5_FAILED();
    return FAIL;

} /* test_sparse_direct_chunk() */

/*-------------------------------------------------------------------------
 * Function:    verify_get_struct_chunk_info (helper function)
 *
 * Purpose:     Verifies that H5Dget_struct_chunk_info returns correct
 *              values for a chunk.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
verify_get_struct_chunk_info(hid_t did, hid_t sid, hsize_t chk_index,
                             const hsize_t H5_ATTR_UNUSED          *exp_offset,
                             H5D_struct_chunk_info_t H5_ATTR_UNUSED exp_chunk_info[],
                             hsize_t H5_ATTR_UNUSED                 exp_chk_size)
{
    hsize_t                 out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t                 out_chk_size  = 0;      /* Size of an allocated/written chunk */
    haddr_t                 out_addr      = 0;      /* Address of an allocated/written chunk */
    H5D_struct_chunk_info_t out_chunk_info[50];

    /* Get info of the chunk specified by chk_index */
    if (H5Dget_struct_chunk_info(did, sid, chk_index, out_offset, out_chunk_info, &out_addr, &out_chk_size) <
        0)
        TEST_ERROR;

#ifdef TBD

    /* Verify info from H5Dget_struct_chunk_info() with expected chunk info */

    if (out_offset[0] != exp_offset[0])
        FAIL_PUTS_ERROR("unexpected offset[0]");
    if (out_offset[1] != exp_offset[1])
        FAIL_PUTS_ERROR("unexpected offset[1]");

    Compare out_chunk_info with exp_chunk_info

        if (HADDR_UNDEF == out_addr) FAIL_PUTS_ERROR("address cannot be HADDR_UNDEF");

    if (out_chk_size != exp_chk_size)
        FAIL_PUTS_ERROR("unexpected chunk size");

#endif

    /* For now, just return SUCCEED */

    return SUCCEED;

error:
    return FAIL;
} /* verify_get_struct_chunk_info() */

/*-------------------------------------------------------------------------
 *
 * Function:    verify_get_struct_chunk_info_by_coord (helper function)
 *
 * Purpose:     Verifies that H5Dget_struct_chunk_info_by_coord returns correct
 *              values for a chunk.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
verify_get_struct_chunk_info_by_coord(hid_t did, hsize_t *offset,
                                      H5D_struct_chunk_info_t H5_ATTR_UNUSED exp_chunk_info[],
                                      hsize_t H5_ATTR_UNUSED                 exp_chk_size)
{
    hsize_t                 out_chk_size = 0; /* Size of an allocated/written chunk */
    haddr_t                 out_addr     = 0; /* Address of an allocated/written chunk */
    H5D_struct_chunk_info_t out_chunk_info[50];

    /* Get info of the chunk at logical coordinates specified by offset */
    if (H5Dget_struct_chunk_info_by_coord(did, offset, out_chunk_info, &out_addr, &out_chk_size) < 0)
        TEST_ERROR;

#ifdef TBD
    {
        /* Verify info from H5Dget_struct_chunk_info_by_coord() with expected chunk info */

        if (HADDR_UNDEF == out_addr)
            FAIL_PUTS_ERROR("address cannot be HADDR_UNDEF");

        Compare out_chunk_info with exp_chunk_info

            if (out_chk_size != exp_chk_size) FAIL_PUTS_ERROR("unexpected chunk size");
    }
#endif

    /* For now, just return SUCCEED */

    return SUCCEED;

error:
    return FAIL;
} /* verify_get_struct_chunk_info_by_coord() */

typedef struct struct_chunk_iter_info_t {
    hsize_t                  offset[2];
    H5D_struct_chunk_info_t *chunk_info;
    haddr_t                  addr;
    hsize_t                  chunk_size;
} struct_chunk_iter_info_t;

typedef struct struct_chunk_iter_udata_t {
    struct_chunk_iter_info_t *struct_chunk_info;
    int                       last_index;
} struct_chunk_iter_udata_t;

static int
iter_cb_struct(const hsize_t *offset, H5D_struct_chunk_info_t *chunk_info, haddr_t *addr, hsize_t *chunk_size,
               void *op_data)
{
    struct_chunk_iter_udata_t *cidata = (struct_chunk_iter_udata_t *)op_data;
    int                        idx    = cidata->last_index + 1;

    cidata->struct_chunk_info[idx].offset[0]  = offset[0];
    cidata->struct_chunk_info[idx].offset[1]  = offset[1];
    cidata->struct_chunk_info[idx].chunk_info = chunk_info;
    cidata->struct_chunk_info[idx].addr       = *addr;
    cidata->struct_chunk_info[idx].chunk_size = *chunk_size;

    cidata->last_index++;

    return H5_ITER_CONT;
} /* iter_cb_struct() */

/*-------------------------------------------------------------------------
 * Function:    test_sparse_direct_chunk_query
 *
 * Purpose:     Verify APIs for direct chunk I/O query on structured chunk:
 *                  --H5Dget_struct_chunk_info()
 *                  --H5Dget_struct_chunk_info_by_coord()
 *                  --H5Dstruct_chunk_iter()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_sparse_direct_chunk_query(hid_t fapl)
{
    char    filename[FILENAME_BUF_SIZE];          /* File name */
    hid_t   fid           = H5I_INVALID_HID;      /* File ID */
    hid_t   sid           = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   did           = H5I_INVALID_HID;      /* Dataset ID */
    hid_t   dcpl          = H5I_INVALID_HID;      /* Creation plist */
    hsize_t dims[2]       = {NX, NY};             /* Dataset dimensions */
    hsize_t chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */

    struct_chunk_iter_info_t  chunk_infos[2]; /* Chunk infos filled up by iterator */
    struct_chunk_iter_udata_t udata;          /* udata for iteration */
    H5D_struct_chunk_info_t   chk_info;

    uint16_t filter_mask[2]  = {0, 0};
    hsize_t  offset[2]       = {0, 0};
    size_t   section_size[2] = {32, 48};
    void    *write_buf[2];
    hsize_t  in0[4] = {3, 2, 4, 4};              /* Encoded coordinates: [3,2] - [4,4] */
    hsize_t  in1[6] = {66, 69, 72, 96, 99, 102}; /* Data: 66,69,72,96,99,102 */

    TESTING("APIs for direct chunk I/O query on structured chunk");

    SKIPPED();
    return 0;

    /* Create the file */
    h5_fixname(FILENAME_TBD[2], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Enable chunking */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /* Create a new dataset using dcpl creation properties */
    did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (did < 0)
        TEST_ERROR;

    write_buf[0] = in0;
    write_buf[1] = in1;

    chk_info.type              = 4; /* should be H5D_SPARSE_CHUNK */
    chk_info.num_sections      = 2;
    chk_info.filter_mask       = filter_mask;
    chk_info.section_size      = section_size;
    chk_info.section_orig_size = section_size;

    /* Write the structured chunk at offset */
    if (H5Dwrite_struct_chunk(did, H5P_DEFAULT, offset, &chk_info, write_buf) < 0)
        TEST_ERROR;

    /* Verify info of the first and only chunk via H5Dget_struct_chunk_info() */
    if (verify_get_struct_chunk_info(did, H5S_ALL, 0, offset, &chk_info, CHK_SIZE) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_struct_chunk_info failed\n");

    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;

    /* Write the structured chunk at offset */
    if (H5Dwrite_struct_chunk(did, H5P_DEFAULT, offset, &chk_info, write_buf) < 0)
        TEST_ERROR;

    /* Verify info of the chunk at offset [CHUNK_NX,CHUNK_NY] via H5Dget_struct_chunk_info_by_coord() */
    if (verify_get_struct_chunk_info_by_coord(did, offset, &chk_info, CHK_SIZE) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_struct_chunk_info_by_coord failed\n");

    /* For now, H5Dstruct_chunk_iter() just returns SUCCEED without actual iteration */
    udata.struct_chunk_info = chunk_infos;
    udata.last_index        = -1;
    if (H5Dstruct_chunk_iter(did, H5P_DEFAULT, &iter_cb_struct, &udata) < 0)
        TEST_ERROR;

    /* Release resource */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Remove the test file */
    HDremove(filename);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
} /* test_sparse_direct_chunk_query() */

typedef struct chunk_iter_info_t {
    hsize_t  offset[2];
    unsigned filter_mask;
    haddr_t  addr;
    hsize_t  size;
} chunk_iter_info_t;

typedef struct chunk_iter_udata_t {
    chunk_iter_info_t *chunk_info;
    int                last_index;
} chunk_iter_udata_t;

static int
iter_cb(const hsize_t *offset, unsigned filter_mask, haddr_t addr, hsize_t size, void *op_data)
{
    chunk_iter_udata_t *cidata = (chunk_iter_udata_t *)op_data;
    int                 idx    = cidata->last_index + 1;

    cidata->chunk_info[idx].offset[0]   = offset[0];
    cidata->chunk_info[idx].offset[1]   = offset[1];
    cidata->chunk_info[idx].filter_mask = filter_mask;
    cidata->chunk_info[idx].addr        = addr;
    cidata->chunk_info[idx].size        = size;

    cidata->last_index++;

    return H5_ITER_CONT;
} /* iter_cb() */

/*-------------------------------------------------------------------------
 * Function:    test_dense_chunk_api_on_sparse()
 *
 * Purpose: Verify the following dense chunk APIs will fail for
 *          H5D_SPARSE_CHUNK layout:
 *             --H5Dwrite_chunk()
 *             --H5Dget_chunk_info()
 *             --H5Dget_chunk_info_by_coord()
 *             --H5Dchunk_iter()
 *          Verify the following dense chunk APIs will succeed for
 *          H5D_SPARSE_CHUNK layout:
 *              --H5Dread_chunk()
 *              --H5Dget_chunk_storage_size()
 *              --H5Dget_num_chunks()
 *
 * Return:      # of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_dense_chunk_api_on_sparse(hid_t fapl)
{
    char               filename[FILENAME_BUF_SIZE];          /* File name */
    hid_t              fid           = H5I_INVALID_HID;      /* File ID */
    hid_t              sid           = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t              did           = H5I_INVALID_HID;      /* Dataset ID */
    hid_t              dcpl          = H5I_INVALID_HID;      /* Creation plist */
    hsize_t            dims[2]       = {NX, NY};             /* Dataset dimensions */
    hsize_t            chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */
    chunk_iter_info_t  chunk_infos[2];
    chunk_iter_udata_t udata;
    hsize_t            nchunks = 0;
    hsize_t            chunk_nbytes;
    hsize_t            offset[2] = {0, 0};
    int                direct_buf[CHUNK_NX][CHUNK_NY];
    haddr_t            addr    = 0;
    uint32_t           filters = 0;

    TESTING("APIs for direct chunk I/O: dense chunk functions on sparse layout");

    /* Create the file */
    h5_fixname(FILENAME_TBD[3], fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Enable chunking */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* TBD: need to set to H5D_SPARSE_CHUNK */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0)
        TEST_ERROR;

    /* The layout is set to H5D_CHUNKED as a side-effect */
    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /* Create a new dataset using dcpl creation properties */
    did = H5Dcreate2(fid, SPARSE_DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (did < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        H5Dwrite_chunk(did, H5P_DEFAULT, 0, offset, CHK_SIZE, direct_buf);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5E_BEGIN_TRY
    {
        H5Dget_chunk_info(did, H5S_ALL, 0, NULL, NULL, &addr, NULL);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5E_BEGIN_TRY
    {
        H5Dget_chunk_info_by_coord(did, offset, NULL, &addr, NULL);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5E_BEGIN_TRY
    {
        udata.chunk_info = chunk_infos;
        udata.last_index = -1;
        H5Dchunk_iter(did, H5P_DEFAULT, &iter_cb, &udata);
    }
    H5E_END_TRY
    /* TBD: set return status and verify that it should fail */

    H5Dread_chunk(did, H5P_DEFAULT, offset, &filters, direct_buf);
    /* TBD: should succeed */

    H5Dget_num_chunks(did, sid, &nchunks);
    /* TBD: should succeed */

    H5Dget_chunk_storage_size(did, offset, &chunk_nbytes);
    /* TBD: should succeed */

    /* Release resource */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Remove the test file */
    HDremove(filename);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
} /* test_dense_chunk_api_on_sparse() */

#endif /* TBD */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests for structured chunk layout
 *              Some are copied and modified from:
 *               --test/dsets.c
 *               --test/direct_chunk.c
 *               --test/chunk_info.c
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned     paged;
    unsigned     filtered;
    unsigned     early;
    int          nerrors = 0;
    const char  *driver_name;
    bool         contig_addr_vfd; /* Whether VFD used has a contiguous address space */
    bool         driver_is_default_compatible;
    hid_t        fcpl        = H5I_INVALID_HID;
    hid_t        page_fcpl   = H5I_INVALID_HID;
    hid_t        fapl        = H5I_INVALID_HID;
    hid_t        libver_fapl = H5I_INVALID_HID;
    H5F_libver_t low, high; /* File format bounds */

    /* Don't run this test using certain file drivers */
    driver_name = h5_get_test_driver_name();

    /* Current VFD that does not support contiguous address space */
    contig_addr_vfd = (bool)(strcmp(driver_name, "split") != 0 && strcmp(driver_name, "multi") != 0);

    /* Testing setup */
    h5_test_init();

    fapl = h5_fileaccess();

    if (h5_driver_is_default_vfd_compatible(fapl, &driver_is_default_compatible) < 0)
        TEST_ERROR;

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    if ((page_fcpl = H5Pcopy(fcpl)) < 0)
        TEST_ERROR;

    /* Set file space strategy to paged aggregation and persisting free-space */
    if (H5Pset_file_space_strategy(page_fcpl, H5F_FSPACE_STRATEGY_PAGE, true, (hsize_t)1) < 0)
        TEST_ERROR;

    /* Test with paged aggregation enabled or not */
    for (paged = false; paged <= true; paged++) {

        /* Temporary: skip testing for multi/split drivers:
             fail file create when persisting free-space or using paged aggregation strategy */
        if (!contig_addr_vfd && paged)
            continue;

        for (early = false; early <= true; early++) {

            for (filtered = false; filtered <= true; filtered++) {

                for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
                    if ((libver_fapl = H5Pcopy(fapl)) < 0)
                        TEST_ERROR;

                    for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

                        hid_t       my_fcpl = H5I_INVALID_HID;
                        herr_t      ret;
                        const char *low_string;  /* Message for library version low bound */
                        const char *high_string; /* Message for library version high bound */

                        /* Set version bounds */
                        H5E_BEGIN_TRY
                        {
                            ret = H5Pset_libver_bounds(libver_fapl, low, high);
                        }
                        H5E_END_TRY

                        if (ret < 0) /* Invalid low/high combinations */
                            continue;

                        /* Paged aggregation needs high bound to be at least H5F_LIBVER_V110 */
                        if (paged && high < H5F_LIBVER_V110)
                            continue;

                        low_string  = h5_get_version_string(low);
                        high_string = h5_get_version_string(high);

                        if (paged) {
                            my_fcpl = page_fcpl;

                            if (early) {
                                if (filtered)
                                    printf("\nTesting with paged aggregation, early alloc, filtered and "
                                           "libver (%s, %s)\n",
                                           low_string, high_string);
                                else
                                    printf("\nTesting with paged aggregation, early alloc, non-filtered and "
                                           "libver (%s, %s)\n",
                                           low_string, high_string);
                            }
                            else {

                                if (filtered)
                                    printf("\nTesting with paged aggregation, default alloc, filtered and "
                                           "libver (%s, %s)\n",
                                           low_string, high_string);
                                else
                                    printf("\nTesting with paged aggregation, default alloc, non-filtered "
                                           "and libver (%s, %s)\n",
                                           low_string, high_string);
                            }
                        }
                        else {
                            my_fcpl = fcpl;

                            if (early) {
                                if (filtered)
                                    printf("\nTesting with non-paged aggregation, early alloc, filtered and "
                                           "libver (%s, %s)\n",
                                           low_string, high_string);
                                else
                                    printf("\nTesting with non-paged aggregation, early alloc, non-filtered "
                                           "and libver (%s, %s)\n",
                                           low_string, high_string);
                            }
                            else {
                                if (filtered)
                                    printf("\nTesting with non-paged aggregation, default alloc, filtered "
                                           "and libver (%s, %s)\n",
                                           low_string, high_string);
                                else
                                    printf("\nTesting with non-paged aggregation, default alloc, "
                                           "non-filtered and libver (%s, %s)\n",
                                           low_string, high_string);
                            }
                        }

                        nerrors +=
                            (test_struct_chunk_info_1d(my_fcpl, libver_fapl, filtered, early, CHK_SINGLE) < 0
                                 ? 1
                                 : 0);
                        nerrors +=
                            (test_struct_chunk_info_1d(my_fcpl, libver_fapl, filtered, early, CHK_FA) < 0
                                 ? 1
                                 : 0);
                        nerrors +=
                            (test_struct_chunk_info_1d(my_fcpl, libver_fapl, filtered, early, CHK_EA) < 0
                                 ? 1
                                 : 0);
                        nerrors +=
                            (test_struct_chunk_info_2d_bt2(my_fcpl, libver_fapl, filtered, early) < 0 ? 1
                                                                                                      : 0);
                        nerrors +=
                            (test_struct_chunk_extent_1d(my_fcpl, libver_fapl, filtered, early) < 0 ? 1 : 0);
                        nerrors +=
                            (test_struct_chunk_extent_2d(my_fcpl, libver_fapl, filtered, early, true) < 0
                                 ? 1
                                 : 0);
                        nerrors +=
                            (test_struct_chunk_extent_2d(my_fcpl, libver_fapl, filtered, early, false) < 0
                                 ? 1
                                 : 0);

                        nerrors += (test_struct_chunk_api(my_fcpl, libver_fapl) < 0 ? 1 : 0);
                        nerrors +=
                            (test_struct_chunk_1d_single(my_fcpl, libver_fapl, filtered, early) < 0 ? 1 : 0);
                        nerrors +=
                            (test_struct_chunk_2d_bt2(my_fcpl, libver_fapl, filtered, early) < 0 ? 1 : 0);
                        nerrors +=
                            (test_struct_chunk_1d_fa(my_fcpl, libver_fapl, filtered, early) < 0 ? 1 : 0);
                        nerrors +=
                            (test_struct_chunk_2d_ea(my_fcpl, libver_fapl, filtered, early) < 0 ? 1 : 0);
                        nerrors += (test_struct_chunk_filter_register(my_fcpl, libver_fapl) < 0 ? 1 : 0);

                        /* Tests to be worked on when APIs are implemented */
#ifdef TBD
                        nerrors += (test_struct_chunk_api_defined_erase(my_fapl) < 0 ? 1 : 0);
                        nerrors += (test_sparse_direct_chunk(my_fapl) < 0 ? 1 : 0);
                        nerrors += (test_sparse_direct_chunk_query(my_fapl) < 0 ? 1 : 0);
                        nerrors += (test_dense_chunk_api_on_sparse(my_fapl) < 0 ? 1 : 0);
#endif
                    } /* end for high */

                    h5_delete_all_test_files(FILENAME, libver_fapl);
                    if (H5Pclose(libver_fapl) < 0)
                        TEST_ERROR;

                } /* end for low */

            } /* end filtered */

        } /* end early */

    } /* end paged */

    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;

    if (H5Pclose(page_fcpl) < 0)
        TEST_ERROR;

    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    if (nerrors)
        goto error;
    printf("All structured chunk storage tests passed.\n");

    exit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d STRUCTURED CHUNK STORAGE TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    exit(EXIT_FAILURE);
} /* end main() */
