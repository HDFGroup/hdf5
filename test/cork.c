/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Vailin Choi
 *              Feb 20, 2014
 *
 * This file contains tests for:
 *  H5Odisable_mdc_flushes()
 *  H5Oenable_mdc_flushes()
 *  H5Oare_mdc_flushes_disabled()
 */
#include "h5test.h"

/*
 * This file needs to access private information from the H5C package.
 * This file also needs to access the metadata cache testing code.
 */
#define H5C_FRIEND        /*suppress error about including H5Cpkg      */
#define H5C_TESTING        /*suppress warning about H5C testing funcs*/
#include "H5Cpkg.h"        /* Cache                */


/* ============ */
/* Test Defines */
/* ============ */

#define FILENAME    "test_cork.h5"
#define ATTR        "ATTR"
#define DSET        "DSET"
#define DSET_BT1    "DSET_BT1"
#define DSET_COMPACT    "DSET_COMPACT"
#define DSET_CONTIG     "DSET_CONTIG"
#define DSET_EA     "DSET_EA"
#define DSET_BT2    "DSET_BT2"
#define DSET_FA     "DSET_FA"
#define DSET_NONE   "DSET_NONE"
#define GRP         "GRP"
#define GRP2        "GRP2"
#define GRP3        "GRP3"
#define DT      "DT"
#define DT2         "DT2"
#define DT3         "DT3"
#define GRP_ATTR    "GRP_ATTR"
#define DSET_ATTR   "DSET_ATTR"
#define DT_ATTR     "DT_ATTR"

#define RANK    2
#define DIM0    5
#define DIM1    10
#define DIMS0   50
#define DIMS1   100


/* ===================== */
/* Function Declarations */
/* ===================== */

/* Tests */
static unsigned test_objs_cork(hbool_t swmr, hbool_t new_format);
static unsigned test_dset_cork(hbool_t swmr, hbool_t new_format);
static unsigned verify_old_dset_cork(void);
static unsigned verify_obj_dset_cork(hbool_t swmr);
static unsigned verify_dset_cork(hbool_t swmr, hbool_t new_format);
static unsigned verify_group_cork(hbool_t swmr);
static unsigned verify_named_cork(hbool_t swmr);
static unsigned verify_multiple_cork(hbool_t swmr);


/*-------------------------------------------------------------------------
 * Function:    verify_old_dset_cork
 *
 * Purpose:     This function verifies corking operation for datasets
 *      created with old library format.  Cache entries associated with the
 *      object tag are checked for the correct cork status.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
verify_old_dset_cork(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                     /* File ID */
    hid_t did = -1, did2 = -1, did3 = -1;   /* Dataset IDs */
    hid_t dcpl = -1, dcpl2 = -1, dcpl3 = -1;     /* Dataset creation property lists */
    hid_t sid = -1, sid2 = -1, sid3 = -1;   /* Dataspace IDs */
    hsize_t dims[2] = {100, 20};        /* Dataset dimension sizes */
    hsize_t max_dims[2] = {100, H5S_UNLIMITED}; /* Dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {2, 5};     /* Dataset chunked dimension sizes */
    int buf[100][20];               /* Data buffer */
    int i = 0, j = 0;               /* Local index variable */
    H5O_info_t oinfo, oinfo2, oinfo3;       /* Object metadata information */
    hsize_t dims2[2] = {8, 16};         /* Dataset dimension sizes */

    /* Testing Macro */
    TESTING("cork status for datasets with old format");

    /* Create the file */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create dcpl */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    /* Set to use chunked dataset */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR

    /* Create chunked dataset with v1-btree indexing: DSET_BT1 */
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        TEST_ERROR
    if((did = H5Dcreate2(fid, DSET_BT1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get dataset object header address: DSET_BT1 */
    if(H5Oget_info2(did, &oinfo, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_BT1 */
    if(H5Odisable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Verify cork status */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Initialize data buffer */
    for(i = 0; i < (int)dims[0]; i++)
        for(j = 0; j < (int)dims[1]; j++)
            buf[i][j] = (i + 1) * (j + 1);

    /* Write to the dataset: DSET_BT1 */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_BT1 */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Create compact dataset: DSET_COMPACT */
    if((sid2 = H5Screate_simple(2, dims2, NULL)) < 0)
        FAIL_STACK_ERROR
    if((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl2, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_alloc_time(dcpl2, H5D_ALLOC_TIME_EARLY) < 0)
        FAIL_STACK_ERROR
    if((did2 = H5Dcreate2(fid, DSET_COMPACT, H5T_NATIVE_INT, sid2, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Get dataset object address */
    if(H5Oget_info2(did2, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_COMPACT */
    if(H5Odisable_mdc_flushes(did2) < 0)
        TEST_ERROR

    /* Verify cork status */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    if(H5Dclose(did2) < 0)
        TEST_ERROR
    if(H5Sclose(sid2) < 0)
        TEST_ERROR
    if(H5Pclose(dcpl2) < 0)
        TEST_ERROR

    if(H5Fclose(fid) < 0)
        TEST_ERROR

    /* Reopen the file */
    if((fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Initialize data buffer */
    for(i = 0; i < (int)dims[0]; i++)
        for(j = 0; j < (int)dims[1]; j++)
            buf[i][j] = (i + 1) * (j + 1);

    /* Open and write to the dataset: DSET_BT1 */
    if((did = H5Dopen2(fid, DSET_BT1, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create contiguous dataset: DSET_CONTIG */
    if((sid3 = H5Screate_simple(2, dims2, NULL)) < 0)
        FAIL_STACK_ERROR
    if((dcpl3 = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl3, H5D_CONTIGUOUS) < 0)
        FAIL_STACK_ERROR
    if((did3 = H5Dcreate2(fid, DSET_CONTIG, H5T_NATIVE_INT, sid3, H5P_DEFAULT, dcpl3, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Get dataset object address: DSET_CONTIG */
    if(H5Oget_info2(did3, &oinfo3, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_CONTIG */
    if(H5Odisable_mdc_flushes(did3) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_CONTIG */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_BT1 */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR

    /* Un-cork the dataset: DSET_CONTIG */
    if(H5Oenable_mdc_flushes(did3) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_CONTIG */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, FALSE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Dclose(did3) < 0)
        TEST_ERROR
    if(H5Pclose(dcpl3) < 0)
        TEST_ERROR
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Sclose(sid2);
        H5Sclose(sid3);
        H5Dclose(did);
        H5Dclose(did2);
        H5Dclose(did3);
        H5Pclose(dcpl);
        H5Pclose(dcpl2);
        H5Pclose(dcpl3);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* verify_old_dset_cork */


/*-------------------------------------------------------------------------
 * Function:    verify_obj_dset_cork
 *
 * Purpose:     This function verifies corking operations for dataset objects.
 *      Cache entries associated with the object tag are checked
 *              for the correct cork status.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
verify_obj_dset_cork(hbool_t swmr)
{
    /* Variable Declarations */
    hid_t fid = -1;             /* File ID */
    hid_t fapl = -1;            /* File access property list */
    hid_t aid = -1;             /* Attribute ID */
    hid_t sid = -1, sid2 = -1;  /* Dataspace IDs */
    hid_t did = -1, did2 = -1;  /* Dataset IDs */
    hid_t oid = -1;             /* Object ID */
    hid_t dcpl2;                /* Dataset creation property list */
    int i = 0;                  /* Local index variable */
    hsize_t dim[1] = {100};     /* Dataset dimension size */
    hsize_t chunk_dim[1] = {7}; /* Dataset chunk dimension size */
    H5O_info_t oinfo, oinfo2;   /* Object metadata information */
    char attrname[500];         /* Name of attribute */
    unsigned flags;             /* File access flags */

    if(swmr) {
        TESTING("cork status for dataset objects with attributes (SWMR)");
    } else {
        TESTING("cork status for dataset objects with attributes");
    }

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    /* Set to use latest format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    /* Create the file with/without SWMR access */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR

    /* Create dataset: DSET */
    if((did = H5Dcreate2(fid, DSET, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get dataset object header address */
    if(H5Oget_info2(did, &oinfo, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET */
    if(H5Odisable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Attach and write to an attribute to the dataset: DSET */
    if((aid = H5Acreate2(did, ATTR, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Close the attribute */
    if(H5Aclose(aid) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Create dcpl */
    if((dcpl2 = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    /* Set to early allocation for dataset space */
    if(H5Pset_alloc_time(dcpl2, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create chunked dataset with implicit indexing: DSET_NONE */
    if(H5Pset_chunk(dcpl2, 1, chunk_dim) < 0)
        FAIL_STACK_ERROR
    if((sid2 = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR
    if((did2 = H5Dcreate2(fid, DSET_NONE, H5T_NATIVE_INT, sid2, H5P_DEFAULT, dcpl2, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get dataset object header address */
    if(H5Oget_info2(did2, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_NONE */
    if(H5Odisable_mdc_flushes(did2) < 0)
        TEST_ERROR

    /* Attach 8 attributes to the dataset */
    for(i = 0; i < 8; i++) {
        HDsprintf(attrname, "attr %d", i);
        if((aid = H5Acreate2(did2, attrname, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR
        if(H5Awrite(aid, H5T_NATIVE_INT, &i) < 0)
            TEST_ERROR
        if(H5Aclose(aid) < 0)
            TEST_ERROR
    } /* end for */

    /* Verify cork status of the dataset: DSET_NONE */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Oclose(did) < 0)
        TEST_ERROR
    if(H5Oclose(did2) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Sclose(sid2) < 0)
        TEST_ERROR
    if(H5Pclose(dcpl2) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    /* Re-open the file */
    flags = H5F_ACC_RDWR;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fopen(FILENAME, flags, fapl)) < 0)
        TEST_ERROR

    /* Open the dataset object: DSET_NONE */
    if((oid = H5Oopen(fid, DSET_NONE, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, FALSE) < 0)
        TEST_ERROR

    /* Open the attribute attached to the dataset object: DSET_NONE */
    if((aid = H5Aopen_by_idx(oid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)4, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_NONE */
    if(H5Odisable_mdc_flushes(oid) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET_NONE */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Close the attribute */
    if(H5Aclose(aid) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Oclose(oid) < 0)
        TEST_ERROR
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
        H5Sclose(sid);
        H5Sclose(sid2);
        H5Dclose(did);
        H5Dclose(did2);
        H5Oclose(oid);
        H5Pclose(dcpl2);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* verify_obj_dset_cork */


/*-------------------------------------------------------------------------
 * Function:    verify_dset_cork
 *
 * Purpose:     This function verifies corking operations for chunked datasets
 *      with different indexing types.
 *      Cache entries associated with the object tag are checked
 *              for the correct cork status.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
verify_dset_cork(hbool_t swmr, hbool_t new_format)
{
    /* Variable Declarations */
    hid_t fid = -1;                     /* File ID */
    hid_t fapl = -1;                    /* File access property list */
    hid_t did = -1, did2 = -1, did3 = -1;   /* Dataset IDs */
    hid_t dcpl = -1;                    /* Dataset creation property list */
    hid_t sid = -1, sid2 = -1, sid3 = -1;   /* Dataspace IDs */
    hsize_t dims[2] = {100, 20};        /* Dataset dimension sizes */
    hsize_t max_dims[2] = {100, H5S_UNLIMITED}; /* Dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {2, 5};     /* Dataset chunked dimension sizes */
    int buf[100][20]; int i = 0, j = 0;         /* Data buffer */
    H5O_info_t oinfo, oinfo2, oinfo3;       /* Object metadata information */
    unsigned flags;             /* File access flags */

    /* Testing Macro */
    if(swmr) {
        if(new_format) {
            TESTING("cork status for chunked datasets with different indexing types (SWMR & latest)");
        } else {
            TESTING("cork status for chunked datasets with different indexing types (SWMR & non-latest)");
        } /* end if */
    } else {
        if(new_format) {
            TESTING("cork status for chunked datasets with different indexing types (non-SWMR & latest)");
        } else {
            TESTING("cork status for chunked datasets with different indexing types (non-SWMR & non-latest)");
        } /* end if */
    } /* end if */

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    if(new_format) {
        /* Set to use latest format */
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            TEST_ERROR
    } /* end if */

    /* Create the file */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create dcpl */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    /* Set to use chunked dataset */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR

    /* Create chunked dataset with extensive array indexing: DSET_EA */
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        TEST_ERROR
    if((did = H5Dcreate2(fid, DSET_EA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get dataset object header address: DSET_EA */
    if(H5Oget_info2(did, &oinfo, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_EA */
    if(H5Odisable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Verify cork status */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Create chunked dataset with fixed array indexing: DSET_FA */
    if((sid2 = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR
    if((did2 = H5Dcreate2(fid, DSET_FA, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get dataset object header address: DSET_FA */
    if(H5Oget_info2(did2, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_FA */
    if(H5Odisable_mdc_flushes(did2) < 0)
        TEST_ERROR

    /* Uncork the dataset: DSET_EA */
    if(H5Oenable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_FA */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_EA */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR

    /* Create chunked dataset with v2-Btree indexing */
    max_dims[0] = H5S_UNLIMITED;
    if((sid3 = H5Screate_simple(2, dims, max_dims)) < 0)
        TEST_ERROR
    if((did3 = H5Dcreate2(fid, DSET_BT2, H5T_NATIVE_INT, sid3, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get dataset object header address: DSET_BT2 */
    if(H5Oget_info2(did3, &oinfo3, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_BT2 */
    if(H5Odisable_mdc_flushes(did3) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_BT2 */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Dclose(did3) < 0)
        TEST_ERROR
    if(H5Sclose(sid3) < 0)
        TEST_ERROR

    if(H5Dclose(did2) < 0)
        TEST_ERROR
    if(H5Sclose(sid2) < 0)
        TEST_ERROR

    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR

    if(H5Pclose(dcpl) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    /* Reopen the file */
    flags = H5F_ACC_RDWR;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fopen(FILENAME, flags, fapl)) < 0)
        TEST_ERROR

    /* Initialize data buffer */
    for(i = 0; i < (int)dims[0]; i++)
        for(j = 0; j < (int)dims[1]; j++)
            buf[i][j] = (i + 1) * (j + 1);

    /* Open and write to the dataset: DSET_EA */
    if((did = H5Dopen2(fid, DSET_EA, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_EA */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR

    /* Open and write to the dataset: DSET_FA */
    if((did2 = H5Dopen2(fid, DSET_FA, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_FA */
    if(H5Odisable_mdc_flushes(did2) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_FA */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Open and write to the dataset: DSET_BT2 */
    if((did3 = H5Dopen2(fid, DSET_BT2, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(did3, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_BT2 */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, FALSE) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET_BT2 */
    if(H5Odisable_mdc_flushes(did3) < 0)
        TEST_ERROR

    /* Verify the cork status for DSET_BT2 */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Dclose(did2) < 0)
        TEST_ERROR
    if(H5Dclose(did3) < 0)
        TEST_ERROR
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Sclose(sid2);
        H5Sclose(sid3);
        H5Dclose(did);
        H5Dclose(did2);
        H5Dclose(did3);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* verify_dset_cork */



/*-------------------------------------------------------------------------
 * Function:    verify_group_cork
 *
 * Purpose:     This function verifies corking operations for groups.
 *      Cache entries associated with the object tag are checked
 *              for the correct cork status.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
verify_group_cork(hbool_t swmr)
{
    /* Variable Declarations */
    hid_t fid = -1;                     /* File ID */
    hid_t fapl = -1;                    /* File access property list */
    hid_t gid = -1, gid2 = -1, gid3 = -1;   /* Group IDs */
    H5O_info_t oinfo, oinfo2, oinfo3;       /* Object metadata information */
    hid_t aid;                  /* Attribute ID */
    hid_t sid;                  /* Dataspace ID */
    char attrname[500];                 /* Name of attribute */
    unsigned flags;             /* File access flags */
    int i = 0;                          /* Local index variable */

    /* Testing Macro */
    if(swmr) {
        TESTING("cork status for groups (SWMR)");
    } else {
        TESTING("cork status for groups");
    }

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    /* Set to use latest format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    /* Create the file */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create 3 groups */
    if((gid = H5Gcreate2(fid, GRP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((gid2 = H5Gcreate2(gid, GRP2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((gid3 = H5Gcreate2(gid2, GRP3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the second group: GRP2 */
    if(H5Odisable_mdc_flushes(gid2) < 0)
        TEST_ERROR

    /* Get group object header addresses */
    if(H5Oget_info2(gid, &oinfo, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5Oget_info2(gid2, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5Oget_info2(gid3, &oinfo3, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Verify cork status of the groups */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, FALSE) < 0)
        TEST_ERROR

    /* Close the second group: GRP2 */
    if(H5Gclose(gid2) < 0)
        TEST_ERROR

    /* Re-open the second group: GRP2 */
    if((gid2 = H5Gopen2(gid, GRP2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify cork status of the second group: GRP2 */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, FALSE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Gclose(gid) < 0)
        TEST_ERROR
    if(H5Gclose(gid2) < 0)
        TEST_ERROR
    if(H5Gclose(gid3) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    /* Re-open the file and the three groups */
    flags = H5F_ACC_RDWR;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fopen(FILENAME, flags, fapl)) < 0)
        FAIL_STACK_ERROR
    if((gid = H5Gopen2(fid, GRP, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((gid2 = H5Gopen2(gid, GRP2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((gid3 = H5Gopen2(gid2, GRP3, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create dataspace */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR

    /* Attach 8 attributes to the third group: GRP3 */
    for(i = 0;i < 8; i++) {
        HDsprintf(attrname, "attr %d", i);
        if((aid = H5Acreate2(gid3, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR
        if(H5Awrite(aid, H5T_NATIVE_UINT, &i) < 0)
            TEST_ERROR
        /* Cork the third group while attaching attributes */
        if(i == 3) {
            if(H5Odisable_mdc_flushes(gid3) < 0)
                TEST_ERROR
            if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
                TEST_ERROR
        }
        if(H5Aclose(aid) < 0)
            TEST_ERROR
    } /* end for */

    /* Verify cork status of the third group: GRP3 */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Gclose(gid) < 0)
        TEST_ERROR
    if(H5Gclose(gid2) < 0)
        TEST_ERROR
    if(H5Gclose(gid3) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    H5Gclose(gid);
    H5Gclose(gid2);
    H5Gclose(gid3);
    H5Sclose(sid);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* verify_group_cork */


/*-------------------------------------------------------------------------
 * Function:    verify_named_cork
 *
 * Purpose:     This function verifies corking operations for named datatypes.
 *      Cache entries associated with the object tag are checked
 *              for the correct cork status.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
verify_named_cork(hbool_t swmr)
{
    /* Variable Declarations */
    hid_t fid = -1;                     /* File ID */
    hid_t fapl = -1;                    /* File access property list */
    hid_t tid = -1, tid2 = -1, tid3 = -1;   /* Datatype IDs */
    hid_t gid = -1, gid2 = -1;          /* Group IDs */
    H5O_info_t oinfo, oinfo2, oinfo3, oinfo4;   /* Object metadata information */
    hid_t aid = -1;             /* Attribute ID */
    hid_t sid;                  /* Dataspace ID */
    hid_t did;                  /* Dataset ID */
    char attrname[500];                 /* Name of attribute */
    unsigned flags;             /* File access flags */
    int i = 0;                          /* Local index variable */

    /* Testing Macro */
    if(swmr) {
        TESTING("cork status for named datatypes (SWMR)");
    } else {
        TESTING("cork status for named datatypes");
    }

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    /* Set to use latest format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    /* Create the file */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create 3 copies of datatypes */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR
    if((tid2 = H5Tcopy(H5T_NATIVE_LONG)) < 0)
        TEST_ERROR
    if((tid3 = H5Tcopy(H5T_NATIVE_CHAR)) < 0)
        TEST_ERROR

    /* Commit datatype /DT */
    if(H5Tcommit2(fid, DT, tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Create /GRP */
    if((gid = H5Gcreate2(fid, GRP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    /* Commit datatype /GRP/DT2 */
    if(H5Tcommit2(gid, DT2, tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Create /GRP/GRP2 */
    if((gid2 = H5Gcreate2(gid, GRP2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    /* Commit datatype /GRP/GRP2/DT3 */
    if(H5Tcommit2(gid2, DT3, tid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Cork 2 named datatypes: /DT and /GRP/GRP2/DT3 */
    if(H5Odisable_mdc_flushes(tid) < 0)
        TEST_ERROR
    if(H5Odisable_mdc_flushes(tid3) < 0)
        TEST_ERROR

    /* Get named datatype object header addresses */
    if(H5Oget_info2(tid, &oinfo, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5Oget_info2(tid2, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5Oget_info2(tid3, &oinfo3, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Verify cork status of the named datatypes */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, FALSE) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Close the datatypes */
    if(H5Tclose(tid) < 0)
        TEST_ERROR
    if(H5Tclose(tid2) < 0)
        TEST_ERROR
    if(H5Tclose(tid3) < 0)
        TEST_ERROR

    /* Re-open the named datatypes */
    if((tid = H5Topen2(fid, DT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((tid2 = H5Topen2(gid, DT2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((tid3 = H5Topen2(gid2, DT3, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Verify cork status of the named datatypes */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, FALSE) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, FALSE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Tclose(tid) < 0)
        TEST_ERROR
    if(H5Tclose(tid2) < 0)
        TEST_ERROR
    if(H5Tclose(tid3) < 0)
        TEST_ERROR
    if(H5Gclose(gid) < 0)
        TEST_ERROR
    if(H5Gclose(gid2) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR


    /* Re-open the file and the three groups */
    flags = H5F_ACC_RDWR;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fopen(FILENAME, flags, fapl)) < 0)
        FAIL_STACK_ERROR
    if((gid = H5Gopen2(fid, GRP, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((gid2 = H5Gopen2(gid, GRP2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Re-open the named datatypes */
    if((tid = H5Topen2(fid, DT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((tid2 = H5Topen2(gid, DT2, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if((tid3 = H5Topen2(gid2, DT3, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Cork the datatype: DT2 */
    if(H5Odisable_mdc_flushes(tid2) < 0)
        TEST_ERROR

    /* Create dataspace */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR

    /* Attach 8 attributes to datatype: DT3 */
    for(i = 0;i < 8; i++) {
        HDsprintf(attrname, "attr %d", i);
        if((aid = H5Acreate2(tid3, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR
        if(H5Awrite(aid, H5T_NATIVE_UINT, &i) < 0)
            TEST_ERROR
        /* Cork the datatype while attaching attributes */
        if(i == 3) {
            if(H5Odisable_mdc_flushes(tid3) < 0)
                TEST_ERROR
            if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
                TEST_ERROR
        }
        if(H5Aclose(aid) < 0)
            TEST_ERROR
    } /* end for */

    /* Create a dataset with named datatype: DT */
    if((did = H5Dcreate2(fid, DSET, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Get dataset object header address */
    if(H5Oget_info2(did, &oinfo4, H5O_INFO_BASIC) < 0)
        TEST_ERROR

    /* Cork the dataset: DSET */
    if(H5Odisable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Verify cork status of the datatype: DT */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, FALSE) < 0)
        TEST_ERROR
    /* Verify cork status of the datatype: DT2 */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR
    /* Verify cork status of the datatype: DT3 */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Un-cork the datatype: DT3 */
    if(H5Oenable_mdc_flushes(tid3) < 0)
        TEST_ERROR
    /* Verify cork status of the datatype: DT3 */
    if(H5C__verify_cork_tag_test(fid, oinfo3.addr, FALSE) < 0)
        TEST_ERROR

    /* Cork the datatype: DT */
    if(H5Odisable_mdc_flushes(tid) < 0)
        TEST_ERROR

    /* Verify cork status of the datatype: DT */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR
    /* Verify cork status of the datatype: DT2 */
    if(H5C__verify_cork_tag_test(fid, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo4.addr, TRUE) < 0)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        TEST_ERROR

    /* Verify cork status of the datatype: DT */
    if(H5C__verify_cork_tag_test(fid, oinfo.addr, TRUE) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: DSET */
    if(H5C__verify_cork_tag_test(fid, oinfo4.addr, FALSE) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Tclose(tid) < 0)
        TEST_ERROR
    if(H5Tclose(tid2) < 0)
        TEST_ERROR
    if(H5Tclose(tid3) < 0)
        TEST_ERROR
    if(H5Gclose(gid) < 0)
        TEST_ERROR
    if(H5Gclose(gid2) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR
    if(H5Pclose(fapl) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
    H5Tclose(tid);
    H5Tclose(tid2);
    H5Tclose(tid3);
    H5Gclose(gid);
    H5Gclose(gid2);
    H5Dclose(did);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* verify_named_cork */


/*-------------------------------------------------------------------------
 * Function:    verify_multiple_cork
 *
 * Purpose:     This function verifies corking operations when there are
 *      multiple opens of files, objects, attributes.
 *      (based on test_attr_bug5() in tattr.c)
 *      Cache entries associated with the object tag are checked
 *              for the correct cork status.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
verify_multiple_cork(hbool_t swmr)
{
    /* Variable Declarations */
    hid_t fid1 = -1, fid2 = -1;     /* File ID */
    hid_t fapl = -1;                /* File access property list */
    hid_t tid1 = -1, tid2 = -1;     /* Datatype IDs */
    hid_t gid1 = -1, gid2 = -1;     /* Group IDs */
    hid_t did1 = -1, did2 = -1;     /* Dataset ID */
    hid_t aidg1 = -1, aidg2 = -1;   /* Attribute ID */
    hid_t aidd1 = -1, aidd2 = -1;   /* Attribute ID */
    hid_t aidt1 = -1, aidt2 = -1;   /* Attribute ID */
    hid_t sid = -1;         /* Dataspace ID */
    H5O_info_t oinfo1, oinfo2, oinfo3;  /* Object metadata information */
    hsize_t dim[1] = {5};       /* Dimension sizes */
    unsigned flags;         /* File access flags */
    hbool_t corked;         /* Cork status */
    herr_t ret;                     /* Return value */

    /* Testing Macro */
    if(swmr) {
        TESTING("cork status for multiple opens (SWMR)");
    } else {
        TESTING("cork status for multiple opens");
    }

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR
    /* Set to use latest format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR

    /* Create the file */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid1 = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Open root group */
    if((gid1 = H5Gopen2(fid1, "/", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create and commit datatype */
    if((tid1 = H5Tcopy(H5T_STD_I32LE)) < 0)
        TEST_ERROR
    if(H5Tcommit2(fid1, DT, tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Create dataset */
    if((sid = H5Screate_simple(1, dim, NULL)) < 0)
        TEST_ERROR
    if((did1 = H5Dcreate2(fid1, DSET, tid1, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create attribute on root group */
    if((aidg1 = H5Acreate2(gid1, GRP_ATTR, tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create attribute on dataset */
    if((aidd1 = H5Acreate2(did1, DSET_ATTR, tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create attribute on datatype */
    if((aidt1 = H5Acreate2(tid1, DT_ATTR, tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Closing */
    if(H5Aclose(aidt1) < 0)
        TEST_ERROR
    if(H5Aclose(aidd1) < 0)
        TEST_ERROR
    if(H5Aclose(aidg1) < 0)
        TEST_ERROR
    if(H5Dclose(did1) < 0)
        TEST_ERROR
    if(H5Tclose(tid1) < 0)
        TEST_ERROR
    if(H5Gclose(gid1) < 0)
        TEST_ERROR
    if(H5Fclose(fid1) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR

    /* Open the file twice: fid1, fid2 */
    flags = H5F_ACC_RDWR;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid1 = H5Fopen(FILENAME, flags, fapl)) < 0)
        TEST_ERROR
    if((fid2 = H5Fopen(FILENAME, flags, fapl)) < 0)
        TEST_ERROR

    /* Open the root group twice: gid1, gid2 */
    if((gid1 = H5Gopen2(fid1, "/", H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((gid2 = H5Gopen2(fid2, "/", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Open the root group attribute twice: aidg1, aidg2 */
    if((aidg1 = H5Aopen(gid1, GRP_ATTR, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((aidg2 = H5Aopen(gid2, GRP_ATTR, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the group: gid2 */
    if(H5Odisable_mdc_flushes(gid2) < 0)
        TEST_ERROR

    /* Verify cork status of the group: gid2 */
    if(H5Oget_info2(gid2, &oinfo1, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid2, oinfo1.addr, TRUE) < 0)
        TEST_ERROR

    /* Check cork status of the group: gid1 */
    if(H5Oare_mdc_flushes_disabled(gid1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Open the dataset twice: did1, did2 */
    if((did1 = H5Dopen2(fid1, DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((did2 = H5Dopen2(fid2, DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Open the dataset attribute twice: aidd1, aidd2 */
    if((aidd1 = H5Aopen(did1, DSET_ATTR, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((aidd2 = H5Aopen(did2, DSET_ATTR, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the dataset: did1 */
    if(H5Odisable_mdc_flushes(did1) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: did1 */
    if(H5Oget_info2(did1, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid1, oinfo2.addr, TRUE) < 0)
        TEST_ERROR

    /* Check cork status of the dataset: did2 */
    if(H5Oare_mdc_flushes_disabled(did2, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Open the datatype twice: tid1, tid2 */
    if((tid1 = H5Topen2(fid1, DT, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((tid2 = H5Topen2(fid2, DT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Open the datatype attribute twice: aidt1, aidt2 */
    if((aidt1 = H5Aopen(tid1, DT_ATTR, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if((aidt2 = H5Aopen(tid2, DT_ATTR, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the datatype: tid2 */
    if(H5Odisable_mdc_flushes(tid2) < 0)
        TEST_ERROR

    /* Verify cork status of the datatype: tid2 */
    if(H5Oget_info2(tid2, &oinfo3, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid2, oinfo3.addr, TRUE) < 0)
        TEST_ERROR

    /* Check cork status of the datatype: tid1 */
    if(H5Oare_mdc_flushes_disabled(tid1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Uncork the group: gid1 */
    if(H5Oenable_mdc_flushes(gid1) < 0)
        TEST_ERROR

    /* Verify cork status of the group: gid1 */
    if(H5Oget_info2(gid1, &oinfo1, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid1, oinfo1.addr, FALSE) < 0)
        TEST_ERROR

    /* Check cork status of the group: gid2 */
    if(H5Oare_mdc_flushes_disabled(gid2, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Close the group: gid2 */
    if(H5Gclose(gid2) < 0)
        TEST_ERROR

    /* Check cork status of the group: gid1 */
    if(H5Oare_mdc_flushes_disabled(gid1, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Verify cork status of the group: gid1 */
    if(H5C__verify_cork_tag_test(fid1, oinfo1.addr, FALSE) < 0)
        TEST_ERROR

    /* Close the group: gid1 */
    if(H5Gclose(gid1) < 0)
        TEST_ERROR

    /* Uncork the dataset: gid2 */
    if(H5Oenable_mdc_flushes(did2) < 0)
        TEST_ERROR

    /* Verify cork status of the dataset: did2 */
    if(H5Oget_info2(did2, &oinfo2, H5O_INFO_BASIC) < 0)
        TEST_ERROR
    if(H5C__verify_cork_tag_test(fid2, oinfo2.addr, FALSE) < 0)
        TEST_ERROR

    /* Check cork status of the dataset: did1 */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Close the dataset: did2 */
    if(H5Dclose(did2) < 0)
        TEST_ERROR

    /* Check cork status of the dataset: did1 */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Verify cork status of the dataset: did1 */
    if(H5C__verify_cork_tag_test(fid1, oinfo2.addr, FALSE) < 0)
        TEST_ERROR

    /* Close the dataset: did1 */
    if(H5Dclose(did1) < 0)
        TEST_ERROR

    /* Check cork status of the datatype: tid1 */
    if(H5Oare_mdc_flushes_disabled(tid1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close datatype: tid1 */
    if(H5Tclose(tid1) < 0)
        TEST_ERROR

    /* Check cork status of the datatype: tid2 */
    if(H5Oare_mdc_flushes_disabled(tid2, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close datatype: tid2 */
    if(H5Tclose(tid2) < 0)
        TEST_ERROR

    /* Should fail to cork the attribute: aidg2; not an object */
    H5E_BEGIN_TRY {
        ret = H5Odisable_mdc_flushes(aidg2);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Should fail to uncork the attribute: aidd1; not an object */
    H5E_BEGIN_TRY {
        ret = H5Odisable_mdc_flushes(aidd1);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Should fail to check cork status of the attribute: aidt2; not an object */
    H5E_BEGIN_TRY {
        ret = H5Oare_mdc_flushes_disabled(aidt2, &corked);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Close all attributes */
    if(H5Aclose(aidg1) < 0)
        TEST_ERROR
    if(H5Aclose(aidg2) < 0)
        TEST_ERROR
    if(H5Aclose(aidd1) < 0)
        TEST_ERROR
    if(H5Aclose(aidd2) < 0)
        TEST_ERROR
    if(H5Aclose(aidt1) < 0)
        TEST_ERROR
    if(H5Aclose(aidt2) < 0)
        TEST_ERROR

    /* Should fail to cork the file: fid1; not an object */
    H5E_BEGIN_TRY {
        ret = H5Oare_mdc_flushes_disabled(fid1, &corked);
        ret = H5Odisable_mdc_flushes(fid1);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Should fail to uncork the file: fid2; not an object */
    H5E_BEGIN_TRY {
        ret = H5Oenable_mdc_flushes(fid2);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Closing */
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    if(H5Fclose(fid1) < 0)
        TEST_ERROR
    if(H5Fclose(fid2) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Tclose(tid1);
        H5Tclose(tid2);
        H5Gclose(gid1);
        H5Gclose(gid2);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Aclose(aidg1);
        H5Aclose(aidg2);
        H5Aclose(aidd1);
        H5Aclose(aidt1);
        H5Aclose(aidt2);
        H5Aclose(aidd2);
        H5Pclose(fapl);
        H5Fclose(fid1);
        H5Fclose(fid1);
    } H5E_END_TRY;
    return 1;
} /* verify_multiple_cork */

/*-------------------------------------------------------------------------
 * Function:    test_objs_cork
 *
 * Purpose:     This function verifies H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled public
 *      routines are working as specified.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_objs_cork(hbool_t swmr, hbool_t new_format)
{
    hid_t   fid = H5I_INVALID_HID;                  /* HDF5 File ID */
    hid_t   fapl = H5I_INVALID_HID;                 /* File access property list */
    hid_t   gid = H5I_INVALID_HID;
    hid_t   did = H5I_INVALID_HID;
    hid_t   tid = H5I_INVALID_HID;                  /* Object IDs */
    hid_t   sid = H5I_INVALID_HID;                  /* Dataspace ID */
    hid_t   aid = H5I_INVALID_HID;                  /* Attribute ID */
    hsize_t     dims[RANK];     /* Dataset dimension sizes */
    hbool_t     corked;         /* Cork status of an object */
    unsigned flags;             /* File access flags */
    herr_t      ret;                    /* Return value */

    /* Testing Macro */
    if(new_format) {
        if(swmr) {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled (new library format) (SWMR)");
        } /* end if */
        else {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled (new library format)");
        } /* end else */
    } else {
        if(swmr) {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled (old library format) (SWMR)");
        } /* end if */
        else {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled (old library format)");
        } /* end else */
    } /* end else */

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Set to use latest format */
    if(new_format) {
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            TEST_ERROR
    } /* end if */

    /* Create the file with/without SWMR access */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create group */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of the group: not corked */
    if(H5Oare_mdc_flushes_disabled(gid, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Cork the group: an object */
    if(H5Odisable_mdc_flushes(gid) < 0)
        TEST_ERROR

    /* Check cork status of the group: corked */
    if(H5Oare_mdc_flushes_disabled(gid, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the group */
    if(H5Gclose(gid) < 0)
        TEST_ERROR

    /* Create a transient copy of a native type */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR

    /* Should fail to cork the datatype: not an object */
    H5E_BEGIN_TRY {
        ret = H5Odisable_mdc_flushes(tid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Create a named datatype */
    if(H5Tcommit2(fid, "group/datatype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Check cork status of the named datatype: not corked */
    if(H5Oare_mdc_flushes_disabled(tid, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Cork the named datatype: an object */
    if(H5Odisable_mdc_flushes(tid) < 0)
        TEST_ERROR

    /* Check cork status of the named datatype: corked */
    if(H5Oare_mdc_flushes_disabled(tid, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the named datatype */
    if(H5Tclose(tid) < 0)
        TEST_ERROR

    /* Create dataspace */
    dims[0] = DIM0;
    dims[1] = DIM1;
    if((sid = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Should fail to uncork the dataspace: not an object */
    H5E_BEGIN_TRY {
        ret = H5Oenable_mdc_flushes(sid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Create dataset. */
    if((did = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create attribute on the dataset */
    if((aid = H5Acreate2(did, "attr", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Should fail to check cork status of the attribute: not an object */
    H5E_BEGIN_TRY {
        ret = H5Oare_mdc_flushes_disabled(aid, &corked);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Check cork status of the dataset: not corked */
    if(H5Oare_mdc_flushes_disabled(did, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Cork the dataset: an object */
    if(H5Odisable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Check cork status of the dataset: corked */
    if(H5Oare_mdc_flushes_disabled(did, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the dataset and dataspace */
    if(H5Dclose(did) < 0)
        TEST_ERROR

    /* Open the group */
    if((gid = H5Oopen(fid, "group", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of the group */
    if(H5Oare_mdc_flushes_disabled(gid, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Cork the group */
    if(H5Odisable_mdc_flushes(gid) < 0)
        TEST_ERROR

    /* Should fail to cork the group again */
    H5E_BEGIN_TRY {
        ret = H5Odisable_mdc_flushes(gid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Check cork status of the group */
    if(H5Oare_mdc_flushes_disabled(gid, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Open the named datatype */
    if((tid = H5Oopen(fid, "group/datatype", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of the named datatype */
    if(H5Oare_mdc_flushes_disabled(tid, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Should fail to un-cork the named datatype that is not corked yet */
    H5E_BEGIN_TRY {
        ret = H5Oenable_mdc_flushes(tid);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR

    /* Cork the named datatype */
    if(H5Odisable_mdc_flushes(tid) < 0)
        TEST_ERROR

    /* Check cork status of the named datatype */
    if(H5Oare_mdc_flushes_disabled(tid, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Open the dataset */
    if((did = H5Oopen(fid, "/dataset", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of the dataset */
    if(H5Oare_mdc_flushes_disabled(did, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Cork the dataset */
    if(H5Odisable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Check cork status of dataset */
    if(H5Oare_mdc_flushes_disabled(did, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Un-cork the dataset */
    if(H5Oenable_mdc_flushes(did) < 0)
        TEST_ERROR

    /* Check cork status of the dataset */
    if(H5Oare_mdc_flushes_disabled(did, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Closing */
    if(H5Tclose(tid) < 0)
        TEST_ERROR
    if(H5Gclose(gid) < 0)
        TEST_ERROR
    if(H5Dclose(did) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Aclose(aid) < 0)
        TEST_ERROR
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Aclose(aid);
        H5Dclose(did);
        H5Gclose(gid);
        H5Tclose(tid);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
} /* test_objs_cork() */


/*-------------------------------------------------------------------------
 * Function:    test_dset_cork
 *
 * Purpose:     This function verifies H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled are
 *      working as specified when manipulating datasets.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_dset_cork(hbool_t swmr, hbool_t new_format)
{
    hid_t       fid;                            /* File ID */
    hid_t       fapl;                           /* File access property list */
    hid_t       gid;                /* Groupd ID */
    hid_t   did1, did2;         /* Dataset IDs */
    hid_t   tid1, tid2;             /* Datatype IDs */
    hid_t   sid;                /* Dataspace ID */
    hid_t   dcpl;               /* Dataset creation property list */
    hsize_t     dims[RANK];                 /* Dataset dimensions */
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};    /* Maximum dataset dimensions */
    hsize_t     cdims[RANK] = {2,2};                /* Chunk dimensions */
    int     fillval = 0;            /* Fill value */
    int     i, j, k = 0;            /* Local index variables */
    int     data[DIMS0][DIMS1];     /* Data buffer */
    int     rbuf[DIMS0][DIMS1];     /* Data buffer */
    hbool_t     corked;             /* Cork status of an object */
    unsigned flags;                 /* File access flags */

    /* Testing Macro */
    if(new_format) {
        if(swmr) {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled on datasets (new library format) (SWMR)");
        } /* end if */
        else {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled on datasets (new library format)");
        } /* end else */
    } else {
        if(swmr) {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled on datasets (old library format) (SWMR)");
        } /* end if */
        else {
            TESTING("H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled on datasets (old library format)");
        } /* end else */
    } /* end if */

    /* Create fapl */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Set to use latest format */
    if(new_format) {
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            TEST_ERROR
    } /* end if */

    /* Create the file with/without SWMR access */
    flags = H5F_ACC_TRUNC;
    if(swmr)
        flags |= H5F_ACC_SWMR_WRITE;
    if((fid = H5Fcreate(FILENAME, flags, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create a group */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Commit the datatype with the group */
    if((tid1 = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR
    if(H5Tcommit2(gid, "datatype", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Cork the named datatype */
    if(H5Odisable_mdc_flushes(tid1) < 0)
        TEST_ERROR

    /* Set up dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if(H5Pset_chunk(dcpl, RANK, cdims) < 0)
        TEST_ERROR

    /* Set up a fill value */
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR

    /* Create dataspace */
    dims[0] = DIMS0;
    dims[1] = DIMS1;
    if((sid = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Create the dataset inside the group with the named datatype */
    if((did1 = H5Dcreate2(gid, "dataset", tid1, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of the named datatype */
    if(H5Oare_mdc_flushes_disabled(tid1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Cork the dataset */
    if(H5Odisable_mdc_flushes(did1) < 0)
        TEST_ERROR

    /* Check cork status of the dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Check cork status of the group */
    if(H5Oare_mdc_flushes_disabled(gid, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Initialize the buffer */
    for(i = 0; i < DIMS0;i++)
        for(j = 0;j < DIMS1;j++)
            data[i][j] = k++;

    /* Write to the dataset */
    if(H5Dwrite(did1, tid1, sid, sid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Flush the dataset */
    if(H5Oflush(did1) < 0)
        TEST_ERROR

    /* Check cork status of the dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Check cork status of the named datatype */
    if(H5Oare_mdc_flushes_disabled(tid1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did1) < 0)
        TEST_ERROR

    /* Open the dataset again */
    if((did1 = H5Dopen2(gid, "dataset", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Read from the dataset */
    if(H5Dread(did1, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR

    /* Cork the dataset */
    if(H5Odisable_mdc_flushes(did1) < 0)
        TEST_ERROR

    /* Delete the dataset */
    if(H5Ldelete(gid, "dataset", H5P_DEFAULT) < 0)
        TEST_ERROR

    /* Check cork status of the dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the dataset */
    if(H5Oclose(did1) < 0)
        TEST_ERROR

    /* Create the dataset again */
    if((did1 = H5Dcreate2(gid, "dataset", tid1, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the dataset */
    if(H5Odisable_mdc_flushes(did1) < 0)
        TEST_ERROR

    /* Write to the dataset */
    if(H5Dwrite(did1, tid1, sid, sid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Refresh the dataset */
    if(H5Drefresh(did1) < 0)
        TEST_ERROR

    /* Check cork status of the dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did1) < 0)
        TEST_ERROR

    /* First open of the dataset */
    if((did1 = H5Dopen2(gid, "dataset", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Second open of the dataset */
    if((did2 = H5Dopen2(gid, "dataset", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Cork the first opened dataset */
    if(H5Odisable_mdc_flushes(did1) < 0)
        TEST_ERROR

    /* Check cork status of the first opened dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Check cork status of the second opened dataset */
    if(H5Oare_mdc_flushes_disabled(did2, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the second opened dataset */
    if(H5Dclose(did2) < 0)
        TEST_ERROR

    /* Check cork status of the first opened dataset */
    if(H5Oare_mdc_flushes_disabled(did1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Close the first opened dastaset */
    if(H5Dclose(did1) < 0)
        TEST_ERROR

    /* Check cork status of the named datatype */
    if(H5Oare_mdc_flushes_disabled(tid1, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Second open of the named datatype */
    if((tid2 = H5Topen2(gid, "datatype", H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Check cork status of the second opened named datatype */
    if(H5Oare_mdc_flushes_disabled(tid2, &corked) < 0)
        TEST_ERROR
    if(!corked)
        TEST_ERROR

    /* Uncork the second opened named datatype */
    if(H5Oenable_mdc_flushes(tid2) < 0)
        TEST_ERROR

    /* Check cork status of the second opened named datatype */
    if(H5Oare_mdc_flushes_disabled(tid2, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Check cork status of the first opened named datatype */
    if(H5Oare_mdc_flushes_disabled(tid1, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Close the first opened datatype */
    if(H5Tclose(tid1) < 0)
        TEST_ERROR

    /* Close the second opened datatype */
    if(H5Tclose(tid2) < 0)
        TEST_ERROR

    /* Check cork status of the group */
    if(H5Oare_mdc_flushes_disabled(gid, &corked) < 0)
        TEST_ERROR
    if(corked)
        TEST_ERROR

    /* Closing */
    if(H5Gclose(gid) < 0)
        TEST_ERROR
    if(H5Sclose(sid) < 0)
        TEST_ERROR
    if(H5Pclose(fapl) < 0)
        TEST_ERROR
    if(H5Fclose(fid) < 0)
        TEST_ERROR
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Tclose(tid1);
        H5Tclose(tid2);
        H5Pclose(dcpl);
        H5Gclose(gid);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;

} /* test_dset_cork() */



/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run tests to verify the library's corking operations.
 *
 * Return:      Success:
 *
 *              Failure:
 *
 * Programmer:  Vailin Choi; Feb 2014
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned swmr;              /* Loop over SWMR/non-SWMR */
    unsigned nerrs = 0;         /* Error Encountered */

    /* Test for dataset created with old library format */
    nerrs += verify_old_dset_cork();

    for(swmr = 0; swmr <= 1; swmr++) {
        /* Tests with new/old library format */
        /* This is the test moved from th5o.c: test_h5o_cork() */
        nerrs += test_objs_cork(swmr, TRUE);
        nerrs += test_objs_cork(swmr, FALSE);
        /* This is the test moved from th5o.c: test_h5o_cork_dataset() */
        nerrs += test_dset_cork(swmr, TRUE);
        nerrs += test_dset_cork(swmr, FALSE);

        /* Tests with/without SWMR access */
        nerrs += verify_obj_dset_cork(swmr);
        nerrs += verify_dset_cork(swmr, TRUE);
        nerrs += verify_dset_cork(swmr, FALSE);
        nerrs += verify_group_cork(swmr);
        nerrs += verify_named_cork(swmr);
        nerrs += verify_multiple_cork(swmr);
    } /* end for */

    /* Delete test files */
    HDremove(FILENAME);

    /* Return Errors */
    return(nerrs > 0);
} /* main */

