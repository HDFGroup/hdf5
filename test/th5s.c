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

/***********************************************************
 *
 * Test program:     th5s
 *
 * Test the dataspace functionality
 *
 *************************************************************/

#include "testhdf5.h"
#include "H5srcdir.h"

#include "H5Iprivate.h"
#include "H5Pprivate.h"

/*
 * This file needs to access private information from the H5S package.
 * This file also needs to access the dataspace testing code.
 */
#define H5S_FRIEND  /*suppress error about including H5Spkg   */
#define H5S_TESTING /*suppress warning about H5S testing funcs*/
#include "H5Spkg.h" /* Dataspaces               */

/*
 * This file needs to access private information from the H5O package.
 * This file also needs to access the dataspace testing code.
 */
#define H5O_FRIEND /*suppress error about including H5Opkg   */
#define H5O_TESTING
#include "H5Opkg.h" /* Object header            */

#define TESTFILE      "th5s.h5"
#define DATAFILE      "th5s1.h5"
#define NULLFILE      "th5s2.h5"
#define BASICFILE     "th5s3.h5"
#define ZEROFILE      "th5s4.h5"
#define BASICDATASET  "basic_dataset"
#define BASICDATASET1 "basic_dataset1"
#define BASICDATASET2 "basic_dataset2"
#define BASICDATASET3 "basic_dataset3"
#define BASICDATASET4 "basic_dataset4"
#define BASICATTR     "basic_attribute"
#define NULLDATASET   "null_dataset"
#define NULLATTR      "null_attribute"
#define EXTFILE_NAME  "ext_file"

/* 3-D dataset with fixed dimensions */
#define SPACE1_RANK 3
#define SPACE1_DIM1 3
#define SPACE1_DIM2 15
#define SPACE1_DIM3 13

/* 4-D dataset with one unlimited dimension */
#define SPACE2_RANK 4
#define SPACE2_DIM1 0
#define SPACE2_DIM2 15
#define SPACE2_DIM3 13
#define SPACE2_DIM4 23
#define SPACE2_MAX1 H5S_UNLIMITED
#define SPACE2_MAX2 15
#define SPACE2_MAX3 13
#define SPACE2_MAX4 23

/* Scalar dataset with simple datatype */
#define SPACE3_RANK 0
static unsigned space3_data = 65;

/* Scalar dataset with compound datatype */
#define SPACE4_FIELDNAME1 "c1"
#define SPACE4_FIELDNAME2 "u"
#define SPACE4_FIELDNAME3 "f"
#define SPACE4_FIELDNAME4 "c2"
static size_t space4_field1_off = 0;
static size_t space4_field2_off = 0;
static size_t space4_field3_off = 0;
static size_t space4_field4_off = 0;
static struct space4_struct {
    char     c1;
    unsigned u;
    float    f;
    char     c2;
} space4_data = {'v', 987123, -3.14F, 'g'}; /* Test data for 4th dataspace */

/*
 *  Testing configuration defines used by:
 *      test_h5s_encode_regular_hyper()
 *      test_h5s_encode_irregular_hyper()
 *      test_h5s_encode_points()
 */
#define CONFIG_8  1
#define CONFIG_16 2
#define CONFIG_32 3
#define POWER8    256        /* 2^8 */
#define POWER16   65536      /* 2^16 */
#define POWER32   4294967296 /* 2^32 */

/****************************************************************
**
**  test_h5s_basic(): Test basic H5S (dataspace) code.
**
****************************************************************/
static void
test_h5s_basic(void)
{
    hid_t    fid1;       /* HDF5 File IDs        */
    hid_t    sid1, sid2; /* Dataspace ID            */
    hid_t    dset1;      /* Dataset ID            */
    hid_t    aid1;       /* Attribute ID                 */
    int      rank;       /* Logical rank of dataspace    */
    hsize_t  dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t  dims2[] = {SPACE2_DIM1, SPACE2_DIM2, SPACE2_DIM3, SPACE2_DIM4};
    hsize_t  dims3[H5S_MAX_RANK + 1];
    hsize_t  max2[] = {SPACE2_MAX1, SPACE2_MAX2, SPACE2_MAX3, SPACE2_MAX4};
    hsize_t  tdims[4]; /* Dimension array to test with */
    hsize_t  tmax[4];
    hssize_t n; /* Number of dataspace elements */
    bool     vol_is_native;
    bool     driver_is_default_compatible;
    herr_t   ret; /* Generic return value        */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace Manipulation\n"));

    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        MESSAGE(5, (" -- SKIPPED --\n"));
        return;
    }

    sid1 = H5Screate_simple(SPACE1_RANK, dims1, max2);
    CHECK(sid1, FAIL, "H5Screate_simple");

    n = H5Sget_simple_extent_npoints(sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");

    rank = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tdims, dims1, SPACE1_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");

    sid2 = H5Screate_simple(SPACE2_RANK, dims2, max2);
    CHECK(sid2, FAIL, "H5Screate_simple");

    n = H5Sget_simple_extent_npoints(sid2);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, SPACE2_DIM1 * SPACE2_DIM2 * SPACE2_DIM3 * SPACE2_DIM4, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid2);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE2_RANK, "H5Sget_simple_extent_ndims");

    rank = H5Sget_simple_extent_dims(sid2, tdims, tmax);
    CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tdims, dims2, SPACE2_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tmax, max2, SPACE2_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");

    /* Change max dims to be equal to the dimensions */
    ret = H5Sset_extent_simple(sid1, SPACE1_RANK, dims1, NULL);
    CHECK(ret, FAIL, "H5Sset_extent_simple");
    rank = H5Sget_simple_extent_dims(sid1, tdims, tmax);
    CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tdims, dims1, SPACE1_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tmax, dims1, SPACE1_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");

    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /*
     * Check to be sure we can't create a simple dataspace that has too many
     * dimensions.
     */
    H5E_BEGIN_TRY
    {
        sid1 = H5Screate_simple(H5S_MAX_RANK + 1, dims3, NULL);
    }
    H5E_END_TRY
    VERIFY(sid1, FAIL, "H5Screate_simple");

    /*
     * Try reading a file that has been prepared that has a dataset with a
     * higher dimensionality than what the library can handle.
     *
     * If this test fails and the H5S_MAX_RANK variable has changed, follow
     * the instructions in space_overflow.c for regenerating the th5s.h5 file.
     */
    /* Check if native VOL is being used */
    CHECK(h5_using_native_vol(H5P_DEFAULT, H5I_INVALID_HID, &vol_is_native), FAIL, "h5_using_native_vol");
    /* Check if VFD used is native file format compatible */
    ret = h5_driver_is_default_vfd_compatible(H5P_DEFAULT, &driver_is_default_compatible);
    CHECK_I(ret, "h5_driver_is_default_vfd_compatible");

    if (vol_is_native && driver_is_default_compatible) {
        const char *testfile = H5_get_srcdir_filename(TESTFILE); /* Corrected test file name */

        fid1 = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
        CHECK_I(fid1, "H5Fopen");
        if (fid1 >= 0) {
            dset1 = H5Dopen2(fid1, "dset", H5P_DEFAULT);
            VERIFY(dset1, FAIL, "H5Dopen2");
            ret = H5Fclose(fid1);
            CHECK_I(ret, "H5Fclose");
        }
        else
            printf("***cannot open the pre-created H5S_MAX_RANK test file (%s)\n", testfile);
    }

    /* Verify that incorrect dimensions don't work */
    dims1[0] = H5S_UNLIMITED;
    H5E_BEGIN_TRY
    {
        sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    }
    H5E_END_TRY
    VERIFY(sid1, FAIL, "H5Screate_simple");

    dims1[0] = H5S_UNLIMITED;
    sid1     = H5Screate(H5S_SIMPLE);
    CHECK(sid1, FAIL, "H5Screate");

    H5E_BEGIN_TRY
    {
        ret = H5Sset_extent_simple(sid1, SPACE1_RANK, dims1, NULL);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Sset_extent_simple");

    ret = H5Sclose(sid1);
    CHECK_I(ret, "H5Sclose");

    /*
     * Try writing simple dataspaces without setting their extents
     */
    /* Create the file */
    fid1 = H5Fcreate(BASICFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    dims1[0] = SPACE1_DIM1;

    sid1 = H5Screate(H5S_SIMPLE);
    CHECK(sid1, FAIL, "H5Screate");
    sid2 = H5Screate_simple(1, dims1, dims1);
    CHECK(sid2, FAIL, "H5Screate");

    /* This dataset's space has no extent; it should not be created */
    H5E_BEGIN_TRY
    {
        dset1 = H5Dcreate2(fid1, BASICDATASET, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    VERIFY(dset1, FAIL, "H5Dcreate2");

    dset1 = H5Dcreate2(fid1, BASICDATASET2, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset1, FAIL, "H5Dcreate2");

    /* Try some writes with the bad dataspace (sid1) */
    H5E_BEGIN_TRY
    {
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, &n);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dwrite");

    H5E_BEGIN_TRY
    {
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, sid1, H5P_DEFAULT, &n);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dwrite");

    H5E_BEGIN_TRY
    {
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, sid1, sid1, H5P_DEFAULT, &n);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dwrite");

    /* Try to iterate using the bad dataspace */
    H5E_BEGIN_TRY
    {
        ret = H5Diterate(&n, H5T_NATIVE_INT, sid1, NULL, NULL);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Diterate");

    /* Try to fill using the bad dataspace */
    H5E_BEGIN_TRY
    {
        ret = H5Dfill(NULL, H5T_NATIVE_INT, &n, H5T_NATIVE_INT, sid1);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dfill");

    /* Now use the bad dataspace as the space for an attribute */
    H5E_BEGIN_TRY
    {
        aid1 = H5Acreate2(dset1, BASICATTR, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    VERIFY(aid1, FAIL, "H5Acreate2");

    /* Make sure that dataspace reads using the bad dataspace fail */
    H5E_BEGIN_TRY
    {
        ret = H5Dread(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, &n);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dread");

    H5E_BEGIN_TRY
    {
        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, sid1, H5P_DEFAULT, &n);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dread");

    H5E_BEGIN_TRY
    {
        ret = H5Dread(dset1, H5T_NATIVE_INT, sid1, sid1, H5P_DEFAULT, &n);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Dread");

    /* Clean up */
    ret = H5Dclose(dset1);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_h5s_basic() */

/****************************************************************
**
**  test_h5s_null(): Test NULL dataspace
**
****************************************************************/
static void
test_h5s_null(void)
{
    hid_t        fid;                 /* File ID */
    hid_t        sid;                 /* Dataspace IDs */
    hid_t        dset_sid, dset_sid2; /* Dataspace IDs */
    hid_t        attr_sid;            /* Dataspace IDs */
    hid_t        did;                 /* Dataset ID */
    hid_t        attr;                /*Attribute ID */
    H5S_class_t  stype;               /* dataspace type */
    hssize_t     nelem;               /* Number of elements */
    unsigned     uval = 2;            /* Buffer for writing to dataset */
    int          val  = 1;            /* Buffer for writing to attribute */
    H5S_sel_type sel_type;            /* Type of selection currently */
    hsize_t      dims[1] = {10};      /* Dimensions for converting null dataspace to simple */
    H5S_class_t  space_type;          /* Type of dataspace */
    herr_t       ret;                 /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Null Dataspace\n"));

    /* Create the file */
    fid = H5Fcreate(NULLFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    sid = H5Screate(H5S_NULL);
    CHECK(sid, FAIL, "H5Screate");

    /* Check that the null dataspace actually has 0 elements */
    nelem = H5Sget_simple_extent_npoints(sid);
    VERIFY(nelem, 0, "H5Sget_simple_extent_npoints");

    /* Check that the dataspace was created with an "all" selection */
    sel_type = H5Sget_select_type(sid);
    VERIFY(sel_type, H5S_SEL_ALL, "H5Sget_select_type");

    /* Check that the null dataspace has 0 elements selected */
    nelem = H5Sget_select_npoints(sid);
    VERIFY(nelem, 0, "H5Sget_select_npoints");

    /* Change to "none" selection */
    ret = H5Sselect_none(sid);
    CHECK(ret, FAIL, "H5Sselect_none");

    /* Check that the null dataspace has 0 elements selected */
    nelem = H5Sget_select_npoints(sid);
    VERIFY(nelem, 0, "H5Sget_select_npoints");

    /* Check to be sure we can't set a hyperslab selection on a null dataspace */
    H5E_BEGIN_TRY
    {
        hsize_t start[1] = {0};
        hsize_t count[1] = {0};

        ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, NULL, count, NULL);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Sselect_hyperslab");

    /* Check to be sure we can't set a point selection on a null dataspace */
    H5E_BEGIN_TRY
    {
        hsize_t coord[1][1]; /* Coordinates for point selection */

        coord[0][0] = 0;
        ret         = H5Sselect_elements(sid, H5S_SELECT_SET, (size_t)1, (const hsize_t *)coord);
    }
    H5E_END_TRY
    VERIFY(ret, FAIL, "H5Sselect_elements");

    /* Create first dataset */
    did = H5Dcreate2(fid, NULLDATASET, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dcreate2");

    /* Write "nothing" to the dataset */
    ret = H5Dwrite(did, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &uval);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Write "nothing" to the dataset (with type conversion :-) */
    ret = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &val);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Try reading from the dataset (make certain our buffer is unmodified) */
    ret = H5Dread(did, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &uval);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(uval, 2, "H5Dread");

    /* Try reading from the dataset (with type conversion :-) (make certain our buffer is unmodified) */
    ret = H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &val);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(val, 1, "H5Dread");

    /* Create an attribute for the group */
    attr = H5Acreate2(did, NULLATTR, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Acreate2");

    /* Write "nothing" to the attribute */
    ret = H5Awrite(attr, H5T_NATIVE_INT, &val);
    CHECK(ret, FAIL, "H5Awrite");

    /* Write "nothing" to the attribute (with type conversion :-) */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, &uval);
    CHECK(ret, FAIL, "H5Awrite");

    /* Try reading from the attribute (make certain our buffer is unmodified) */
    ret = H5Aread(attr, H5T_NATIVE_INT, &val);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(val, 1, "H5Aread");

    /* Try reading from the attribute (with type conversion :-) (make certain our buffer is unmodified) */
    ret = H5Aread(attr, H5T_NATIVE_UINT, &uval);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(uval, 2, "H5Aread");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close the dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Verify that we've got the right kind of dataspace */
    space_type = H5Sget_simple_extent_type(sid);
    VERIFY(space_type, H5S_NULL, "H5Sget_simple_extent_type");

    /* Convert the null dataspace to a simple dataspace */
    ret = H5Sset_extent_simple(sid, 1, dims, NULL);
    CHECK(ret, FAIL, "H5Sset_extent_simple");

    /* Verify that we've got the right kind of dataspace now */
    space_type = H5Sget_simple_extent_type(sid);
    VERIFY(space_type, H5S_SIMPLE, "H5Sget_simple_extent_type");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /*============================================
     *  Reopen the file to check the dataspace
     *============================================
     */
    fid = H5Fopen(NULLFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Reopen the dataset */
    did = H5Dopen2(fid, NULLDATASET, H5P_DEFAULT);
    CHECK(did, FAIL, "H5Dopen2");

    /* Get the space of the dataset */
    dset_sid = H5Dget_space(did);
    CHECK(dset_sid, FAIL, "H5Dget_space");

    /* Query the NULL dataspace */
    dset_sid2 = H5Scopy(dset_sid);
    CHECK(dset_sid2, FAIL, "H5Scopy");

    /* Verify the class type of dataspace */
    stype = H5Sget_simple_extent_type(dset_sid2);
    VERIFY(stype, H5S_NULL, "H5Sget_simple_extent_type");

    /* Verify there is zero element in the dataspace */
    ret = (herr_t)H5Sget_simple_extent_npoints(dset_sid2);
    VERIFY(ret, 0, "H5Sget_simple_extent_npoints");

    /* Try reading from the dataset (make certain our buffer is unmodified) */
    ret = H5Dread(did, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &uval);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(uval, 2, "H5Dread");

    /* Close the dataspace */
    ret = H5Sclose(dset_sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(dset_sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /* Open the attribute for the dataset */
    attr = H5Aopen(did, NULLATTR, H5P_DEFAULT);
    CHECK(attr, FAIL, "H5Aopen");

    /* Get the space of the dataset */
    attr_sid = H5Aget_space(attr);
    CHECK(attr_sid, FAIL, "H5Aget_space");

    /* Verify the class type of dataspace */
    stype = H5Sget_simple_extent_type(attr_sid);
    VERIFY(stype, H5S_NULL, "H5Sget_simple_extent_type");

    /* Verify there is zero element in the dataspace */
    ret = (herr_t)H5Sget_simple_extent_npoints(attr_sid);
    VERIFY(ret, 0, "H5Sget_simple_extent_npoints");

    /* Close the dataspace */
    ret = H5Sclose(attr_sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Try reading from the attribute (make certain our buffer is unmodified) */
    ret = H5Aread(attr, H5T_NATIVE_INT, &val);
    CHECK(ret, FAIL, "H5Aread");
    VERIFY(val, 1, "H5Aread");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close the dataset */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_h5s_null() */

/****************************************************************
**
**  test_h5s_zero_dim(): Test the code for dataspace with zero dimension size
**
****************************************************************/
static void
test_h5s_zero_dim(void)
{
    hid_t            fid1;           /* HDF5 File IDs        */
    hid_t            sid1, attr_sid; /* Dataspace ID            */
    hid_t            sid_chunk;      /* Dataspace ID for chunked dataset */
    hid_t            dset1;          /* Dataset ID            */
    hid_t            plist_id;       /* Dataset creation property list */
    hid_t            attr;           /* Attribute ID                 */
    int              rank;           /* Logical rank of dataspace    */
    hsize_t          dims1[]       = {0, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t          max_dims[]    = {SPACE1_DIM1 + 1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t          extend_dims[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t          chunk_dims[]  = {SPACE1_DIM1, SPACE1_DIM2 / 3, SPACE1_DIM3};
    hsize_t          tdims[SPACE1_RANK]; /* Dimension array to test with */
    int              wdata[SPACE1_DIM2][SPACE1_DIM3];
    int              rdata[SPACE1_DIM2][SPACE1_DIM3];
    short            wdata_short[SPACE1_DIM2][SPACE1_DIM3];
    short            rdata_short[SPACE1_DIM2][SPACE1_DIM3];
    int              wdata_real[SPACE1_DIM1][SPACE1_DIM2][SPACE1_DIM3];
    int              rdata_real[SPACE1_DIM1][SPACE1_DIM2][SPACE1_DIM3];
    int              val     = 3;
    hsize_t          start[] = {0, 0, 0};
    hsize_t          count[] = {3, 15, 13};
    hsize_t          coord[1][3]; /* Coordinates for point selection */
    hssize_t         nelem;       /* Number of elements           */
    H5S_sel_type     sel_type;    /* Type of selection currently  */
    H5S_class_t      stype;       /* dataspace type               */
    H5D_alloc_time_t alloc_time;  /* Space allocation time        */
    herr_t           ret;         /* Generic return value            */
    unsigned int     i, j, k;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace with zero dimension size\n"));

    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        MESSAGE(5, (" -- SKIPPED --\n"));
        return;
    }

    /* Initialize the data */
    for (i = 0; i < SPACE1_DIM2; i++)
        for (j = 0; j < SPACE1_DIM3; j++) {
            wdata[i][j]       = (int)(i + j);
            rdata[i][j]       = 7;
            wdata_short[i][j] = (short)(i + j);
            rdata_short[i][j] = 7;
        }

    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < SPACE1_DIM2; j++)
            for (k = 0; k < SPACE1_DIM3; k++)
                wdata_real[i][j][k] = (int)(i + j + k);

    /* Test with different space allocation times */
    for (alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; alloc_time++) {

        /* Make sure we can create the space with the dimension size 0 (starting from v1.8.7).
         * The dimension doesn't need to be unlimited. */
        dims1[0] = 0;
        dims1[1] = SPACE1_DIM2;
        dims1[2] = SPACE1_DIM3;
        sid1     = H5Screate_simple(SPACE1_RANK, dims1, NULL);
        CHECK(sid1, FAIL, "H5Screate_simple");

        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        sid1 = H5Screate(H5S_SIMPLE);
        CHECK(sid1, FAIL, "H5Screate");

        /* SID1 has the 1st dimension size as zero.  The maximal dimension will be
         * the same as the dimension because of the NULL passed in. */
        ret = H5Sset_extent_simple(sid1, SPACE1_RANK, dims1, NULL);
        CHECK(ret, FAIL, "H5Sset_extent_simple");

        /* Check that the dataspace actually has 0 elements */
        nelem = H5Sget_simple_extent_npoints(sid1);
        VERIFY(nelem, 0, "H5Sget_simple_extent_npoints");

        /* Check that the dataspace was created with an "all" selection */
        sel_type = H5Sget_select_type(sid1);
        VERIFY(sel_type, H5S_SEL_ALL, "H5Sget_select_type");

        /* Check that the dataspace has 0 elements selected */
        nelem = H5Sget_select_npoints(sid1);
        VERIFY(nelem, 0, "H5Sget_select_npoints");

        /* Change to "none" selection */
        ret = H5Sselect_none(sid1);
        CHECK(ret, FAIL, "H5Sselect_none");

        /* Check that the dataspace has 0 elements selected */
        nelem = H5Sget_select_npoints(sid1);
        VERIFY(nelem, 0, "H5Sget_select_npoints");

        /* Try to select all dataspace */
        ret = H5Sselect_all(sid1);
        CHECK(ret, FAIL, "H5Sselect_all");

        /* Check that the dataspace has 0 elements selected */
        nelem = H5Sget_select_npoints(sid1);
        VERIFY(nelem, 0, "H5Sget_select_npoints");

        /* Create the dataspace for chunked dataset with the first dimension size as zero.
         * The maximal dimensions are bigger than the dimensions for later expansion. */
        sid_chunk = H5Screate_simple(SPACE1_RANK, dims1, max_dims);
        CHECK(sid_chunk, FAIL, "H5Screate_simple");

        /*============================================
         * Make sure we can use 0-dimension to create
         * contiguous, chunked, compact, and external
         * datasets, and also attribute.
         *============================================
         */
        fid1 = H5Fcreate(ZEROFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(fid1, FAIL, "H5Fcreate");

        /*===================== Contiguous dataset =======================*/
        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(plist_id, FAIL, "H5Pcreate");

        ret = H5Pset_alloc_time(plist_id, alloc_time);
        CHECK(ret, FAIL, "H5Pset_alloc_time");

        dset1 = H5Dcreate2(fid1, BASICDATASET, H5T_NATIVE_INT, sid1, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");

        ret = H5Pclose(plist_id);
        CHECK(ret, FAIL, "H5Pclose");

        /* Write "nothing" to the dataset */
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, wdata);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }
        }

        /* Write "nothing" to the dataset (with type conversion :-) */
        ret = H5Dwrite(dset1, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata_short);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, rdata_short);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata_short[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata_short[i][j]);
                }
            }
        }

        /* Select a hyperslab beyond its current dimension sizes, then try to write
         * the data.  It should fail. */
        ret = H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        H5E_BEGIN_TRY
        {
            ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, sid1, H5P_DEFAULT, wdata);
        }
        H5E_END_TRY
        VERIFY(ret, FAIL, "H5Dwrite");

        /* Change to "none" selection */
        ret = H5Sselect_none(sid1);
        CHECK(ret, FAIL, "H5Sselect_none");

        /* Select a point beyond the dimension size, then try to write the data.
         * It should fail. */
        coord[0][0] = 2;
        coord[0][1] = 5;
        coord[0][2] = 3;
        ret         = H5Sselect_elements(sid1, H5S_SELECT_SET, (size_t)1, (const hsize_t *)coord);
        CHECK(ret, FAIL, "H5Sselect_elements");

        H5E_BEGIN_TRY
        {
            ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, sid1, H5P_DEFAULT, &val);
        }
        H5E_END_TRY
        VERIFY(ret, FAIL, "H5Dwrite");

        /* Restore the selection to all */
        ret = H5Sselect_all(sid1);
        CHECK(ret, FAIL, "H5Sselect_all");

        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /*=================== Chunked dataset ====================*/
        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(plist_id, FAIL, "H5Pcreate");

        ret = H5Pset_chunk(plist_id, SPACE1_RANK, chunk_dims);
        CHECK(ret, FAIL, "H5Pset_chunk");

        /* ret = H5Pset_alloc_time(plist_id, alloc_time); */
        /* CHECK(ret, FAIL, "H5Pset_alloc_time"); */

        dset1 =
            H5Dcreate2(fid1, BASICDATASET1, H5T_NATIVE_INT, sid_chunk, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");

        /* Write "nothing" to the dataset */
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++)
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }

        /* Now extend the dataset to SPACE1_DIM1*SPACE1_DIM2*SPACE1_DIM3 and make sure
         * we can write data to it */
        extend_dims[0] = SPACE1_DIM1;
        ret            = H5Dset_extent(dset1, extend_dims);
        CHECK(ret, FAIL, "H5Dset_extent");

        ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata_real);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata_real);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM1; i++) {
            for (j = 0; j < SPACE1_DIM2; j++) {
                for (k = 0; k < SPACE1_DIM3; k++) {
                    if (rdata_real[i][j][k] != wdata_real[i][j][k]) {
                        H5_FAILED();
                        printf("element [%d][%d][%d] is %d but should have been %d\n", i, j, k,
                               rdata_real[i][j][k], wdata_real[i][j][k]);
                    }
                }
            }
        }

        /* Now shrink the first dimension size of the dataset to 0 and make sure no data is in it */
        extend_dims[0] = 0;
        ret            = H5Dset_extent(dset1, extend_dims);
        CHECK(ret, FAIL, "H5Dset_extent");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++)
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }

        /* Now extend the first dimension size of the dataset to SPACE1_DIM1*3 past the maximal size.
         * It is supposed to fail. */
        extend_dims[0] = SPACE1_DIM1 * 3;
        H5E_BEGIN_TRY
        {
            ret = H5Dset_extent(dset1, extend_dims);
        }
        H5E_END_TRY
        VERIFY(ret, FAIL, "H5Dset_extent");

        ret = H5Pclose(plist_id);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /*=================== Compact dataset =====================*/
        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(plist_id, FAIL, "H5Pcreate");

        ret = H5Pset_layout(plist_id, H5D_COMPACT);
        CHECK(ret, FAIL, "H5Pset_layout");

        /* Don't set the allocation time for compact storage datasets (must be early) */

        dset1 = H5Dcreate2(fid1, BASICDATASET2, H5T_NATIVE_INT, sid1, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");

        /* Write "nothing" to the dataset */
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++)
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }

        ret = H5Pclose(plist_id);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /*=========== Contiguous dataset with external storage ============*/
        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(plist_id, FAIL, "H5Pcreate");

        /* Change the DCPL for contiguous layout with external storage.  The size of the reserved
         * space in the external file is the size of the dataset (zero because one dimension size is zero).
         * There's no need to clean up the external file since the library doesn't create it
         * until the data is written to it. */
        ret = H5Pset_external(plist_id, EXTFILE_NAME, 0, (hsize_t)0);
        CHECK(ret, FAIL, "H5Pset_external");

        ret = H5Pset_alloc_time(plist_id, alloc_time);
        CHECK(ret, FAIL, "H5Pset_alloc_time");

        dset1 = H5Dcreate2(fid1, BASICDATASET3, H5T_NATIVE_INT, sid1, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");

        /* Write "nothing" to the dataset */
        ret = H5Dwrite(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, wdata);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, sid1, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }
        }

        ret = H5Pclose(plist_id);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /*=============== Create an attribute for the file ================*/
        attr = H5Acreate2(fid1, NULLATTR, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Acreate2");

        /* Write "nothing" to the attribute */
        ret = H5Awrite(attr, H5T_NATIVE_INT, wdata);
        CHECK(ret, FAIL, "H5Awrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the attribute (make certain our buffer is unmodified) */
        ret = H5Aread(attr, H5T_NATIVE_INT, rdata);
        CHECK(ret, FAIL, "H5Aread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }
        }

        /* Write "nothing" to the attribute (with type conversion :-) */
        ret = H5Awrite(attr, H5T_NATIVE_SHORT, wdata_short);
        CHECK(ret, FAIL, "H5Awrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        /* Try reading from the attribute (with type conversion :-) (make certain our buffer is unmodified) */
        ret = H5Aread(attr, H5T_NATIVE_SHORT, rdata_short);
        CHECK(ret, FAIL, "H5Aread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata_short[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata_short[i][j]);
                }
            }
        }

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /*===============================================================
         * Extend the dimension to make it a normal dataspace (3x15x13).
         * Verify that data can be written to and read from the chunked
         * dataset now.
         *===============================================================
         */
        dims1[0] = SPACE1_DIM1;
        ret      = H5Sset_extent_simple(sid_chunk, SPACE1_RANK, dims1, max_dims);
        CHECK(ret, FAIL, "H5Sset_extent_simple");

        nelem = H5Sget_simple_extent_npoints(sid_chunk);
        CHECK(nelem, FAIL, "H5Sget_simple_extent_npoints");
        VERIFY(nelem, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, "H5Sget_simple_extent_npoints");

        rank = H5Sget_simple_extent_ndims(sid_chunk);
        CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
        VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");

        rank = H5Sget_simple_extent_dims(sid_chunk, tdims, NULL);
        CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
        VERIFY(memcmp(tdims, dims1, SPACE1_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");

        /* Set it to chunked dataset */
        plist_id = H5Pcreate(H5P_DATASET_CREATE);
        CHECK(plist_id, FAIL, "H5Pcreate");

        ret = H5Pset_chunk(plist_id, SPACE1_RANK, chunk_dims);
        CHECK(ret, FAIL, "H5Pset_chunk");

        ret = H5Pset_alloc_time(plist_id, alloc_time);
        CHECK(ret, FAIL, "H5Pset_alloc_time");

        dset1 =
            H5Dcreate2(fid1, BASICDATASET4, H5T_NATIVE_INT, sid_chunk, H5P_DEFAULT, plist_id, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dcreate2");

        ret = H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata_real);
        CHECK(ret, FAIL, "H5Dwrite");

        ret = H5Fflush(fid1, H5F_SCOPE_GLOBAL);
        CHECK(ret, FAIL, "H5Fflush");

        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata_real);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM1; i++) {
            for (j = 0; j < SPACE1_DIM2; j++) {
                for (k = 0; k < SPACE1_DIM3; k++) {
                    if (rdata_real[i][j][k] != wdata_real[i][j][k]) {
                        H5_FAILED();
                        printf("element [%d][%d][%d] is %d but should have been %d\n", i, j, k,
                               rdata_real[i][j][k], wdata_real[i][j][k]);
                    }
                }
            }
        }

        ret = H5Pclose(plist_id);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /* Change the dimensions to make them zero size again (0x0x0).  Verify that
         * no element is in the dataspace. */
        dims1[0] = dims1[1] = dims1[2] = 0;
        ret                            = H5Sset_extent_simple(sid_chunk, SPACE1_RANK, dims1, NULL);
        CHECK(ret, FAIL, "H5Sset_extent_simple");

        /* Check that the dataspace actually has 0 elements */
        nelem = H5Sget_simple_extent_npoints(sid_chunk);
        VERIFY(nelem, 0, "H5Sget_simple_extent_npoints");

        /* Check that the dataspace was created with an "all" selection */
        sel_type = H5Sget_select_type(sid_chunk);
        VERIFY(sel_type, H5S_SEL_ALL, "H5Sget_select_type");

        /* Check that the dataspace has 0 elements selected */
        nelem = H5Sget_select_npoints(sid_chunk);
        VERIFY(nelem, 0, "H5Sget_select_npoints");

        /* Change to "none" selection */
        ret = H5Sselect_none(sid_chunk);
        CHECK(ret, FAIL, "H5Sselect_none");

        /* Check that the dataspace has 0 elements selected */
        nelem = H5Sget_select_npoints(sid_chunk);
        VERIFY(nelem, 0, "H5Sget_select_npoints");

        ret = H5Sclose(sid_chunk);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /*============================================
         *  Reopen the file to check the dataspace
         *============================================
         */
        fid1 = H5Fopen(ZEROFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
        CHECK(fid1, FAIL, "H5Fopen");

        /* Reopen the chunked dataset */
        dset1 = H5Dopen2(fid1, BASICDATASET1, H5P_DEFAULT);
        CHECK(dset1, FAIL, "H5Dopen2");

        /* Get the space of the dataset and query it */
        sid1 = H5Dget_space(dset1);
        CHECK(sid1, FAIL, "H5Dget_space");

        /* Verify the class type of dataspace */
        stype = H5Sget_simple_extent_type(sid1);
        VERIFY(stype, H5S_SIMPLE, "H5Sget_simple_extent_type");

        /* Verify there is zero element in the dataspace */
        nelem = H5Sget_simple_extent_npoints(sid1);
        VERIFY(nelem, 0, "H5Sget_simple_extent_npoints");

        /* Verify the dimension sizes are correct */
        rank = H5Sget_simple_extent_dims(sid1, tdims, NULL);
        CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
        VERIFY(tdims[0], 0, "H5Sget_simple_extent_dims");
        VERIFY(tdims[1], SPACE1_DIM2, "H5Sget_simple_extent_dims");
        VERIFY(tdims[2], SPACE1_DIM3, "H5Sget_simple_extent_dims");

        /* Try reading from the dataset (make certain our buffer is unmodified) */
        ret = H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
        CHECK(ret, FAIL, "H5Dread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata[i][j]);
                }
            }
        }

        /* Close the dataset and its dataspace */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        /* Open the attribute for the file */
        attr = H5Aopen(fid1, NULLATTR, H5P_DEFAULT);
        CHECK(attr, FAIL, "H5Aopen");

        /* Get the space of the dataset */
        attr_sid = H5Aget_space(attr);
        CHECK(attr_sid, FAIL, "H5Aget_space");

        /* Verify the class type of dataspace */
        stype = H5Sget_simple_extent_type(attr_sid);
        VERIFY(stype, H5S_SIMPLE, "H5Sget_simple_extent_type");

        /* Verify there is zero element in the dataspace */
        nelem = H5Sget_simple_extent_npoints(attr_sid);
        VERIFY(nelem, 0, "H5Sget_simple_extent_npoints");

        /* Try reading from the attribute (make certain our buffer is unmodified) */
        ret = H5Aread(attr, H5T_NATIVE_SHORT, rdata_short);
        CHECK(ret, FAIL, "H5Aread");

        /* Check results */
        for (i = 0; i < SPACE1_DIM2; i++) {
            for (j = 0; j < SPACE1_DIM3; j++) {
                if (rdata_short[i][j] != 7) {
                    H5_FAILED();
                    printf("element [%d][%d] is %d but should have been 7\n", i, j, rdata_short[i][j]);
                }
            }
        }

        /* Close attribute */
        ret = H5Aclose(attr);
        CHECK(ret, FAIL, "H5Aclose");

        /* Close the dataspace */
        ret = H5Sclose(attr_sid);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");
    } /* end for */
} /* test_h5s_zero_dim() */

/****************************************************************
**
**  test_h5s_encode(): Test H5S (dataspace) encoding and decoding.
**
**  Note: See "RFC: H5Sencode/H5Sdecode Format Change".
**
****************************************************************/
static void
test_h5s_encode(H5F_libver_t low, H5F_libver_t high)
{
    hid_t          sid1, sid2, sid3; /* Dataspace ID        */
    hid_t          decoded_sid1, decoded_sid2, decoded_sid3;
    int            rank;                        /* Logical rank of dataspace    */
    hid_t          fapl      = H5I_INVALID_HID; /* File access property list ID */
    hsize_t        dims1[]   = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    size_t         sbuf_size = 0, null_size = 0, scalar_size = 0;
    unsigned char *sbuf = NULL, *null_sbuf = NULL, *scalar_buf = NULL;
    hsize_t        tdims[4]; /* Dimension array to test with */
    hssize_t       n;        /* Number of dataspace elements */
    hsize_t        start[]  = {0, 0, 0};
    hsize_t        stride[] = {2, 5, 3};
    hsize_t        count[]  = {2, 2, 2};
    hsize_t        block[]  = {1, 3, 1};
    H5S_sel_type   sel_type;
    H5S_class_t    space_type;
    hssize_t       nblocks;
    hid_t          ret_id; /* Generic hid_t return value    */
    herr_t         ret;    /* Generic return value        */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace Encoding and Decoding\n"));

    /*-------------------------------------------------------------------------
     * Test encoding and decoding of simple dataspace and hyperslab selection.
     *-------------------------------------------------------------------------
     */

    /* Create the file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set low/high bounds in the fapl */
    ret = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the dataspace */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Set the hyperslab selection */
    ret = H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, stride, count, block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Encode simple dataspace in a buffer with the fapl setting */
    ret = H5Sencode2(sid1, NULL, &sbuf_size, fapl);
    CHECK(ret, FAIL, "H5Sencode2");

    if (sbuf_size > 0) {
        sbuf = (unsigned char *)calloc((size_t)1, sbuf_size);
        CHECK_PTR(sbuf, "calloc");
    }

    /* Try decoding bogus buffer */
    H5E_BEGIN_TRY
    {
        ret_id = H5Sdecode(sbuf);
    }
    H5E_END_TRY
    VERIFY(ret_id, FAIL, "H5Sdecode");

    /* Encode the simple dataspace in a buffer with the fapl setting */
    ret = H5Sencode2(sid1, sbuf, &sbuf_size, fapl);
    CHECK(ret, FAIL, "H5Sencode");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid1 = H5Sdecode(sbuf);
    CHECK(decoded_sid1, FAIL, "H5Sdecode");

    /* Verify the decoded dataspace */
    n = H5Sget_simple_extent_npoints(decoded_sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, "H5Sget_simple_extent_npoints");

    /* Retrieve and verify the dataspace rank */
    rank = H5Sget_simple_extent_ndims(decoded_sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");

    /* Retrieve and verify the dataspace dimensions */
    rank = H5Sget_simple_extent_dims(decoded_sid1, tdims, NULL);
    CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tdims, dims1, SPACE1_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");

    /* Verify the type of dataspace selection */
    sel_type = H5Sget_select_type(decoded_sid1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify the number of hyperslab blocks */
    nblocks = H5Sget_select_hyper_nblocks(decoded_sid1);
    VERIFY(nblocks, 2 * 2 * 2, "H5Sget_select_hyper_nblocks");

    /* Close the dataspaces */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(decoded_sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /*-------------------------------------------------------------------------
     * Test encoding and decoding of null dataspace.
     *-------------------------------------------------------------------------
     */
    sid2 = H5Screate(H5S_NULL);
    CHECK(sid2, FAIL, "H5Screate");

    /* Encode null dataspace in a buffer */
    ret = H5Sencode2(sid2, NULL, &null_size, fapl);
    CHECK(ret, FAIL, "H5Sencode");

    if (null_size > 0) {
        null_sbuf = (unsigned char *)calloc((size_t)1, null_size);
        CHECK_PTR(null_sbuf, "calloc");
    }

    /* Encode the null dataspace in the buffer */
    ret = H5Sencode2(sid2, null_sbuf, &null_size, fapl);
    CHECK(ret, FAIL, "H5Sencode2");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid2 = H5Sdecode(null_sbuf);
    CHECK(decoded_sid2, FAIL, "H5Sdecode");

    /* Verify the decoded dataspace type */
    space_type = H5Sget_simple_extent_type(decoded_sid2);
    VERIFY(space_type, H5S_NULL, "H5Sget_simple_extent_type");

    /* Close the dataspaces */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(decoded_sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /*-------------------------------------------------------------------------
     * Test encoding and decoding of scalar dataspace.
     *-------------------------------------------------------------------------
     */
    /* Create scalar dataspace */
    sid3 = H5Screate(H5S_SCALAR);
    CHECK(sid3, FAIL, "H5Screate_simple");

    /* Encode scalar dataspace in a buffer */
    ret = H5Sencode2(sid3, NULL, &scalar_size, fapl);
    CHECK(ret, FAIL, "H5Sencode");

    if (scalar_size > 0) {
        scalar_buf = (unsigned char *)calloc((size_t)1, scalar_size);
        CHECK_PTR(scalar_buf, "calloc");
    }

    /* Encode the scalar dataspace in the buffer */
    ret = H5Sencode2(sid3, scalar_buf, &scalar_size, fapl);
    CHECK(ret, FAIL, "H5Sencode2");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid3 = H5Sdecode(scalar_buf);
    CHECK(decoded_sid3, FAIL, "H5Sdecode");

    /* Verify extent type */
    space_type = H5Sget_simple_extent_type(decoded_sid3);
    VERIFY(space_type, H5S_SCALAR, "H5Sget_simple_extent_type");

    /* Verify decoded dataspace */
    n = H5Sget_simple_extent_npoints(decoded_sid3);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    /* Retrieve and verify the dataspace rank */
    rank = H5Sget_simple_extent_ndims(decoded_sid3);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, 0, "H5Sget_simple_extent_ndims");

    /* Close the dataspaces */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(decoded_sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close the file access property list */
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

    /* Release resources */
    if (sbuf)
        free(sbuf);
    if (null_sbuf)
        free(null_sbuf);
    if (scalar_buf)
        free(scalar_buf);
} /* test_h5s_encode() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/****************************************************************
**
**  test_h5s_encode(): Test H5S (dataspace) encoding and decoding.
**
****************************************************************/
static void
test_h5s_encode1(void)
{
    hid_t          sid1, sid2, sid3; /* Dataspace ID */
    hid_t          decoded_sid1, decoded_sid2, decoded_sid3;
    int            rank; /* Logical rank of dataspace */
    hsize_t        dims1[]   = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    size_t         sbuf_size = 0, null_size = 0, scalar_size = 0;
    unsigned char *sbuf = NULL, *null_sbuf = NULL, *scalar_buf = NULL;
    hsize_t        tdims[4]; /* Dimension array to test with */
    hssize_t       n;        /* Number of dataspace elements */
    hsize_t        start[]  = {0, 0, 0};
    hsize_t        stride[] = {2, 5, 3};
    hsize_t        count[]  = {2, 2, 2};
    hsize_t        block[]  = {1, 3, 1};
    H5S_sel_type   sel_type;
    H5S_class_t    space_type;
    hssize_t       nblocks;
    hid_t          ret_id; /* Generic hid_t return value   */
    herr_t         ret;    /* Generic return value     */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace Encoding (H5Sencode1) and Decoding\n"));

    /*-------------------------------------------------------------------------
     * Test encoding and decoding of simple dataspace and hyperslab selection.
     *-------------------------------------------------------------------------
     */
    /* Create the dataspace */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Set the hyperslab selection */
    ret = H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, stride, count, block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Encode simple dataspace in a buffer with the fapl setting */
    ret = H5Sencode1(sid1, NULL, &sbuf_size);
    CHECK(ret, FAIL, "H5Sencode2");

    if (sbuf_size > 0) {
        sbuf = (unsigned char *)calloc((size_t)1, sbuf_size);
        CHECK_PTR(sbuf, "calloc");
    }

    /* Try decoding bogus buffer */
    H5E_BEGIN_TRY
    {
        ret_id = H5Sdecode(sbuf);
    }
    H5E_END_TRY
    VERIFY(ret_id, FAIL, "H5Sdecode");

    /* Encode the simple dataspace in a buffer */
    ret = H5Sencode1(sid1, sbuf, &sbuf_size);
    CHECK(ret, FAIL, "H5Sencode");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid1 = H5Sdecode(sbuf);
    CHECK(decoded_sid1, FAIL, "H5Sdecode");

    /* Verify the decoded dataspace */
    n = H5Sget_simple_extent_npoints(decoded_sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3, "H5Sget_simple_extent_npoints");

    /* Retrieve and verify the dataspace rank */
    rank = H5Sget_simple_extent_ndims(decoded_sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE1_RANK, "H5Sget_simple_extent_ndims");

    /* Retrieve and verify the dataspace dimensions */
    rank = H5Sget_simple_extent_dims(decoded_sid1, tdims, NULL);
    CHECK(rank, FAIL, "H5Sget_simple_extent_dims");
    VERIFY(memcmp(tdims, dims1, SPACE1_RANK * sizeof(hsize_t)), 0, "H5Sget_simple_extent_dims");

    /* Verify the type of dataspace selection */
    sel_type = H5Sget_select_type(decoded_sid1);
    VERIFY(sel_type, H5S_SEL_HYPERSLABS, "H5Sget_select_type");

    /* Verify the number of hyperslab blocks */
    nblocks = H5Sget_select_hyper_nblocks(decoded_sid1);
    VERIFY(nblocks, 2 * 2 * 2, "H5Sget_select_hyper_nblocks");

    /* Close the dataspaces */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(decoded_sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /*-------------------------------------------------------------------------
     * Test encoding and decoding of null dataspace.
     *-------------------------------------------------------------------------
     */
    sid2 = H5Screate(H5S_NULL);
    CHECK(sid2, FAIL, "H5Screate");

    /* Encode null dataspace in a buffer */
    ret = H5Sencode1(sid2, NULL, &null_size);
    CHECK(ret, FAIL, "H5Sencode");

    if (null_size > 0) {
        null_sbuf = (unsigned char *)calloc((size_t)1, null_size);
        CHECK_PTR(null_sbuf, "calloc");
    }

    /* Encode the null dataspace in the buffer */
    ret = H5Sencode1(sid2, null_sbuf, &null_size);
    CHECK(ret, FAIL, "H5Sencode2");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid2 = H5Sdecode(null_sbuf);
    CHECK(decoded_sid2, FAIL, "H5Sdecode");

    /* Verify the decoded dataspace type */
    space_type = H5Sget_simple_extent_type(decoded_sid2);
    VERIFY(space_type, H5S_NULL, "H5Sget_simple_extent_type");

    /* Close the dataspaces */
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(decoded_sid2);
    CHECK(ret, FAIL, "H5Sclose");

    /*-------------------------------------------------------------------------
     * Test encoding and decoding of scalar dataspace.
     *-------------------------------------------------------------------------
     */
    /* Create scalar dataspace */
    sid3 = H5Screate(H5S_SCALAR);
    CHECK(sid3, FAIL, "H5Screate");

    /* Encode scalar dataspace in a buffer */
    ret = H5Sencode1(sid3, NULL, &scalar_size);
    CHECK(ret, FAIL, "H5Sencode");

    if (scalar_size > 0) {
        scalar_buf = (unsigned char *)calloc((size_t)1, scalar_size);
        CHECK_PTR(scalar_buf, "calloc");
    }

    /* Encode the scalar dataspace in the buffer */
    ret = H5Sencode1(sid3, scalar_buf, &scalar_size);
    CHECK(ret, FAIL, "H5Sencode2");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid3 = H5Sdecode(scalar_buf);
    CHECK(decoded_sid3, FAIL, "H5Sdecode");

    /* Verify extent type */
    space_type = H5Sget_simple_extent_type(decoded_sid3);
    VERIFY(space_type, H5S_SCALAR, "H5Sget_simple_extent_type");

    /* Verify decoded dataspace */
    n = H5Sget_simple_extent_npoints(decoded_sid3);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    /* Retrieve and verify the dataspace rank */
    rank = H5Sget_simple_extent_ndims(decoded_sid3);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, 0, "H5Sget_simple_extent_ndims");

    /* Close the dataspaces */
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(decoded_sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Release resources */
    if (sbuf)
        free(sbuf);
    if (null_sbuf)
        free(null_sbuf);
    if (scalar_buf)
        free(scalar_buf);
} /* test_h5s_encode1() */

#endif /* H5_NO_DEPRECATED_SYMBOLS */

/****************************************************************
**
**  test_h5s_check_encoding():
**      This is the helper routine to verify that H5Sencode2()
**      works as specified in the RFC for the library format setting
**      in the file access property list.
**      See "RFC: H5Sencode/H5Sdeocde Format Change".
**
**      This routine is used by:
**          test_h5s_encode_regular_hyper()
**          test_h5s_encode_irregular_hyper()
**          test_h5s_encode_points()
**
****************************************************************/
static herr_t
test_h5s_check_encoding(hid_t in_fapl, hid_t in_sid, uint32_t expected_version, uint8_t expected_enc_size,
                        bool expected_to_fail)
{
    char   *buf = NULL;              /* Pointer to the encoded buffer */
    size_t  buf_size;                /* Size of the encoded buffer */
    hid_t   d_sid = H5I_INVALID_HID; /* The decoded dataspace ID */
    htri_t  check;
    hsize_t in_low_bounds[1];  /* The low bounds for the selection for in_sid */
    hsize_t in_high_bounds[1]; /* The high bounds for the selection for in_sid */
    hsize_t d_low_bounds[1];   /* The low bounds for the selection for d_sid */
    hsize_t d_high_bounds[1];  /* The high bounds for the selection for d_sid */
    herr_t  ret;               /* Return value */

    /* Get buffer size for encoding with the format setting in in_fapl */
    H5E_BEGIN_TRY
    {
        ret = H5Sencode2(in_sid, NULL, &buf_size, in_fapl);
    }
    H5E_END_TRY

    if (expected_to_fail) {
        VERIFY(ret, FAIL, "H5Screate_simple");
    }
    else {

        CHECK(ret, FAIL, "H5Sencode2");

        /* Allocate the buffer for encoding */
        buf = (char *)malloc(buf_size);
        CHECK_PTR(buf, "malloc");

        /* Encode according to the setting in in_fapl */
        ret = H5Sencode2(in_sid, buf, &buf_size, in_fapl);
        CHECK(ret, FAIL, "H5Sencode2");

        /* Decode the buffer */
        d_sid = H5Sdecode(buf);
        CHECK(d_sid, FAIL, "H5Sdecode");

        /* Verify the number of selected points for in_sid and d_sid */
        VERIFY(H5Sget_select_npoints(in_sid), H5Sget_select_npoints(d_sid), "Compare npoints");

        /* Verify if the two dataspace selections (in_sid, d_sid) are the same shape */
        check = H5Sselect_shape_same(in_sid, d_sid);
        VERIFY(check, true, "H5Sselect_shape_same");

        /* Compare the starting/ending coordinates of the bounding box for in_sid and d_sid */
        ret = H5Sget_select_bounds(in_sid, in_low_bounds, in_high_bounds);
        CHECK(ret, FAIL, "H5Sget_select_bounds");
        ret = H5Sget_select_bounds(d_sid, d_low_bounds, d_high_bounds);
        CHECK(ret, FAIL, "H5Sget_select_bounds");
        VERIFY(in_low_bounds[0], d_low_bounds[0], "Compare selection low bounds");
        VERIFY(in_high_bounds[0], d_high_bounds[0], "Compare selection high bounds");

        /*
         * See "RFC: H5Sencode/H5Sdeocde Format Change" for the verification of:
         *   H5S_SEL_POINTS:
         *      --the expected version for point selection info
         *      --the expected encoded size (version 2 points selection info)
         *   H5S_SEL_HYPERSLABS:
         *      --the expected version for hyperslab selection info
         *      --the expected encoded size (version 3 hyperslab selection info)
         */

        if (H5Sget_select_type(in_sid) == H5S_SEL_POINTS) {

            /* Verify the version */
            VERIFY((uint32_t)buf[35], expected_version, "Version for point selection");

            /* Verify the encoded size for version 2 */
            if (expected_version == 2)
                VERIFY((uint8_t)buf[39], expected_enc_size, "Encoded size of point selection info");
        }

        if (H5Sget_select_type(in_sid) == H5S_SEL_HYPERSLABS) {

            /* Verify the version */
            VERIFY((uint32_t)buf[35], expected_version, "Version for hyperslab selection info");

            /* Verify the encoded size for version 3 */
            if (expected_version == 3)
                VERIFY((uint8_t)buf[40], expected_enc_size, "Encoded size of selection info");

        } /* hyperslab selection */

        ret = H5Sclose(d_sid);
        CHECK(ret, FAIL, "H5Sclose");
        if (buf)
            free(buf);
    }

    return (0);

} /* test_h5s_check_encoding */

/****************************************************************
**
**  test_h5s_encode_regular_hyper():
**      This test verifies that H5Sencode2() works as specified in
**      the RFC for regular hyperslabs.
**      See "RFC: H5Sencode/H5Sdeocde Format Change".
**
****************************************************************/
static void
test_h5s_encode_regular_hyper(H5F_libver_t low, H5F_libver_t high)
{
    hid_t    fapl            = H5I_INVALID_HID; /* File access property list ID */
    hid_t    sid             = H5I_INVALID_HID; /* Dataspace ID */
    hsize_t  numparticles    = 8388608;         /* Used to calculate dimension size */
    unsigned num_dsets       = 513;             /* Used to calculate dimension size */
    hsize_t  total_particles = numparticles * num_dsets;
    hsize_t  vdsdims[1]      = {total_particles}; /* Dimension size */
    hsize_t  start, stride, count, block;         /* Selection info */
    unsigned config;                              /* Testing configuration */
    unsigned unlim;                               /* H5S_UNLIMITED setting or not */
    herr_t   ret;                                 /* Generic return value */
    uint32_t expected_version  = 0;               /* Expected version for selection info */
    uint8_t  expected_enc_size = 0;               /* Expected encoded size for selection info */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace encoding of regular hyperslabs\n"));

    /* Create the file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set the low/high bounds in the fapl */
    ret = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the dataspace */
    sid = H5Screate_simple(1, vdsdims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Testing with each configuration */
    for (config = CONFIG_16; config <= CONFIG_32; config++) {
        bool expected_to_fail = false;

        /* Testing with unlimited or not */
        for (unlim = 0; unlim <= 1; unlim++) {
            start = 0;
            count = unlim ? H5S_UNLIMITED : 2;

            if ((high <= H5F_LIBVER_V18) && (unlim || config == CONFIG_32))
                expected_to_fail = true;

            if (low >= H5F_LIBVER_V112)
                expected_version = 3;
            else if (config == CONFIG_16 && !unlim)
                expected_version = 1;
            else
                expected_version = 2;

            /* test 1 */
            switch (config) {
                case CONFIG_16:
                    stride            = POWER16 - 1;
                    block             = 4;
                    expected_enc_size = (uint8_t)(expected_version == 3 ? 2 : 4);
                    break;
                case CONFIG_32:
                    stride            = POWER32 - 1;
                    block             = 4;
                    expected_enc_size = (uint8_t)(expected_version == 3 ? 4 : 8);

                    break;
                default:
                    assert(0);
                    break;
            } /* end switch */

            /* Set the hyperslab selection */
            ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Verify the version and encoded size expected for this configuration */
            ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
            CHECK(ret, FAIL, "test_h5s_check_encoding");

            /* test 2 */
            switch (config) {
                case CONFIG_16:
                    stride            = POWER16 - 1;
                    block             = POWER16 - 2;
                    expected_enc_size = (uint8_t)(expected_version == 3 ? 2 : 4);
                    break;
                case CONFIG_32:
                    stride            = POWER32 - 1;
                    block             = POWER32 - 2;
                    expected_enc_size = (uint8_t)(expected_version == 3 ? 4 : 8);
                    break;
                default:
                    assert(0);
                    break;
            } /* end switch */

            /* Set the hyperslab selection */
            ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Verify the version and encoded size for this configuration */
            ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
            CHECK(ret, FAIL, "test_h5s_check_encoding");

            /* test 3 */
            switch (config) {
                case CONFIG_16:
                    stride            = POWER16 - 1;
                    block             = POWER16 - 1;
                    expected_enc_size = 4;
                    break;
                case CONFIG_32:
                    stride            = POWER32 - 1;
                    block             = POWER32 - 1;
                    expected_enc_size = 8;
                    break;
                default:
                    assert(0);
                    break;
            }

            /* Set the hyperslab selection */
            ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Verify the version and encoded size expected for this configuration */
            ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
            CHECK(ret, FAIL, "test_h5s_check_encoding");

            /* test 4 */
            switch (config) {
                case CONFIG_16:
                    stride            = POWER16;
                    block             = POWER16 - 2;
                    expected_enc_size = 4;
                    break;
                case CONFIG_32:
                    stride            = POWER32;
                    block             = POWER32 - 2;
                    expected_enc_size = 8;
                    break;
                default:
                    assert(0);
                    break;
            } /* end switch */

            /* Set the hyperslab selection */
            ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Verify the version and encoded size expected for this configuration */
            ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
            CHECK(ret, FAIL, "test_h5s_check_encoding");

            /* test 5 */
            switch (config) {
                case CONFIG_16:
                    stride            = POWER16;
                    block             = 1;
                    expected_enc_size = 4;
                    break;
                case CONFIG_32:
                    stride            = POWER32;
                    block             = 1;
                    expected_enc_size = 8;
                    break;
                default:
                    assert(0);
                    break;
            }

            /* Set the hyperslab selection */
            ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
            CHECK(ret, FAIL, "H5Sselect_hyperslab");

            /* Verify the version and encoded size expected for this configuration */
            ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
            CHECK(ret, FAIL, "test_h5s_check_encoding");

        } /* for unlim */
    }     /* for config */

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");

} /* test_h5s_encode_regular_hyper() */

/****************************************************************
**
**  test_h5s_encode_irregular_hyper():
**      This test verifies that H5Sencode2() works as specified in
**      the RFC for irregular hyperslabs.
**      See "RFC: H5Sencode/H5Sdeocde Format Change".
**
****************************************************************/
static void
test_h5s_encode_irregular_hyper(H5F_libver_t low, H5F_libver_t high)
{
    hid_t    fapl = H5I_INVALID_HID;    /* File access property list ID */
    hid_t    sid;                       /* Dataspace ID */
    hsize_t  numparticles    = 8388608; /* Used to calculate dimension size */
    unsigned num_dsets       = 513;     /* Used to calculate dimension size */
    hsize_t  total_particles = numparticles * num_dsets;
    hsize_t  vdsdims[1]      = {total_particles}; /* Dimension size */
    hsize_t  start, stride, count, block;         /* Selection info */
    htri_t   is_regular;                          /* Is this a regular hyperslab */
    unsigned config;                              /* Testing configuration */
    herr_t   ret;                                 /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace encoding of irregular hyperslabs\n"));

    /* Create the file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set the low/high bounds in the fapl */
    ret = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the dataspace */
    sid = H5Screate_simple(1, vdsdims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Testing with each configuration */
    for (config = CONFIG_8; config <= CONFIG_32; config++) {
        bool     expected_to_fail  = false; /* Whether H5Sencode2 is expected to fail */
        uint32_t expected_version  = 0;     /* Expected version for selection info */
        uint32_t expected_enc_size = 0;     /* Expected encoded size for selection info */

        start = 0;
        count = 2;
        block = 4;

        /* H5Sencode2 is expected to fail for library v110 and below
           when the selection exceeds the 32 bits integer limit */
        if (high <= H5F_LIBVER_V110 && config == CONFIG_32)
            expected_to_fail = true;

        if (low >= H5F_LIBVER_V112 || config == CONFIG_32)
            expected_version = 3;
        else
            expected_version = 1;

        switch (config) {
            case CONFIG_8:
                stride = POWER8 - 2;
                break;

            case CONFIG_16:
                stride = POWER16 - 2;
                break;

            case CONFIG_32:
                stride = POWER32 - 2;
                break;

            default:
                assert(0);
                break;
        }

        /* Set the hyperslab selection */
        ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        start = 8;
        count = 5;
        block = 2;

        switch (config) {
            case CONFIG_8:
                stride            = POWER8;
                expected_enc_size = expected_version == 3 ? 2 : 4;
                break;

            case CONFIG_16:
                stride            = POWER16;
                expected_enc_size = 4;
                break;

            case CONFIG_32:
                stride            = POWER32;
                expected_enc_size = 8;
                break;

            default:
                assert(0);
                break;
        }

        /* Set the hyperslab selection */
        ret = H5Sselect_hyperslab(sid, H5S_SELECT_OR, &start, &stride, &count, &block);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        /* Should be irregular hyperslab */
        is_regular = H5Sis_regular_hyperslab(sid);
        VERIFY(is_regular, false, "H5Sis_regular_hyperslab");

        /* Verify the version and encoded size expected for the configuration */
        assert(expected_enc_size <= 255);
        ret = test_h5s_check_encoding(fapl, sid, expected_version, (uint8_t)expected_enc_size,
                                      expected_to_fail);
        CHECK(ret, FAIL, "test_h5s_check_encoding");

    } /* for config */

    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

} /* test_h5s_encode_irregular_hyper() */

/****************************************************************
**
**  test_h5s_encode_points():
**      This test verifies that H5Sencode2() works as specified in
**      the RFC for point selection.
**      See "RFC: H5Sencode/H5Sdeocde Format Change".
**
****************************************************************/
static void
test_h5s_encode_points(H5F_libver_t low, H5F_libver_t high)
{
    hid_t    fapl = H5I_INVALID_HID;    /* File access property list ID */
    hid_t    sid;                       /* Dataspace ID */
    hsize_t  numparticles    = 8388608; /* Used to calculate dimension size */
    unsigned num_dsets       = 513;     /* used to calculate dimension size */
    hsize_t  total_particles = numparticles * num_dsets;
    hsize_t  vdsdims[1]      = {total_particles}; /* Dimension size */
    hsize_t  coord[4];                            /* The point coordinates */
    herr_t   ret;                                 /* Generic return value */
    bool     expected_to_fail  = false;           /* Expected to fail or not */
    uint32_t expected_version  = 0;               /* Expected version for selection info */
    uint8_t  expected_enc_size = 0;               /* Expected encoded size of selection info */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspace encoding of points selection\n"));

    /* Create the file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Set the low/high bounds in the fapl */
    ret = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the dataspace */
    sid = H5Screate_simple(1, vdsdims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* test 1 */
    coord[0] = 5;
    coord[1] = 15;
    coord[2] = POWER16;
    coord[3] = 19;
    ret      = H5Sselect_elements(sid, H5S_SELECT_SET, (size_t)4, coord);
    CHECK(ret, FAIL, "H5Sselect_elements");

    expected_to_fail  = false;
    expected_enc_size = 4;
    expected_version  = 1;

    if (low >= H5F_LIBVER_V112)
        expected_version = 2;

    /* Verify the version and encoded size expected for the configuration */
    ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
    CHECK(ret, FAIL, "test_h5s_check_encoding");

    /* test 2 */
    coord[0] = 5;
    coord[1] = 15;
    coord[2] = POWER32 - 1;
    coord[3] = 19;
    ret      = H5Sselect_elements(sid, H5S_SELECT_SET, (size_t)4, coord);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Expected result same as test 1 */
    ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
    CHECK(ret, FAIL, "test_h5s_check_encoding");

    /* test 3 */
    if (high <= H5F_LIBVER_V110)
        expected_to_fail = true;

    if (high >= H5F_LIBVER_V112) {
        expected_version  = 2;
        expected_enc_size = 8;
    }

    coord[0] = 5;
    coord[1] = 15;
    coord[2] = POWER32 + 1;
    coord[3] = 19;
    ret      = H5Sselect_elements(sid, H5S_SELECT_SET, (size_t)4, coord);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Verify the version and encoded size expected for the configuration */
    ret = test_h5s_check_encoding(fapl, sid, expected_version, expected_enc_size, expected_to_fail);
    CHECK(ret, FAIL, "test_h5s_check_encoding");

    /* Close the dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

} /* test_h5s_encode_points() */

/****************************************************************
**
**  test_h5s_encode_length():
**      Test to verify HDFFV-10271 is fixed.
**      Verify that version 2 hyperslab encoding length is correct.
**
**  See "RFC: H5Sencode/H5Sdecode Format Change" for the
**  description of the encoding format.
**
****************************************************************/
static void
test_h5s_encode_length(void)
{
    hid_t          sid;                         /* Dataspace ID */
    hid_t          decoded_sid;                 /* Dataspace ID from H5Sdecode2 */
    size_t         sbuf_size = 0;               /* Buffer size for H5Sencode2/1 */
    unsigned char *sbuf      = NULL;            /* Buffer for H5Sencode2/1 */
    hsize_t        dims[1]   = {500};           /* Dimension size */
    hsize_t        start, count, block, stride; /* Hyperslab selection specifications */
    herr_t         ret;                         /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Version 2 Hyperslab Encoding Length is correct\n"));

    /* Create dataspace */
    sid = H5Screate_simple(1, dims, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Setting H5S_UNLIMITED in count will use version 2 for hyperslab encoding */
    start  = 0;
    stride = 10;
    block  = 4;
    count  = H5S_UNLIMITED;

    /* Set hyperslab selection */
    ret = H5Sselect_hyperslab(sid, H5S_SELECT_SET, &start, &stride, &count, &block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Encode simple dataspace in a buffer */
    ret = H5Sencode2(sid, NULL, &sbuf_size, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Sencode");

    /* Allocate the buffer */
    if (sbuf_size > 0) {
        sbuf = (unsigned char *)calloc((size_t)1, sbuf_size);
        CHECK_PTR(sbuf, "H5Sencode2");
    }

    /* Encode the dataspace */
    ret = H5Sencode2(sid, sbuf, &sbuf_size, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Sencode");

    /* Verify that length stored at this location in the buffer is correct */
    VERIFY((uint32_t)sbuf[40], 36, "Length for encoding version 2");
    VERIFY((uint32_t)sbuf[35], 2, "Hyperslab encoding version is 2");

    /* Decode from the dataspace buffer and return an object handle */
    decoded_sid = H5Sdecode(sbuf);
    CHECK(decoded_sid, FAIL, "H5Sdecode");

    /* Verify that the original and the decoded dataspace are equal */
    VERIFY(H5Sget_select_npoints(sid), H5Sget_select_npoints(decoded_sid), "Compare npoints");

    /* Close the decoded dataspace */
    ret = H5Sclose(decoded_sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Free the buffer */
    if (sbuf)
        free(sbuf);

    /* Close the original dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

} /* test_h5s_encode_length() */

/****************************************************************
**
**  test_h5s_scalar_write(): Test scalar H5S (dataspace) writing code.
**
****************************************************************/
static void
test_h5s_scalar_write(void)
{
    hid_t       fid1;     /* HDF5 File IDs        */
    hid_t       dataset;  /* Dataset ID            */
    hid_t       sid1;     /* Dataspace ID            */
    int         rank;     /* Logical rank of dataspace    */
    hsize_t     tdims[4]; /* Dimension array to test with */
    hssize_t    n;        /* Number of dataspace elements */
    H5S_class_t ext_type; /* Extent type */
    herr_t      ret;      /* Generic return value        */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation during Writing\n"));

    /* Create file */
    fid1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Verify a non-zero rank fails with a NULL dimension. */
    H5E_BEGIN_TRY
    {
        sid1 = H5Screate_simple(SPACE1_RANK, NULL, NULL);
    }
    H5E_END_TRY
    VERIFY(sid1, FAIL, "H5Screate_simple");

    /* Create scalar dataspace */
    sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Retrieve the number of elements in the dataspace selection */
    n = H5Sget_simple_extent_npoints(sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    /* Get the dataspace rank */
    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    /* Get the dataspace dimension sizes */
    rank = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(rank, 0, "H5Sget_simple_extent_dims");

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SCALAR, "H5Sget_simple_extent_type");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write to the dataset */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &space3_data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_h5s_scalar_write() */

/****************************************************************
**
**  test_h5s_scalar_read(): Test scalar H5S (dataspace) reading code.
**
****************************************************************/
static void
test_h5s_scalar_read(void)
{
    hid_t       fid1;     /* HDF5 File IDs        */
    hid_t       dataset;  /* Dataset ID            */
    hid_t       sid1;     /* Dataspace ID            */
    int         rank;     /* Logical rank of dataspace    */
    hsize_t     tdims[4]; /* Dimension array to test with */
    hssize_t    n;        /* Number of dataspace elements */
    unsigned    rdata;    /* Scalar data read in         */
    herr_t      ret;      /* Generic return value        */
    H5S_class_t ext_type; /* Extent type */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation during Reading\n"));

    /* Create file */
    fid1 = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Create a dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    sid1 = H5Dget_space(dataset);
    CHECK(sid1, FAIL, "H5Dget_space");

    n = H5Sget_simple_extent_npoints(sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    rank = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(rank, 0, "H5Sget_simple_extent_dims");

    /* Verify extent type */
    ext_type = H5Sget_simple_extent_type(sid1);
    VERIFY(ext_type, H5S_SCALAR, "H5Sget_simple_extent_type");

    ret = H5Dread(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    VERIFY(rdata, space3_data, "H5Dread");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_h5s_scalar_read() */

/****************************************************************
**
**  test_h5s_compound_scalar_write(): Test scalar H5S (dataspace) writing for
**          compound datatypes.
**
****************************************************************/
static void
test_h5s_compound_scalar_write(void)
{
    hid_t    fid1;     /* HDF5 File IDs        */
    hid_t    dataset;  /* Dataset ID            */
    hid_t    tid1;     /* Attribute datatype ID    */
    hid_t    sid1;     /* Dataspace ID            */
    int      rank;     /* Logical rank of dataspace    */
    hsize_t  tdims[4]; /* Dimension array to test with */
    hssize_t n;        /* Number of dataspace elements */
    herr_t   ret;      /* Generic return value        */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation for Writing Compound Datatypes\n"));

    /* Create file */
    fid1 = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create the compound datatype.  */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct space4_struct));
    CHECK(tid1, FAIL, "H5Tcreate");
    space4_field1_off = HOFFSET(struct space4_struct, c1);
    ret               = H5Tinsert(tid1, SPACE4_FIELDNAME1, space4_field1_off, H5T_NATIVE_SCHAR);
    CHECK(ret, FAIL, "H5Tinsert");
    space4_field2_off = HOFFSET(struct space4_struct, u);
    ret               = H5Tinsert(tid1, SPACE4_FIELDNAME2, space4_field2_off, H5T_NATIVE_UINT);
    CHECK(ret, FAIL, "H5Tinsert");
    space4_field3_off = HOFFSET(struct space4_struct, f);
    ret               = H5Tinsert(tid1, SPACE4_FIELDNAME3, space4_field3_off, H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");
    space4_field4_off = HOFFSET(struct space4_struct, c2);
    ret               = H5Tinsert(tid1, SPACE4_FIELDNAME4, space4_field4_off, H5T_NATIVE_SCHAR);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create scalar dataspace */
    sid1 = H5Screate_simple(SPACE3_RANK, NULL, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    n = H5Sget_simple_extent_npoints(sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    rank = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(rank, 0, "H5Sget_simple_extent_dims");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &space4_data);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close compound datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* test_h5s_compound_scalar_write() */

/****************************************************************
**
**  test_h5s_compound_scalar_read(): Test scalar H5S (dataspace) reading for
**          compound datatypes.
**
****************************************************************/
static void
test_h5s_compound_scalar_read(void)
{
    hid_t                fid1;     /* HDF5 File IDs        */
    hid_t                dataset;  /* Dataset ID            */
    hid_t                sid1;     /* Dataspace ID            */
    hid_t                type;     /* Datatype                 */
    int                  rank;     /* Logical rank of dataspace    */
    hsize_t              tdims[4]; /* Dimension array to test with */
    hssize_t             n;        /* Number of dataspace elements */
    struct space4_struct rdata;    /* Scalar data read in         */
    herr_t               ret;      /* Generic return value        */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Scalar Dataspace Manipulation for Reading Compound Datatypes\n"));

    /* Create file */
    fid1 = H5Fopen(DATAFILE, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Create a dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    sid1 = H5Dget_space(dataset);
    CHECK(sid1, FAIL, "H5Dget_space");

    n = H5Sget_simple_extent_npoints(sid1);
    CHECK(n, FAIL, "H5Sget_simple_extent_npoints");
    VERIFY(n, 1, "H5Sget_simple_extent_npoints");

    rank = H5Sget_simple_extent_ndims(sid1);
    CHECK(rank, FAIL, "H5Sget_simple_extent_ndims");
    VERIFY(rank, SPACE3_RANK, "H5Sget_simple_extent_ndims");

    rank = H5Sget_simple_extent_dims(sid1, tdims, NULL);
    VERIFY(rank, 0, "H5Sget_simple_extent_dims");

    type = H5Dget_type(dataset);
    CHECK(type, FAIL, "H5Dget_type");

    ret = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata);
    CHECK(ret, FAIL, "H5Dread");
    if (memcmp(&space4_data, &rdata, sizeof(struct space4_struct)) != 0) {
        printf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n", space4_data.c1, rdata.c1);
        printf("scalar data different: space4_data.u=%u, read_data4.u=%u\n", space4_data.u, rdata.u);
        printf("scalar data different: space4_data.f=%f, read_data4.f=%f\n", (double)space4_data.f,
               (double)rdata.f);
        TestErrPrintf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n", space4_data.c1,
                      rdata.c2);
    } /* end if */

    /* Close datatype */
    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close scalar dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_h5s_compound_scalar_read() */

/* Data array sizes for chunk test */
#define CHUNK_DATA_NX 50000
#define CHUNK_DATA_NY 3

/****************************************************************
**
**  test_h5s_chunk(): Exercise chunked I/O, testing when data conversion
**      is necessary and the entire chunk read in doesn't fit into the
**      conversion buffer
**
****************************************************************/
static void
test_h5s_chunk(void)
{
    herr_t   status;
    hid_t    fileID, dsetID;
    hid_t    plist_id;
    hid_t    space_id;
    hsize_t  dims[2];
    hsize_t  csize[2];
    double **chunk_data_dbl      = NULL;
    double  *chunk_data_dbl_data = NULL;
    float  **chunk_data_flt      = NULL;
    float   *chunk_data_flt_data = NULL;
    int      i, j;

    /* Allocate memory */
    chunk_data_dbl_data = (double *)calloc(CHUNK_DATA_NX * CHUNK_DATA_NY, sizeof(double));
    CHECK_PTR(chunk_data_dbl_data, "calloc");
    chunk_data_dbl = (double **)calloc(CHUNK_DATA_NX, sizeof(chunk_data_dbl_data));
    CHECK_PTR(chunk_data_dbl, "calloc");
    for (i = 0; i < CHUNK_DATA_NX; i++)
        chunk_data_dbl[i] = chunk_data_dbl_data + (i * CHUNK_DATA_NY);

    chunk_data_flt_data = (float *)calloc(CHUNK_DATA_NX * CHUNK_DATA_NY, sizeof(float));
    CHECK_PTR(chunk_data_flt_data, "calloc");
    chunk_data_flt = (float **)calloc(CHUNK_DATA_NX, sizeof(chunk_data_flt_data));
    CHECK_PTR(chunk_data_flt, "calloc");
    for (i = 0; i < CHUNK_DATA_NX; i++)
        chunk_data_flt[i] = chunk_data_flt_data + (i * CHUNK_DATA_NY);

    fileID = H5Fcreate(DATAFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fileID, FAIL, "H5Fcreate");

    plist_id = H5Pcreate(H5P_DATASET_CREATE);
    CHECK(plist_id, FAIL, "H5Pcreate");

    csize[0] = CHUNK_DATA_NX;
    csize[1] = CHUNK_DATA_NY;
    status   = H5Pset_chunk(plist_id, 2, csize);
    CHECK(status, FAIL, "H5Pset_chunk");

    /* Create the dataspace */
    dims[0]  = CHUNK_DATA_NX;
    dims[1]  = CHUNK_DATA_NY;
    space_id = H5Screate_simple(2, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple");

    dsetID = H5Dcreate2(fileID, "coords", H5T_NATIVE_FLOAT, space_id, H5P_DEFAULT, plist_id, H5P_DEFAULT);
    CHECK(dsetID, FAIL, "H5Dcreate2");

    /* Initialize float array */
    for (i = 0; i < CHUNK_DATA_NX; i++)
        for (j = 0; j < CHUNK_DATA_NY; j++)
            chunk_data_flt[i][j] = (float)(i + 1) * 2.5F - (float)j * 100.3F;

    status = H5Dwrite(dsetID, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, chunk_data_flt_data);
    CHECK(status, FAIL, "H5Dwrite");

    status = H5Pclose(plist_id);
    CHECK(status, FAIL, "H5Pclose");
    status = H5Sclose(space_id);
    CHECK(status, FAIL, "H5Sclose");
    status = H5Dclose(dsetID);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Fclose(fileID);
    CHECK(status, FAIL, "H5Fclose");

    /* Reset/initialize the data arrays to read in */
    memset(chunk_data_dbl_data, 0, sizeof(double) * CHUNK_DATA_NX * CHUNK_DATA_NY);
    memset(chunk_data_flt_data, 0, sizeof(float) * CHUNK_DATA_NX * CHUNK_DATA_NY);

    fileID = H5Fopen(DATAFILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fileID, FAIL, "H5Fopen");
    dsetID = H5Dopen2(fileID, "coords", H5P_DEFAULT);
    CHECK(dsetID, FAIL, "H5Dopen2");

    status = H5Dread(dsetID, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, chunk_data_dbl_data);
    CHECK(status, FAIL, "H5Dread");
    status = H5Dread(dsetID, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, chunk_data_flt_data);
    CHECK(status, FAIL, "H5Dread");

    status = H5Dclose(dsetID);
    CHECK(status, FAIL, "H5Dclose");
    status = H5Fclose(fileID);
    CHECK(status, FAIL, "H5Fclose");

    for (i = 0; i < CHUNK_DATA_NX; i++) {
        for (j = 0; j < CHUNK_DATA_NY; j++) {
            /* Check if the two values are within 0.001% range. */
            if (!H5_DBL_REL_EQUAL(chunk_data_dbl[i][j], (double)chunk_data_flt[i][j], 0.00001))
                TestErrPrintf("%u: chunk_data_dbl[%d][%d]=%e, chunk_data_flt[%d][%d]=%e\n",
                              (unsigned)__LINE__, i, j, chunk_data_dbl[i][j], i, j,
                              (double)chunk_data_flt[i][j]);
        } /* end for */
    }     /* end for */

    free(chunk_data_dbl);
    free(chunk_data_dbl_data);
    free(chunk_data_flt);
    free(chunk_data_flt_data);
} /* test_h5s_chunk() */

/****************************************************************
**
**  test_h5s_extent_equal(): Exercise extent comparison code
**
****************************************************************/
static void
test_h5s_extent_equal(void)
{
    hid_t   null_space;                                 /* Null dataspace */
    hid_t   scalar_space;                               /* Scalar dataspace */
    hid_t   d1_space1, d1_space2, d1_space3, d1_space4; /* 1-D dataspaces */
    hid_t   d2_space1, d2_space2, d2_space3, d2_space4; /* 2-D dataspaces */
    hid_t   d3_space1, d3_space2, d3_space3, d3_space4; /* 3-D dataspaces */
    hsize_t d1_dims1[1] = {10},                         /* 1-D dimensions */
        d1_dims2[1] = {20}, d1_dims3[1] = {H5S_UNLIMITED};
    hsize_t d2_dims1[2] = {10, 10}, /* 2-D dimensions */
        d2_dims2[2] = {20, 20}, d2_dims3[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t d3_dims1[3] = {10, 10, 10}, /* 3-D dimensions */
        d3_dims2[3] = {20, 20, 20}, d3_dims3[3] = {H5S_UNLIMITED, H5S_UNLIMITED, H5S_UNLIMITED};
    htri_t ext_equal; /* Whether two dataspace extents are equal */
    herr_t ret;       /* Generic error return */

    /* Create dataspaces */
    null_space = H5Screate(H5S_NULL);
    CHECK(null_space, FAIL, "H5Screate");

    scalar_space = H5Screate(H5S_SCALAR);
    CHECK(scalar_space, FAIL, "H5Screate");

    d1_space1 = H5Screate_simple(1, d1_dims1, NULL);
    CHECK(d1_space1, FAIL, "H5Screate");
    d1_space2 = H5Screate_simple(1, d1_dims2, NULL);
    CHECK(d1_space2, FAIL, "H5Screate");
    d1_space3 = H5Screate_simple(1, d1_dims1, d1_dims2);
    CHECK(d1_space3, FAIL, "H5Screate");
    d1_space4 = H5Screate_simple(1, d1_dims1, d1_dims3);
    CHECK(d1_space4, FAIL, "H5Screate");

    d2_space1 = H5Screate_simple(2, d2_dims1, NULL);
    CHECK(d2_space1, FAIL, "H5Screate");
    d2_space2 = H5Screate_simple(2, d2_dims2, NULL);
    CHECK(d2_space2, FAIL, "H5Screate");
    d2_space3 = H5Screate_simple(2, d2_dims1, d2_dims2);
    CHECK(d2_space3, FAIL, "H5Screate");
    d2_space4 = H5Screate_simple(2, d2_dims1, d2_dims3);
    CHECK(d2_space4, FAIL, "H5Screate");

    d3_space1 = H5Screate_simple(3, d3_dims1, NULL);
    CHECK(d3_space1, FAIL, "H5Screate");
    d3_space2 = H5Screate_simple(3, d3_dims2, NULL);
    CHECK(d3_space2, FAIL, "H5Screate");
    d3_space3 = H5Screate_simple(3, d3_dims1, d3_dims2);
    CHECK(d3_space3, FAIL, "H5Screate");
    d3_space4 = H5Screate_simple(3, d3_dims1, d3_dims3);
    CHECK(d3_space4, FAIL, "H5Screate");

    /* Compare all dataspace combinations */

    /* Compare null dataspace against all others, including itself */
    ext_equal = H5Sextent_equal(null_space, null_space);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(null_space, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare scalar dataspace against all others, including itself */
    ext_equal = H5Sextent_equal(scalar_space, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, scalar_space);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(scalar_space, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 1-D dataspace w/no max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d1_space1, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d1_space1);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space1, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare larger 1-D dataspace w/no max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d1_space2, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d1_space2);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space2, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 1-D dataspace w/fixed max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d1_space3, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d1_space3);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space3, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 1-D dataspace w/unlimited max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d1_space4, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d1_space4);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d1_space4, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 2-D dataspace w/no max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d2_space1, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d2_space1);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space1, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare larger 2-D dataspace w/no max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d2_space2, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d2_space2);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space2, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 2-D dataspace w/fixed max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d2_space3, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d2_space3);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space3, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 2-D dataspace w/unlimited max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d2_space4, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d2_space4);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d2_space4, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 3-D dataspace w/no max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d3_space1, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d3_space1);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space1, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare larger 2-D dataspace w/no max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d3_space2, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d3_space2);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space2, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 2-D dataspace w/fixed max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d3_space3, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d3_space3);
    VERIFY(ext_equal, true, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space3, d3_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");

    /* Compare small 2-D dataspace w/unlimited max. dims against all others, including itself */
    ext_equal = H5Sextent_equal(d3_space4, null_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, scalar_space);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d1_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d1_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d1_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d1_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d2_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d2_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d2_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d2_space4);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d3_space1);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d3_space2);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d3_space3);
    VERIFY(ext_equal, false, "H5Sextent_equal");
    ext_equal = H5Sextent_equal(d3_space4, d3_space4);
    VERIFY(ext_equal, true, "H5Sextent_equal");

    /* Close dataspaces */
    ret = H5Sclose(null_space);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(scalar_space);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(d1_space1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d1_space2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d1_space3);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d1_space4);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(d2_space1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d2_space2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d2_space3);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d2_space4);
    CHECK(ret, FAIL, "H5Sclose");

    ret = H5Sclose(d3_space1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d3_space2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d3_space3);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(d3_space4);
    CHECK(ret, FAIL, "H5Sclose");
} /* test_h5s_extent_equal() */

/****************************************************************
**
**  test_h5s_extent_copy(): Exercise extent copy code
**
****************************************************************/
static void
test_h5s_extent_copy(void)
{
    hid_t spaces[14] = {
        H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
        H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
        H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID}; /* Array of all dataspaces */
    hid_t   tmp_space   = H5I_INVALID_HID;
    hsize_t d1_dims1[1] = {10}, /* 1-D dimensions */
        d1_dims2[1] = {20}, d1_dims3[1] = {H5S_UNLIMITED};
    hsize_t d2_dims1[2] = {10, 10}, /* 2-D dimensions */
        d2_dims2[2] = {20, 20}, d2_dims3[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t d3_dims1[3] = {10, 10, 10}, /* 3-D dimensions */
        d3_dims2[3] = {20, 20, 20}, d3_dims3[3] = {H5S_UNLIMITED, H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t        npoints[14]; /* Expected number of points in selection for each element in spaces */
    hssize_t       npoints_ret; /* Number of points returned by H5Sget_select_npoints() */
    htri_t         ext_equal;   /* Whether two dataspace extents are equal */
    const unsigned num_spaces = sizeof(spaces) / sizeof(spaces[0]);
    unsigned       i, j;
    herr_t         ret; /* Generic error return */

    /* Create dataspaces */
    spaces[0] = H5Screate(H5S_NULL);
    CHECK(spaces[0], FAIL, "H5Screate");
    npoints[0] = (hsize_t)0;

    spaces[1] = H5Screate(H5S_SCALAR);
    CHECK(spaces[1], FAIL, "H5Screate");
    npoints[1] = (hsize_t)1;

    spaces[2] = H5Screate_simple(1, d1_dims1, NULL);
    CHECK(spaces[2], FAIL, "H5Screate");
    npoints[2] = d1_dims1[0];
    spaces[3]  = H5Screate_simple(1, d1_dims2, NULL);
    CHECK(spaces[3], FAIL, "H5Screate");
    npoints[3] = d1_dims2[0];
    spaces[4]  = H5Screate_simple(1, d1_dims1, d1_dims2);
    CHECK(spaces[4], FAIL, "H5Screate");
    npoints[4] = d1_dims1[0];
    spaces[5]  = H5Screate_simple(1, d1_dims1, d1_dims3);
    CHECK(spaces[5], FAIL, "H5Screate");
    npoints[5] = d1_dims1[0];

    spaces[6] = H5Screate_simple(2, d2_dims1, NULL);
    CHECK(spaces[6], FAIL, "H5Screate");
    npoints[6] = d2_dims1[0] * d2_dims1[1];
    spaces[7]  = H5Screate_simple(2, d2_dims2, NULL);
    CHECK(spaces[7], FAIL, "H5Screate");
    npoints[7] = d2_dims2[0] * d2_dims2[1];
    spaces[8]  = H5Screate_simple(2, d2_dims1, d2_dims2);
    CHECK(spaces[8], FAIL, "H5Screate");
    npoints[8] = d2_dims1[0] * d2_dims1[1];
    spaces[9]  = H5Screate_simple(2, d2_dims1, d2_dims3);
    CHECK(spaces[9], FAIL, "H5Screate");
    npoints[9] = d2_dims1[0] * d2_dims1[1];

    spaces[10] = H5Screate_simple(3, d3_dims1, NULL);
    CHECK(spaces[10], FAIL, "H5Screate");
    npoints[10] = d3_dims1[0] * d3_dims1[1] * d3_dims1[2];
    spaces[11]  = H5Screate_simple(3, d3_dims2, NULL);
    CHECK(spaces[11], FAIL, "H5Screate");
    npoints[11] = d3_dims2[0] * d3_dims2[1] * d3_dims2[2];
    spaces[12]  = H5Screate_simple(3, d3_dims1, d3_dims2);
    CHECK(spaces[12], FAIL, "H5Screate");
    npoints[12] = d3_dims1[0] * d3_dims1[1] * d3_dims1[2];
    spaces[13]  = H5Screate_simple(3, d3_dims1, d3_dims3);
    CHECK(spaces[13], FAIL, "H5Screate");
    npoints[13] = d3_dims1[0] * d3_dims1[1] * d3_dims1[2];

    tmp_space = H5Screate(H5S_NULL);
    CHECK(tmp_space, FAIL, "H5Screate");

    /* Copy between all dataspace combinations.  Note there are a few
     * duplicates. */
    for (i = 0; i < num_spaces; i++)
        for (j = i; j < num_spaces; j++) {
            /* Copy from i to j, unless the inner loop just restarted, in which
             * case i and j are the same, so the second call to H5Sextent_copy()
             * will test copying from i/j to i/j */
            ret = H5Sextent_copy(tmp_space, spaces[j]);
            CHECK(ret, FAIL, "H5Sextent_copy");

            /* Verify that the extents are equal */
            ext_equal = H5Sextent_equal(tmp_space, spaces[j]);
            VERIFY(ext_equal, true, "H5Sextent_equal");

            /* Verify that the correct number of elements is selected */
            npoints_ret = H5Sget_select_npoints(tmp_space);
            VERIFY((hsize_t)npoints_ret, npoints[j], "H5Sget_select_npoints");

            /* Copy from j to i */
            ret = H5Sextent_copy(tmp_space, spaces[i]);
            CHECK(ret, FAIL, "H5Sextent_copy");

            /* Verify that the extents are equal */
            ext_equal = H5Sextent_equal(tmp_space, spaces[i]);
            VERIFY(ext_equal, true, "H5Sextent_equal");

            /* Verify that the correct number of elements is selected */
            npoints_ret = H5Sget_select_npoints(tmp_space);
            VERIFY((hsize_t)npoints_ret, npoints[i], "H5Sget_select_npoints");
        } /* end for */

    /* Close dataspaces */
    for (i = 0; i < num_spaces; i++) {
        ret = H5Sclose(spaces[i]);
        CHECK(ret, FAIL, "H5Sclose");
        spaces[i] = -1;
    } /* end for */

    ret = H5Sclose(tmp_space);
    CHECK(ret, FAIL, "H5Sclose");
} /* test_h5s_extent_copy() */

/****************************************************************
**
**  test_h5s_bug1(): Test Creating dataspace with H5Screate then
*                    setting extent with H5Sextent_copy.
**
****************************************************************/
static void
test_h5s_bug1(void)
{
    hid_t   space1;              /* Dataspace to copy extent to */
    hid_t   space2;              /* Scalar dataspace */
    hsize_t dims[2]  = {10, 10}; /* Dimensions */
    hsize_t start[2] = {0, 0};   /* Hyperslab start */
    htri_t  select_valid;        /* Whether the dataspace selection is valid */
    herr_t  ret;                 /* Generic error return */

    /* Create dataspaces */
    space1 = H5Screate(H5S_SIMPLE);
    CHECK(space1, FAIL, "H5Screate");
    space2 = H5Screate_simple(2, dims, NULL);
    CHECK(space2, FAIL, "H5Screate");

    /* Copy extent to space1 */
    ret = H5Sextent_copy(space1, space2);
    CHECK(ret, FAIL, "H5Sextent_copy");

    /* Select hyperslab in space1 containing entire extent */
    ret = H5Sselect_hyperslab(space1, H5S_SELECT_SET, start, NULL, dims, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Check that space1's selection is valid */
    select_valid = H5Sselect_valid(space1);
    CHECK(select_valid, FAIL, "H5Sselect_valid");
    VERIFY(select_valid, true, "H5Sselect_valid result");

    /* Close dataspaces */
    ret = H5Sclose(space1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(space2);
    CHECK(ret, FAIL, "H5Sclose");
} /* test_h5s_bug1() */

/****************************************************************
**
**  test_h5s_bug2(): Test combining hyperslabs in a way that used
**                   to trip up H5S__hyper_update_diminfo()
**
****************************************************************/
static void
test_h5s_bug2(void)
{
    hid_t    space;             /* Dataspace to copy extent to */
    hsize_t  dims[2]  = {1, 5}; /* Dimensions */
    hsize_t  start[2] = {0, 0}; /* Hyperslab start */
    hsize_t  count[2] = {1, 1}; /* Hyperslab start */
    htri_t   select_valid;      /* Whether the dataspace selection is valid */
    hssize_t elements_selected; /* Number of elements selected */
    herr_t   ret;               /* Generic error return */

    /* Create dataspace */
    space = H5Screate_simple(2, dims, NULL);
    CHECK(space, FAIL, "H5Screate");

    /* Select hyperslab in space containing first element */
    ret = H5Sselect_hyperslab(space, H5S_SELECT_SET, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Add hyperslab in space containing last element */
    start[1] = 4;
    ret      = H5Sselect_hyperslab(space, H5S_SELECT_OR, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Add hyperslab in space containing the first 3 elements */
    start[1] = 0;
    count[1] = 3;
    ret      = H5Sselect_hyperslab(space, H5S_SELECT_OR, start, NULL, count, NULL);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Check that space's selection is valid */
    select_valid = H5Sselect_valid(space);
    CHECK(select_valid, FAIL, "H5Sselect_valid");
    VERIFY(select_valid, true, "H5Sselect_valid result");

    /* Check that 4 elements are selected */
    elements_selected = H5Sget_select_npoints(space);
    CHECK(elements_selected, FAIL, "H5Sselect_valid");
    VERIFY(elements_selected, 4, "H5Sselect_valid result");

    /* Close dataspaces */
    ret = H5Sclose(space);
    CHECK(ret, FAIL, "H5Sclose");
} /* test_h5s_bug2() */

/*-------------------------------------------------------------------------
 * Function:    test_versionbounds
 *
 * Purpose:     Tests version bounds with dataspace.
 *
 * Description:
 *              This function creates a file with lower bounds then later
 *              reopens it with higher bounds to show that the dataspace
 *              version is upgraded appropriately.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
#define VERBFNAME   "tverbounds_dspace.h5"
#define BASIC_DSET  "Basic Dataset"
#define LATEST_DSET "Latest Dataset"
static void
test_versionbounds(void)
{
    hid_t        file       = H5I_INVALID_HID; /* File ID */
    hid_t        space      = H5I_INVALID_HID; /* Dataspace ID */
    hid_t        dset       = H5I_INVALID_HID; /* Dataset ID */
    hid_t        fapl       = H5I_INVALID_HID; /* File access property list ID */
    hid_t        dset_space = H5I_INVALID_HID; /* Retrieved dataset's dataspace ID */
    hsize_t      dim[1];                       /* Dataset dimensions */
    H5F_libver_t low, high;                    /* File format bounds */
    H5S_t       *spacep = NULL;                /* Pointer to internal dataspace */
    bool         vol_is_native;
    herr_t       ret = 0; /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Version Bounds\n"));

    /* Create a file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, FAIL, "H5Pcreate");

    /* Check if native VOL is being used */
    CHECK(h5_using_native_vol(fapl, H5I_INVALID_HID, &vol_is_native), FAIL, "h5_using_native_vol");

    /* Create dataspace */
    dim[0] = 10;
    space  = H5Screate_simple(1, dim, NULL);
    CHECK(space, FAIL, "H5Screate");

    /* Its version should be H5O_SDSPACE_VERSION_1 */
    spacep = (H5S_t *)H5I_object(space);
    CHECK_PTR(spacep, "H5I_object");
    VERIFY(spacep->extent.version, H5O_SDSPACE_VERSION_1, "basic dataspace version bound");

    /* Set high bound to V18 */
    low  = H5F_LIBVER_EARLIEST;
    high = H5F_LIBVER_V18;
    ret  = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create the file */
    file = H5Fcreate(VERBFNAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(file, FAIL, "H5Fcreate");

    /* Create a basic dataset */
    dset = H5Dcreate2(file, BASIC_DSET, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dset > 0) /* dataset created successfully */
    {
        /* Get the internal dataspace pointer */
        dset_space = H5Dget_space(dset);
        CHECK(dset_space, FAIL, "H5Dget_space");

        if (vol_is_native) {
            spacep = (H5S_t *)H5I_object(dset_space);
            CHECK_PTR(spacep, "H5I_object");

            /* Dataspace version should remain as H5O_SDSPACE_VERSION_1 */
            VERIFY(spacep->extent.version, H5O_SDSPACE_VERSION_1, "basic dataspace version bound");
        }

        /* Close dataspace */
        ret = H5Sclose(dset_space);
        CHECK(ret, FAIL, "H5Sclose");
    }

    /* Close basic dataset and the file */
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");

    /* Set low and high bounds to latest to trigger the increment of the
       dataspace version */
    low  = H5F_LIBVER_LATEST;
    high = H5F_LIBVER_LATEST;
    ret  = H5Pset_libver_bounds(fapl, low, high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Reopen the file with new version bounds, LATEST/LATEST */
    file = H5Fopen(VERBFNAME, H5F_ACC_RDWR, fapl);

    /* Create another dataset using the same dspace as the previous dataset */
    dset = H5Dcreate2(file, LATEST_DSET, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dcreate2");

    /* Dataset created successfully.  Verify that dataspace version has been
       upgraded per the low bound */

    /* Get the internal dataspace pointer */
    dset_space = H5Dget_space(dset);
    CHECK(dset_space, FAIL, "H5Dget_space");

    if (vol_is_native) {
        spacep = (H5S_t *)H5I_object(dset_space);
        CHECK_PTR(spacep, "H5I_object");

        /* Verify the dataspace version */
        VERIFY(spacep->extent.version, H5O_sdspace_ver_bounds[low], "upgraded dataspace version");
    }

    /* Close everything */
    ret = H5Sclose(dset_space);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Sclose(space);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Pclose(fapl);
    CHECK(ret, FAIL, "H5Pclose");
    ret = H5Fclose(file);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_versionbounds() */

/****************************************************************
**
**  test_h5s(): Main H5S (dataspace) testing routine.
**
****************************************************************/
void
test_h5s(void)
{
    H5F_libver_t low, high; /* Low and high bounds */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataspaces\n"));

    test_h5s_basic();    /* Test basic H5S code */
    test_h5s_null();     /* Test Null dataspace H5S code */
    test_h5s_zero_dim(); /* Test dataspace with zero dimension size */

    /* Loop through all the combinations of low/high version bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Invalid combinations, just continue */
            if (high == H5F_LIBVER_EARLIEST || high < low)
                continue;

            test_h5s_encode(low, high);                 /* Test encoding and decoding */
            test_h5s_encode_regular_hyper(low, high);   /* Test encoding regular hyperslabs */
            test_h5s_encode_irregular_hyper(low, high); /* Test encoding irregular hyperslabs */
            test_h5s_encode_points(low, high);          /* Test encoding points */

        } /* end high bound */
    }     /* end low bound */

    test_h5s_encode_length(); /* Test version 2 hyperslab encoding length is correct */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    test_h5s_encode1(); /* Test operations with old API routine (H5Sencode1) */
#endif                  /* H5_NO_DEPRECATED_SYMBOLS */

    test_h5s_scalar_write(); /* Test scalar H5S writing code */
    test_h5s_scalar_read();  /* Test scalar H5S reading code */

    test_h5s_compound_scalar_write(); /* Test compound datatype scalar H5S writing code */
    test_h5s_compound_scalar_read();  /* Test compound datatype scalar H5S reading code */

    /* This test was added later to exercise a bug in chunked I/O */
    test_h5s_chunk(); /* Exercise bug fix for chunked I/O */

    test_h5s_extent_equal(); /* Test extent comparison code */
    test_h5s_extent_copy();  /* Test extent copy code */
    test_h5s_bug1();         /* Test bug in offset initialization */
    test_h5s_bug2();         /* Test bug found in H5S__hyper_update_diminfo() */
    test_versionbounds();    /* Test version bounds with dataspace */
} /* test_h5s() */

/*-------------------------------------------------------------------------
 * Function:    cleanup_h5s
 *
 * Purpose:    Cleanup temporary test files
 *
 * Return:    none
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_h5s(void)
{
    H5E_BEGIN_TRY
    {
        H5Fdelete(DATAFILE, H5P_DEFAULT);
        H5Fdelete(NULLFILE, H5P_DEFAULT);
        H5Fdelete(BASICFILE, H5P_DEFAULT);
        H5Fdelete(ZEROFILE, H5P_DEFAULT);
        H5Fdelete(VERBFNAME, H5P_DEFAULT);
    }
    H5E_END_TRY
}
