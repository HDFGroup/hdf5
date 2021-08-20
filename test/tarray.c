/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Test program:     tarray
 *
 * Test the Array Datatype functionality
 *
 *************************************************************/

#include "testhdf5.h"
#include "H5srcdir.h"

#define FILENAME "tarray1.h5"
#define TESTFILE "tarrold.h5"

/* 1-D array datatype */
#define ARRAY1_RANK 1
#define ARRAY1_DIM1 4

/* 3-D array datatype */
#define ARRAY2_RANK 3
#define ARRAY2_DIM1 3
#define ARRAY2_DIM2 4
#define ARRAY2_DIM3 5

/* 2-D array datatype */
#define ARRAY3_RANK 2
#define ARRAY3_DIM1 6
#define ARRAY3_DIM2 3

/* 1-D dataset with fixed dimensions */
#define SPACE1_RANK 1
#define SPACE1_DIM1 4

/* Parameters used with the test_array_bkg() test */
#define FIELDNAME "ArrayofStructures"
#define LENGTH    5
#define ALEN      10
#define RANK      1
#define NMAX      100

/* Struct used with test_array_bkg() test */
typedef struct {
    int    nsubfields;
    char * name[NMAX];
    size_t offset[NMAX];
    hid_t  datatype[NMAX];

} CmpDTSinfo;

/* Forward declarations for custom vlen memory manager functions */
void *test_array_alloc_custom(size_t size, void *info);
void  test_array_free_custom(void *mem, void *info);

/*-------------------------------------------------------------------------
 * Function:    test_array_atomic_1d
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 1-D array of atomic datatypes.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_atomic_1d(void)
{
    int     wdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information to write         */
    int     rdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information read in          */
    hid_t   fid1;                            /* HDF5 File IDs                */
    hid_t   dataset;                         /* Dataset ID                   */
    hid_t   sid1;                            /* Dataspace ID                 */
    hid_t   tid1;                            /* Datatype ID                  */
    hsize_t sdims1[] = {SPACE1_DIM1};
    hsize_t tdims1[] = {ARRAY1_DIM1};
    int     ndims;                /* Array rank for reading       */
    hsize_t rdims1[H5S_MAX_RANK]; /* Array dimensions for reading */
    int     i, j;                 /* counting variables           */
    herr_t  ret;                  /* Generic return value         */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Array of Atomic Datatypes Functionality\n"));

    /* Allocate and initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++)
            wdata[i][j] = i * 10 + j;

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid1 = H5Tarray_create2(H5T_NATIVE_INT, ARRAY1_RANK, tdims1);
    CHECK(tid1, FAIL, "H5Tarray_create2");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the datatype */
    tid1 = H5Dget_type(dataset);
    CHECK(tid1, FAIL, "H5Dget_type");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid1);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid1, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++)
            if (wdata[i][j] != rdata[i][j]) {
                TestErrPrintf("Array data information doesn't match!, wdata[%d][%d]=%d, rdata[%d][%d]=%d\n",
                              (int)i, (int)j, (int)wdata[i][j], (int)i, (int)j, (int)rdata[i][j]);
                continue;
            } /* end if */

    /* Close Datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_array_atomic_1d() */

/*-------------------------------------------------------------------------
 * Function:    test_array_funcs
 *
 * Purpose:     Test some type functions that are and aren't supposed to
 *              work with array type.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_funcs(void)
{
    hid_t      type; /* Datatype ID                  */
    hsize_t    tdims1[] = {ARRAY1_DIM1};
    size_t     size;
    H5T_pad_t  inpad;
    H5T_norm_t norm;
    H5T_cset_t cset;
    H5T_str_t  strpad;
    herr_t     ret; /* Generic return value         */

    /* Create a datatype to refer to */
    type = H5Tarray_create2(H5T_IEEE_F32BE, ARRAY1_RANK, tdims1);
    CHECK(type, FAIL, "H5Tarray_create2");

    size = H5Tget_precision(type);
    CHECK(size, 0, "H5Tget_precision");

    size = H5Tget_size(type);
    CHECK(size, 0, "H5Tget_size");

    size = H5Tget_ebias(type);
    CHECK(size, 0, "H5Tget_ebias");

    ret = H5Tset_pad(type, H5T_PAD_ZERO, H5T_PAD_ONE);
    CHECK(ret, FAIL, "H5Tset_pad");

    inpad = H5Tget_inpad(type);
    CHECK(inpad, FAIL, "H5Tget_inpad");

    norm = H5Tget_norm(type);
    CHECK(norm, FAIL, "H5Tget_norm");

    ret = H5Tset_offset(type, (size_t)16);
    CHECK(ret, FAIL, "H5Tset_offset");

    H5E_BEGIN_TRY
    {
        cset = H5Tget_cset(type);
    }
    H5E_END_TRY;
    VERIFY(cset, FAIL, "H5Tget_cset");

    H5E_BEGIN_TRY
    {
        strpad = H5Tget_strpad(type);
    }
    H5E_END_TRY;
    VERIFY(strpad, FAIL, "H5Tget_strpad");

    /* Close datatype */
    ret = H5Tclose(type);
    CHECK(ret, FAIL, "H5Tclose");
} /* end test_array_funcs() */

/*-------------------------------------------------------------------------
 * Function:    test_array_atomic_3d
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 3-D array of atomic datatypes.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_atomic_3d(void)
{
    int     wdata[SPACE1_DIM1][ARRAY2_DIM1][ARRAY2_DIM2][ARRAY2_DIM3]; /* Information to write */
    int     rdata[SPACE1_DIM1][ARRAY2_DIM1][ARRAY2_DIM2][ARRAY2_DIM3]; /* Information read in */
    hid_t   fid;                                                       /* HDF5 File IDs                */
    hid_t   dataset;                                                   /* Dataset ID                   */
    hid_t   sid;                                                       /* Dataspace ID                 */
    hid_t   tid;                                                       /* Datatype ID                  */
    hsize_t sdims1[] = {SPACE1_DIM1};
    hsize_t tdims2[] = {ARRAY2_DIM1, ARRAY2_DIM2, ARRAY2_DIM3};
    int     ndims;                /* Array rank for reading       */
    hsize_t rdims2[H5S_MAX_RANK]; /* Array dimensions for reading */
    int     i, j, k, l;           /* counting variables           */
    herr_t  ret;                  /* Generic return value         */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 3-D Array of Atomic Datatypes Functionality\n"));

    /* Allocate and initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY2_DIM1; j++)
            for (k = 0; k < ARRAY2_DIM2; k++)
                for (l = 0; l < ARRAY2_DIM3; l++)
                    wdata[i][j][k][l] = i * 1000 + j * 100 + k * 10 + l;

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a datatype to refer to */
    tid = H5Tarray_create2(H5T_NATIVE_INT, ARRAY2_RANK, tdims2);
    CHECK(tid, FAIL, "H5Tarray_create2");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, "Dataset1", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the datatype */
    tid = H5Dget_type(dataset);
    CHECK(tid, FAIL, "H5Dget_type");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid);
    VERIFY(ndims, ARRAY2_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid, rdims2);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims2[i] != tdims2[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims2[%d]=%d, tdims2[%d]=%d\n",
                          (int)i, (int)rdims2[i], (int)i, (int)tdims2[i]);
            continue;
        } /* end if */

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY2_DIM1; j++)
            for (k = 0; k < ARRAY2_DIM2; k++)
                for (l = 0; l < ARRAY2_DIM3; l++)
                    if (wdata[i][j][k][l] != rdata[i][j][k][l]) {
                        TestErrPrintf("Array data information doesn't match!, wdata[%d][%d][%d][%d]=%d, "
                                      "rdata[%d][%d][%d][%d]=%d\n",
                                      (int)i, (int)j, (int)k, (int)l, (int)wdata[i][j][k][l], (int)i, (int)j,
                                      (int)k, (int)l, (int)rdata[i][j][k][l]);
                        continue;
                    } /* end if */

    /* Close Datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_array_atomic_3d() */

/*-------------------------------------------------------------------------
 * Function:    test_array_array_atomic
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 1-D array 2-D arrays of atomic datatypes.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_array_atomic(void)
{
    int     wdata[SPACE1_DIM1][ARRAY1_DIM1][ARRAY3_DIM1][ARRAY3_DIM2]; /* Information to write */
    int     rdata[SPACE1_DIM1][ARRAY1_DIM1][ARRAY3_DIM1][ARRAY3_DIM2]; /* Information read in */
    hid_t   fid;                                                       /* HDF5 File IDs                */
    hid_t   dataset;                                                   /* Dataset ID                   */
    hid_t   sid;                                                       /* Dataspace ID                 */
    hid_t   tid1;                                                      /* 1-D array Datatype ID        */
    hid_t   tid2;                                                      /* 2-D array Datatype ID        */
    hsize_t sdims1[] = {SPACE1_DIM1};
    hsize_t tdims1[] = {ARRAY1_DIM1};
    hsize_t tdims2[] = {ARRAY3_DIM1, ARRAY3_DIM2};
    int     ndims1;               /* Array rank for reading       */
    int     ndims2;               /* Array rank for reading       */
    hsize_t rdims1[H5S_MAX_RANK]; /* Array dimensions for reading */
    hsize_t rdims2[H5S_MAX_RANK]; /* Array dimensions for reading */
    int     i, j, k, l;           /* counting variables           */
    herr_t  ret;                  /* Generic return value         */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Array 2-D Arrays of Atomic Datatypes Functionality\n"));

    /* Allocate and initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++)
            for (k = 0; k < ARRAY3_DIM1; k++)
                for (l = 0; l < ARRAY3_DIM2; l++)
                    wdata[i][j][k][l] = i * 1000 + j * 100 + k * 10 + l;

    /* Create file */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid, FAIL, "H5Screate_simple");

    /* Create a 2-D datatype to refer to */
    tid2 = H5Tarray_create2(H5T_NATIVE_INT, ARRAY3_RANK, tdims2);
    CHECK(tid2, FAIL, "H5Tarray_create2");

    /* Create a 1-D datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);
    CHECK(tid1, FAIL, "H5Tarray_create2");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, "Dataset1", tid1, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatypes */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the 1-D datatype */
    tid1 = H5Dget_type(dataset);
    CHECK(tid1, FAIL, "H5Dget_type");

    /* Check the 1-D array rank */
    ndims1 = H5Tget_array_ndims(tid1);
    VERIFY(ndims1, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the 1-D array dimensions */
    ret = H5Tget_array_dims2(tid1, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims1; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Get the 2-D datatype */
    tid2 = H5Tget_super(tid1);
    CHECK(tid2, FAIL, "H5Tget_super");

    /* Check the 2-D array rank */
    ndims2 = H5Tget_array_ndims(tid2);
    VERIFY(ndims2, ARRAY3_RANK, "H5Tget_array_ndims");

    /* Get the 2-D array dimensions */
    ret = H5Tget_array_dims2(tid2, rdims2);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims2; i++)
        if (rdims2[i] != tdims2[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims2[%d]=%d, tdims2[%d]=%d\n",
                          (int)i, (int)rdims2[i], (int)i, (int)tdims2[i]);
            continue;
        } /* end if */

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++)
            for (k = 0; k < ARRAY3_DIM1; k++)
                for (l = 0; l < ARRAY3_DIM2; l++)
                    if (wdata[i][j][k][l] != rdata[i][j][k][l]) {
                        TestErrPrintf("Array data information doesn't match!, wdata[%d][%d][%d][%d]=%d, "
                                      "rdata[%d][%d][%d][%d]=%d\n",
                                      (int)i, (int)j, (int)k, (int)l, (int)wdata[i][j][k][l], (int)i, (int)j,
                                      (int)k, (int)l, (int)rdata[i][j][k][l]);
                        continue;
                    } /* end if */

    /* Close Datatypes */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_array_array_atomic() */

/*-------------------------------------------------------------------------
 * Function:    test_array_compound_atomic
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 1-D array of compound datatypes (with no array fields).
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_compound_atomic(void)
{
    typedef struct { /* Typedef for compound datatype */
        int   i;
        float f;
    } s1_t;

    s1_t    wdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information to write         */
    s1_t    rdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information read in          */
    hid_t   fid1;                            /* HDF5 File IDs                */
    hid_t   dataset;                         /* Dataset ID                   */
    hid_t   sid1;                            /* Dataspace ID                 */
    hid_t   tid1;                            /* Array Datatype ID            */
    hid_t   tid2;                            /* Compound Datatype ID         */
    hsize_t sdims1[] = {SPACE1_DIM1};
    hsize_t tdims1[] = {ARRAY1_DIM1};
    int     ndims;                /* Array rank for reading       */
    hsize_t rdims1[H5S_MAX_RANK]; /* Array dimensions for reading */
    int     nmemb;                /* Number of compound members   */
    char *  mname;                /* Name of compound field       */
    size_t  off;                  /* Offset of compound field     */
    hid_t   mtid;                 /* Datatype ID for field        */
    int     i, j;                 /* counting variables           */
    herr_t  ret;                  /* Generic return value         */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Array of Compound Atomic Datatypes Functionality\n"));

    /* Initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++) {
            wdata[i][j].i = i * 10 + j;
            wdata[i][j].f = (float)i * 2.5F + (float)j;
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid2, FAIL, "H5Tcreate");

    /* Insert integer field */
    ret = H5Tinsert(tid2, "i", HOFFSET(s1_t, i), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Insert float field */
    ret = H5Tinsert(tid2, "f", HOFFSET(s1_t, f), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);
    CHECK(tid1, FAIL, "H5Tarray_create2");

    /* Close compound datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the datatype */
    tid1 = H5Dget_type(dataset);
    CHECK(tid1, FAIL, "H5Dget_type");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid1);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid1, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Get the compound datatype */
    tid2 = H5Tget_super(tid1);
    CHECK(tid2, FAIL, "H5Tget_super");

    /* Check the number of members */
    nmemb = H5Tget_nmembers(tid2);
    VERIFY(nmemb, 2, "H5Tget_nmembers");

    /* Check the 1st field's name */
    mname = H5Tget_member_name(tid2, 0);
    CHECK_PTR(mname, "H5Tget_member_name");
    if (HDstrcmp(mname, "i") != 0)
        TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
    H5free_memory(mname);

    /* Check the 1st field's offset */
    off = H5Tget_member_offset(tid2, 0);
    VERIFY(off, HOFFSET(s1_t, i), "H5Tget_member_offset");

    /* Check the 1st field's datatype */
    mtid = H5Tget_member_type(tid2, 0);
    CHECK(mtid, FAIL, "H5Tget_member_type");
    if ((ret = H5Tequal(mtid, H5T_NATIVE_INT)) <= 0)
        TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
    ret = H5Tclose(mtid);
    CHECK(mtid, FAIL, "H5Tclose");

    /* Check the 2nd field's name */
    mname = H5Tget_member_name(tid2, 1);
    CHECK_PTR(mname, "H5Tget_member_name");
    if (HDstrcmp(mname, "f") != 0)
        TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
    H5free_memory(mname);

    /* Check the 2nd field's offset */
    off = H5Tget_member_offset(tid2, 1);
    VERIFY(off, HOFFSET(s1_t, f), "H5Tget_member_offset");

    /* Check the 2nd field's datatype */
    mtid = H5Tget_member_type(tid2, 1);
    CHECK(mtid, FAIL, "H5Tget_member_type");
    if ((ret = H5Tequal(mtid, H5T_NATIVE_FLOAT)) <= 0)
        TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
    ret = H5Tclose(mtid);
    CHECK(mtid, FAIL, "H5Tclose");

    /* Close Compound Datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++) {
            if (wdata[i][j].i != rdata[i][j].i) {
                TestErrPrintf(
                    "Array data information doesn't match!, wdata[%d][%d].i=%d, rdata[%d][%d].i=%d\n", (int)i,
                    (int)j, (int)wdata[i][j].i, (int)i, (int)j, (int)rdata[i][j].i);
                continue;
            } /* end if */
            if (!H5_FLT_ABS_EQUAL(wdata[i][j].f, rdata[i][j].f)) {
                TestErrPrintf(
                    "Array data information doesn't match!, wdata[%d][%d].f=%f, rdata[%d][%d].f=%f\n", (int)i,
                    (int)j, (double)wdata[i][j].f, (int)i, (int)j, (double)rdata[i][j].f);
                continue;
            } /* end if */
        }     /* end for */

    /* Close Datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");
} /* end test_array_compound_atomic() */

/*-------------------------------------------------------------------------
 * Function:    test_array_compound_array
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 1-D array of compound datatypes (with array fields).
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_compound_array(void)
{
    typedef struct { /* Typedef for compound datatype */
        int   i;
        float f[ARRAY1_DIM1];
    } s1_t;

    s1_t        wdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information to write         */
    s1_t        rdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information read in          */
    hid_t       fid1;                            /* HDF5 File IDs                */
    hid_t       dataset;                         /* Dataset ID                   */
    hid_t       sid1;                            /* Dataspace ID                 */
    hid_t       tid1;                            /* Array Datatype ID            */
    hid_t       tid2;                            /* Compound Datatype ID         */
    hid_t       tid3;                            /* Nested Array Datatype ID     */
    hsize_t     sdims1[] = {SPACE1_DIM1};
    hsize_t     tdims1[] = {ARRAY1_DIM1};
    int         ndims;                /* Array rank for reading       */
    hsize_t     rdims1[H5S_MAX_RANK]; /* Array dimensions for reading */
    int         nmemb;                /* Number of compound members   */
    char *      mname;                /* Name of compound field       */
    size_t      off;                  /* Offset of compound field     */
    hid_t       mtid;                 /* Datatype ID for field        */
    H5T_class_t mclass;               /* Datatype class for field     */
    int         i, j, k;              /* counting variables           */
    herr_t      ret;                  /* Generic return value         */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Array of Compound Array Datatypes Functionality\n"));

    /* Initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++) {
            wdata[i][j].i = i * 10 + j;
            for (k = 0; k < ARRAY1_DIM1; k++)
                wdata[i][j].f[k] = (float)i * 10.0F + (float)j * 2.5F + (float)k;
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a compound datatype to refer to */
    tid2 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid2, FAIL, "H5Tcreate");

    /* Insert integer field */
    ret = H5Tinsert(tid2, "i", HOFFSET(s1_t, i), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create an array of floats datatype */
    tid3 = H5Tarray_create2(H5T_NATIVE_FLOAT, ARRAY1_RANK, tdims1);
    CHECK(tid3, FAIL, "H5Tarray_create2");

    /* Insert float array field */
    ret = H5Tinsert(tid2, "f", HOFFSET(s1_t, f), tid3);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Close array of floats field datatype */
    ret = H5Tclose(tid3);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);
    CHECK(tid1, FAIL, "H5Tarray_create2");

    /* Close compound datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the datatype */
    tid1 = H5Dget_type(dataset);
    CHECK(tid1, FAIL, "H5Dget_type");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid1);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid1, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Get the compound datatype */
    tid2 = H5Tget_super(tid1);
    CHECK(tid2, FAIL, "H5Tget_super");

    /* Check the number of members */
    nmemb = H5Tget_nmembers(tid2);
    VERIFY(nmemb, 2, "H5Tget_nmembers");

    /* Check the 1st field's name */
    mname = H5Tget_member_name(tid2, 0);
    CHECK_PTR(mname, "H5Tget_member_name");
    if (HDstrcmp(mname, "i") != 0)
        TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
    H5free_memory(mname);

    /* Check the 1st field's offset */
    off = H5Tget_member_offset(tid2, 0);
    VERIFY(off, HOFFSET(s1_t, i), "H5Tget_member_offset");

    /* Check the 1st field's datatype */
    mtid = H5Tget_member_type(tid2, 0);
    CHECK(mtid, FAIL, "H5Tget_member_type");
    if ((ret = H5Tequal(mtid, H5T_NATIVE_INT)) <= 0)
        TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
    ret = H5Tclose(mtid);
    CHECK(mtid, FAIL, "H5Tclose");

    /* Check the 2nd field's name */
    mname = H5Tget_member_name(tid2, 1);
    CHECK_PTR(mname, "H5Tget_member_name");
    if (HDstrcmp(mname, "f") != 0)
        TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
    H5free_memory(mname);

    /* Check the 2nd field's offset */
    off = H5Tget_member_offset(tid2, 1);
    VERIFY(off, HOFFSET(s1_t, f), "H5Tget_member_offset");

    /* Check the 2nd field's datatype */
    mtid = H5Tget_member_type(tid2, 1);
    CHECK(mtid, FAIL, "H5Tget_member_type");

    /* Get the 2nd field's class */
    mclass = H5Tget_class(mtid);
    VERIFY(mclass, H5T_ARRAY, "H5Tget_class");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(mtid);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(mtid, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf(
                "Nested array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Check the nested array's datatype */
    tid3 = H5Tget_super(mtid);
    CHECK(tid3, FAIL, "H5Tget_super");

    if ((ret = H5Tequal(tid3, H5T_NATIVE_FLOAT)) <= 0)
        TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);

    /* Close the array's base type datatype */
    ret = H5Tclose(tid3);
    CHECK(mtid, FAIL, "H5Tclose");

    /* Close the member datatype */
    ret = H5Tclose(mtid);
    CHECK(mtid, FAIL, "H5Tclose");

    /* Close Compound Datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++) {
        for (j = 0; j < ARRAY1_DIM1; j++) {
            if (wdata[i][j].i != rdata[i][j].i) {
                TestErrPrintf(
                    "Array data information doesn't match!, wdata[%d][%d].i=%d, rdata[%d][%d].i=%d\n", (int)i,
                    (int)j, (int)wdata[i][j].i, (int)i, (int)j, (int)rdata[i][j].i);
                continue;
            } /* end if */
            for (k = 0; k < ARRAY1_DIM1; k++)
                if (!H5_FLT_ABS_EQUAL(wdata[i][j].f[k], rdata[i][j].f[k])) {
                    TestErrPrintf("Array data information doesn't match!, wdata[%d][%d].f[%d]=%f, "
                                  "rdata[%d][%d].f[%d]=%f\n",
                                  (int)i, (int)j, (int)k, (double)wdata[i][j].f[k], (int)i, (int)j, (int)k,
                                  (double)rdata[i][j].f[k]);
                    continue;
                } /* end if */
        }         /* end for */
    }             /* end for */

    /* Close Datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_array_compound_array() */

/****************************************************************
**
**  test_array_alloc_custom(): Test VL datatype custom memory
**      allocation routines.  This routine just uses malloc to
**      allocate the memory and increments the amount of memory
**      allocated.
**
****************************************************************/

/*-------------------------------------------------------------------------
 * Function:    test_array_alloc_custom
 *
 * Purpose:     Memory allocator for testing VL datatype custom memory
 *              allocation routines.
 *
 *              This routine just uses malloc to allocate the memory and
 *              increments the amount of memory allocated.
 *
 * Return:
 *
 *  Success:    A memory buffer
 *  Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
test_array_alloc_custom(size_t size, void *info)
{
    void *  ret_value = NULL;           /* Pointer to return            */
    size_t *mem_used  = (size_t *)info; /* Pointer to the memory used   */
    size_t  extra;                      /* Extra space needed           */

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */
    extra = MAX(sizeof(void *), sizeof(size_t));

    if ((ret_value = HDmalloc(extra + size)) != NULL) {
        *(size_t *)ret_value = size;
        *mem_used += size;
    } /* end if */

    ret_value = ((unsigned char *)ret_value) + extra;
    return ret_value;
} /* end test_array_alloc_custom() */

/*-------------------------------------------------------------------------
 * Function:    test_array_free_custom
 *
 * Purpose:     Memory free function for testing VL datatype custom memory
 *              allocation routines.
 *
 *              This routine just uses free to free the memory and
 *              decrements the amount of memory allocated.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
test_array_free_custom(void *_mem, void *info)
{
    unsigned char *mem      = NULL;           /* Pointer to mem to be freed   */
    size_t *       mem_used = (size_t *)info; /* Pointer to the memory used   */
    size_t         extra;                     /* Extra space needed           */

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */
    extra = MAX(sizeof(void *), sizeof(size_t));

    if (_mem != NULL) {
        mem = ((unsigned char *)_mem) - extra;
        *mem_used -= *(size_t *)((void *)mem);
        HDfree(mem);
    } /* end if */

} /* end test_array_free_custom() */

/*-------------------------------------------------------------------------
 * Function:    test_array_vlen_atomic
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 1-D array of atomic VL datatypes.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_vlen_atomic(void)
{
    hvl_t       wdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information to write        */
    hvl_t       rdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information read in         */
    hid_t       fid1;                            /* HDF5 File IDs                    */
    hid_t       dataset;                         /* Dataset ID                       */
    hid_t       sid1;                            /* Dataspace ID                     */
    hid_t       tid1;                            /* Array Datatype ID                */
    hid_t       tid2;                            /* VL Datatype ID                   */
    hid_t       tid3;                            /* Atomic Datatype ID               */
    hsize_t     sdims1[] = {SPACE1_DIM1};
    hsize_t     tdims1[] = {ARRAY1_DIM1};
    int         ndims;                /* Array rank for reading           */
    hsize_t     rdims1[H5S_MAX_RANK]; /* Array dimensions for reading     */
    H5T_class_t mclass;               /* Datatype class for VL            */
    hid_t       xfer_pid;             /* Dataset transfer property list ID    */
    hsize_t     size;                 /* Number of bytes which will be used   */
    size_t      mem_used = 0;         /* Memory used during allocation    */
    int         i, j, k;              /* counting variables               */
    herr_t      ret;                  /* Generic return value             */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Array of Atomic Variable-Length Datatypes Functionality\n"));

    /* Initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++) {
            wdata[i][j].p   = HDmalloc((size_t)(i + j + 1) * sizeof(unsigned int));
            wdata[i][j].len = (size_t)(i + j + 1);
            for (k = 0; k < (i + j + 1); k++)
                ((unsigned int *)wdata[i][j].p)[k] = (unsigned int)(i * 100 + j * 10 + k);
        } /* end for */

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create a compound datatype to refer to */
    tid2 = H5Tvlen_create(H5T_NATIVE_UINT);
    CHECK(tid2, FAIL, "H5Tcreate");

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);
    CHECK(tid1, FAIL, "H5Tarray_create2");

    /* Close VL datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the dataspace */
    sid1 = H5Dget_space(dataset);
    CHECK(sid1, FAIL, "H5Dget_space");

    /* Get the datatype */
    tid1 = H5Dget_type(dataset);
    CHECK(tid1, FAIL, "H5Dget_type");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid1);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid1, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Get the VL datatype */
    tid2 = H5Tget_super(tid1);
    CHECK(tid2, FAIL, "H5Tget_super");

    /* Get the 2nd field's class */
    mclass = H5Tget_class(tid2);
    VERIFY(mclass, H5T_VLEN, "H5Tget_class");

    /* Check the VL datatype's base type */
    tid3 = H5Tget_super(tid2);
    CHECK(tid3, FAIL, "H5Tget_super");

    if ((ret = H5Tequal(tid3, H5T_NATIVE_UINT)) <= 0)
        TestErrPrintf("VL base datatype is incorrect!, ret=%d\n", (int)ret);

    /* Close the array's base type datatype */
    ret = H5Tclose(tid3);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close VL Datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Change to the custom memory allocation routines for reading VL data */
    xfer_pid = H5Pcreate(H5P_DATASET_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret = H5Pset_vlen_mem_manager(xfer_pid, test_array_alloc_custom, &mem_used, test_array_free_custom,
                                  &mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret = H5Dvlen_get_buf_size(dataset, tid1, sid1, &size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* # elements allocated = (1 + 2 + 3 + 4) + (2 + 3 + 4 + 5) +
     *  (3 + 4 + 5 + 6) + (4 + 5 + 6 + 7) = 64 elements
     */
    VERIFY(size, 64 * sizeof(unsigned int), "H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, xfer_pid, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    /* # elements allocated = (1 + 2 + 3 + 4) + (2 + 3 + 4 + 5) +
     *  (3 + 4 + 5 + 6) + (4 + 5 + 6 + 7) = 64 elements
     */
    VERIFY(mem_used, 64 * sizeof(unsigned int), "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++) {
        for (j = 0; j < ARRAY1_DIM1; j++) {
            if (wdata[i][j].len != rdata[i][j].len) {
                TestErrPrintf("VL data length don't match!, wdata[%d][%d].len=%d, rdata[%d][%d].len=%d\n",
                              (int)i, (int)j, (int)wdata[i][j].len, (int)i, (int)j, (int)rdata[i][j].len);
                continue;
            } /* end if */
            for (k = 0; k < (int)rdata[i][j].len; k++) {
                if (((unsigned int *)wdata[i][j].p)[k] != ((unsigned int *)rdata[i][j].p)[k]) {
                    TestErrPrintf(
                        "VL data values don't match!, wdata[%d][%d].p[%d]=%d, rdata[%d][%d].p[%d]=%d\n",
                        (int)i, (int)j, (int)k, (int)((unsigned int *)wdata[i][j].p)[k], (int)i, (int)j,
                        (int)k, (int)((unsigned int *)rdata[i][j].p)[k]);
                    continue;
                } /* end if */
            }     /* end for */
        }         /* end for */
    }             /* end for */

    /* Reclaim the read VL data */
    ret = H5Treclaim(tid1, sid1, xfer_pid, rdata);
    CHECK(ret, FAIL, "H5Treclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used, 0, "H5Treclaim");

    /* Reclaim the write VL data */
    ret = H5Treclaim(tid1, sid1, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Treclaim");

    /* Close dataset transfer property list */
    ret = H5Pclose(xfer_pid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close Datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_array_vlen_atomic() */

/*-------------------------------------------------------------------------
 * Function:    test_array_vlen_array
 *
 * Purpose:     Test basic array datatype code.
 *              Tests 1-D array of 1-D array VL datatypes.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_vlen_array(void)
{
    hvl_t       wdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information to write        */
    hvl_t       rdata[SPACE1_DIM1][ARRAY1_DIM1]; /* Information read in         */
    hid_t       fid1;                            /* HDF5 File IDs                    */
    hid_t       dataset;                         /* Dataset ID                       */
    hid_t       sid1;                            /* Dataspace ID                     */
    hid_t       tid1;                            /* Array Datatype ID                */
    hid_t       tid2;                            /* VL Datatype ID                   */
    hid_t       tid3;                            /* Nested Array Datatype ID         */
    hid_t       tid4;                            /* Atomic Datatype ID               */
    hsize_t     sdims1[] = {SPACE1_DIM1};
    hsize_t     tdims1[] = {ARRAY1_DIM1};
    int         ndims;                /* Array rank for reading           */
    hsize_t     rdims1[H5S_MAX_RANK]; /* Array dimensions for reading     */
    H5T_class_t mclass;               /* Datatype class for VL            */
    hid_t       xfer_pid;             /* Dataset transfer property list ID    */
    hsize_t     size;                 /* Number of bytes which will be used   */
    size_t      mem_used = 0;         /* Memory used during allocation    */
    int         i, j, k, l;           /* Index variables                  */
    herr_t      ret;                  /* Generic return value             */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Array of 1-D Array Variable-Length Datatypes Functionality\n"));

    /* Initialize array data to write */
    for (i = 0; i < SPACE1_DIM1; i++)
        for (j = 0; j < ARRAY1_DIM1; j++) {
            wdata[i][j].p   = HDmalloc((size_t)(i + j + 1) * sizeof(unsigned int) * (size_t)ARRAY1_DIM1);
            wdata[i][j].len = (size_t)(i + j + 1);
            for (k = 0; k < (i + j + 1); k++)
                for (l = 0; l < ARRAY1_DIM1; l++)
                    ((unsigned int *)wdata[i][j].p)[k * ARRAY1_DIM1 + l] =
                        (unsigned int)(i * 1000 + j * 100 + k * 10 + l);
        }

    /* Create file */
    fid1 = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, sdims1, NULL);
    CHECK(sid1, FAIL, "H5Screate_simple");

    /* Create the nested array datatype to refer to */
    tid3 = H5Tarray_create2(H5T_NATIVE_UINT, ARRAY1_RANK, tdims1);
    CHECK(tid3, FAIL, "H5Tarray_create2");

    /* Create a VL datatype of 1-D arrays to refer to */
    tid2 = H5Tvlen_create(tid3);
    CHECK(tid2, FAIL, "H5Tcreate");

    /* Close nested array datatype */
    ret = H5Tclose(tid3);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create an array datatype to refer to */
    tid1 = H5Tarray_create2(tid2, ARRAY1_RANK, tdims1);
    CHECK(tid1, FAIL, "H5Tarray_create2");

    /* Close VL datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset1", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write dataset to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open file */
    fid1 = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK(fid1, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    /* Get the dataspace */
    sid1 = H5Dget_space(dataset);
    CHECK(sid1, FAIL, "H5Dget_space");

    /* Get the datatype */
    tid1 = H5Dget_type(dataset);
    CHECK(tid1, FAIL, "H5Dget_type");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid1);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid1, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Get the VL datatype */
    tid2 = H5Tget_super(tid1);
    CHECK(tid2, FAIL, "H5Tget_super");

    /* Get the VL datatype's class */
    mclass = H5Tget_class(tid2);
    VERIFY(mclass, H5T_VLEN, "H5Tget_class");

    /* Check the VL datatype's base type */
    tid3 = H5Tget_super(tid2);
    CHECK(tid3, FAIL, "H5Tget_super");

    /* Get the nested array datatype's class */
    mclass = H5Tget_class(tid3);
    VERIFY(mclass, H5T_ARRAY, "H5Tget_class");

    /* Check the array rank */
    ndims = H5Tget_array_ndims(tid3);
    VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

    /* Get the array dimensions */
    ret = H5Tget_array_dims2(tid3, rdims1);
    CHECK(ret, FAIL, "H5Tget_array_dims2");

    /* Check the array dimensions */
    for (i = 0; i < ndims; i++)
        if (rdims1[i] != tdims1[i]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                          rdims1[i], i, tdims1[i]);
            continue;
        } /* end if */

    /* Check the array's base type */
    tid4 = H5Tget_super(tid3);
    CHECK(tid4, FAIL, "H5Tget_super");

    if ((ret = H5Tequal(tid4, H5T_NATIVE_UINT)) <= 0)
        TestErrPrintf("VL base datatype is incorrect!, ret=%d\n", (int)ret);

    /* Close the array's base type datatype */
    ret = H5Tclose(tid4);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close the nested array datatype */
    ret = H5Tclose(tid3);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close VL Datatype */
    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Change to the custom memory allocation routines for reading VL data */
    xfer_pid = H5Pcreate(H5P_DATASET_XFER);
    CHECK(xfer_pid, FAIL, "H5Pcreate");

    ret = H5Pset_vlen_mem_manager(xfer_pid, test_array_alloc_custom, &mem_used, test_array_free_custom,
                                  &mem_used);
    CHECK(ret, FAIL, "H5Pset_vlen_mem_manager");

    /* Make certain the correct amount of memory will be used */
    ret = H5Dvlen_get_buf_size(dataset, tid1, sid1, &size);
    CHECK(ret, FAIL, "H5Dvlen_get_buf_size");

    /* # elements allocated = (1 + 2 + 3 + 4) + (2 + 3 + 4 + 5) +
     *  (3 + 4 + 5 + 6) + (4 + 5 + 6 + 7) = 64*ARRAY1_DIM1 elements
     */
    VERIFY(size, 64 * (sizeof(unsigned int) * ARRAY1_DIM1), "H5Dvlen_get_buf_size");

    /* Read dataset from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, xfer_pid, rdata);
    CHECK(ret, FAIL, "H5Dread");

    /* Make certain the correct amount of memory has been used */
    /* # elements allocated = (1 + 2 + 3 + 4) + (2 + 3 + 4 + 5) +
     *  (3 + 4 + 5 + 6) + (4 + 5 + 6 + 7) = 64*ARRAY1_DIM1 elements
     */
    VERIFY(mem_used, 64 * (sizeof(unsigned int) * ARRAY1_DIM1), "H5Dread");

    /* Compare data read in */
    for (i = 0; i < SPACE1_DIM1; i++) {
        for (j = 0; j < ARRAY1_DIM1; j++) {
            if (wdata[i][j].len != rdata[i][j].len) {
                TestErrPrintf("VL data length don't match!, wdata[%d][%d].len=%d, rdata[%d][%d].len=%d\n",
                              (int)i, (int)j, (int)wdata[i][j].len, (int)i, (int)j, (int)rdata[i][j].len);
                continue;
            } /* end if */
            for (k = 0; k < (int)rdata[i][j].len; k++) {
                for (l = 0; l < ARRAY1_DIM1; l++) {
                    if (((unsigned int *)wdata[i][j].p)[k * ARRAY1_DIM1 + l] !=
                        ((unsigned int *)rdata[i][j].p)[k * ARRAY1_DIM1 + l]) {
                        TestErrPrintf("VL data values don't match!, wdata[%d][%d].p[%d][%d]=%d, "
                                      "rdata[%d][%d].p[%d][%d]=%d\n",
                                      (int)i, (int)j, (int)k, (int)l,
                                      (int)((unsigned int *)wdata[i][j].p)[k * ARRAY1_DIM1 + l], (int)i,
                                      (int)j, (int)k, (int)l,
                                      (int)((unsigned int *)rdata[i][j].p)[k * ARRAY1_DIM1 + l]);
                        continue;
                    } /* end if */
                }     /* end for */
            }         /* end for */
        }             /* end for */
    }                 /* end for */

    /* Reclaim the read VL data */
    ret = H5Treclaim(tid1, sid1, xfer_pid, rdata);
    CHECK(ret, FAIL, "H5Treclaim");

    /* Make certain the VL memory has been freed */
    VERIFY(mem_used, 0, "H5Treclaim");

    /* Reclaim the write VL data */
    ret = H5Treclaim(tid1, sid1, H5P_DEFAULT, wdata);
    CHECK(ret, FAIL, "H5Treclaim");

    /* Close dataset transfer property list */
    ret = H5Pclose(xfer_pid);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close Datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

} /* end test_array_vlen_array() */

/*-------------------------------------------------------------------------
 * Function:    test_array_bkg
 *
 * Purpose:     Test basic array datatype code.
 *              Tests reading compound datatype with array fields and
 *              writing partial fields.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_array_bkg(void)
{
    herr_t status = -1;

    hid_t fid, array_dt;
    hid_t space;
    hid_t type;
    hid_t dataset;

    hsize_t dim[]  = {LENGTH};
    hsize_t dima[] = {ALEN};

    int      i, j;
    unsigned ndims[3] = {1, 1, 1};

    typedef struct {
        int    a[ALEN];
        float  b[ALEN];
        double c[ALEN];
    } CmpField;

    CmpField    cf[LENGTH];
    CmpField    cfr[LENGTH];
    CmpDTSinfo *dtsinfo = NULL;

    typedef struct {
        float b[ALEN];
    } fld_t;

    fld_t fld[LENGTH];
    fld_t fldr[LENGTH];

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Partial I/O of Array Fields in Compound Datatype Functionality\n"));

    /* Initialize the data */
    /* ------------------- */
    dtsinfo = (CmpDTSinfo *)HDmalloc(sizeof(CmpDTSinfo));
    CHECK_PTR(dtsinfo, "HDmalloc");
    HDmemset(dtsinfo, 0, sizeof(CmpDTSinfo));
    for (i = 0; i < LENGTH; i++) {
        for (j = 0; j < ALEN; j++) {
            cf[i].a[j] = 100 * (i + 1) + j;
            cf[i].b[j] = 100.0F * ((float)i + 1.0F) + 0.01F * (float)j;
            cf[i].c[j] = (double)(100.0F * ((float)i + 1.0F) + 0.02F * (float)j);
        } /* end for */
    }     /* end for */

    /* Set the number of data members */
    /* ------------------------------ */
    dtsinfo->nsubfields = 3;

    /* Initialize the offsets  */
    /* ----------------------- */
    dtsinfo->offset[0] = HOFFSET(CmpField, a);
    dtsinfo->offset[1] = HOFFSET(CmpField, b);
    dtsinfo->offset[2] = HOFFSET(CmpField, c);

    /* Initialize the data type IDs */
    /* ---------------------------- */
    dtsinfo->datatype[0] = H5T_NATIVE_INT;
    dtsinfo->datatype[1] = H5T_NATIVE_FLOAT;
    dtsinfo->datatype[2] = H5T_NATIVE_DOUBLE;

    /* Initialize the names of data members */
    /* ------------------------------------ */
    for (i = 0; i < dtsinfo->nsubfields; i++)
        dtsinfo->name[i] = (char *)HDcalloc((size_t)20, sizeof(char));

    HDstrcpy(dtsinfo->name[0], "One");
    HDstrcpy(dtsinfo->name[1], "Two");
    HDstrcpy(dtsinfo->name[2], "Three");

    /* Create file */
    /* ----------- */
    fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fcreate");

    /* Create data space */
    /* ----------------- */
    space = H5Screate_simple(RANK, dim, NULL);
    CHECK(space, FAIL, "H5Screate_simple");

    /* Create the memory data type */
    /* --------------------------- */
    type = H5Tcreate(H5T_COMPOUND, sizeof(CmpField));
    CHECK(type, FAIL, "H5Tcreate");

    /* Add  members to the compound data type */
    /* -------------------------------------- */
    for (i = 0; i < dtsinfo->nsubfields; i++) {
        array_dt = H5Tarray_create2(dtsinfo->datatype[i], ndims[i], dima);
        CHECK(array_dt, FAIL, "H5Tarray_create2");

        status = H5Tinsert(type, dtsinfo->name[i], dtsinfo->offset[i], array_dt);
        CHECK(status, FAIL, "H5Tinsert");

        status = H5Tclose(array_dt);
        CHECK(status, FAIL, "H5Tclose");
    } /* end for */

    /* Create the dataset */
    /* ------------------ */
    dataset = H5Dcreate2(fid, FIELDNAME, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Write data to the dataset */
    /* ------------------------- */
    status = H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, cf);
    CHECK(status, FAIL, "H5Dwrite");

    status = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, cfr);
    CHECK(status, FAIL, "H5Dread");

    /* Verify correct data */
    /* ------------------- */
    for (i = 0; i < LENGTH; i++) {
        for (j = 0; j < ALEN; j++) {
            if (cf[i].a[j] != cfr[i].a[j]) {
                TestErrPrintf("Field a data doesn't match, cf[%d].a[%d]=%d, cfr[%d].a[%d]=%d\n", (int)i,
                              (int)j, (int)cf[i].a[j], (int)i, (int)j, (int)cfr[i].a[j]);
                continue;
            } /* end if */
            if (!H5_FLT_ABS_EQUAL(cf[i].b[j], cfr[i].b[j])) {
                TestErrPrintf("Field b data doesn't match, cf[%d].b[%d]=%f, cfr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)cf[i].b[j], (int)i, (int)j, (double)cfr[i].b[j]);
                continue;
            } /* end if */
            if (!H5_DBL_ABS_EQUAL(cf[i].c[j], cfr[i].c[j])) {
                TestErrPrintf("Field c data doesn't match, cf[%d].b[%d]=%f, cfr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)cf[i].c[j], (int)i, (int)j, (double)cfr[i].c[j]);
                continue;
            } /* end if */
        }     /* end for */
    }         /* end for */

    /* Release memory resources */
    /* ------------------------ */
    for (i = 0; i < dtsinfo->nsubfields; i++)
        HDfree(dtsinfo->name[i]);

    /* Release IDs */
    /* ----------- */
    status = H5Tclose(type);
    CHECK(status, FAIL, "H5Tclose");

    status = H5Sclose(space);
    CHECK(status, FAIL, "H5Sclose");

    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");

    status = H5Fclose(fid);
    CHECK(status, FAIL, "H5Fclose");

    /******************************/
    /* Reopen the file and update */
    /******************************/

    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    dataset = H5Dopen2(fid, FIELDNAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    type = H5Tcreate(H5T_COMPOUND, sizeof(fld_t));
    CHECK(type, FAIL, "H5Tcreate");

    array_dt = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, dima);
    CHECK(array_dt, FAIL, "H5Tarray_create2");

    status = H5Tinsert(type, "Two", HOFFSET(fld_t, b), array_dt);
    CHECK(status, FAIL, "H5Tinsert");

    /* Initialize the data to overwrite */
    /* -------------------------------- */
    for (i = 0; i < LENGTH; i++)
        for (j = 0; j < ALEN; j++)
            cf[i].b[j] = fld[i].b[j] = 1.313F;

    status = H5Dwrite(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, fld);
    CHECK(status, FAIL, "H5Dwrite");

    /* Read just the field changed */
    status = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, fldr);
    CHECK(status, FAIL, "H5Dread");

    for (i = 0; i < LENGTH; i++)
        for (j = 0; j < ALEN; j++)
            if (!H5_FLT_ABS_EQUAL(fld[i].b[j], fldr[i].b[j])) {
                TestErrPrintf("Field data doesn't match, fld[%d].b[%d]=%f, fldr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)fld[i].b[j], (int)i, (int)j, (double)fldr[i].b[j]);
                continue;
            } /* end if */

    status = H5Tclose(type);
    CHECK(status, FAIL, "H5Tclose");

    status = H5Tclose(array_dt);
    CHECK(status, FAIL, "H5Tclose");

    type = H5Dget_type(dataset);
    CHECK(type, FAIL, "H5Dget_type");

    /* Read the entire dataset again */
    status = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, cfr);
    CHECK(status, FAIL, "H5Dread");

    /* Verify correct data */
    /* ------------------- */
    for (i = 0; i < LENGTH; i++) {
        for (j = 0; j < ALEN; j++) {
            if (cf[i].a[j] != cfr[i].a[j]) {
                TestErrPrintf("Field a data doesn't match, cf[%d].a[%d]=%d, cfr[%d].a[%d]=%d\n", (int)i,
                              (int)j, (int)cf[i].a[j], (int)i, (int)j, (int)cfr[i].a[j]);
                continue;
            } /* end if */
            if (!H5_FLT_ABS_EQUAL(cf[i].b[j], cfr[i].b[j])) {
                TestErrPrintf("Field b data doesn't match, cf[%d].b[%d]=%f, cfr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)cf[i].b[j], (int)i, (int)j, (double)cfr[i].b[j]);
                continue;
            } /* end if */
            if (!H5_DBL_ABS_EQUAL(cf[i].c[j], cfr[i].c[j])) {
                TestErrPrintf("Field c data doesn't match, cf[%d].b[%d]=%f, cfr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)cf[i].c[j], (int)i, (int)j, (double)cfr[i].c[j]);
                continue;
            } /* end if */
        }     /* end for */
    }         /* end for */

    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");

    status = H5Tclose(type);
    CHECK(status, FAIL, "H5Tclose");

    status = H5Fclose(fid);
    CHECK(status, FAIL, "H5Fclose");

    /****************************************************/
    /* Reopen the file and print out all the data again */
    /****************************************************/

    fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    dataset = H5Dopen2(fid, FIELDNAME, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dopen2");

    type = H5Dget_type(dataset);
    CHECK(type, FAIL, "H5Dget_type");

    /* Reset the data to read in */
    /* ------------------------- */
    HDmemset(cfr, 0, sizeof(CmpField) * LENGTH);

    status = H5Dread(dataset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, cfr);
    CHECK(status, FAIL, "H5Dread");

    /* Verify correct data */
    /* ------------------- */
    for (i = 0; i < LENGTH; i++) {
        for (j = 0; j < ALEN; j++) {
            if (cf[i].a[j] != cfr[i].a[j]) {
                TestErrPrintf("Field a data doesn't match, cf[%d].a[%d]=%d, cfr[%d].a[%d]=%d\n", (int)i,
                              (int)j, (int)cf[i].a[j], (int)i, (int)j, (int)cfr[i].a[j]);
                continue;
            } /* end if */
            if (!H5_FLT_ABS_EQUAL(cf[i].b[j], cfr[i].b[j])) {
                TestErrPrintf("Field b data doesn't match, cf[%d].b[%d]=%f, cfr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)cf[i].b[j], (int)i, (int)j, (double)cfr[i].b[j]);
                continue;
            } /* end if */
            if (!H5_DBL_ABS_EQUAL(cf[i].c[j], cfr[i].c[j])) {
                TestErrPrintf("Field c data doesn't match, cf[%d].b[%d]=%f, cfr[%d].b[%d]=%f\n", (int)i,
                              (int)j, (double)cf[i].c[j], (int)i, (int)j, (double)cfr[i].c[j]);
                continue;
            } /* end if */
        }     /* end for */
    }         /* end for */

    status = H5Dclose(dataset);
    CHECK(status, FAIL, "H5Dclose");

    status = H5Tclose(type);
    CHECK(status, FAIL, "H5Tclose");

    status = H5Fclose(fid);
    CHECK(status, FAIL, "H5Fclose");

    HDfree(dtsinfo);
} /* end test_array_bkg() */

/*-------------------------------------------------------------------------
 * Function:    test_compat
 *
 * Purpose:     Test array datatype compatibility code.
 *
 *              Reads file containing old version of datatype object header
 *              messages for compound datatypes and verifies reading the older
 *              version of the is working correctly.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
test_compat(void)
{
    const char *testfile = H5_get_srcdir_filename(TESTFILE); /* Corrected test file name */
    hid_t       fid1;                                        /* HDF5 File IDs                    */
    hid_t       dataset;                                     /* Dataset ID                       */
    hid_t       tid1;                                        /* Array Datatype ID                */
    hid_t       tid2;                                        /* Datatype ID                      */
    hsize_t     tdims1[] = {ARRAY1_DIM1};
    int         ndims;                /* Array rank for reading           */
    hsize_t     rdims1[H5S_MAX_RANK]; /* Array dimensions for reading     */
    H5T_class_t mclass;               /* Datatype class for VL            */
    int         nmemb;                /* Number of compound members       */
    char *      mname;                /* Name of compound field           */
    size_t      off;                  /* Offset of compound field         */
    hid_t       mtid;                 /* Datatype ID for field            */
    int         i;                    /* Index variables                  */
    herr_t      ret;                  /* Generic return value             */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Array Datatypes Compatibility Functionality\n"));

    /*
     * Try reading a file that has been prepared that has datasets with
     * compound datatypes which use an older version (version 1) of the
     * datatype object header message for describing the datatype.
     *
     * If this test fails and the datatype object header message version has
     *  changed, follow the instructions in gen_old_array.c for regenerating
     *  the tarrold.h5 file.
     */

    /* Open the testfile */
    fid1 = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
    CHECK_I(fid1, "H5Fopen");

    /* Only try to proceed if the file is around */
    if (fid1 >= 0) {
        /* Open the first dataset (with no array fields) */
        dataset = H5Dopen2(fid1, "Dataset1", H5P_DEFAULT);
        CHECK_I(dataset, "H5Dopen2");

        /* Get the datatype */
        tid1 = H5Dget_type(dataset);
        CHECK_I(tid1, "H5Dget_type");

        /* Verify datatype class */
        mclass = H5Tget_class(tid1);
        VERIFY(mclass, H5T_COMPOUND, "H5Tget_class");

        /* Get the number of compound datatype fields */
        nmemb = H5Tget_nmembers(tid1);
        VERIFY(nmemb, 3, "H5Tget_nmembers");

        /* Check the 1st field's name */
        mname = H5Tget_member_name(tid1, 0);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (HDstrcmp(mname, "i") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        H5free_memory(mname);

        /* Check the 1st field's offset */
        off = H5Tget_member_offset(tid1, 0);
        VERIFY(off, 0, "H5Tget_member_offset");

        /* Check the 1st field's datatype */
        mtid = H5Tget_member_type(tid1, 0);
        CHECK(mtid, FAIL, "H5Tget_member_type");
        if ((ret = H5Tequal(mtid, H5T_STD_I16LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(mtid);
        CHECK(mtid, FAIL, "H5Tclose");

        /* Check the 2nd field's name */
        mname = H5Tget_member_name(tid1, 1);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (HDstrcmp(mname, "f") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        H5free_memory(mname);

        /* Check the 2nd field's offset */
        off = H5Tget_member_offset(tid1, 1);
        VERIFY(off, 4, "H5Tget_member_offset");

        /* Check the 2nd field's datatype */
        mtid = H5Tget_member_type(tid1, 1);
        CHECK(mtid, FAIL, "H5Tget_member_type");
        if ((ret = H5Tequal(mtid, H5T_IEEE_F32LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(mtid);
        CHECK(mtid, FAIL, "H5Tclose");

        /* Check the 3rd field's name */
        mname = H5Tget_member_name(tid1, 2);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (HDstrcmp(mname, "l") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        H5free_memory(mname);

        /* Check the 3rd field's offset */
        off = H5Tget_member_offset(tid1, 2);
        VERIFY(off, 8, "H5Tget_member_offset");

        /* Check the 3rd field's datatype */
        mtid = H5Tget_member_type(tid1, 2);
        CHECK(mtid, FAIL, "H5Tget_member_type");
        if ((ret = H5Tequal(mtid, H5T_STD_I32LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(mtid);
        CHECK(mtid, FAIL, "H5Tclose");

        /* Close the datatype */
        ret = H5Tclose(tid1);
        CHECK_I(ret, "H5Tclose");

        /* Close the dataset */
        ret = H5Dclose(dataset);
        CHECK_I(ret, "H5Dclose");

        /* Open the second dataset (with array fields) */
        dataset = H5Dopen2(fid1, "Dataset2", H5P_DEFAULT);
        CHECK_I(dataset, "H5Dopen2");

        /* Get the datatype */
        tid1 = H5Dget_type(dataset);
        CHECK_I(tid1, "H5Dget_type");

        /* Verify datatype class */
        mclass = H5Tget_class(tid1);
        VERIFY(mclass, H5T_COMPOUND, "H5Tget_class");

        /* Get the number of compound datatype fields */
        nmemb = H5Tget_nmembers(tid1);
        VERIFY(nmemb, 4, "H5Tget_nmembers");

        /* Check the 1st field's name */
        mname = H5Tget_member_name(tid1, 0);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (mname && HDstrcmp(mname, "i") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        if (mname)
            H5free_memory(mname);

        /* Check the 1st field's offset */
        off = H5Tget_member_offset(tid1, 0);
        VERIFY(off, 0, "H5Tget_member_offset");

        /* Check the 1st field's datatype */
        mtid = H5Tget_member_type(tid1, 0);
        CHECK(mtid, FAIL, "H5Tget_member_type");
        if ((ret = H5Tequal(mtid, H5T_STD_I16LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(mtid);
        CHECK(mtid, FAIL, "H5Tclose");

        /* Check the 2nd field's name */
        mname = H5Tget_member_name(tid1, 1);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (mname && HDstrcmp(mname, "f") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        if (mname)
            H5free_memory(mname);

        /* Check the 2nd field's offset */
        off = H5Tget_member_offset(tid1, 1);
        VERIFY(off, 4, "H5Tget_member_offset");

        /* Check the 2nd field's datatype */
        mtid = H5Tget_member_type(tid1, 1);
        CHECK(mtid, FAIL, "H5Tget_member_type");

        /* Verify datatype class */
        mclass = H5Tget_class(mtid);
        VERIFY(mclass, H5T_ARRAY, "H5Tget_class");

        /* Check the array rank */
        ndims = H5Tget_array_ndims(mtid);
        VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

        /* Get the array dimensions */
        ret = H5Tget_array_dims2(mtid, rdims1);
        CHECK(ret, FAIL, "H5Tget_array_dims2");

        /* Check the array dimensions */
        for (i = 0; i < ndims; i++)
            if (rdims1[i] != tdims1[i]) {
                TestErrPrintf(
                    "Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                    rdims1[i], i, tdims1[i]);
                continue;
            } /* end if */

        /* Check the array's base datatype */
        tid2 = H5Tget_super(mtid);
        CHECK(tid2, FAIL, "H5Tget_super");

        if ((ret = H5Tequal(tid2, H5T_IEEE_F32LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(tid2);
        CHECK(ret, FAIL, "H5Tclose");
        ret = H5Tclose(mtid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Check the 3rd field's name */
        mname = H5Tget_member_name(tid1, 2);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (mname && HDstrcmp(mname, "l") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        if (mname)
            H5free_memory(mname);

        /* Check the 3rd field's offset */
        off = H5Tget_member_offset(tid1, 2);
        VERIFY(off, 20, "H5Tget_member_offset");

        /* Check the 3rd field's datatype */
        mtid = H5Tget_member_type(tid1, 2);
        CHECK(mtid, FAIL, "H5Tget_member_type");

        /* Verify datatype class */
        mclass = H5Tget_class(mtid);
        VERIFY(mclass, H5T_ARRAY, "H5Tget_class");

        /* Check the array rank */
        ndims = H5Tget_array_ndims(mtid);
        VERIFY(ndims, ARRAY1_RANK, "H5Tget_array_ndims");

        /* Get the array dimensions */
        ret = H5Tget_array_dims2(mtid, rdims1);
        CHECK(ret, FAIL, "H5Tget_array_dims2");

        /* Check the array dimensions */
        for (i = 0; i < ndims; i++)
            if (rdims1[i] != tdims1[i]) {
                TestErrPrintf(
                    "Array dimension information doesn't match!, rdims1[%d]=%llu, tdims1[%d]=%llu\n", i,
                    rdims1[i], i, tdims1[i]);
                continue;
            } /* end if */

        /* Check the array's base datatype */
        tid2 = H5Tget_super(mtid);
        CHECK(tid2, FAIL, "H5Tget_super");

        if ((ret = H5Tequal(tid2, H5T_STD_I32LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(tid2);
        CHECK(ret, FAIL, "H5Tclose");
        ret = H5Tclose(mtid);
        CHECK(ret, FAIL, "H5Tclose");

        /* Check the 4th field's name */
        mname = H5Tget_member_name(tid1, 3);
        CHECK_PTR(mname, "H5Tget_member_name");
        if (mname && HDstrcmp(mname, "d") != 0)
            TestErrPrintf("Compound field name doesn't match!, mname=%s\n", mname);
        if (mname)
            H5free_memory(mname);

        /* Check the 4th field's offset */
        off = H5Tget_member_offset(tid1, 3);
        VERIFY(off, 36, "H5Tget_member_offset");

        /* Check the 4th field's datatype */
        mtid = H5Tget_member_type(tid1, 3);
        CHECK(mtid, FAIL, "H5Tget_member_type");
        if ((ret = H5Tequal(mtid, H5T_IEEE_F64LE)) <= 0)
            TestErrPrintf("Compound data type is incorrect!, ret=%d\n", (int)ret);
        ret = H5Tclose(mtid);
        CHECK(mtid, FAIL, "H5Tclose");

        /* Close the datatype */
        ret = H5Tclose(tid1);
        CHECK_I(ret, "H5Tclose");

        /* Close the dataset */
        ret = H5Dclose(dataset);
        CHECK_I(ret, "H5Dclose");

        /* Close the file */
        ret = H5Fclose(fid1);
        CHECK_I(ret, "H5Fclose");
    } /* end if */
    else
        HDprintf("***cannot open the pre-created compound datatype test file (%s)\n", testfile);

} /* end test_compat() */

/*-------------------------------------------------------------------------
 * Function:    test_array
 *
 * Purpose:     Main array datatype testing routine.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
test_array(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Array Datatypes\n"));

    /* These tests use the same file... */
    test_array_atomic_1d();       /* Test 1-D array of atomic datatypes                                   */
    test_array_atomic_3d();       /* Test 3-D array of atomic datatypes                                   */
    test_array_array_atomic();    /* Test 1-D array of 2-D arrays of atomic datatypes                     */
    test_array_compound_atomic(); /* Test 1-D array of compound datatypes (with no array fields)          */
    test_array_compound_array();  /* Test 1-D array of compound datatypes (with array fields)             */
    test_array_vlen_atomic();     /* Test 1-D array of atomic VL datatypes                                */
    test_array_vlen_array();      /* Test 1-D array of 1-D array VL datatypes                             */
    test_array_funcs();           /* Test type functions with array types                                 */

    test_array_bkg(); /* Read compound datatype with array fields and background fields read  */

    /* This test uses a custom file */
    test_compat(); /* Test compatibility changes for compound datatype fields              */

} /* end test_array() */

/*-------------------------------------------------------------------------
 * Function:    cleanup_array
 *
 * Purpose:     Cleanup temporary test files
 *
 * Return:      void
 *
 * Programmer:  Quincey Koziol
 *              June 8, 1999
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_array(void)
{
    HDremove(FILENAME);
} /* end cleanup_array() */
