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
 * Test program:     trefer
 *
 * Test the Reference functionality
 *
 *************************************************************/

#include "testhdf5.h"

#define FILE_REF_PARAM     "trefer_param.h5"
#define FILE_REF_OBJ       "trefer_obj.h5"
#define FILE_REF_VL_OBJ    "trefer_vl_obj.h5"
#define FILE_REF_CMPND_OBJ "trefer_cmpnd_obj.h5"
#define FILE_REF_REG       "trefer_reg.h5"
#define FILE_REF_REG_1D    "trefer_reg_1d.h5"
#define FILE_REF_OBJ_DEL   "trefer_obj_del.h5"
#define FILE_REF_GRP       "trefer_grp.h5"
#define FILE_REF_ATTR      "trefer_attr.h5"
#define FILE_REF_EXT1      "trefer_ext1.h5"
#define FILE_REF_EXT2      "trefer_ext2.h5"
#define FILE_REF_COMPAT    "trefer_compat.h5"

/* 1-D dataset with fixed dimensions */
#define SPACE1_RANK 1
#define SPACE1_DIM1 4

/* 2-D dataset with fixed dimensions */
#define SPACE2_RANK 2
#define SPACE2_DIM1 10
#define SPACE2_DIM2 10

/* Larger 1-D dataset with fixed dimensions */
#define SPACE3_RANK 1
#define SPACE3_DIM1 100

/* Element selection information */
#define POINT1_NPOINTS 10

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float        c;
} s1_t;

/* Compound datatype with reference */
typedef struct s2_t {
    H5R_ref_t    ref0;    /* reference  */
    H5R_ref_t    ref1;    /* reference  */
    H5R_ref_t    ref2;    /* reference  */
    H5R_ref_t    ref3;    /* reference  */
    unsigned int dim_idx; /* dimension index of the dataset */
} s2_t;

#define GROUPNAME  "/group"
#define GROUPNAME2 "group2"
#define GROUPNAME3 "group3"
#define DSETNAME   "/dset"
#define DSETNAME2  "dset2"
#define NAME_SIZE  16

#define MAX_ITER_CREATE 1000
#define MAX_ITER_WRITE  MAX_ITER_CREATE
#define MAX_ITER_READ   MAX_ITER_CREATE

/****************************************************************
**
**  test_reference_params(): Test basic H5R (reference) parameters
**                           for correct processing
**
****************************************************************/
static void
test_reference_params(void)
{
    hid_t fid1;         /* HDF5 File IDs                    */
    hid_t dataset,      /* Dataset ID                       */
        dset2;          /* Dereferenced dataset ID          */
    hid_t      group;   /* Group ID                         */
    hid_t      attr;    /* Attribute ID                     */
    hid_t      sid1;    /* Dataspace ID                     */
    hid_t      tid1;    /* Datatype ID                      */
    hid_t      aapl_id; /* Attribute access property list   */
    hid_t      dapl_id; /* Dataset access property list     */
    hsize_t    dims1[] = {SPACE1_DIM1};
    H5R_ref_t *wbuf, /* buffer to write to disk          */
        *rbuf,       /* buffer read from disk            */
        *tbuf;       /* temp. buffer read from disk      */
    unsigned    *obuf;
    H5R_type_t   type; /* Reference type                   */
    unsigned int i;    /* Counters                         */
#if 0
    const char  *write_comment = "Foo!"; /* Comments for group   */
#endif
    hid_t   ret_id;    /* Generic hid_t return value       */
    ssize_t name_size; /* Size of reference name           */
    herr_t  ret;       /* Generic return value             */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Reference Parameters\n"));

    /* Allocate write & read buffers */
    wbuf = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    rbuf = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    tbuf = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    obuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);

    for (i = 0; i < SPACE1_DIM1; i++)
        obuf[i] = i * 3;

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_PARAM, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create attribute access property list */
    aapl_id = H5Pcreate(H5P_ATTRIBUTE_ACCESS);
    CHECK(aapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");
#if 0
    /* Set group's comment */
    ret = H5Oset_comment(group, write_comment);
    CHECK(ret, FAIL, "H5Oset_comment");
#endif
    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, obuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(dataset, "Attr", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, obuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, H5I_INVALID_HID, "H5Dcreate2");

    /* Test parameters to H5Rcreate_object */
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_object(fid1, "/Group1/Dataset1", H5P_DEFAULT, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_object ref");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_object(H5I_INVALID_HID, "/Group1/Dataset1", H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_object loc_id");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_object(fid1, NULL, H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_object name");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_object(fid1, "", H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_object null name");

    /* Test parameters to H5Rcreate_region */
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_region(fid1, "/Group1/Dataset1", sid1, H5P_DEFAULT, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_region ref");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_region(H5I_INVALID_HID, "/Group1/Dataset1", sid1, H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_region loc_id");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_region(fid1, NULL, sid1, H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_region name");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_region(fid1, "/Group1/Dataset1", H5I_INVALID_HID, H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_region dataspace");

    /* Test parameters to H5Rcreate_attr */
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_attr(fid1, "/Group1/Dataset2", "Attr", H5P_DEFAULT, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_attr ref");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_attr(H5I_INVALID_HID, "/Group1/Dataset2", "Attr", H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_attr loc_id");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_attr(fid1, NULL, "Attr", H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_attr name");
    H5E_BEGIN_TRY
    {
        ret = H5Rcreate_attr(fid1, "/Group1/Dataset2", NULL, H5P_DEFAULT, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcreate_attr attr_name");

    /* Test parameters to H5Rdestroy */
    H5E_BEGIN_TRY
    {
        ret = H5Rdestroy(NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rdestroy");

    /* Test parameters to H5Rget_type */
    H5E_BEGIN_TRY
    {
        type = H5Rget_type(NULL);
    }
    H5E_END_TRY;
    VERIFY(type, H5R_BADTYPE, "H5Rget_type ref");

    /* Test parameters to H5Requal */
    H5E_BEGIN_TRY
    {
        ret = H5Requal(NULL, &rbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Requal ref1");
    H5E_BEGIN_TRY
    {
        ret = H5Requal(&rbuf[0], NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Requal ref2");

    /* Test parameters to H5Rcopy */
    H5E_BEGIN_TRY
    {
        ret = H5Rcopy(NULL, &wbuf[0]);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcopy src_ref");
    H5E_BEGIN_TRY
    {
        ret = H5Rcopy(&rbuf[0], NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rcopy dest_ref");

    /* Test parameters to H5Ropen_object */
    H5E_BEGIN_TRY
    {
        dset2 = H5Ropen_object(&rbuf[0], H5I_INVALID_HID, H5I_INVALID_HID);
    }
    H5E_END_TRY;
    VERIFY(dset2, H5I_INVALID_HID, "H5Ropen_object oapl_id");
    H5E_BEGIN_TRY
    {
        dset2 = H5Ropen_object(NULL, H5P_DEFAULT, dapl_id);
    }
    H5E_END_TRY;
    VERIFY(dset2, H5I_INVALID_HID, "H5Ropen_object ref");

    /* Test parameters to H5Ropen_region */
    H5E_BEGIN_TRY
    {
        ret_id = H5Ropen_region(NULL, H5I_INVALID_HID, H5I_INVALID_HID);
    }
    H5E_END_TRY;
    VERIFY(ret_id, H5I_INVALID_HID, "H5Ropen_region ref");

    /* Test parameters to H5Ropen_attr */
    H5E_BEGIN_TRY
    {
        ret_id = H5Ropen_attr(NULL, H5P_DEFAULT, aapl_id);
    }
    H5E_END_TRY;
    VERIFY(ret_id, H5I_INVALID_HID, "H5Ropen_attr ref");

    /* Test parameters to H5Rget_obj_type3 */
    H5E_BEGIN_TRY
    {
        ret = H5Rget_obj_type3(NULL, H5P_DEFAULT, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5Rget_obj_type3 ref");

    /* Test parameters to H5Rget_file_name */
    H5E_BEGIN_TRY
    {
        name_size = H5Rget_file_name(NULL, NULL, 0);
    }
    H5E_END_TRY;
    VERIFY(name_size, (-1), "H5Rget_file_name ref");

    /* Test parameters to H5Rget_obj_name */
    H5E_BEGIN_TRY
    {
        name_size = H5Rget_obj_name(NULL, H5P_DEFAULT, NULL, 0);
    }
    H5E_END_TRY;
    VERIFY(name_size, (-1), "H5Rget_obj_name ref");

    /* Test parameters to H5Rget_attr_name */
    H5E_BEGIN_TRY
    {
        name_size = H5Rget_attr_name(NULL, NULL, 0);
    }
    H5E_END_TRY;
    VERIFY(name_size, (-1), "H5Rget_attr_name ref");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close attribute access property list */
    ret = H5Pclose(aapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(tbuf);
    HDfree(obuf);
} /* test_reference_params() */

/****************************************************************
**
**  test_reference_obj(): Test basic H5R (reference) object reference code.
**      Tests references to various kinds of objects
**
****************************************************************/
static void
test_reference_obj(void)
{
    hid_t fid1;       /* HDF5 File IDs                    */
    hid_t dataset,    /* Dataset ID                       */
        dset2;        /* Dereferenced dataset ID          */
    hid_t      group; /* Group ID                         */
    hid_t      sid1;  /* Dataspace ID                     */
    hid_t      tid1;  /* Datatype ID                      */
    hsize_t    dims1[] = {SPACE1_DIM1};
    hid_t      dapl_id; /* Dataset access property list     */
    H5R_ref_t *wbuf,    /* buffer to write to disk          */
        *rbuf;          /* buffer read from disk            */
    unsigned  *ibuf, *obuf;
    unsigned   i, j;     /* Counters                         */
    H5O_type_t obj_type; /* Object type                      */
    herr_t     ret;      /* Generic return value             */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    rbuf = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    ibuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);
    obuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);

    for (i = 0; i < SPACE1_DIM1; i++)
        obuf[i] = i * 3;

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_OBJ, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, obuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset1", H5P_DEFAULT, &wbuf[0]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset2", H5P_DEFAULT, &wbuf[1]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[1], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to group */
    ret = H5Rcreate_object(fid1, "/Group1", H5P_DEFAULT, &wbuf[2]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[2], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type3");

    /* Create reference to named datatype */
    ret = H5Rcreate_object(fid1, "/Group1/Datatype1", H5P_DEFAULT, &wbuf[3]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[3], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type3");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE_REF_OBJ, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Open dataset object */
    dset2 = H5Ropen_object(&rbuf[0], H5P_DEFAULT, dapl_id);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, H5I_INVALID_HID, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, SPACE1_DIM1, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset2, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf);
    CHECK(ret, FAIL, "H5Dread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(ibuf[i], i * 3, "Data");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open group object.  GAPL isn't supported yet.  But it's harmless to pass in */
    group = H5Ropen_object(&rbuf[2], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Ropen_object");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open datatype object. TAPL isn't supported yet.  But it's harmless to pass in */
    tid1 = H5Ropen_object(&rbuf[3], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tid1, H5I_INVALID_HID, "H5Ropen_object");

    /* Verify correct datatype */
    {
        H5T_class_t tclass;

        tclass = H5Tget_class(tid1);
        VERIFY(tclass, H5T_COMPOUND, "H5Tget_class");

        ret = H5Tget_nmembers(tid1);
        VERIFY(ret, 3, "H5Tget_nmembers");
    }

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy references */
    for (j = 0; j < SPACE1_DIM1; j++) {
        ret = H5Rdestroy(&wbuf[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&rbuf[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(ibuf);
    HDfree(obuf);
} /* test_reference_obj() */

/****************************************************************
**
**  test_reference_vlen_obj(): Test basic H5R (reference) object reference
**      within a vlen type.
**      Tests references to various kinds of objects
**
****************************************************************/
static void
test_reference_vlen_obj(void)
{
    hid_t fid1;       /* HDF5 File IDs                    */
    hid_t dataset,    /* Dataset ID                       */
        dset2;        /* Dereferenced dataset ID          */
    hid_t      group; /* Group ID                         */
    hid_t      sid1;  /* Dataspace ID                     */
    hid_t      tid1;  /* Datatype ID                      */
    hsize_t    dims1[]   = {SPACE1_DIM1};
    hsize_t    vl_dims[] = {1};
    hid_t      dapl_id; /* Dataset access property list     */
    H5R_ref_t *wbuf,    /* buffer to write to disk          */
        *rbuf = NULL;   /* buffer read from disk            */
    unsigned  *ibuf, *obuf;
    unsigned   i, j;     /* Counters                         */
    H5O_type_t obj_type; /* Object type                      */
    herr_t     ret;      /* Generic return value             */
    hvl_t      vl_wbuf = {0, NULL}, vl_rbuf = {0, NULL};

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Functions within VLEN type\n"));

    /* Allocate write & read buffers */
    wbuf = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    ibuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);
    obuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);

    for (i = 0; i < SPACE1_DIM1; i++)
        obuf[i] = i * 3;

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_VL_OBJ, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, obuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create vlen type */
    tid1 = H5Tvlen_create(H5T_STD_REF);
    CHECK(tid1, H5I_INVALID_HID, "H5Tvlen_create");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, vl_dims, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset1", H5P_DEFAULT, &wbuf[0]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset2", H5P_DEFAULT, &wbuf[1]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[1], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to group */
    ret = H5Rcreate_object(fid1, "/Group1", H5P_DEFAULT, &wbuf[2]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[2], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type3");

    /* Create reference to named datatype */
    ret = H5Rcreate_object(fid1, "/Group1/Datatype1", H5P_DEFAULT, &wbuf[3]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[3], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type3");

    /* Store references into vlen */
    vl_wbuf.len = SPACE1_DIM1;
    vl_wbuf.p   = wbuf;

    /* Write selection to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vl_wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE_REF_VL_OBJ, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    tid1 = H5Dget_type(dataset);
    CHECK(tid1, H5I_INVALID_HID, "H5Dget_type");

    /* Read selection from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &vl_rbuf);
    CHECK(ret, FAIL, "H5Dread");

    VERIFY(vl_rbuf.len, SPACE1_DIM1, "H5Dread");
    rbuf = vl_rbuf.p;

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Open dataset object */
    dset2 = H5Ropen_object(&rbuf[0], H5P_DEFAULT, dapl_id);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, H5I_INVALID_HID, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, SPACE1_DIM1, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset2, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf);
    CHECK(ret, FAIL, "H5Dread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(ibuf[i], i * 3, "Data");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open group object.  GAPL isn't supported yet.  But it's harmless to pass in */
    group = H5Ropen_object(&rbuf[2], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Ropen_object");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open datatype object. TAPL isn't supported yet.  But it's harmless to pass in */
    tid1 = H5Ropen_object(&rbuf[3], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tid1, H5I_INVALID_HID, "H5Ropen_object");

    /* Verify correct datatype */
    {
        H5T_class_t tclass;

        tclass = H5Tget_class(tid1);
        VERIFY(tclass, H5T_COMPOUND, "H5Tget_class");

        ret = H5Tget_nmembers(tid1);
        VERIFY(ret, 3, "H5Tget_nmembers");
    }

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy references */
    for (j = 0; j < SPACE1_DIM1; j++) {
        ret = H5Rdestroy(&wbuf[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&rbuf[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(ibuf);
    HDfree(obuf);
} /* test_reference_vlen_obj() */

/****************************************************************
**
**  test_reference_cmpnd_obj(): Test basic H5R (reference) object reference
**      within a compound type.
**      Tests references to various kinds of objects
**
****************************************************************/
static void
test_reference_cmpnd_obj(void)
{
    hid_t fid1;       /* HDF5 File IDs                    */
    hid_t dataset,    /* Dataset ID                       */
        dset2;        /* Dereferenced dataset ID          */
    hid_t      group; /* Group ID                         */
    hid_t      sid1;  /* Dataspace ID                     */
    hid_t      tid1;  /* Datatype ID                      */
    hsize_t    dims1[]      = {SPACE1_DIM1};
    hsize_t    cmpnd_dims[] = {1};
    hid_t      dapl_id; /* Dataset access property list     */
    unsigned  *ibuf, *obuf;
    unsigned   i;        /* Counter                          */
    H5O_type_t obj_type; /* Object type                      */
    herr_t     ret;      /* Generic return value             */
    s2_t       cmpnd_wbuf, cmpnd_rbuf;

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Functions within compound type\n"));

    /* Allocate write & read buffers */
    ibuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);
    obuf = HDcalloc(sizeof(unsigned), SPACE1_DIM1);

    for (i = 0; i < SPACE1_DIM1; i++)
        obuf[i] = i * 3;

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_CMPND_OBJ, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, obuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create compound type */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s2_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "ref0", HOFFSET(s2_t, ref0), H5T_STD_REF);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "ref1", HOFFSET(s2_t, ref1), H5T_STD_REF);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "ref2", HOFFSET(s2_t, ref2), H5T_STD_REF);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "ref3", HOFFSET(s2_t, ref3), H5T_STD_REF);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "dim_idx", HOFFSET(s2_t, dim_idx), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, cmpnd_dims, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", tid1, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Reset buffer for writing */
    HDmemset(&cmpnd_wbuf, 0, sizeof(cmpnd_wbuf));

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset1", H5P_DEFAULT, &cmpnd_wbuf.ref0);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&cmpnd_wbuf.ref0, H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset2", H5P_DEFAULT, &cmpnd_wbuf.ref1);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&cmpnd_wbuf.ref1, H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to group */
    ret = H5Rcreate_object(fid1, "/Group1", H5P_DEFAULT, &cmpnd_wbuf.ref2);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&cmpnd_wbuf.ref2, H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type3");

    /* Create reference to named datatype */
    ret = H5Rcreate_object(fid1, "/Group1/Datatype1", H5P_DEFAULT, &cmpnd_wbuf.ref3);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&cmpnd_wbuf.ref3, H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type3");

    /* Store dimensions */
    cmpnd_wbuf.dim_idx = SPACE1_DIM1;

    /* Write selection to disk */
    ret = H5Dwrite(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &cmpnd_wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE_REF_CMPND_OBJ, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    tid1 = H5Dget_type(dataset);
    CHECK(tid1, H5I_INVALID_HID, "H5Dget_type");

    /* Read selection from disk */
    ret = H5Dread(dataset, tid1, H5S_ALL, H5S_ALL, H5P_DEFAULT, &cmpnd_rbuf);
    CHECK(ret, FAIL, "H5Dread");

    VERIFY(cmpnd_rbuf.dim_idx, SPACE1_DIM1, "H5Dread");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Open dataset object */
    dset2 = H5Ropen_object(&cmpnd_rbuf.ref0, H5P_DEFAULT, dapl_id);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, H5I_INVALID_HID, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, SPACE1_DIM1, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset2, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf);
    CHECK(ret, FAIL, "H5Dread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(ibuf[i], i * 3, "Data");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open group object.  GAPL isn't supported yet.  But it's harmless to pass in */
    group = H5Ropen_object(&cmpnd_rbuf.ref2, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Ropen_object");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Open datatype object. TAPL isn't supported yet.  But it's harmless to pass in */
    tid1 = H5Ropen_object(&cmpnd_rbuf.ref3, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tid1, H5I_INVALID_HID, "H5Ropen_object");

    /* Verify correct datatype */
    {
        H5T_class_t tclass;

        tclass = H5Tget_class(tid1);
        VERIFY(tclass, H5T_COMPOUND, "H5Tget_class");

        ret = H5Tget_nmembers(tid1);
        VERIFY(ret, 3, "H5Tget_nmembers");
    }

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy references */
    ret = H5Rdestroy(&cmpnd_wbuf.ref0);
    CHECK(ret, FAIL, "H5Rdestroy");
    ret = H5Rdestroy(&cmpnd_wbuf.ref1);
    CHECK(ret, FAIL, "H5Rdestroy");
    ret = H5Rdestroy(&cmpnd_wbuf.ref2);
    CHECK(ret, FAIL, "H5Rdestroy");
    ret = H5Rdestroy(&cmpnd_wbuf.ref3);
    CHECK(ret, FAIL, "H5Rdestroy");

    ret = H5Rdestroy(&cmpnd_rbuf.ref0);
    CHECK(ret, FAIL, "H5Rdestroy");
    ret = H5Rdestroy(&cmpnd_rbuf.ref1);
    CHECK(ret, FAIL, "H5Rdestroy");
    ret = H5Rdestroy(&cmpnd_rbuf.ref2);
    CHECK(ret, FAIL, "H5Rdestroy");
    ret = H5Rdestroy(&cmpnd_rbuf.ref3);
    CHECK(ret, FAIL, "H5Rdestroy");

    /* Free memory buffers */
    HDfree(ibuf);
    HDfree(obuf);
} /* test_reference_cmpnd_obj() */

/****************************************************************
**
**  test_reference_region(): Test basic H5R (reference) object reference code.
**      Tests references to various kinds of objects
**
**  Note: The libver_low/libver_high parameters are added to create the file
**        with the low and high bounds setting in fapl.
**        Please see the RFC for "H5Sencode/H5Sdecode Format Change".
**
****************************************************************/
static void
test_reference_region(H5F_libver_t libver_low, H5F_libver_t libver_high)
{
    hid_t fid1;         /* HDF5 File IDs */
    hid_t fapl;         /* File access property list */
    hid_t dset1,        /* Dataset ID */
        dset2;          /* Dereferenced dataset ID */
    hid_t sid1,         /* Dataspace ID #1 */
        sid2;           /* Dataspace ID #2 */
    hid_t      dapl_id; /* Dataset access property list */
    hsize_t    dims1[] = {SPACE1_DIM1}, dims2[] = {SPACE2_DIM1, SPACE2_DIM2};
    hsize_t    start[SPACE2_RANK];                         /* Starting location of hyperslab */
    hsize_t    stride[SPACE2_RANK];                        /* Stride of hyperslab */
    hsize_t    count[SPACE2_RANK];                         /* Element count of hyperslab */
    hsize_t    block[SPACE2_RANK];                         /* Block size of hyperslab */
    hsize_t    coord1[POINT1_NPOINTS][SPACE2_RANK];        /* Coordinates for point selection */
    hsize_t   *coords;                                     /* Coordinate buffer */
    hsize_t    low[SPACE2_RANK];                           /* Selection bounds */
    hsize_t    high[SPACE2_RANK];                          /* Selection bounds */
    H5R_ref_t *wbuf     = NULL,                            /* buffer to write to disk */
        *rbuf           = NULL;                            /* buffer read from disk */
    H5R_ref_t nvrbuf[3] = {{{{0}}}, {{{101}}}, {{{255}}}}; /* buffer with non-valid refs */
    uint8_t  *dwbuf     = NULL,                            /* Buffer for writing numeric data to disk */
        *drbuf          = NULL;                            /* Buffer for reading numeric data from disk */
    uint8_t   *tu8;                                        /* Temporary pointer to uint8 data */
    H5O_type_t obj_type;                                   /* Type of object */
    int        i, j;                                       /* Counters */
    hssize_t   hssize_ret;                                 /* hssize_t return value */
    htri_t     tri_ret;                                    /* htri_t return value */
    herr_t     ret;                                        /* Generic return value     */
    hid_t      dset_NA;                                    /* Dataset id for undefined reference */
    hid_t      space_NA;                                   /* Dataspace id for undefined reference */
    hsize_t    dims_NA[1] = {1};                           /* Dims array for undefined reference */
    H5R_ref_t  rdata_NA[1];                                /* Read buffer */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Dataset Region Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf  = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    rbuf  = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    dwbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)(SPACE2_DIM1 * SPACE2_DIM2));
    drbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)(SPACE2_DIM1 * SPACE2_DIM2));

    for (tu8 = dwbuf, i = 0; i < (SPACE2_DIM1 * SPACE2_DIM2); i++)
        *tu8++ = (uint8_t)(i * 3);

    /* Create file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Set the low/high version bounds in fapl */
    ret = H5Pset_libver_bounds(fapl, libver_low, libver_high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create file with the fapl */
    fid1 = H5Fcreate(FILE_REF_REG, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a dataset */
    dset2 = H5Dcreate2(fid1, "Dataset2", H5T_STD_U8LE, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dset2, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a dataset */
    H5E_BEGIN_TRY
    {
        dset1 = H5Dcreate2(fid1, "Dataset1", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY;

    if (dset1 < 0) {
        VERIFY(libver_high <= H5F_LIBVER_V110, TRUE, "H5Dcreate2");

        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Sclose(sid2);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Pclose(fapl);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");
    }
    else {

        CHECK(dset1, H5I_INVALID_HID, "H5Dcreate2");

        /* Create references */

        /* Select 6x6 hyperslab for first reference */
        start[0]  = 2;
        start[1]  = 2;
        stride[0] = 1;
        stride[1] = 1;
        count[0]  = 1;
        count[1]  = 1;
        block[0]  = 6;
        block[1]  = 6;
        ret       = H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        ret = (int)H5Sget_select_npoints(sid2);
        VERIFY(ret, 36, "H5Sget_select_npoints");

        /* Store first dataset region */
        ret = H5Rcreate_region(fid1, "/Dataset2", sid2, H5P_DEFAULT, &wbuf[0]);
        CHECK(ret, FAIL, "H5Rcreate_region");
        ret = H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type);
        CHECK(ret, FAIL, "H5Rget_obj_type3");
        VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

        /* Select sequence of ten points for second reference */
        coord1[0][0] = 6;
        coord1[0][1] = 9;
        coord1[1][0] = 2;
        coord1[1][1] = 2;
        coord1[2][0] = 8;
        coord1[2][1] = 4;
        coord1[3][0] = 1;
        coord1[3][1] = 6;
        coord1[4][0] = 2;
        coord1[4][1] = 8;
        coord1[5][0] = 3;
        coord1[5][1] = 2;
        coord1[6][0] = 0;
        coord1[6][1] = 4;
        coord1[7][0] = 9;
        coord1[7][1] = 0;
        coord1[8][0] = 7;
        coord1[8][1] = 1;
        coord1[9][0] = 3;
        coord1[9][1] = 3;
        ret = H5Sselect_elements(sid2, H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);
        CHECK(ret, FAIL, "H5Sselect_elements");

        ret = (int)H5Sget_select_npoints(sid2);
        VERIFY(ret, SPACE2_DIM2, "H5Sget_select_npoints");

        /* Store second dataset region */
        ret = H5Rcreate_region(fid1, "/Dataset2", sid2, H5P_DEFAULT, &wbuf[1]);
        CHECK(ret, FAIL, "H5Rcreate_region");

        /* Select unlimited hyperslab for third reference */
        start[0]  = 1;
        start[1]  = 8;
        stride[0] = 4;
        stride[1] = 1;
        count[0]  = H5S_UNLIMITED;
        count[1]  = 1;
        block[0]  = 2;
        block[1]  = 2;
        ret       = H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        hssize_ret = H5Sget_select_npoints(sid2);
        VERIFY(hssize_ret, (hssize_t)H5S_UNLIMITED, "H5Sget_select_npoints");

        /* Store third dataset region */
        ret = H5Rcreate_region(fid1, "/Dataset2", sid2, H5P_DEFAULT, &wbuf[2]);
        CHECK(ret, FAIL, "H5Rcreate_region");

        ret = H5Rget_obj_type3(&wbuf[2], H5P_DEFAULT, &obj_type);
        CHECK(ret, FAIL, "H5Rget_obj_type3");
        VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

        /* Store fourth dataset region */
        ret = H5Rcreate_region(fid1, "/Dataset2", sid2, H5P_DEFAULT, &wbuf[3]);
        CHECK(ret, FAIL, "H5Rcreate_region");

        /* Write selection to disk */
        ret = H5Dwrite(dset1, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);

        /*
         * Store a dataset region reference which will not get written to disk
         */

        /* Create the dataspace of the region references */
        space_NA = H5Screate_simple(1, dims_NA, NULL);
        CHECK(space_NA, H5I_INVALID_HID, "H5Screate_simple");

        /* Create the dataset and write the region references to it */
        dset_NA = H5Dcreate2(fid1, "DS_NA", H5T_STD_REF, space_NA, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(dset_NA, H5I_INVALID_HID, "H5Dcreate");

        /* Close and release resources for undefined region reference tests */
        ret = H5Dclose(dset_NA);
        CHECK(ret, FAIL, "H5Dclose");
        ret = H5Sclose(space_NA);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close disk dataspace */
        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close Dataset */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close uint8 dataset dataspace */
        ret = H5Sclose(sid2);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close file */
        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /* Re-open the file */
        fid1 = H5Fopen(FILE_REF_REG, H5F_ACC_RDWR, fapl);
        CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

        /*
         * Start the test of an undefined reference
         */

        /* Open the dataset of the undefined references */
        dset_NA = H5Dopen2(fid1, "DS_NA", H5P_DEFAULT);
        CHECK(dset_NA, H5I_INVALID_HID, "H5Dopen2");

        /* Read the data */
        ret = H5Dread(dset_NA, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata_NA);
        CHECK(ret, FAIL, "H5Dread");

        /*
         * Dereference an undefined reference (should fail)
         */
        H5E_BEGIN_TRY
        {
            dset2 = H5Ropen_object(&rdata_NA[0], H5P_DEFAULT, H5P_DEFAULT);
        }
        H5E_END_TRY;
        VERIFY(dset2, H5I_INVALID_HID, "H5Ropen_object");

        /* Close and release resources. */
        ret = H5Dclose(dset_NA);
        CHECK(ret, FAIL, "H5Dclose");

        /* This close should fail since H5Ropen_object never created
         * the id of the referenced object. */
        H5E_BEGIN_TRY
        {
            ret = H5Dclose(dset2);
        }
        H5E_END_TRY;
        VERIFY(ret, FAIL, "H5Dclose");

        /*
         * End the test of an undefined reference
         */

        /* Open the dataset */
        dset1 = H5Dopen2(fid1, "/Dataset1", H5P_DEFAULT);
        CHECK(dset1, H5I_INVALID_HID, "H5Dopen2");

        /* Read selection from disk */
        ret = H5Dread(dset1, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
        CHECK(ret, FAIL, "H5Dread");

        /* Try to open objects */
        dset2 = H5Ropen_object(&rbuf[0], H5P_DEFAULT, dapl_id);
        CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

        /* Check what H5Rget_obj_type3 function returns */
        ret = H5Rget_obj_type3(&rbuf[0], H5P_DEFAULT, &obj_type);
        CHECK(ret, FAIL, "H5Rget_obj_type3");
        VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

        /* Check information in referenced dataset */
        sid1 = H5Dget_space(dset2);
        CHECK(sid1, H5I_INVALID_HID, "H5Dget_space");

        ret = (int)H5Sget_simple_extent_npoints(sid1);
        VERIFY(ret, (SPACE2_DIM1 * SPACE2_DIM2), "H5Sget_simple_extent_npoints");

        /* Read from disk */
        ret = H5Dread(dset2, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, drbuf);
        CHECK(ret, FAIL, "H5Dread");

        for (tu8 = (uint8_t *)drbuf, i = 0; i < (SPACE2_DIM1 * SPACE2_DIM2); i++, tu8++)
            VERIFY(*tu8, (uint8_t)(i * 3), "Data");

        /* Get the hyperslab selection */
        sid2 = H5Ropen_region(&rbuf[0], H5P_DEFAULT, H5P_DEFAULT);
        CHECK(sid2, H5I_INVALID_HID, "H5Ropen_region");

        /* Verify correct hyperslab selected */
        ret = (int)H5Sget_select_npoints(sid2);
        VERIFY(ret, 36, "H5Sget_select_npoints");
        ret = (int)H5Sget_select_hyper_nblocks(sid2);
        VERIFY(ret, 1, "H5Sget_select_hyper_nblocks");

        /* allocate space for the hyperslab blocks */
        coords = (hsize_t *)HDmalloc((size_t)ret * SPACE2_RANK * sizeof(hsize_t) * 2);

        ret = H5Sget_select_hyper_blocklist(sid2, (hsize_t)0, (hsize_t)ret, coords);
        CHECK(ret, FAIL, "H5Sget_select_hyper_blocklist");
        VERIFY(coords[0], 2, "Hyperslab Coordinates");
        VERIFY(coords[1], 2, "Hyperslab Coordinates");
        VERIFY(coords[2], 7, "Hyperslab Coordinates");
        VERIFY(coords[3], 7, "Hyperslab Coordinates");
        HDfree(coords);
        ret = H5Sget_select_bounds(sid2, low, high);
        CHECK(ret, FAIL, "H5Sget_select_bounds");
        VERIFY(low[0], 2, "Selection Bounds");
        VERIFY(low[1], 2, "Selection Bounds");
        VERIFY(high[0], 7, "Selection Bounds");
        VERIFY(high[1], 7, "Selection Bounds");

        /* Close region space */
        ret = H5Sclose(sid2);
        CHECK(ret, FAIL, "H5Sclose");

        /* Get the element selection */
        sid2 = H5Ropen_region(&rbuf[1], H5P_DEFAULT, H5P_DEFAULT);
        CHECK(sid2, H5I_INVALID_HID, "H5Ropen_region");

        /* Verify correct elements selected */
        ret = (int)H5Sget_select_npoints(sid2);
        VERIFY(ret, SPACE2_DIM2, "H5Sget_select_npoints");
        ret = (int)H5Sget_select_elem_npoints(sid2);
        VERIFY(ret, SPACE2_DIM2, "H5Sget_select_elem_npoints");

        /* allocate space for the element points */
        coords = (hsize_t *)HDmalloc((size_t)ret * SPACE2_RANK * sizeof(hsize_t));

        ret = H5Sget_select_elem_pointlist(sid2, (hsize_t)0, (hsize_t)ret, coords);
        CHECK(ret, FAIL, "H5Sget_select_elem_pointlist");
        VERIFY(coords[0], coord1[0][0], "Element Coordinates");
        VERIFY(coords[1], coord1[0][1], "Element Coordinates");
        VERIFY(coords[2], coord1[1][0], "Element Coordinates");
        VERIFY(coords[3], coord1[1][1], "Element Coordinates");
        VERIFY(coords[4], coord1[2][0], "Element Coordinates");
        VERIFY(coords[5], coord1[2][1], "Element Coordinates");
        VERIFY(coords[6], coord1[3][0], "Element Coordinates");
        VERIFY(coords[7], coord1[3][1], "Element Coordinates");
        VERIFY(coords[8], coord1[4][0], "Element Coordinates");
        VERIFY(coords[9], coord1[4][1], "Element Coordinates");
        VERIFY(coords[10], coord1[5][0], "Element Coordinates");
        VERIFY(coords[11], coord1[5][1], "Element Coordinates");
        VERIFY(coords[12], coord1[6][0], "Element Coordinates");
        VERIFY(coords[13], coord1[6][1], "Element Coordinates");
        VERIFY(coords[14], coord1[7][0], "Element Coordinates");
        VERIFY(coords[15], coord1[7][1], "Element Coordinates");
        VERIFY(coords[16], coord1[8][0], "Element Coordinates");
        VERIFY(coords[17], coord1[8][1], "Element Coordinates");
        VERIFY(coords[18], coord1[9][0], "Element Coordinates");
        VERIFY(coords[19], coord1[9][1], "Element Coordinates");
        HDfree(coords);
        ret = H5Sget_select_bounds(sid2, low, high);
        CHECK(ret, FAIL, "H5Sget_select_bounds");
        VERIFY(low[0], 0, "Selection Bounds");
        VERIFY(low[1], 0, "Selection Bounds");
        VERIFY(high[0], 9, "Selection Bounds");
        VERIFY(high[1], 9, "Selection Bounds");

        /* Close region space */
        ret = H5Sclose(sid2);
        CHECK(ret, FAIL, "H5Sclose");

        /* Get the unlimited selection */
        sid2 = H5Ropen_region(&rbuf[2], H5P_DEFAULT, H5P_DEFAULT);
        CHECK(sid2, H5I_INVALID_HID, "H5Ropen_region");

        /* Verify correct hyperslab selected */
        hssize_ret = H5Sget_select_npoints(sid2);
        VERIFY(hssize_ret, (hssize_t)H5S_UNLIMITED, "H5Sget_select_npoints");
        tri_ret = H5Sis_regular_hyperslab(sid2);
        CHECK(tri_ret, FAIL, "H5Sis_regular_hyperslab");
        VERIFY(tri_ret, TRUE, "H5Sis_regular_hyperslab Result");
        ret = H5Sget_regular_hyperslab(sid2, start, stride, count, block);
        CHECK(ret, FAIL, "H5Sget_regular_hyperslab");
        VERIFY(start[0], (hsize_t)1, "Hyperslab Coordinates");
        VERIFY(start[1], (hsize_t)8, "Hyperslab Coordinates");
        VERIFY(stride[0], (hsize_t)4, "Hyperslab Coordinates");
        VERIFY(stride[1], (hsize_t)1, "Hyperslab Coordinates");
        VERIFY(count[0], H5S_UNLIMITED, "Hyperslab Coordinates");
        VERIFY(count[1], (hsize_t)1, "Hyperslab Coordinates");
        VERIFY(block[0], (hsize_t)2, "Hyperslab Coordinates");
        VERIFY(block[1], (hsize_t)2, "Hyperslab Coordinates");

        /* Close region space */
        ret = H5Sclose(sid2);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close first space */
        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close dereferenced Dataset */
        ret = H5Dclose(dset2);
        CHECK(ret, FAIL, "H5Dclose");

        /* Attempting to retrieve type of object using non-valid refs */
        for (j = 0; j < 3; j++) {
            H5E_BEGIN_TRY
            {
                ret = H5Rget_obj_type3(&nvrbuf[j], H5P_DEFAULT, &obj_type);
            }
            H5E_END_TRY;
            VERIFY(ret, FAIL, "H5Rget_obj_type3");
        } /* end for */

        /* Close Dataset */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close dataset access property list */
        ret = H5Pclose(dapl_id);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /* Destroy references */
        for (j = 0; j < SPACE1_DIM1; j++) {
            ret = H5Rdestroy(&wbuf[j]);
            CHECK(ret, FAIL, "H5Rdestroy");
            ret = H5Rdestroy(&rbuf[j]);
            CHECK(ret, FAIL, "H5Rdestroy");
        }
    }

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(dwbuf);
    HDfree(drbuf);

} /* test_reference_region() */

/****************************************************************
**
**  test_reference_region_1D(): Test H5R (reference) object reference code.
**      Tests 1-D references to various kinds of objects
**
**  Note: The libver_low/libver_high parameters are added to create the file
**        with the low and high bounds setting in fapl.
**        Please see the RFC for "H5Sencode/H5Sdecode Format Change".
**
****************************************************************/
static void
test_reference_region_1D(H5F_libver_t libver_low, H5F_libver_t libver_high)
{
    hid_t fid1;            /* HDF5 File IDs        */
    hid_t fapl;            /* File access property list */
    hid_t dset1,           /* Dataset ID            */
        dset3;             /* Dereferenced dataset ID */
    hid_t sid1,            /* Dataspace ID    #1        */
        sid3;              /* Dataspace ID    #3        */
    hid_t   dapl_id;       /* Dataset access property list */
    hsize_t dims1[] = {2}, /* Must be 2 */
        dims3[]     = {SPACE3_DIM1};
    hsize_t    start[SPACE3_RANK];                  /* Starting location of hyperslab */
    hsize_t    stride[SPACE3_RANK];                 /* Stride of hyperslab */
    hsize_t    count[SPACE3_RANK];                  /* Element count of hyperslab */
    hsize_t    block[SPACE3_RANK];                  /* Block size of hyperslab */
    hsize_t    coord1[POINT1_NPOINTS][SPACE3_RANK]; /* Coordinates for point selection */
    hsize_t   *coords;                              /* Coordinate buffer */
    hsize_t    low[SPACE3_RANK];                    /* Selection bounds */
    hsize_t    high[SPACE3_RANK];                   /* Selection bounds */
    H5R_ref_t *wbuf = NULL,                         /* buffer to write to disk */
        *rbuf       = NULL;                         /* buffer read from disk */
    uint8_t *dwbuf  = NULL,                         /* Buffer for writing numeric data to disk */
        *drbuf      = NULL;                         /* Buffer for reading numeric data from disk */
    uint8_t   *tu8;                                 /* Temporary pointer to uint8 data */
    H5O_type_t obj_type;                            /* Object type */
    int        i;                                   /* Counter */
    herr_t     ret;                                 /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing 1-D Dataset Region Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf  = HDcalloc(sizeof(H5R_ref_t), (size_t)SPACE1_DIM1);
    rbuf  = HDcalloc(sizeof(H5R_ref_t), (size_t)SPACE1_DIM1);
    dwbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)SPACE3_DIM1);
    drbuf = (uint8_t *)HDcalloc(sizeof(uint8_t), (size_t)SPACE3_DIM1);

    for (tu8 = dwbuf, i = 0; i < SPACE3_DIM1; i++)
        *tu8++ = (uint8_t)(i * 3);

    /* Create the file access property list */
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    CHECK(fapl, H5I_INVALID_HID, "H5Pcreate");

    /* Set the low/high version bounds in fapl */
    ret = H5Pset_libver_bounds(fapl, libver_low, libver_high);
    CHECK(ret, FAIL, "H5Pset_libver_bounds");

    /* Create file with the fapl */
    fid1 = H5Fcreate(FILE_REF_REG_1D, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid3 = H5Screate_simple(SPACE3_RANK, dims3, NULL);
    CHECK(sid3, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a dataset */
    dset3 = H5Dcreate2(fid1, "Dataset2", H5T_STD_U8LE, sid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset3, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dset3, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dset3);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create dataspace for the reference dataset */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a dataset */
    H5E_BEGIN_TRY
    {
        dset1 = H5Dcreate2(fid1, "Dataset1", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY;

    if (dset1 < 0) {

        VERIFY(libver_high <= H5F_LIBVER_V110, TRUE, "H5Dcreate2");

        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Sclose(sid3);
        CHECK(ret, FAIL, "H5Sclose");

        ret = H5Pclose(fapl);
        CHECK(ret, FAIL, "H5Pclose");

        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");
    }
    else {

        CHECK(ret, FAIL, "H5Dcreate2");

        /* Create references */

        /* Select 15 2x1 hyperslabs for first reference */
        start[0]  = 2;
        stride[0] = 5;
        count[0]  = 15;
        block[0]  = 2;
        ret       = H5Sselect_hyperslab(sid3, H5S_SELECT_SET, start, stride, count, block);
        CHECK(ret, FAIL, "H5Sselect_hyperslab");

        ret = (int)H5Sget_select_npoints(sid3);
        VERIFY(ret, (block[0] * count[0]), "H5Sget_select_npoints");

        /* Store first dataset region */
        ret = H5Rcreate_region(fid1, "/Dataset2", sid3, H5P_DEFAULT, &wbuf[0]);
        CHECK(ret, FAIL, "H5Rcreate_region");
        ret = H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type);
        CHECK(ret, FAIL, "H5Rget_obj_type3");
        VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

        /* Select sequence of ten points for second reference */
        coord1[0][0] = 16;
        coord1[1][0] = 22;
        coord1[2][0] = 38;
        coord1[3][0] = 41;
        coord1[4][0] = 52;
        coord1[5][0] = 63;
        coord1[6][0] = 70;
        coord1[7][0] = 89;
        coord1[8][0] = 97;
        coord1[9][0] = 03;
        ret = H5Sselect_elements(sid3, H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);
        CHECK(ret, FAIL, "H5Sselect_elements");

        ret = (int)H5Sget_select_npoints(sid3);
        VERIFY(ret, POINT1_NPOINTS, "H5Sget_select_npoints");

        /* Store second dataset region */
        ret = H5Rcreate_region(fid1, "/Dataset2", sid3, H5P_DEFAULT, &wbuf[1]);
        CHECK(ret, FAIL, "H5Rcreate_region");

        /* Write selection to disk */
        ret = H5Dwrite(dset1, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
        CHECK(ret, FAIL, "H5Dwrite");

        /* Close disk dataspace */
        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close Dataset */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close uint8 dataset dataspace */
        ret = H5Sclose(sid3);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close file */
        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /* Re-open the file */
        fid1 = H5Fopen(FILE_REF_REG_1D, H5F_ACC_RDWR, fapl);
        CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

        /* Open the dataset */
        dset1 = H5Dopen2(fid1, "/Dataset1", H5P_DEFAULT);
        CHECK(dset1, H5I_INVALID_HID, "H5Dopen2");

        /* Read selection from disk */
        ret = H5Dread(dset1, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
        CHECK(ret, FAIL, "H5Dread");

        /* Try to open objects */
        dset3 = H5Ropen_object(&rbuf[0], H5P_DEFAULT, dapl_id);
        CHECK(dset3, H5I_INVALID_HID, "H5Ropen_object");

        /* Check what H5Rget_obj_type3 function returns */
        ret = H5Rget_obj_type3(&rbuf[0], H5P_DEFAULT, &obj_type);
        CHECK(ret, FAIL, "H5Rget_obj_type3");
        VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

        /* Check information in referenced dataset */
        sid1 = H5Dget_space(dset3);
        CHECK(sid1, H5I_INVALID_HID, "H5Dget_space");

        ret = (int)H5Sget_simple_extent_npoints(sid1);
        VERIFY(ret, SPACE3_DIM1, "H5Sget_simple_extent_npoints");

        /* Read from disk */
        ret = H5Dread(dset3, H5T_STD_U8LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, drbuf);
        CHECK(ret, FAIL, "H5Dread");

        for (tu8 = (uint8_t *)drbuf, i = 0; i < SPACE3_DIM1; i++, tu8++)
            VERIFY(*tu8, (uint8_t)(i * 3), "Data");

        /* Get the hyperslab selection */
        sid3 = H5Ropen_region(&rbuf[0], H5P_DEFAULT, H5P_DEFAULT);
        CHECK(sid3, H5I_INVALID_HID, "H5Ropen_region");

        /* Verify correct hyperslab selected */
        ret = (int)H5Sget_select_npoints(sid3);
        VERIFY(ret, 30, "H5Sget_select_npoints");
        ret = (int)H5Sget_select_hyper_nblocks(sid3);
        VERIFY(ret, 15, "H5Sget_select_hyper_nblocks");

        /* allocate space for the hyperslab blocks */
        coords = (hsize_t *)HDmalloc((size_t)ret * SPACE3_RANK * sizeof(hsize_t) * 2);

        ret = H5Sget_select_hyper_blocklist(sid3, (hsize_t)0, (hsize_t)ret, coords);
        CHECK(ret, FAIL, "H5Sget_select_hyper_blocklist");
        VERIFY(coords[0], 2, "Hyperslab Coordinates");
        VERIFY(coords[1], 3, "Hyperslab Coordinates");
        VERIFY(coords[2], 7, "Hyperslab Coordinates");
        VERIFY(coords[3], 8, "Hyperslab Coordinates");
        VERIFY(coords[4], 12, "Hyperslab Coordinates");
        VERIFY(coords[5], 13, "Hyperslab Coordinates");
        VERIFY(coords[6], 17, "Hyperslab Coordinates");
        VERIFY(coords[7], 18, "Hyperslab Coordinates");
        VERIFY(coords[8], 22, "Hyperslab Coordinates");
        VERIFY(coords[9], 23, "Hyperslab Coordinates");
        VERIFY(coords[10], 27, "Hyperslab Coordinates");
        VERIFY(coords[11], 28, "Hyperslab Coordinates");
        VERIFY(coords[12], 32, "Hyperslab Coordinates");
        VERIFY(coords[13], 33, "Hyperslab Coordinates");
        VERIFY(coords[14], 37, "Hyperslab Coordinates");
        VERIFY(coords[15], 38, "Hyperslab Coordinates");
        VERIFY(coords[16], 42, "Hyperslab Coordinates");
        VERIFY(coords[17], 43, "Hyperslab Coordinates");
        VERIFY(coords[18], 47, "Hyperslab Coordinates");
        VERIFY(coords[19], 48, "Hyperslab Coordinates");
        VERIFY(coords[20], 52, "Hyperslab Coordinates");
        VERIFY(coords[21], 53, "Hyperslab Coordinates");
        VERIFY(coords[22], 57, "Hyperslab Coordinates");
        VERIFY(coords[23], 58, "Hyperslab Coordinates");
        VERIFY(coords[24], 62, "Hyperslab Coordinates");
        VERIFY(coords[25], 63, "Hyperslab Coordinates");
        VERIFY(coords[26], 67, "Hyperslab Coordinates");
        VERIFY(coords[27], 68, "Hyperslab Coordinates");
        VERIFY(coords[28], 72, "Hyperslab Coordinates");
        VERIFY(coords[29], 73, "Hyperslab Coordinates");
        HDfree(coords);
        ret = H5Sget_select_bounds(sid3, low, high);
        CHECK(ret, FAIL, "H5Sget_select_bounds");
        VERIFY(low[0], 2, "Selection Bounds");
        VERIFY(high[0], 73, "Selection Bounds");

        /* Close region space */
        ret = H5Sclose(sid3);
        CHECK(ret, FAIL, "H5Sclose");

        /* Get the element selection */
        sid3 = H5Ropen_region(&rbuf[1], H5P_DEFAULT, H5P_DEFAULT);
        CHECK(sid3, H5I_INVALID_HID, "H5Ropen_region");

        /* Verify correct elements selected */
        ret = (int)H5Sget_select_npoints(sid3);
        VERIFY(ret, 10, "H5Sget_select_npoints");
        ret = (int)H5Sget_select_elem_npoints(sid3);
        VERIFY(ret, 10, "H5Sget_select_elem_npoints");

        /* allocate space for the element points */
        coords = (hsize_t *)HDmalloc((size_t)ret * SPACE3_RANK * sizeof(hsize_t));

        ret = H5Sget_select_elem_pointlist(sid3, (hsize_t)0, (hsize_t)ret, coords);
        CHECK(ret, FAIL, "H5Sget_select_elem_pointlist");
        VERIFY(coords[0], coord1[0][0], "Element Coordinates");
        VERIFY(coords[1], coord1[1][0], "Element Coordinates");
        VERIFY(coords[2], coord1[2][0], "Element Coordinates");
        VERIFY(coords[3], coord1[3][0], "Element Coordinates");
        VERIFY(coords[4], coord1[4][0], "Element Coordinates");
        VERIFY(coords[5], coord1[5][0], "Element Coordinates");
        VERIFY(coords[6], coord1[6][0], "Element Coordinates");
        VERIFY(coords[7], coord1[7][0], "Element Coordinates");
        VERIFY(coords[8], coord1[8][0], "Element Coordinates");
        VERIFY(coords[9], coord1[9][0], "Element Coordinates");
        HDfree(coords);
        ret = H5Sget_select_bounds(sid3, low, high);
        CHECK(ret, FAIL, "H5Sget_select_bounds");
        VERIFY(low[0], 3, "Selection Bounds");
        VERIFY(high[0], 97, "Selection Bounds");

        /* Close region space */
        ret = H5Sclose(sid3);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close first space */
        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close dereferenced Dataset */
        ret = H5Dclose(dset3);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close Dataset */
        ret = H5Dclose(dset1);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close dataset access property list */
        ret = H5Pclose(dapl_id);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file access property list */
        ret = H5Pclose(fapl);
        CHECK(ret, FAIL, "H5Pclose");

        /* Close file */
        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /* Destroy references */
        for (i = 0; i < 2; i++) {
            ret = H5Rdestroy(&wbuf[i]);
            CHECK(ret, FAIL, "H5Rdestroy");
            ret = H5Rdestroy(&rbuf[i]);
            CHECK(ret, FAIL, "H5Rdestroy");
        }
    }

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(dwbuf);
    HDfree(drbuf);

} /* test_reference_region_1D() */

/****************************************************************
**
**  test_reference_obj_deleted(): Test H5R (reference) object reference code.
**      Tests for correct failures for deleted and non-existent objects
**
****************************************************************/
static void
test_reference_obj_deleted(void)
{
    hid_t fid1;          /* HDF5 File IDs            */
    hid_t dataset,       /* Dataset ID               */
        dset2;           /* Dereferenced dataset ID  */
    hid_t      sid1;     /* Dataspace ID             */
    H5R_ref_t  oref;     /* Object Reference to test */
    H5O_type_t obj_type; /* Object type              */
    herr_t     ret;      /* Generic return value     */

    MESSAGE(5, ("Testing References to Deleted Objects\n"));

    if ((vol_cap_flags_g & H5VL_CAP_FLAG_REF_BASIC) && (vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) &&
        (vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) && (vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC)) {
        /* Create file */
        fid1 = H5Fcreate(FILE_REF_OBJ_DEL, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

        /* Create scalar dataspace for datasets */
        sid1 = H5Screate_simple(0, NULL, NULL);
        CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

        /* Create a dataset to reference (deleted later) */
        dataset = H5Dcreate2(fid1, "Dataset1", H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

        /* Close Dataset */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");

        /* Create a dataset */
        dataset = H5Dcreate2(fid1, "Dataset2", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

        /* Create reference to dataset */
        ret = H5Rcreate_object(fid1, "/Dataset1", H5P_DEFAULT, &oref);
        CHECK(ret, FAIL, "H5Rcreate_object");
        ret = H5Rget_obj_type3(&oref, H5P_DEFAULT, &obj_type);
        CHECK(ret, FAIL, "H5Rget_obj_type3");
        VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

        /* Write selection to disk */
        ret = H5Dwrite(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, &oref);
        CHECK(ret, FAIL, "H5Dwrite");

        /* Close Dataset */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");

        /* Delete referenced dataset */
        ret = H5Ldelete(fid1, "/Dataset1", H5P_DEFAULT);
        CHECK(ret, FAIL, "H5Ldelete");

        /* Close disk dataspace */
        ret = H5Sclose(sid1);
        CHECK(ret, FAIL, "H5Sclose");

        /* Close file */
        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /* Destroy reference */
        ret = H5Rdestroy(&oref);
        CHECK(ret, FAIL, "H5Rdestroy");

        /* Re-open the file */
        fid1 = H5Fopen(FILE_REF_OBJ_DEL, H5F_ACC_RDWR, H5P_DEFAULT);
        CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

        /* Open the dataset */
        dataset = H5Dopen2(fid1, "/Dataset2", H5P_DEFAULT);
        CHECK(ret, H5I_INVALID_HID, "H5Dopen2");

        /* Read selection from disk */
        ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, &oref);
        CHECK(ret, FAIL, "H5Dread");

        /* Open deleted dataset object */
        dset2 = H5Ropen_object(&oref, H5P_DEFAULT, H5P_DEFAULT);
        VERIFY(dset2, H5I_INVALID_HID, "H5Ropen_object");

        /* Close Dataset */
        ret = H5Dclose(dataset);
        CHECK(ret, FAIL, "H5Dclose");

        /* Close file */
        ret = H5Fclose(fid1);
        CHECK(ret, FAIL, "H5Fclose");

        /* Destroy reference */
        ret = H5Rdestroy(&oref);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
} /* test_reference_obj_deleted() */

/****************************************************************
**
**  test_deref_iter_op(): Iterator callback for test_reference_group_iterate()
**      test.
**
****************************************************************/
static herr_t
test_deref_iter_op(hid_t H5_ATTR_UNUSED group, const char *name, const H5L_info2_t H5_ATTR_UNUSED *info,
                   void *op_data)
{
    int   *count = (int *)op_data; /* Pointer to name counter */
    herr_t ret_value;

    /* Simple check for correct names */
    if (*count == 0) {
        if (HDstrcmp(name, DSETNAME2) == 0)
            ret_value = 0;
        else
            ret_value = -1;
    } /* end if */
    else if (*count == 1) {
        if (HDstrcmp(name, GROUPNAME2) == 0)
            ret_value = 0;
        else
            ret_value = -1;
    } /* end if */
    else if (*count == 2) {
        if (HDstrcmp(name, GROUPNAME3) == 0)
            ret_value = 0;
        else
            ret_value = -1;
    } /* end if */
    else
        ret_value = -1;

    (*count)++;

    return (ret_value);
} /* end test_deref_iter_op() */

/****************************************************************
**
**  test_reference_group(): Test H5R (reference) object reference code.
**      Tests for correct behavior of various routines on dereferenced group
**
****************************************************************/
static void
test_reference_group(void)
{
    hid_t       fid = -1;            /* File ID */
    hid_t       gid = -1, gid2 = -1; /* Group IDs */
    hid_t       did;                 /* Dataset ID */
    hid_t       sid;                 /* Dataspace ID */
    H5R_ref_t   wref;                /* Reference to write */
    H5R_ref_t   rref;                /* Reference to read */
    H5G_info_t  ginfo;               /* Group info struct */
    char        objname[NAME_SIZE];  /* Buffer to store name */
    H5O_info2_t oinfo;               /* Object info struct */
    int         count = 0;           /* Count within iterated group */
    ssize_t     size;                /* Name length */
    herr_t      ret;

    /* Create file with a group and a dataset containing an object reference to the group */
    fid = H5Fcreate(FILE_REF_GRP, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace to use for dataset */
    sid = H5Screate(H5S_SCALAR);
    CHECK(sid, H5I_INVALID_HID, "H5Screate");

    /* Create group to refer to */
    gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, H5I_INVALID_HID, "H5Gcreate2");

    /* Create nested groups */
    gid2 = H5Gcreate2(gid, GROUPNAME2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, H5I_INVALID_HID, "H5Gcreate2");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    gid2 = H5Gcreate2(gid, GROUPNAME3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid2, H5I_INVALID_HID, "H5Gcreate2");
    ret = H5Gclose(gid2);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create bottom dataset */
    did = H5Dcreate2(gid, DSETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, H5I_INVALID_HID, "H5Dcreate2");
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");

    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create dataset */
    did = H5Dcreate2(fid, DSETNAME, H5T_STD_REF, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(did, H5I_INVALID_HID, "H5Dcreate2");

    /* Create reference to group */
    ret = H5Rcreate_object(fid, GROUPNAME, H5P_DEFAULT, &wref);
    CHECK(ret, FAIL, "H5Rcreate_object");

    /* Write reference to disk */
    ret = H5Dwrite(did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wref);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close objects */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy reference */
    ret = H5Rdestroy(&wref);
    CHECK(ret, FAIL, "H5Rdestroy");

    /* Re-open file */
    fid = H5Fopen(FILE_REF_GRP, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, H5I_INVALID_HID, "H5Fopen");

    /* Re-open dataset */
    did = H5Dopen2(fid, DSETNAME, H5P_DEFAULT);
    CHECK(did, H5I_INVALID_HID, "H5Dopen2");

    /* Read in the reference */
    ret = H5Dread(did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rref);
    CHECK(ret, FAIL, "H5Dread");

    /* Dereference to get the group */
    gid = H5Ropen_object(&rref, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(gid, H5I_INVALID_HID, "H5Ropen_object");

    /* Iterate through objects in dereferenced group */
    ret = H5Literate2(gid, H5_INDEX_NAME, H5_ITER_INC, NULL, test_deref_iter_op, &count);
    CHECK(ret, FAIL, "H5Literate");

    /* Various queries on the group opened */
    ret = H5Gget_info(gid, &ginfo);
    CHECK(ret, FAIL, "H5Gget_info");
    VERIFY(ginfo.nlinks, 3, "H5Gget_info");

    size = H5Lget_name_by_idx(gid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, objname, (size_t)NAME_SIZE,
                              H5P_DEFAULT);
    CHECK(size, (-1), "H5Lget_name_by_idx");
    VERIFY_STR(objname, DSETNAME2, "H5Lget_name_by_idx");

    ret = H5Oget_info_by_idx3(gid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)0, &oinfo, H5O_INFO_BASIC,
                              H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Oget_info_by_idx3");
    VERIFY(oinfo.type, H5O_TYPE_DATASET, "H5Oget_info_by_idx3");

    /* Unlink one of the objects in the dereferenced group */
    ret = H5Ldelete(gid, GROUPNAME2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Delete dataset object in dereferenced group (with other dataset still open) */
    ret = H5Ldelete(gid, DSETNAME2, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Ldelete");

    /* Close objects */
    ret = H5Dclose(did);
    CHECK(ret, FAIL, "H5Dclose");
    ret = H5Gclose(gid);
    CHECK(ret, FAIL, "H5Gclose");
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy reference */
    ret = H5Rdestroy(&rref);
    CHECK(ret, FAIL, "H5Rdestroy");
} /* test_reference_group() */

/****************************************************************
**
**  test_reference_attr(): Test basic H5R (reference) attribute reference code.
**      Tests references to attributes on various kinds of objects
**
****************************************************************/
static void
test_reference_attr(void)
{
    hid_t     fid;     /* HDF5 File ID */
    hid_t     dataset; /* Dataset ID */
    hid_t     group;   /* Group ID */
    hid_t     attr;    /* Attribute ID */
    hid_t     sid;     /* Dataspace ID */
    hid_t     tid;     /* Datatype ID */
    hsize_t   dims[] = {SPACE1_DIM1};
    hid_t     dapl_id;               /* Dataset access property list */
    H5R_ref_t ref_wbuf[SPACE1_DIM1], /* Buffer to write to disk */
        ref_rbuf[SPACE1_DIM1];       /* Buffer read from disk */
    unsigned   wbuf[SPACE1_DIM1], rbuf[SPACE1_DIM1];
    unsigned   i;        /* Local index variables */
    H5O_type_t obj_type; /* Object type */
    herr_t     ret;      /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Attribute Reference Functions\n"));

    /* Create file */
    fid = H5Fcreate(FILE_REF_ATTR, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(group, "Attr2", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    for (i = 0; i < SPACE1_DIM1; i++)
        wbuf[i] = (i * 3) + 1;

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, wbuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(dataset, "Attr1", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    for (i = 0; i < SPACE1_DIM1; i++)
        wbuf[i] = i * 3;

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, wbuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create an attribute for the datatype */
    attr = H5Acreate2(tid, "Attr3", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    for (i = 0; i < SPACE1_DIM1; i++)
        wbuf[i] = (i * 3) + 2;

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, wbuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid, "Dataset3", H5T_STD_REF, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create reference to dataset1 attribute */
    ret = H5Rcreate_attr(fid, "/Group1/Dataset1", "Attr1", H5P_DEFAULT, &ref_wbuf[0]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to dataset2 attribute */
    ret = H5Rcreate_attr(fid, "/Group1/Dataset2", "Attr1", H5P_DEFAULT, &ref_wbuf[1]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[1], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to group attribute */
    ret = H5Rcreate_attr(fid, "/Group1", "Attr2", H5P_DEFAULT, &ref_wbuf[2]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[2], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type3");

    /* Create reference to named datatype attribute */
    ret = H5Rcreate_attr(fid, "/Group1/Datatype1", "Attr3", H5P_DEFAULT, &ref_wbuf[3]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[3], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type3");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid = H5Fopen(FILE_REF_ATTR, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid, "/Dataset3", H5P_DEFAULT);
    CHECK(ret, H5I_INVALID_HID, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Open attribute on dataset object */
    attr = H5Ropen_attr(&ref_rbuf[0], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Ropen_attr");

    /* Check information in referenced dataset */
    sid = H5Aget_space(attr);
    CHECK(sid, H5I_INVALID_HID, "H5Aget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid);
    VERIFY(ret, SPACE1_DIM1, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Aread(attr, H5T_NATIVE_UINT, rbuf);
    CHECK(ret, FAIL, "H5Aread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(rbuf[i], i * 3, "Data");

    /* Close dereferenced Dataset */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open attribute on group object */
    attr = H5Ropen_attr(&ref_rbuf[2], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Ropen_attr");

    /* Read from disk */
    ret = H5Aread(attr, H5T_NATIVE_UINT, rbuf);
    CHECK(ret, FAIL, "H5Aread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(rbuf[i], (i * 3) + 1, "Data");

    /* Close attribute  */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open attribute on named datatype object */
    attr = H5Ropen_attr(&ref_rbuf[3], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Ropen_attr");

    /* Read from disk */
    ret = H5Aread(attr, H5T_NATIVE_UINT, rbuf);
    CHECK(ret, FAIL, "H5Aread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(rbuf[i], (i * 3) + 2, "Data");

    /* Close attribute  */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    for (i = 0; i < SPACE1_DIM1; i++) {
        ret = H5Rdestroy(&ref_wbuf[i]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&ref_rbuf[i]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
} /* test_reference_attr() */

/****************************************************************
**
**  test_reference_external():
**      Tests external references on various kinds of objects
**
****************************************************************/
static void
test_reference_external(void)
{
    hid_t     fid1, fid2; /* HDF5 File ID */
    hid_t     dataset;    /* Dataset ID */
    hid_t     group;      /* Group ID */
    hid_t     attr;       /* Attribute ID */
    hid_t     sid;        /* Dataspace ID */
    hid_t     tid;        /* Datatype ID */
    hsize_t   dims[] = {SPACE1_DIM1};
    hid_t     dapl_id;               /* Dataset access property list */
    H5R_ref_t ref_wbuf[SPACE1_DIM1], /* Buffer to write to disk */
        ref_rbuf[SPACE1_DIM1];       /* Buffer read from disk */
    unsigned   wbuf[SPACE1_DIM1], rbuf[SPACE1_DIM1];
    unsigned   i;        /* Local index variables */
    H5O_type_t obj_type; /* Object type */
    herr_t     ret;      /* Generic return value */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing External References Functions\n"));

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_EXT1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(group, "Attr2", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    for (i = 0; i < SPACE1_DIM1; i++)
        wbuf[i] = (i * 3) + 1;

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, wbuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create an attribute for the dataset */
    attr = H5Acreate2(dataset, "Attr1", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    for (i = 0; i < SPACE1_DIM1; i++)
        wbuf[i] = i * 3;

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, wbuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Create an attribute for the datatype */
    attr = H5Acreate2(tid, "Attr3", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Acreate2");

    for (i = 0; i < SPACE1_DIM1; i++)
        wbuf[i] = (i * 3) + 2;

    /* Write attribute to disk */
    ret = H5Awrite(attr, H5T_NATIVE_UINT, wbuf);
    CHECK(ret, FAIL, "H5Awrite");

    /* Close attribute */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close datatype */
    ret = H5Tclose(tid);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create reference to dataset1 attribute */
    ret = H5Rcreate_attr(fid1, "/Group1/Dataset1", "Attr1", H5P_DEFAULT, &ref_wbuf[0]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to dataset2 attribute */
    ret = H5Rcreate_attr(fid1, "/Group1/Dataset2", "Attr1", H5P_DEFAULT, &ref_wbuf[1]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[1], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Create reference to group attribute */
    ret = H5Rcreate_attr(fid1, "/Group1", "Attr2", H5P_DEFAULT, &ref_wbuf[2]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[2], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type3");

    /* Create reference to named datatype attribute */
    ret = H5Rcreate_attr(fid1, "/Group1/Datatype1", "Attr3", H5P_DEFAULT, &ref_wbuf[3]);
    CHECK(ret, FAIL, "H5Rcreate_attr");
    ret = H5Rget_obj_type3(&ref_wbuf[3], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type3");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Create file */
    fid2 = H5Fcreate(FILE_REF_EXT2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid2, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid = H5Screate_simple(SPACE1_RANK, dims, NULL);
    CHECK(sid, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a dataset */
    dataset = H5Dcreate2(fid2, "Dataset3", H5T_STD_REF, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_wbuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close disk dataspace */
    ret = H5Sclose(sid);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid2 = H5Fopen(FILE_REF_EXT2, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid2, FAIL, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid2, "/Dataset3", H5P_DEFAULT);
    CHECK(ret, H5I_INVALID_HID, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Open attribute on dataset object */
    attr = H5Ropen_attr(&ref_rbuf[0], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Ropen_attr");

    /* Check information in referenced dataset */
    sid = H5Aget_space(attr);
    CHECK(sid, H5I_INVALID_HID, "H5Aget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid);
    VERIFY(ret, SPACE1_DIM1, "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Aread(attr, H5T_NATIVE_UINT, rbuf);
    CHECK(ret, FAIL, "H5Aread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(rbuf[i], i * 3, "Data");

    /* Close dereferenced Dataset */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open attribute on group object */
    attr = H5Ropen_attr(&ref_rbuf[2], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Ropen_attr");

    /* Read from disk */
    ret = H5Aread(attr, H5T_NATIVE_UINT, rbuf);
    CHECK(ret, FAIL, "H5Aread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(rbuf[i], (i * 3) + 1, "Data");

    /* Close attribute  */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Open attribute on named datatype object */
    attr = H5Ropen_attr(&ref_rbuf[3], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(attr, H5I_INVALID_HID, "H5Ropen_attr");

    /* Read from disk */
    ret = H5Aread(attr, H5T_NATIVE_UINT, rbuf);
    CHECK(ret, FAIL, "H5Aread");

    for (i = 0; i < SPACE1_DIM1; i++)
        VERIFY(rbuf[i], (i * 3) + 2, "Data");

    /* Close attribute  */
    ret = H5Aclose(attr);
    CHECK(ret, FAIL, "H5Aclose");

    /* Close dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid2);
    CHECK(ret, FAIL, "H5Fclose");

    /* Free memory buffers */
    for (i = 0; i < SPACE1_DIM1; i++) {
        ret = H5Rdestroy(&ref_wbuf[i]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&ref_rbuf[i]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
} /* test_reference_external() */

/****************************************************************
**
**  test_reference_compat_conv(): Test basic H5R (reference) object reference code.
**      Tests deprecated API routines and type conversion.
**
****************************************************************/
#if 0
static void
test_reference_compat_conv(void)
{
    hid_t   fid1;             /* HDF5 File IDs            */
    hid_t   dataset, dset2;   /* Dataset ID               */
    hid_t   group, group2;    /* Group ID                 */
    hid_t   sid1, sid2, sid3; /* Dataspace IDs            */
    hid_t   tid1, tid2;       /* Datatype ID              */
    hsize_t dims1[] = {SPACE1_DIM1}, dims2[] = {SPACE2_DIM1, SPACE2_DIM2},
            dims3[] = {SPACE1_DIM1};      /* Purposely set dimension larger to test NULL references */
    hsize_t          start[SPACE2_RANK];  /* Starting location of hyperslab */
    hsize_t          stride[SPACE2_RANK]; /* Stride of hyperslab      */
    hsize_t          count[SPACE2_RANK];  /* Element count of hyperslab */
    hsize_t          block[SPACE2_RANK];  /* Block size of hyperslab  */
    hsize_t          coord1[POINT1_NPOINTS][SPACE2_RANK]; /* Coordinates for point selection */
    hobj_ref_t      *wbuf_obj = NULL;                     /* Buffer to write to disk  */
    H5R_ref_t       *rbuf_obj = NULL;                     /* Buffer read from disk    */
    hdset_reg_ref_t *wbuf_reg = NULL;                     /* Buffer to write to disk  */
    H5R_ref_t       *rbuf_reg = NULL;                     /* Buffer read from disk    */
    H5O_type_t       obj_type;                            /* Object type              */
    herr_t           ret;                                 /* Generic return value     */
    unsigned int     i;                                   /* Counter                  */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Deprecated Object Reference Functions\n"));

    /* Allocate write & read buffers */
    wbuf_obj = (hobj_ref_t *)HDcalloc(sizeof(hobj_ref_t), SPACE1_DIM1);
    rbuf_obj = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    wbuf_reg = HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf_reg = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_COMPAT, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create another dataspace for datasets */
    sid2 = H5Screate_simple(SPACE2_RANK, dims2, NULL);
    CHECK(sid2, H5I_INVALID_HID, "H5Screate_simple");

    /* Create another dataspace for datasets */
    sid3 = H5Screate_simple(SPACE1_RANK, dims3, NULL);
    CHECK(sid3, H5I_INVALID_HID, "H5Screate_simple");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset with object reference datatype */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf_obj[0], fid1, "/Group1/Dataset1", H5R_OBJECT, H5I_INVALID_HID);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf_obj[1], fid1, "/Group1/Dataset2", H5R_OBJECT, H5I_INVALID_HID);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Create reference to group */
    ret = H5Rcreate(&wbuf_obj[2], fid1, "/Group1", H5R_OBJECT, H5I_INVALID_HID);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Create reference to named datatype */
    ret = H5Rcreate(&wbuf_obj[3], fid1, "/Group1/Datatype1", H5R_OBJECT, H5I_INVALID_HID);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write references to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_obj);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a dataset with region reference datatype */
    dataset = H5Dcreate2(fid1, "Dataset4", H5T_STD_REF_DSETREG, sid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Select 6x6 hyperslab for first reference */
    start[0]  = 2;
    start[1]  = 2;
    stride[0] = 1;
    stride[1] = 1;
    count[0]  = 1;
    count[1]  = 1;
    block[0]  = 6;
    block[1]  = 6;
    ret       = H5Sselect_hyperslab(sid2, H5S_SELECT_SET, start, stride, count, block);
    CHECK(ret, FAIL, "H5Sselect_hyperslab");

    /* Create first dataset region */
    ret = H5Rcreate(&wbuf_reg[0], fid1, "/Group1/Dataset1", H5R_DATASET_REGION, sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Select sequence of ten points for second reference */
    coord1[0][0] = 6;
    coord1[0][1] = 9;
    coord1[1][0] = 2;
    coord1[1][1] = 2;
    coord1[2][0] = 8;
    coord1[2][1] = 4;
    coord1[3][0] = 1;
    coord1[3][1] = 6;
    coord1[4][0] = 2;
    coord1[4][1] = 8;
    coord1[5][0] = 3;
    coord1[5][1] = 2;
    coord1[6][0] = 0;
    coord1[6][1] = 4;
    coord1[7][0] = 9;
    coord1[7][1] = 0;
    coord1[8][0] = 7;
    coord1[8][1] = 1;
    coord1[9][0] = 3;
    coord1[9][1] = 3;
    ret          = H5Sselect_elements(sid2, H5S_SELECT_SET, (size_t)POINT1_NPOINTS, (const hsize_t *)coord1);
    CHECK(ret, FAIL, "H5Sselect_elements");

    /* Create second dataset region */
    ret = H5Rcreate(&wbuf_reg[1], fid1, "/Group1/Dataset2", H5R_DATASET_REGION, sid2);
    CHECK(ret, FAIL, "H5Rcreate");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_reg);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close disk dataspaces */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid2);
    CHECK(ret, FAIL, "H5Sclose");
    ret = H5Sclose(sid3);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE_REF_COMPAT, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

    /* Open the object reference dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_obj);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify type of objects pointed at */
    ret = H5Rget_obj_type3(&rbuf_obj[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    ret = H5Rget_obj_type3(&rbuf_obj[1], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    ret = H5Rget_obj_type3(&rbuf_obj[2], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_GROUP, "H5Rget_obj_type3");

    ret = H5Rget_obj_type3(&rbuf_obj[3], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_NAMED_DATATYPE, "H5Rget_obj_type3");

    /* Make sure the referenced objects can be opened */
    dset2 = H5Ropen_object(&rbuf_obj[0], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    dset2 = H5Ropen_object(&rbuf_obj[1], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    group2 = H5Ropen_object(&rbuf_obj[2], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group2, H5I_INVALID_HID, "H5Ropen_object");

    ret = H5Gclose(group2);
    CHECK(ret, FAIL, "H5Gclose");

    tid2 = H5Ropen_object(&rbuf_obj[3], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(tid2, H5I_INVALID_HID, "H5Ropen_object");

    ret = H5Tclose(tid2);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Open the dataset region reference dataset */
    dataset = H5Dopen2(fid1, "/Dataset4", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_reg);
    CHECK(ret, FAIL, "H5Dread");

    /* Verify type of objects pointed at */
    ret = H5Rget_obj_type3(&rbuf_reg[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    ret = H5Rget_obj_type3(&rbuf_reg[1], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    /* Make sure the referenced objects can be opened */
    dset2 = H5Ropen_object(&rbuf_reg[0], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    dset2 = H5Ropen_object(&rbuf_reg[1], H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy references */
    for (i = 0; i < dims1[0]; i++) {
        ret = H5Rdestroy(&rbuf_obj[i]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
    for (i = 0; i < dims3[0]; i++) {
        ret = H5Rdestroy(&rbuf_reg[i]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }

    /* Free memory buffers */
    HDfree(wbuf_obj);
    HDfree(rbuf_obj);
    HDfree(wbuf_reg);
    HDfree(rbuf_reg);
} /* test_reference_compat() */
#endif

/****************************************************************
**
**  test_reference_perf(): Test basic H5R (reference) object reference
**  performance.
**
****************************************************************/
static void
test_reference_perf(void)
{
    hid_t fid1;       /* HDF5 File IDs                    */
    hid_t dataset,    /* Dataset ID                       */
        dset2;        /* Dereferenced dataset ID          */
    hid_t      group; /* Group ID                         */
    hid_t      sid1;  /* Dataspace ID                     */
    hid_t      tid1;  /* Datatype ID                      */
    hsize_t    dims1[] = {1};
    hid_t      dapl_id;               /* Dataset access property list     */
    H5R_ref_t *wbuf,                  /* buffer to write to disk          */
        *rbuf,                        /* buffer read from disk            */
        *tbuf;                        /* temp. buffer read from disk      */
    H5R_ref_t *wbuf_reg,              /* buffer to write to disk          */
        *rbuf_reg;                    /* buffer read from disk            */
    hobj_ref_t *wbuf_deprec,          /* deprecated references            */
        *rbuf_deprec;                 /* deprecated references            */
    hdset_reg_ref_t *wbuf_reg_deprec, /* deprecated references*/
        *rbuf_reg_deprec;             /* deprecated references*/
    unsigned  *ibuf, *obuf;
    unsigned   i, j;      /* Counters                         */
    H5O_type_t obj_type;  /* Object type                      */
    herr_t     ret;       /* Generic return value             */
    double     t1, t2, t; /* Timers                           */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing Object Reference Performance\n"));

    /* Allocate write & read buffers */
    wbuf            = HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    obuf            = HDcalloc(sizeof(unsigned), SPACE1_DIM1);
    ibuf            = HDcalloc(sizeof(unsigned), SPACE1_DIM1);
    wbuf_deprec     = (hobj_ref_t *)HDcalloc(sizeof(hobj_ref_t), SPACE1_DIM1);
    rbuf            = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    rbuf_deprec     = (hobj_ref_t *)HDcalloc(sizeof(hobj_ref_t), SPACE1_DIM1);
    tbuf            = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    wbuf_reg        = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    rbuf_reg        = (H5R_ref_t *)HDcalloc(sizeof(H5R_ref_t), SPACE1_DIM1);
    wbuf_reg_deprec = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);
    rbuf_reg_deprec = (hdset_reg_ref_t *)HDcalloc(sizeof(hdset_reg_ref_t), SPACE1_DIM1);

    for (i = 0; i < SPACE1_DIM1; i++)
        obuf[i] = i * 3;

    /* Create file */
    fid1 = H5Fcreate(FILE_REF_OBJ, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fcreate");

    /* Create dataspace for datasets */
    sid1 = H5Screate_simple(SPACE1_RANK, dims1, NULL);
    CHECK(sid1, H5I_INVALID_HID, "H5Screate_simple");

    /* Create dataset access property list */
    dapl_id = H5Pcreate(H5P_DATASET_ACCESS);
    CHECK(dapl_id, H5I_INVALID_HID, "H5Pcreate");

    /* Create a group */
    group = H5Gcreate2(fid1, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(group, H5I_INVALID_HID, "H5Gcreate2");

    /* Create a dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset1", H5T_NATIVE_UINT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    /* Write selection to disk */
    ret = H5Dwrite(dataset, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, obuf);
    CHECK(ret, FAIL, "H5Dwrite");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create another dataset (inside Group1) */
    dataset = H5Dcreate2(group, "Dataset2", H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, FAIL, "H5Dcreate2");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Create a datatype to refer to */
    tid1 = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(tid1, H5I_INVALID_HID, "H5Tcreate");

    /* Insert fields */
    ret = H5Tinsert(tid1, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert");

    ret = H5Tinsert(tid1, "c", HOFFSET(s1_t, c), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert");

    /* Save datatype for later */
    ret = H5Tcommit2(group, "Datatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(ret, FAIL, "H5Tcommit2");

    /* Close datatype */
    ret = H5Tclose(tid1);
    CHECK(ret, FAIL, "H5Tclose");

    /* Close group */
    ret = H5Gclose(group);
    CHECK(ret, FAIL, "H5Gclose");

    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset3", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    t = 0;
    for (i = 0; i < MAX_ITER_CREATE; i++) {
        t1  = H5_get_time();
        ret = H5Rcreate_object(fid1, "/Group1/Dataset1", H5P_DEFAULT, &wbuf[0]);
        CHECK(ret, FAIL, "H5Rcreate_object");
        t2 = H5_get_time();
        t += t2 - t1;
        ret = H5Rdestroy(&wbuf[0]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
    if (VERBOSE_MED)
        HDprintf("--- Object reference create time: %lfs\n", t / MAX_ITER_CREATE);

    /* Create reference to dataset */
    ret = H5Rcreate_object(fid1, "/Group1/Dataset1", H5P_DEFAULT, &wbuf[0]);
    CHECK(ret, FAIL, "H5Rcreate_object");
    ret = H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type);
    CHECK(ret, FAIL, "H5Rget_obj_type3");
    VERIFY(obj_type, H5O_TYPE_DATASET, "H5Rget_obj_type3");

    t = 0;
    for (i = 0; i < MAX_ITER_WRITE; i++) {
        t1 = H5_get_time();
        /* Write selection to disk */
        ret = H5Dwrite(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf);
        CHECK(ret, FAIL, "H5Dwrite");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Object reference  write time: %lfs\n", t / MAX_ITER_WRITE);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#if 0
    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset4", H5T_STD_REF_OBJ, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    t = 0;
    for (i = 0; i < MAX_ITER_CREATE; i++) {
        t1  = H5_get_time();
        ret = H5Rcreate(&wbuf_deprec[0], fid1, "/Group1/Dataset1", H5R_OBJECT1, H5I_INVALID_HID);
        CHECK(ret, FAIL, "H5Rcreate");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Deprecated object reference create time: %lfs\n", t / MAX_ITER_CREATE);

    /* Create reference to dataset */
    ret = H5Rcreate(&wbuf_deprec[0], fid1, "/Group1/Dataset1", H5R_OBJECT1, H5I_INVALID_HID);
    CHECK(ret, FAIL, "H5Rcreate");

    t = 0;
    for (i = 0; i < MAX_ITER_WRITE; i++) {
        t1 = H5_get_time();
        /* Write selection to disk */
        ret = H5Dwrite(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_deprec);
        CHECK(ret, FAIL, "H5Dwrite");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Deprecated object reference  write time: %lfs\n", t / MAX_ITER_WRITE);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#endif
    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset5", H5T_STD_REF, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    t = 0;
    for (i = 0; i < MAX_ITER_CREATE; i++) {
        t1 = H5_get_time();
        /* Store first dataset region */
        ret = H5Rcreate_region(fid1, "/Group1/Dataset1", sid1, H5P_DEFAULT, &wbuf_reg[0]);
        CHECK(ret, FAIL, "H5Rcreate_region");
        t2 = H5_get_time();
        t += t2 - t1;
        ret = H5Rdestroy(&wbuf_reg[0]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
    if (VERBOSE_MED)
        HDprintf("--- Region reference create time: %lfs\n", t / MAX_ITER_CREATE);

    /* Store first dataset region */
    ret = H5Rcreate_region(fid1, "/Group1/Dataset1", sid1, H5P_DEFAULT, &wbuf_reg[0]);
    CHECK(ret, FAIL, "H5Rcreate_region");

    t = 0;
    for (i = 0; i < MAX_ITER_WRITE; i++) {
        t1 = H5_get_time();
        /* Write selection to disk */
        ret = H5Dwrite(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_reg);
        CHECK(ret, FAIL, "H5Dwrite");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Region reference  write time: %lfs\n", t / MAX_ITER_WRITE);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#if 0
    /* Create a dataset */
    dataset = H5Dcreate2(fid1, "Dataset6", H5T_STD_REF_DSETREG, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dcreate2");

    t = 0;
    for (i = 0; i < MAX_ITER_CREATE; i++) {
        t1 = H5_get_time();
        /* Store first dataset region */
        ret = H5Rcreate(&wbuf_reg_deprec[0], fid1, "/Group1/Dataset1", H5R_DATASET_REGION1, sid1);
        CHECK(ret, FAIL, "H5Rcreate");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Deprecated region reference create time: %lfs\n", t / MAX_ITER_CREATE);

    t = 0;
    for (i = 0; i < MAX_ITER_WRITE; i++) {
        t1 = H5_get_time();
        /* Write selection to disk */
        ret = H5Dwrite(dataset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf_reg_deprec);
        CHECK(ret, FAIL, "H5Dwrite");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Deprecated region reference  write time: %lfs\n", t / MAX_ITER_WRITE);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#endif
    /* Close disk dataspace */
    ret = H5Sclose(sid1);
    CHECK(ret, FAIL, "H5Sclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Re-open the file */
    fid1 = H5Fopen(FILE_REF_OBJ, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid1, H5I_INVALID_HID, "H5Fopen");

    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset3", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    t = 0;
    for (i = 0; i < MAX_ITER_READ; i++) {
        t1 = H5_get_time();
        /* Read selection from disk */
        ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
        CHECK(ret, FAIL, "H5Dread");
        t2 = H5_get_time();
        t += t2 - t1;
        ret = H5Rdestroy(&rbuf[0]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
    if (VERBOSE_MED)
        HDprintf("--- Object reference read time: %lfs\n", t / MAX_ITER_READ);

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf);
    CHECK(ret, FAIL, "H5Dread");

    /* Open dataset object */
    dset2 = H5Ropen_object(&rbuf[0], H5P_DEFAULT, dapl_id);
    CHECK(dset2, H5I_INVALID_HID, "H5Ropen_object");

    /* Check information in referenced dataset */
    sid1 = H5Dget_space(dset2);
    CHECK(sid1, H5I_INVALID_HID, "H5Dget_space");

    ret = (int)H5Sget_simple_extent_npoints(sid1);
    VERIFY(ret, dims1[0], "H5Sget_simple_extent_npoints");

    /* Read from disk */
    ret = H5Dread(dset2, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, ibuf);
    CHECK(ret, FAIL, "H5Dread");

    for (i = 0; i < dims1[0]; i++)
        VERIFY(ibuf[i], i * 3, "Data");

    /* Close dereferenced Dataset */
    ret = H5Dclose(dset2);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#if 0
    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset4", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    t = 0;
    for (i = 0; i < MAX_ITER_READ; i++) {
        t1 = H5_get_time();
        /* Read selection from disk */
        ret = H5Dread(dataset, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_deprec);
        CHECK(ret, FAIL, "H5Dread");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Deprecated object reference read time: %lfs\n", t / MAX_ITER_READ);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#endif
    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset5", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    t = 0;
    for (i = 0; i < MAX_ITER_READ; i++) {
        t1 = H5_get_time();
        /* Read selection from disk */
        ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_reg);
        CHECK(ret, FAIL, "H5Dread");
        t2 = H5_get_time();
        t += t2 - t1;
        ret = H5Rdestroy(&rbuf_reg[0]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }
    if (VERBOSE_MED)
        HDprintf("--- Region reference read time: %lfs\n", t / MAX_ITER_READ);

    /* Read selection from disk */
    ret = H5Dread(dataset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_reg);
    CHECK(ret, FAIL, "H5Dread");

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#if 0
    /* Open the dataset */
    dataset = H5Dopen2(fid1, "/Dataset6", H5P_DEFAULT);
    CHECK(dataset, H5I_INVALID_HID, "H5Dopen2");

    t = 0;
    for (i = 0; i < MAX_ITER_READ; i++) {
        t1 = H5_get_time();
        /* Read selection from disk */
        ret = H5Dread(dataset, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf_reg_deprec);
        CHECK(ret, FAIL, "H5Dread");
        t2 = H5_get_time();
        t += t2 - t1;
    }
    if (VERBOSE_MED)
        HDprintf("--- Deprecated region reference read time: %lfs\n", t / MAX_ITER_READ);

    /* Close Dataset */
    ret = H5Dclose(dataset);
    CHECK(ret, FAIL, "H5Dclose");
#endif
    /* Close dataset access property list */
    ret = H5Pclose(dapl_id);
    CHECK(ret, FAIL, "H5Pclose");

    /* Close file */
    ret = H5Fclose(fid1);
    CHECK(ret, FAIL, "H5Fclose");

    /* Destroy references */
    for (j = 0; j < dims1[0]; j++) {
        ret = H5Rdestroy(&wbuf[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&wbuf_reg[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&rbuf[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
        ret = H5Rdestroy(&rbuf_reg[j]);
        CHECK(ret, FAIL, "H5Rdestroy");
    }

    /* Free memory buffers */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(wbuf_reg);
    HDfree(rbuf_reg);
    HDfree(wbuf_deprec);
    HDfree(rbuf_deprec);
    HDfree(wbuf_reg_deprec);
    HDfree(rbuf_reg_deprec);
    HDfree(tbuf);
    HDfree(ibuf);
    HDfree(obuf);
} /* test_reference_perf() */

/****************************************************************
**
**  test_reference(): Main H5R reference testing routine.
**
****************************************************************/
void
test_reference(void)
{
    H5F_libver_t low, high;   /* Low and high bounds */
    const char  *env_h5_drvr; /* File Driver value from environment */

    /* Output message about test being performed */
    MESSAGE(5, ("Testing References\n"));

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv(HDF5_DRIVER);
    if (env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    test_reference_params();    /* Test for correct parameter checking */
    test_reference_obj();       /* Test basic H5R object reference code */
    test_reference_vlen_obj();  /* Test reference within vlen */
    test_reference_cmpnd_obj(); /* Test reference within compound type */

    /* Loop through all the combinations of low/high version bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Invalid combinations, just continue */
            if (high == H5F_LIBVER_EARLIEST || high < low)
                continue;

            test_reference_region(low, high);    /* Test basic H5R dataset region reference code */
            test_reference_region_1D(low, high); /* Test H5R dataset region reference code for 1-D datasets */

        } /* end high bound */
    }     /* end low bound */

    /* The following test is currently broken with the Direct VFD */
    if (HDstrcmp(env_h5_drvr, "direct") != 0) {
        test_reference_obj_deleted(); /* Test H5R object reference code for deleted objects */
    }

    test_reference_group();    /* Test operations on dereferenced groups */
    test_reference_attr();     /* Test attribute references */
    test_reference_external(); /* Test external references */
#if 0
    test_reference_compat_conv();   /* Test operations with old types */
#endif

    test_reference_perf();

} /* test_reference() */

/*-------------------------------------------------------------------------
 * Function:    cleanup_reference
 *
 * Purpose:    Cleanup temporary test files
 *
 * Return:    none
 *
 * Programmer:    Quincey Koziol
 *              September 8, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_reference(void)
{
    H5Fdelete(FILE_REF_PARAM, H5P_DEFAULT);
    H5Fdelete(FILE_REF_OBJ, H5P_DEFAULT);
    H5Fdelete(FILE_REF_VL_OBJ, H5P_DEFAULT);
    H5Fdelete(FILE_REF_CMPND_OBJ, H5P_DEFAULT);
    H5Fdelete(FILE_REF_REG, H5P_DEFAULT);
    H5Fdelete(FILE_REF_REG_1D, H5P_DEFAULT);
    H5Fdelete(FILE_REF_OBJ_DEL, H5P_DEFAULT);
    H5Fdelete(FILE_REF_GRP, H5P_DEFAULT);
    H5Fdelete(FILE_REF_ATTR, H5P_DEFAULT);
    H5Fdelete(FILE_REF_EXT1, H5P_DEFAULT);
    H5Fdelete(FILE_REF_EXT2, H5P_DEFAULT);
    H5Fdelete(FILE_REF_COMPAT, H5P_DEFAULT);
}
