/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Generate the binary hdf5 files for the h5stat tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include "hdf5.h"
#include "H5private.h"

/* For gen_newgrat_file() */
#define NEWGRAT_FILE     "h5stat_newgrat.h5"
#define DATASET_NAME    "DATASET_NAME"
#define GROUP_NAME    "GROUP"
#define ATTR_NAME    "ATTR"
#define NUM_GRPS     35000
#define NUM_ATTRS    100

/* Declarations for gen_idx_file() */
#define IDX_FILE     "h5stat_idx.h5"
#define DSET        "dset"
#define DSET_FILTER    "dset_filter"

/* For gen_threshold_file() */
#define THRESHOLD_FILE         "h5stat_threshold.h5"
#define THRES_ATTR_NAME        "attr"
#define THRES_ATTR_GRP_NAME    "grp_attr"
#define THRES_DSET_NAME     "dset"
#define THRES_NUM        10
#define THRES_NUM_25        25

/* For gen_err_refcount() */
#define ERR_REFCOUNT_FILE   "h5stat_err_refcount.h5"

/*
 * Generate HDF5 file with latest format with
 * NUM_GRPS groups and NUM_ATTRS attributes for the dataset
 */
static void
gen_newgrat_file(const char *fname)
{
    hid_t fcpl          = -1;   /* File creation property */
    hid_t fapl          = -1;   /* File access property */
    hid_t fid           = -1;   /* File id */
    hid_t gid           = -1;   /* Group id */
    hid_t tid           = -1;   /* Datatype id */
    hid_t sid           = -1;   /* Dataspace id */
    hid_t attr_id       = -1;   /* Attribute id */
    hid_t did           = -1;   /* Dataset id */
    char name[30];    /* Group name */
    char attrname[30];    /* Attribute name */
    int  i;        /* Local index variable */

    /* Get a copy file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set to use latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto error;

    /* Get a copy of file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Set file space handling strategy */
    if(H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, 1, (hsize_t)1) < 0)
        goto error;

     /* Create file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        goto error;

    /* Create NUM_GRPS groups in the root group */
    for(i = 1; i <= NUM_GRPS; i++) {
        HDsprintf(name, "%s%d", GROUP_NAME,i);
        if((gid = H5Gcreate2(fid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Gclose(gid) < 0)
            goto error;
    } /* end for */

    /* Create a datatype to commit and use */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;

    /* Create dataspace for dataset */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        goto error;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DATASET_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create NUM_ATTRS for the dataset */
    for(i = 1; i <= NUM_ATTRS; i++) {
        HDsprintf(attrname, "%s%d", ATTR_NAME,i);
        if((attr_id = H5Acreate2(did, attrname, tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Aclose(attr_id) < 0)
            goto error;
    } /* end for */

    /* Close dataset, dataspace, datatype, file */
error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Aclose(attr_id);
        H5Dclose(did);
        H5Tclose(tid);
        H5Sclose(sid);
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;
} /* gen_newgrat_file() */

/*
 * Generate an HDF5 file with groups, datasets, attributes for testing the options:
 *    -l N (--links=N): Set the threshold for # of links when printing information for small groups.
 *    -m N (--dims=N): Set the threshold for the # of dimension sizes when printing information for small datasets.
 *    -a N (--numattrs=N): Set the threshold for the # of attributes when printing information for small # of attributes.
 */
static void
gen_threshold_file(const char *fname)
{
    hid_t fid  = -1;                /* File ID */
    hid_t sid0 = -1;                /* Dataspace IDs */
    hid_t sid1 = -1;                /* Dataspace IDs */
    hid_t sid2 = -1;                /* Dataspace IDs */
    hid_t sid3 = -1;                /* Dataspace IDs */
    hid_t sid4 = -1;                /* Dataspace IDs */
    hid_t did  = -1;                /* Dataset ID */
    hid_t attr_id = -1;             /* Attribute ID */
    hid_t gid = -1;                 /* Group ID */
    hsize_t two_dims[] = {2, 5};    /* Dimension array */
    hsize_t one_dims[] = {6};       /* Dimension array */
    hsize_t zero_dims[] = {0};      /* Dimension array */
    char name[30];                  /* Name */
    unsigned i;                     /* Local index variable */

    /* Create file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create 1-D dataspace with zero dimension size */
    if((sid0 = H5Screate_simple(1, zero_dims, NULL)) < 0)
        goto error;

    /* Create 1-D dataspace with non-zero dimension size*/
    if((sid1 = H5Screate_simple(1, one_dims, NULL)) < 0)
        goto error;

    /* Create 2-D dataspace */
    if((sid2 = H5Screate_simple(2, two_dims, NULL)) < 0)
        goto error;

    /* Create scalar dataspace */
    if((sid3 = H5Screate(H5S_SCALAR)) < 0)
        goto error;

    /* Create null dataspace */
    if((sid4 = H5Screate(H5S_NULL)) < 0)
        goto error;

    /* Create an attribute for the root group */
    if((attr_id = H5Acreate2(fid, "attr", H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Aclose(attr_id) < 0)
        goto error;

    /* Create 1-D dataset with zero dimension size for the root group */
    if((did = H5Dcreate2(fid, "zero_dset", H5T_NATIVE_UCHAR, sid0, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create 11 attributes for the dataset */
    for(i = 1; i <= (THRES_NUM+1); i++) {
        HDsprintf(name, "%s%d", THRES_ATTR_NAME,i);
        if((attr_id = H5Acreate2(did, name, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Aclose(attr_id) < 0)
            goto error;
    }
    if(H5Dclose(did) < 0)
        goto error;

    /* Create dataset with scalar dataspace for the root group */
    if((did = H5Dcreate2(fid, "scalar_dset", H5T_NATIVE_UCHAR, sid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;

    /* Create dataset with null dataspace for the root group */
    if((did = H5Dcreate2(fid, "null_dset", H5T_NATIVE_UCHAR, sid4, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;

    /* Create 2-D dataset for the root group */
    if((did = H5Dcreate2(fid, "dset", H5T_NATIVE_UCHAR, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create 10 attributes for the 2-D dataset */
    for(i = 1; i <= THRES_NUM; i++) {
        HDsprintf(name, "%s%d", THRES_ATTR_NAME,i);
        if((attr_id = H5Acreate2(did, name, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Aclose(attr_id) < 0)
            goto error;
    }
    if(H5Dclose(did) < 0)
        goto error;

    /* Create first group */
    if((gid = H5Gcreate2(fid, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create an attribute for the group */
    if((attr_id = H5Acreate2(gid, "ATTR", H5T_NATIVE_INT, sid3, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Close attribute */
    if(H5Aclose(attr_id) < 0)
        goto error;

    /* Create 10 1-D datasets with non-zero dimension size for the group */
    for(i = 1; i <= THRES_NUM; i++) {
        /* set up dataset name */
        HDsprintf(name, "%s%d", THRES_DSET_NAME,i);

        /* Create the dataset */
        if((did = H5Dcreate2(gid, name, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* Close the dataset */
        if(H5Dclose(did) < 0)
            goto error;
    }

    /* Close the group */
    if(H5Gclose(gid) < 0)
        goto error;


    /* Create second group */
    if((gid = H5Gcreate2(fid, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create 25 attributes for the group */
    for(i = 1; i <= THRES_NUM_25; i++) {
    /* Set up attribute name */
        HDsprintf(name, "%s%d", THRES_ATTR_GRP_NAME,i);

        /* Create the attribute */
        if((attr_id = H5Acreate2(gid, name, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* Close the attribute */
        if(H5Aclose(attr_id) < 0)
            goto error;
    }

    /* Close the group */
    if(H5Gclose(gid) < 0)
        goto error;

    /* Create third group */
    if((gid = H5Gcreate2(fid, "group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create 9 1-D datasets with non-zero dimension size for the group */
    for(i = 1; i < THRES_NUM; i++) {
        /* set up dataset name */
        HDsprintf(name, "%s%d", THRES_DSET_NAME,i);

        /* Create the dataset */
        if((did = H5Dcreate2(gid, name, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* Close the dataset */
        if(H5Dclose(did) < 0)
            goto error;
    }

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Aclose(attr_id);
        H5Dclose(did);
        H5Sclose(sid0);
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Sclose(sid3);
        H5Sclose(sid4);
        H5Fclose(fid);
    } H5E_END_TRY;

} /* gen_threshold_file() */

/*
 * Function: gen_idx_file
 *
 * Purpose: Create a file with datasets that use Fixed Array indexing:
 *       one dataset: fixed dimension, chunked layout, w/o filters
 *         one dataset: fixed dimension, chunked layout, w/ filters
 *
 */
static void
gen_idx_file(const char *fname)
{
    hid_t    fapl = -1;            /* file access property id */
    hid_t    fid = -1;                /* file id */
    hid_t    sid = -1;                /* space id */
    hid_t    dcpl = -1;                /* dataset creation property id */
    hid_t    did = -1, did2 = -1;        /* dataset id */
    hsize_t  dims[1] = {10};     /* dataset dimension */
    hsize_t  c_dims[1] = {2};    /* chunk dimension */
    int      i;            /* local index variable */
    int      buf[10];            /* data buffer */

    /* Get a copy of the file access property */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set the "use the latest format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto error;

    /* Create file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Create data */
    for(i = 0; i < 10; i++)
        buf[i] = i;

    /* Set chunk */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if(H5Pset_chunk(dcpl, 1, c_dims) < 0)
        goto error;

    /* Create a 1D dataset */
    if((sid = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;
    if((did  = H5Dcreate2(fid, DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

#if defined (H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    if(H5Pset_deflate(dcpl, 9) < 0)
        goto error;

    /* Create and write the dataset */
    if((did2  = H5Dcreate2(fid, DSET_FILTER, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close the dataset */
    if(H5Dclose(did2) < 0)
        goto error;
#endif

    /* closing: dataspace, dataset, file */
error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did);
        H5Fclose(fid);
#if defined (H5_HAVE_FILTER_DEFLATE)
        H5Dclose(did2);
#endif
    } H5E_END_TRY;

} /* gen_idx_file() */

/*
 * Function: gen_err_refcount_file
 *
 * Purpose: Create a file with a refcount message ID.
 *          Then a refcount message ID is written to a
 *          message in a version 1 object header.
 *          This will trigger the error as a version 1
 *          object header does not support a refcount message.
 *          This is to verify HDFFV-10333 that h5stat will exit
 *          gracefully when encountered error similar to
 *          H5O_refcount_decode in the jira issue.
 *
 */
static void
gen_err_refcount(const char *fname)
{
    hid_t fid = -1;     /* File identifier */
    hid_t sid = -1;     /* Dataspace message */
    hid_t did = -1;     /* Dataset identifier */
    hid_t gid = -1;     /* Group identifier */
    hid_t aid1 = -1, aid2 = -1;     /* Attribute identifier */
    hid_t tid = -1;     /* Datatype identifier */
    int i, n;           /* Local index variables */
    int buf[10];        /* Data buffer */
    hsize_t dims[1];    /* Dimension size */
    int fd = -1;        /* File descriptor */
    unsigned short val = 22;        /* The refcount message ID */

    /* Initialize data buffer */
    n = 0;
    for(i = 0; i < 10; i++)
        buf[i] = n++;

    /* Create the file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create a group */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create a committed datatype in the group */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    if(H5Tcommit2(gid, "dtype", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto error;

    /* Create the dataspace */
    dims[0] = 10;
    if((sid = H5Screate_simple(1, dims, NULL)) < 0)
        goto error;

    /* Create a dataset with the committed datatype in the file */
    if((did = H5Dcreate2(fid, "dset", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    /* Write to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Attach an attribute with the committed datatype to the group */
    if((aid1 = H5Acreate2(gid, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    /* Attach an attribute with the committed datatype to the dataset */
    if((aid2 = H5Acreate2(did, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Closing */
    if(H5Aclose(aid1) < 0)
        goto error;
    if(H5Aclose(aid2) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Gclose(gid) < 0)
        goto error;
    if(H5Tclose(tid) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    /* This section of code will write a refcount message ID to a message in the
       version 1 object header which does not support a refcount message */
    /* Offset of the message ID to modify is as follows: */
    /* 4520: the offset of the object header containing the attribute message
             with the committed datatype */
    /* 24: the offset in the object header containing the version of the
           attribute message */
    if((fd = HDopen(fname, O_RDWR, 0633)) >= 0) {
        HDlseek(fd, 4520+24, SEEK_SET);
        HDwrite(fd, &val, 2);
        HDclose(fd);
    }

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
        H5Dclose(did);
        H5Tclose(tid);
        H5Sclose(sid);
        H5Aclose(aid1);
        H5Aclose(aid2);
        H5Fclose(fid);
    } H5E_END_TRY;
} /* gen_err_refcount() */

/*
 * The following two test files are generated with older versions
 * of the library for HDFFV-10333.  They are used for testing in
 * testh5stat.sh.in.
 *
 * (1) h5stat_err_old_layout.h5
 *     This file is generated with the 1.6 library so that a file
 *     with a version 2 layout message is created.
 *     Then a "0" is written to the "dimension" field in the layout
 *     message to trigger the error.
 *     This is to verify HDFFV-10333 that h5stat will exit gracefully
 *     when encountered error similar to H5O__layout_decode in the
 *     jira issue.
 *
 * (2) h5stat_err_old_fill.h5
 *     This file is generated with the 1.4 library so that a file
 *     with an old fill value message is created.
 *     Then an illegal size is written to the "size" fild in the
 *     fill value message to trigger the error.
 *     This is to verify HDFFV-10333 that h5stat will exit gracefully
 *     when encountered error similar to H5O_fill_old_decode in the
 *     jira issue.
 */

int main(void)
{
    gen_newgrat_file(NEWGRAT_FILE);
    gen_threshold_file(THRESHOLD_FILE);

    /* Generate an HDF file to test for datasets with Fixed Array indexing */
    gen_idx_file(IDX_FILE);

    /* Generate a file with a refcount message ID */
    gen_err_refcount(ERR_REFCOUNT_FILE);

    return 0;
}

