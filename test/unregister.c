/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Programmer:	Raymond Lu
 *              24 April 2013
 *
 * Purpose:	Tests H5Zunregister function
 */
#include "h5test.h"

#include "H5CXprivate.h"        /* API Contexts                         */

const char *FILENAME[] = {
    "unregister_filter_1",
    "unregister_filter_2",
    NULL
};

#define GROUP_NAME              "test_group"
#define DSET_NAME               "test_dataset"
#define FILENAME_BUF_SIZE       1024
#define DSET_DIM1               100
#define DSET_DIM2               200
#define FILTER_CHUNK_DIM1       2
#define FILTER_CHUNK_DIM2       25
#define GROUP_ITERATION         1000

#define H5Z_FILTER_DUMMY        312

static size_t do_nothing(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* Dummy filter for test_unregister_filters only */
const H5Z_class2_t H5Z_DUMMY[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version              */
    H5Z_FILTER_DUMMY,           /* Filter ID number                 */
    1, 1,                       /* Encoding and decoding enabled    */
    "dummy",                    /* Filter name for debugging        */
    NULL,                       /* The "can apply" callback         */
    NULL,                       /* The "set local" callback         */
    do_nothing,                 /* The actual filter function       */
}};


/*-------------------------------------------------------------------------
 * Function:    do_nothing
 *
 * Purpose:     A dummy compression method that doesn't do anything. This
 *              filter is only for test_unregister_filters. Please don't 
 *              use it for other tests because it may mess up this test.
 *
 * Return:      Data chunk size
 *
 *-------------------------------------------------------------------------
 */
static size_t
do_nothing(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
      size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:    test_unregister_filters
 *
 * Purpose:     Tests unregistering filter before closing the file
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_unregister_filters(hid_t fapl_id)
{
    hid_t       fid1        = H5I_INVALID_HID;
    hid_t       fid2        = H5I_INVALID_HID;
    hid_t       dcpl_id     = H5I_INVALID_HID;
    hid_t       gcpl_id     = H5I_INVALID_HID;
    hid_t       gid         = H5I_INVALID_HID;
    hid_t       gid_loop    = H5I_INVALID_HID;
    hid_t       did         = H5I_INVALID_HID;
    hid_t       sid         = H5I_INVALID_HID;
    int         i, j, n;
    char        group_name[32];
    char        filename[FILENAME_BUF_SIZE];
    const hsize_t chunk_dims[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};  /* Chunk dimensions */
    hsize_t     dims[2];
    int	        data[DSET_DIM1][DSET_DIM2];
    herr_t      ret;

    TESTING("Unregistering filter");

    /* Create first file */
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));
    if((fid1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        goto error;

    /* Create second file */
    h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename));
    if((fid2 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        goto error;

    /* Register DUMMY filter */
    if (H5Zregister(H5Z_DUMMY) < 0)
        goto error;
    if (H5Zfilter_avail(H5Z_FILTER_DUMMY) != TRUE)
        goto error;
 
    /*******************
     * PART 1 - GROUPS *
     *******************/

    /* Use DUMMY filter for creating groups */ 
    if((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        goto error;
    if(H5Pset_filter(gcpl_id, H5Z_FILTER_DUMMY, H5Z_FLAG_MANDATORY, (size_t)0, NULL) < 0)
        goto error;

    /* Create a group using this filter */
    if((gid = H5Gcreate2(fid1, GROUP_NAME, H5P_DEFAULT, gcpl_id, H5P_DEFAULT)) < 0)
        goto error;

    /* Create multiple groups under the main group */
    for(i = 0; i < GROUP_ITERATION; i++) {
        HDsprintf(group_name, "group_%d", i);
        if((gid_loop = H5Gcreate2(gid, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Gclose(gid_loop) < 0)
            goto error; 
    }

    /* Flush the file containing the groups */
    if(H5Fflush(fid1, H5F_SCOPE_GLOBAL) < 0)
        goto error;

    /* Unregister the filter before closing the group.  It should fail */
    H5E_BEGIN_TRY {
        ret = H5Zunregister(H5Z_FILTER_DUMMY);
    } H5E_END_TRY;
    if(ret >= 0) {
        H5_FAILED();
        HDprintf("    Line %d: Should not be able to unregister filter\n", __LINE__);
        goto error;
    }

    /* Close the group */
    if(H5Gclose(gid) < 0)
        goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (gcpl_id) < 0)
        goto error;

    /*********************
     * PART 2 - DATASETS *
     *********************/

    /* Use DUMMY filter for creating datasets */ 
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0)
        goto error;
    if(H5Pset_filter(dcpl_id, H5Z_FILTER_DUMMY, 0, (size_t)0, NULL) < 0)
        goto error;

    /* Initialize the data for writing */
    for(i = n = 0; i < DSET_DIM1; i++)
        for(j = 0; j < DSET_DIM2; j++)
            data[i][j] = n++;

    /* Create the dataspace */
    dims[0] = DSET_DIM1;
    dims[1] = DSET_DIM2;
    if((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Create a dataset in the first file */
    if((did = H5Dcreate2(fid1, DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        goto error;

    /* Unregister the filter before closing the dataset.  It should fail */
    H5E_BEGIN_TRY {
        ret = H5Zunregister(H5Z_FILTER_DUMMY);
    } H5E_END_TRY;
    if(ret >= 0) {
        H5_FAILED();
        HDprintf("    Line %d: Should not be able to unregister filter\n", __LINE__);
        goto error;
    }

    /* Close the dataset */
    if(H5Dclose(did) < 0)
        goto error;

    /* Create a dataset in the second file */
    if((did = H5Dcreate2(fid2, DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        goto error;

    /* Close the dataset in the second file */
    if(H5Dclose(did) < 0)
        goto error;

    /* Unregister the filter after closing all objects but before closing files. 
     * It should flush all files.
     */
    if(H5Zunregister(H5Z_FILTER_DUMMY) < 0)
        goto error;

    /* Clean up objects used for this test */
    if(H5Pclose(dcpl_id) < 0)
        goto error;
    if(H5Fclose(fid1) < 0)
        goto error;
    if(H5Fclose(fid2) < 0)
        goto error;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Pclose(dcpl_id);
        H5Pclose(gcpl_id);
        H5Gclose(gid);
        H5Gclose(gid_loop);
        H5Dclose(did);
        H5Sclose(sid);
    } H5E_END_TRY;

    return FAIL;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests unregistering filter with H5Zunregister
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fapl_id = H5I_INVALID_HID;
    int         nerrors = 0;
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */

    /* Testing setup */
    h5_reset();
    fapl_id = h5_fileaccess();

    /* Push API context */
    if(H5CX_push() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

    /* Test unregistering filter in its own file */
    nerrors += (test_unregister_filters(fapl_id) < 0           ? 1 : 0);

    h5_cleanup(FILENAME, fapl_id);

    if (nerrors)
        goto error;
    HDprintf("All filter unregistration tests passed.\n");

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;


    HDexit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d FILTER UNREGISTRATION TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");

    if(api_ctx_pushed) H5CX_pop();

    HDexit(EXIT_FAILURE);
} /* end main() */

