/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5. The full HDF5 copyright notice, including      *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 * Programmer:	Lucas C. Villa Real
 *              25 March 2022
 *
 * Purpose:	Tests H5Zget_pipeline_file_id() function
 */
#include "h5test.h"

#include "H5CXprivate.h" /* API Contexts                         */

const char *FILENAME[] = {"filter_file_id", NULL};

#define GROUP_NAME        "test_group"
#define DSET_NAME         "test_dataset"
#define FILENAME_BUF_SIZE 1024
#define DSET_DIM1         100
#define DSET_DIM2         200
#define FILTER_CHUNK_DIM1 2
#define FILTER_CHUNK_DIM2 25
#define GROUP_ITERATION   1000

#define H5Z_FILTER_DUMMY 312

static herr_t set_local_cb(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static htri_t can_apply_cb(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t filter_cb(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, size_t nbytes,
                         size_t *buf_size, void **buf);

/* Dummy filter for test_filters only */
const H5Z_class2_t H5Z_DUMMY[1] = {{
    H5Z_CLASS_T_VERS, /* H5Z_class_t version              */
    H5Z_FILTER_DUMMY, /* Filter ID number                 */
    1, 1,             /* Encoding and decoding enabled    */
    "dummy",          /* Filter name for debugging        */
    can_apply_cb,     /* The "can apply" callback         */
    set_local_cb,     /* The "set local" callback         */
    filter_cb,        /* The actual filter function       */
}};

/*-------------------------------------------------------------------------
 * Function:    set_local_cb
 *
 * Purpose:     A dummy set_local method that doesn't do anything (except
 *              for testing of API calls).
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t set_local_cb(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    TESTING("I/O filter 'set_local' callback");
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    can_apply_cb
 *
 * Purpose:     A dummy can_apply method that doesn't do anything (except
 *              for testing of API calls).
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 *-------------------------------------------------------------------------
 */
static htri_t can_apply_cb(hid_t dcpl_id, hid_t type_id, hid_t space_id)
{
    TESTING("I/O filter 'can_apply' callback");
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    filter_cb
 *
 * Purpose:     A dummy compression method that doesn't do anything (except
 *              for testing of API calls).
 *
 * Return:      Data chunk size
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_cb(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
           const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes, size_t H5_ATTR_UNUSED *buf_size,
           void H5_ATTR_UNUSED **buf)
{
    TESTING("I/O filter 'filter' callback");
    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:    test_filter
 *
 * Purpose:     Tests filter callbacks
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter(hid_t fapl_id)
{
    hid_t         fid1     = H5I_INVALID_HID;
    hid_t         dcpl_id  = H5I_INVALID_HID;
    hid_t         gcpl_id  = H5I_INVALID_HID;
    hid_t         gid      = H5I_INVALID_HID;
    hid_t         gid_loop = H5I_INVALID_HID;
    hid_t         did      = H5I_INVALID_HID;
    hid_t         sid      = H5I_INVALID_HID;
    int           i, j, n;
    char          group_name[32];
    char          filename[FILENAME_BUF_SIZE];
    const hsize_t chunk_dims[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2}; /* Chunk dimensions */
    hsize_t       dims[2];
    int **        buf      = NULL;
    int *         buf_data = NULL;
    herr_t        ret;

    TESTING("Retrieval of pipeline file handle");

    /* Set up data array */
    if (NULL == (buf_data = (int *)HDcalloc(DSET_DIM1 * DSET_DIM2, sizeof(int))))
        TEST_ERROR;
    for (i = 0; i < DSET_DIM1 * DSET_DIM2; i++)
        buf_data[i] = i;

    /* Create file */
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));
    if ((fid1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        goto error;

    /* Register DUMMY filter */
    if (H5Zregister(H5Z_DUMMY) < 0)
        goto error;
    if (H5Zfilter_avail(H5Z_FILTER_DUMMY) != TRUE)
        goto error;

    /* Use DUMMY filter for creating datasets */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0)
        goto error;
    if (H5Pset_filter(dcpl_id, H5Z_FILTER_DUMMY, 0, (size_t)0, NULL) < 0)
        goto error;

    /* Create the dataspace */
    dims[0] = DSET_DIM1;
    dims[1] = DSET_DIM2;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Create a dataset */
    if ((did = H5Dcreate2(fid1, DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_data) < 0)
        goto error;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        goto error;

    /* Unregister the filter */
    if (H5Zunregister(H5Z_FILTER_DUMMY) < 0)
        goto error;

    /* Clean up objects used for this test */
    if (H5Pclose(dcpl_id) < 0)
        goto error;
    if (H5Fclose(fid1) < 0)
        goto error;

    HDfree(buf_data);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid1);
        H5Pclose(dcpl_id);
        H5Pclose(gcpl_id);
        H5Gclose(gid);
        H5Gclose(gid_loop);
        H5Dclose(did);
        H5Sclose(sid);
    }
    H5E_END_TRY;

    HDfree(buf_data);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests retrieval of pipeline's file handle
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t   fapl_id        = H5I_INVALID_HID;
    int     nerrors        = 0;
    hbool_t api_ctx_pushed = FALSE; /* Whether API context pushed */

    /* Testing setup */
    h5_reset();
    fapl_id = h5_fileaccess();

    /* Push API context */
    if (H5CX_push() < 0)
        FAIL_STACK_ERROR
    api_ctx_pushed = TRUE;

    /* Test unregistering filter in its own file */
    nerrors += (test_filter(fapl_id) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl_id);

    if (nerrors)
        goto error;
    HDprintf("All filter pipeline file handle tests passed.\n");

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(FALSE) < 0)
        FAIL_STACK_ERROR
    api_ctx_pushed = FALSE;

    HDexit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d FILTER PIPELINE FILE HANDLE TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");

    if (api_ctx_pushed)
        H5CX_pop(FALSE);

    HDexit(EXIT_FAILURE);
} /* end main() */
