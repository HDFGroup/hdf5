/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including    *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code      *
 * distribution tree, or in https://www.hdfgroup.org/licenses.              *
 * If you do not have access to either file, you may request a copy from    *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Matthew Larson
 *              September 9, 2025
 *
 * Purpose:     Test the R-tree spatial indexing implementation.
 */

#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5RT package.
 */
#define H5RT_FRIEND  /*suppress error about including H5RTpkg */
#include "H5RTpkg.h" /* R-tree package         */

#define H5D_FRIEND /*suppress error about including H5Dpkg */
#define H5D_TESTING
#include "H5Dpkg.h" /* Datasets */

#define RTREE_TEST_BASE_COORD 10000
#define RTREE_TEST_BASE_SIZE  1000

#define RTREE_TEST_CREATE_RANK       8
#define RTREE_TEST_CREATE_NUM_COUNTS 4

static const char *FILENAME[] = {"vds_rtree_src",       /* 0: Source file for VDS mappings */
                                 "vds_rtree_dapl",      /* 1: DAPL test file */
                                 "vds_rtree_threshold", /* 2: Threshold test file */
                                 "vds_rtree_rw",        /* 3: Read/write test file */
                                 NULL};

#define FILENAME_BUF_SIZE 1024

#define RTREE_DAPL_VDS_NAME "vdset"

#define RTREE_DAPL_DATASET_DIM1 10
#define RTREE_DAPL_DATASET_DIM2 10

#define RTREE_MAX_TEST_MAPPINGS (H5D_VIRTUAL_TREE_THRESHOLD + 100)

static const size_t test_counts[RTREE_TEST_CREATE_NUM_COUNTS] = {H5D_VIRTUAL_TREE_THRESHOLD, 100, 1000,
                                                                 10000};

/* Helper function to generate leaf data */
static H5RT_leaf_t *generate_leaves(int rank, size_t leaf_count);

/* Helper function to free leaf data */
static void free_leaves(H5RT_leaf_t *leaves, size_t leaf_count);

/* For manual verification of r-tree results */
static H5RT_leaf_t **manual_search(H5RT_leaf_t *leaves, size_t leaf_count, int rank, hsize_t min[],
                                   hsize_t max[], size_t *results_count);

/* Helper function to compare r-tree search results to linear search */
static herr_t verify_rtree_search(H5RT_result_set_t *result_set, H5RT_leaf_t *leaves, size_t leaf_count,
                                  hsize_t min[], hsize_t max[], int rank);

/* Helper to create and initialize virtual dset in a file */
static hid_t create_virtual_dataset(hid_t file_id, hid_t dapl_id, int num_mappings, hid_t src_fapl);

static herr_t
verify_rtree_search(H5RT_result_set_t *result_set, H5RT_leaf_t *leaves, size_t leaf_count, hsize_t min[],
                    hsize_t max[], int rank)
{
    H5RT_leaf_t **manual_results     = NULL;
    size_t        num_manual_results = 0;
    herr_t        ret_value          = SUCCEED;

    assert(result_set);

    /* Perform manual search for comparison */
    manual_results = manual_search(leaves, leaf_count, rank, min, max, &num_manual_results);

    /* Check equality - result_set is never NULL now */
    if (num_manual_results != result_set->count) {
        puts("R-tree search and manual search found different number of results");
        ret_value = FAIL;
        goto done;
    }

    if (num_manual_results > 0) {
        /* Order of results in each list may differ, so we need to check each result individually */
        for (size_t i = 0; i < num_manual_results; i++) {
            H5RT_leaf_t *manual_leaf = manual_results[i];
            bool         found       = false;

            /* Check if this manual result is in the r-tree results */
            for (size_t j = 0; j < result_set->count; j++) {
                if (result_set->results[j] == manual_leaf) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                puts("R-tree search missing a result found in manual search");
                ret_value = FAIL;
                goto done;
            }
        }
    }

done:
    if (manual_results)
        free(manual_results);

    return ret_value;
}

/* Helper function to generate leaf data */
static H5RT_leaf_t *
generate_leaves(int rank, size_t leaf_count)
{
    H5RT_leaf_t *ret_value = NULL;

    assert(rank > 0);
    assert(leaf_count > 0);

    if ((ret_value = calloc(leaf_count, sizeof(H5RT_leaf_t))) == NULL)
        goto done;

    for (size_t i = 0; i < leaf_count; i++) {
        /* Initialize leaf with dynamic coordinate allocation */
        if (H5RT_leaf_init(&ret_value[i], rank, (void *)1) < 0) {
            /* Clean up already initialized leaves */
            for (size_t j = 0; j < i; j++) {
                H5RT_leaf_cleanup(&ret_value[j]);
            }
            free(ret_value);
            ret_value = NULL;
            goto done;
        }

        /* Set coordinates */
        for (int d = 0; d < rank; d++) {
            hsize_t min_coord   = (hsize_t)rand() % RTREE_TEST_BASE_COORD;
            hsize_t size        = 1 + (hsize_t)rand() % RTREE_TEST_BASE_SIZE;
            ret_value[i].min[d] = min_coord;
            ret_value[i].max[d] = min_coord + size;
            ret_value[i].mid[d] = (ret_value[i].max[d] + ret_value[i].min[d]) / 2;
        }
    }

done:
    return ret_value;
}

/* Helper function to free leaf data */
static void
free_leaves(H5RT_leaf_t *leaves, size_t leaf_count)
{
    if (!leaves)
        return;

    for (size_t i = 0; i < leaf_count; i++) {
        H5RT_leaf_cleanup(&leaves[i]);
    }
    free(leaves);
}

static H5RT_leaf_t **
manual_search(H5RT_leaf_t *leaves, size_t leaf_count, int rank, hsize_t min[], hsize_t max[],
              size_t *results_count)
{
    H5RT_leaf_t **ret_value = NULL;
    H5RT_leaf_t **results   = NULL;

    assert(leaves);
    assert(results_count);

    /* Allocate maximum possible results size
     * May need to optimize if this makes testing times impractical */
    if ((results = calloc(leaf_count, sizeof(H5RT_leaf_t *))) == NULL)
        goto done;

    for (size_t i = 0; i < leaf_count; i++) {
        if (H5RT__leaves_intersect(rank, min, max, leaves[i].min, leaves[i].max)) {
            results[(*results_count)++] = &leaves[i];
        }
    }

    ret_value = results;
done:
    if (!ret_value && results)
        free(results);

    return results;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_create
 *
 * Purpose:     Test basic R-tree creation and cleanup
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_create(void)
{
    H5RT_t      *tree       = NULL;
    size_t       leaf_count = 0;
    H5RT_leaf_t *leaves     = NULL;

    TESTING("R-tree creation");
    srand(0);

    for (int cnt_idx = 0; cnt_idx < RTREE_TEST_CREATE_NUM_COUNTS; cnt_idx++) {
        leaf_count = test_counts[cnt_idx];

        for (int rank = 1; rank < RTREE_TEST_CREATE_RANK; rank++) {
            /* Create the data to populate the r-tree */
            if ((leaves = generate_leaves(rank, leaf_count)) == NULL)
                FAIL_STACK_ERROR;

            if ((tree = H5RT_create(rank, leaves, leaf_count)) == NULL)
                FAIL_STACK_ERROR;

            /* Ownership of memory has transferred */
            /* leaves is now NULL */

            if (H5RT_free(tree) < 0)
                FAIL_STACK_ERROR;
        }
    }

    PASSED();
    return SUCCEED;

error:
    if (leaves)
        free_leaves(leaves, leaf_count);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_search
 *
 * Purpose:     Test R-tree spatial query operations
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_search(void)
{
    H5RT_t      *tree        = NULL;
    size_t       leaf_count  = 0;
    H5RT_leaf_t *leaves      = NULL;
    H5RT_leaf_t *leaves_temp = NULL;

    H5RT_result_set_t *result_set = NULL;
    hsize_t            min[H5S_MAX_RANK];
    hsize_t            max[H5S_MAX_RANK];
    hsize_t            size = 0;

    TESTING("R-tree spatial queries");
    srand(0);

    for (int cnt_idx = 0; cnt_idx < RTREE_TEST_CREATE_NUM_COUNTS; cnt_idx++) {
        leaf_count = test_counts[cnt_idx];

        for (int rank = 1; rank < RTREE_TEST_CREATE_RANK; rank++) {
            memset(min, 0, H5S_MAX_RANK * sizeof(hsize_t));
            memset(max, 0, H5S_MAX_RANK * sizeof(hsize_t));

            /* Create data */
            if ((leaves = generate_leaves(rank, leaf_count)) == NULL)
                FAIL_STACK_ERROR;

            /* Create tree */
            leaves_temp = leaves;
            if ((tree = H5RT_create(rank, leaves, leaf_count)) == NULL)
                FAIL_STACK_ERROR;

            /* Ownership is transferred - leaves is now NULL */

            /* Setup search criteria */
            for (int r = 0; r < rank; r++) {
                min[r] = (hsize_t)(rand() % RTREE_TEST_BASE_COORD);
                size   = 1 + (hsize_t)(rand() % RTREE_TEST_BASE_SIZE);
                max[r] = min[r] + size;
            }

            /* Perform r-tree search */
            if (H5RT_search(tree, min, max, &result_set) < 0)
                FAIL_STACK_ERROR;

            /* Verify that results are equivalent to a manual search */
            if (verify_rtree_search(result_set, leaves_temp, leaf_count, min, max, rank) < 0) {
                FAIL_STACK_ERROR;
            }

            /* Free search results */
            if (H5RT_free_results(result_set) < 0)
                FAIL_STACK_ERROR;
            result_set = NULL;

            if (H5RT_free(tree) < 0)
                FAIL_STACK_ERROR;
        }
    }

    PASSED();
    return SUCCEED;

error:
    if (result_set)
        H5RT_free_results(result_set);

    if (leaves)
        free_leaves(leaves, leaf_count);

    if (tree)
        H5RT_free(tree);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_copy
 *
 * Purpose:     Test R-tree deep copy functionality
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_copy(void)
{
    H5RT_t      *tree       = NULL;
    H5RT_t      *tree_copy  = NULL;
    size_t       leaf_count = 0;
    H5RT_leaf_t *leaves     = NULL;

    H5RT_result_set_t *result_set = NULL;
    hsize_t            min[H5S_MAX_RANK];
    hsize_t            max[H5S_MAX_RANK];
    hsize_t            size = 0;

    TESTING("R-tree copy");
    srand(0);

    for (int cnt_idx = 0; cnt_idx < RTREE_TEST_CREATE_NUM_COUNTS; cnt_idx++) {
        leaf_count = test_counts[cnt_idx];

        for (int rank = 1; rank < RTREE_TEST_CREATE_RANK; rank++) {
            memset(min, 0, H5S_MAX_RANK * sizeof(hsize_t));
            memset(max, 0, H5S_MAX_RANK * sizeof(hsize_t));

            /* Create data */
            if ((leaves = generate_leaves(rank, leaf_count)) == NULL)
                FAIL_STACK_ERROR;

            /* Create original tree */
            if ((tree = H5RT_create(rank, leaves, leaf_count)) == NULL)
                FAIL_STACK_ERROR;

            /* Ownership is transferred */
            /* leaves is now NULL */

            /* Deep copy the tree */
            if ((tree_copy = H5RT_copy(tree)) == NULL)
                FAIL_STACK_ERROR;

            /* Delete the original tree */
            if (H5RT_free(tree) < 0)
                FAIL_STACK_ERROR;
            tree = NULL;

            /* Setup search criteria */
            for (int r = 0; r < rank; r++) {
                min[r] = (hsize_t)(rand() % RTREE_TEST_BASE_COORD);
                size   = 1 + (hsize_t)(rand() % RTREE_TEST_BASE_SIZE);
                max[r] = min[r] + size;
            }

            /* Perform search on copied tree */
            if (H5RT_search(tree_copy, min, max, &result_set) < 0)
                FAIL_STACK_ERROR;

            /* Verify that results are equivalent to a manual search */
            if (verify_rtree_search(result_set, tree_copy->leaves, leaf_count, min, max, rank) < 0) {
                H5RT_free_results(result_set);
                FAIL_STACK_ERROR;
            }

            /* Free search results */
            if (H5RT_free_results(result_set) < 0)
                FAIL_STACK_ERROR;
            result_set = NULL;

            if (H5RT_free(tree_copy) < 0)
                FAIL_STACK_ERROR;
            tree_copy = NULL;
        }
    }

    PASSED();
    return SUCCEED;
error:
    if (result_set)
        H5RT_free_results(result_set);

    if (leaves)
        free_leaves(leaves, leaf_count);

    if (tree)
        H5RT_free(tree);

    if (tree_copy)
        H5RT_free(tree_copy);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    create_virtual_dataset
 *
 * Purpose:     Helper function to create a 1D virtual dataset with mappings
 *
 * Return:      Success: dataset ID
 *              Failure: H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_virtual_dataset(hid_t file_id, hid_t dapl_id, int num_mappings, hid_t src_fapl)
{
    hid_t   vspace_id   = H5I_INVALID_HID;
    hid_t   srcspace_id = H5I_INVALID_HID;
    hid_t   vsel_id     = H5I_INVALID_HID;
    hid_t   srcfile_id  = H5I_INVALID_HID;
    hid_t   srcdset_id  = H5I_INVALID_HID;
    hid_t   vdset_id    = H5I_INVALID_HID;
    hid_t   dcpl_id     = H5I_INVALID_HID;
    hsize_t vdims[1]    = {(hsize_t)num_mappings};
    hsize_t srcdims[1]  = {1};
    hsize_t start[1], count[1];
    char    srcdset_name[256];
    char    srcfilename[FILENAME_BUF_SIZE];
    char    srcfilename_map[FILENAME_BUF_SIZE];
    int     wdata;
    int     i;

    /* Generate VFD-specific source filenames */
    h5_fixname(FILENAME[0], src_fapl, srcfilename, sizeof(srcfilename));
    h5_fixname_printf(FILENAME[0], src_fapl, srcfilename_map, sizeof(srcfilename_map));

    /* Create 1D virtual dataset space */
    if ((vspace_id = H5Screate_simple(1, vdims, NULL)) < 0)
        goto error;

    /* Create 1D source dataset space (single element) */
    if ((srcspace_id = H5Screate_simple(1, srcdims, NULL)) < 0)
        goto error;

    /* Create dataset creation property list */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Create source file - use actual filename, not the mapping version */
    if ((srcfile_id = H5Fcreate(srcfilename, H5F_ACC_TRUNC, H5P_DEFAULT, src_fapl)) < 0)
        goto error;

    /* Create multiple source dsets and add virtual mappings */
    for (i = 0; i < num_mappings; i++) {
        sprintf(srcdset_name, "%d_src_dset", i);

        /* Create source dataset */
        if ((srcdset_id = H5Dcreate2(srcfile_id, srcdset_name, H5T_NATIVE_INT, srcspace_id, H5P_DEFAULT,
                                     H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;

        /* Write data to source dataset (value equals index) */
        wdata = i;
        if (H5Dwrite(srcdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0)
            goto error;

        /* Create hyperslab selection for virtual dataset (one element at position i) */
        if ((vsel_id = H5Scopy(vspace_id)) < 0)
            goto error;

        start[0] = (hsize_t)i;
        count[0] = 1;
        if (H5Sselect_hyperslab(vsel_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            goto error;

        /* Add virtual mapping - use the printf-escaped version for VDS mapping */
        if (H5Pset_virtual(dcpl_id, vsel_id, srcfilename_map, srcdset_name, srcspace_id) < 0)
            goto error;

        /* Close source dataset and selection */
        if (H5Dclose(srcdset_id) < 0)
            goto error;
        if (H5Sclose(vsel_id) < 0)
            goto error;

        srcdset_id = H5I_INVALID_HID;
        vsel_id    = H5I_INVALID_HID;
    }

    /* Create virtual dataset */
    if ((vdset_id = H5Dcreate2(file_id, RTREE_DAPL_VDS_NAME, H5T_NATIVE_INT, vspace_id, H5P_DEFAULT, dcpl_id,
                               dapl_id)) < 0)
        goto error;

    /* Cleanup */
    if (H5Sclose(vspace_id) < 0)
        goto error;
    if (H5Sclose(srcspace_id) < 0)
        goto error;
    if (H5Fclose(srcfile_id) < 0)
        goto error;
    if (H5Pclose(dcpl_id) < 0)
        goto error;

    return vdset_id;
error:
    /* Cleanup */
    H5E_BEGIN_TRY
    {
        H5Sclose(vspace_id);
        H5Sclose(srcspace_id);
        H5Sclose(vsel_id);
        H5Dclose(srcdset_id);
        H5Fclose(srcfile_id);
        H5Dclose(vdset_id);
        H5Pclose(dcpl_id);
    }
    H5E_END_TRY;

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_existence_helper
 *
 * Purpose:     Test helper to verify that r-tree existence on a dataset
 *              matches what is expected
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_existence_helper(hid_t vdset_id, bool expect_tree, bool *correct_out)
{
    herr_t                 ret_value = SUCCEED;
    H5D_t                 *dset      = NULL;
    H5O_storage_virtual_t *storage   = NULL;

    assert(correct_out);
    *correct_out = false;

    /* Get the dataset object - this is using internal API for testing */
    if (NULL == (dset = (H5D_t *)H5VL_object(vdset_id))) {
        ret_value = FAIL;
        goto done;
    }

    if (dset->shared->layout.type != H5D_VIRTUAL) {
        ret_value = FAIL;
        goto done;
    }

    /* Get the virtual storage structure */
    storage = &(dset->shared->layout.storage.u.virt);

    /* Verify tree existence matches expectation */
    if (expect_tree) {
        if (storage->tree == NULL) {
            puts("Expected spatial tree to exist but it was NULL");
            *correct_out = false;
            goto done;
        }

        if (storage->not_in_tree_nused > 0 && storage->not_in_tree_list == NULL) {
            puts("Expected not_in_tree_list array to exist but it was NULL");
            *correct_out = false;
            goto done;
        }
    }
    else {
        if (storage->tree != NULL) {
            puts("Expected spatial tree to be NULL but it exists");
            *correct_out = false;
            goto done;
        }
        if (storage->not_in_tree_list != NULL || storage->not_in_tree_nused > 0) {
            puts("Expected not_in_tree_list to be empty but it exists");
            *correct_out = false;
            goto done;
        }
    }

    *correct_out = true;
done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_dapl
 *
 * Purpose:     Test R-tree options on the DAPL
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_dapl(bool use_tree, bool read_init, hid_t vds_fapl, hid_t src_fapl)
{
    hid_t file_id  = H5I_INVALID_HID;
    hid_t dapl_id  = H5I_INVALID_HID;
    hid_t vdset_id = H5I_INVALID_HID;

    int  rbuf[RTREE_MAX_TEST_MAPPINGS];
    int  wbuf[RTREE_MAX_TEST_MAPPINGS];
    bool tree_correct = false;
    char test_str[256];
    char vfilename[FILENAME_BUF_SIZE];

    /* Inverse of use_tree for re-open part of test */
    bool use_tree_inverse = !use_tree;

    memset(test_str, 0, sizeof(test_str));

    if (snprintf(test_str, sizeof(test_str), "spatial tree option %s", use_tree ? "enabled" : "disabled") < 0)
        FAIL_STACK_ERROR;

    if (read_init) {
        strncat(test_str, " with read initialization", sizeof(test_str) - strlen(test_str) - 1);
    }
    else {
        strncat(test_str, " with write initialization", sizeof(test_str) - strlen(test_str) - 1);
    }

    TESTING(test_str);

    memset(rbuf, 0, sizeof(int) * RTREE_MAX_TEST_MAPPINGS);
    memset(wbuf, 0, sizeof(int) * RTREE_MAX_TEST_MAPPINGS);

    /* Generate VFD-specific filename for VDS file */
    h5_fixname(FILENAME[1], vds_fapl, vfilename, sizeof(vfilename));

    /* One-time setup */
    if ((file_id = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        FAIL_STACK_ERROR;

    if ((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Create virtual dataset with enough mappings to use tree */
    if ((vdset_id = create_virtual_dataset(file_id, dapl_id, RTREE_MAX_TEST_MAPPINGS, src_fapl)) < 0)
        FAIL_STACK_ERROR;

    if (H5Dclose(vdset_id) < 0)
        FAIL_STACK_ERROR;

    /* Set the spatial tree property */
    if (H5Pset_virtual_spatial_tree(dapl_id, use_tree) < 0)
        FAIL_STACK_ERROR;

    if ((vdset_id = H5Dopen2(file_id, RTREE_DAPL_VDS_NAME, dapl_id)) < 0)
        FAIL_STACK_ERROR;

    /* Read/write the entire virtual dataset to force tree initialization */
    if (read_init) {
        if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        if (H5Dwrite(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
            FAIL_STACK_ERROR;
    }

    /* Verify tree existence matches expectation */
    if (test_rtree_existence_helper(vdset_id, use_tree, &tree_correct) < 0)
        FAIL_STACK_ERROR;

    if (!tree_correct)
        FAIL_STACK_ERROR;

    /* Close the dataset and re-open it with the opposite value set in DAPL */
    if (H5Dclose(vdset_id) < 0)
        FAIL_STACK_ERROR;

    vdset_id = H5I_INVALID_HID;

    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;

    file_id = H5I_INVALID_HID;

    if (H5Pclose(dapl_id) < 0)
        FAIL_STACK_ERROR;

    dapl_id      = H5I_INVALID_HID;
    tree_correct = false;
    memset(rbuf, 0, sizeof(int) * RTREE_MAX_TEST_MAPPINGS);

    if ((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_virtual_spatial_tree(dapl_id, use_tree_inverse) < 0)
        FAIL_STACK_ERROR;

    if ((file_id = H5Fopen(vfilename, H5F_ACC_RDWR, vds_fapl)) < 0)
        FAIL_STACK_ERROR;

    if ((vdset_id = H5Dopen2(file_id, RTREE_DAPL_VDS_NAME, dapl_id)) < 0)
        FAIL_STACK_ERROR;

    /* Read/write the entire virtual dataset to force tree initialization */
    if (read_init) {
        if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        if (H5Dwrite(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
            FAIL_STACK_ERROR;
    }

    /* Verify tree existence matches expectation after re-open */
    if (test_rtree_existence_helper(vdset_id, use_tree_inverse, &tree_correct) < 0)
        FAIL_STACK_ERROR;

    if (!tree_correct)
        FAIL_STACK_ERROR;

    /* Cleanup */
    if (H5Dclose(vdset_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(vdset_id);
        H5Pclose(dapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_threshold
 *
 * Purpose:     Test that threshold controls r-tree usage properly
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_threshold(bool use_tree, hid_t vds_fapl, hid_t src_fapl)
{
    hid_t file_id  = H5I_INVALID_HID;
    hid_t dapl_id  = H5I_INVALID_HID;
    hid_t vdset_id = H5I_INVALID_HID;
    int   rbuf[RTREE_MAX_TEST_MAPPINGS];
    char  vfilename[FILENAME_BUF_SIZE];

    /* Internal values for introspection */
    H5D_t                 *dset    = NULL;
    H5O_storage_virtual_t *storage = NULL;

    const char *test_str =
        use_tree ? "threshold behavior with tree enabled" : "threshold behavior with tree disabled";

    TESTING(test_str);

    /* Generate VFD-specific filename for VDS file */
    h5_fixname(FILENAME[2], vds_fapl, vfilename, sizeof(vfilename));

    /* Test cases: below threshold, at threshold, above threshold */
    int test_cases[3] = {H5D_VIRTUAL_TREE_THRESHOLD - 1, H5D_VIRTUAL_TREE_THRESHOLD, RTREE_MAX_TEST_MAPPINGS};

    for (int test_idx = 0; test_idx < 3; test_idx++) {
        int  num_mappings = test_cases[test_idx];
        bool expect_tree;

        /* Determine expected tree behavior based on threshold and use_tree setting */
        /* Tree is created only when: tree_enabled AND num_mappings >= threshold */
        expect_tree = (use_tree && (num_mappings >= H5D_VIRTUAL_TREE_THRESHOLD));

        if ((file_id = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
            FAIL_STACK_ERROR;

        if ((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
            FAIL_STACK_ERROR;

        /* Set the spatial tree property */
        if (H5Pset_virtual_spatial_tree(dapl_id, use_tree) < 0)
            FAIL_STACK_ERROR;

        /* Create virtual dataset with specified number of mappings */
        if ((vdset_id = create_virtual_dataset(file_id, dapl_id, num_mappings, src_fapl)) < 0)
            FAIL_STACK_ERROR;

        /* Read the virtual dataset to force initialization */
        if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
            FAIL_STACK_ERROR;

        /* Verify data pattern (each element should equal its index) */
        for (int i = 0; i < num_mappings; i++) {
            if (rbuf[i] != i) {
                printf("%d mappings: Data mismatch at [%d]: expected %d, got %d\n", num_mappings, i, i,
                       rbuf[i]);
                FAIL_STACK_ERROR;
            }
        }

        /* Get the dataset object for introspection */
        if (NULL == (dset = (H5D_t *)H5VL_object(vdset_id)))
            FAIL_STACK_ERROR;

        if (dset->shared->layout.type != H5D_VIRTUAL)
            FAIL_STACK_ERROR;

        /* Get the virtual storage structure */
        storage = &(dset->shared->layout.storage.u.virt);

        /* Verify tree existence matches expectation */
        if (expect_tree) {
            if (storage->tree == NULL) {
                printf("%d mappings: Expected spatial tree to exist but it was NULL\n", num_mappings);
                FAIL_STACK_ERROR;
            }
            /* not_in_tree_list can be NULL if all mappings fit in tree - this is OK */
            /* Just verify consistency: if nused > 0, then list should exist */
            if (storage->not_in_tree_nused > 0 && storage->not_in_tree_list == NULL) {
                printf("%d mappings: Expected not_in_tree_list array to exist but it was NULL\n",
                       num_mappings);
                FAIL_STACK_ERROR;
            }
        }
        else {
            if (storage->tree != NULL) {
                printf("%d mappings: Expected spatial tree to be NULL but it exists\n", num_mappings);
                FAIL_STACK_ERROR;
            }
            if (storage->not_in_tree_list != NULL) {
                printf("%d mappings: Expected not_in_tree_list array to be NULL but it exists\n",
                       num_mappings);
                FAIL_STACK_ERROR;
            }
        }

        /* Cleanup */
        if (H5Dclose(vdset_id) < 0)
            FAIL_STACK_ERROR;
        if (H5Pclose(dapl_id) < 0)
            FAIL_STACK_ERROR;
        if (H5Fclose(file_id) < 0)
            FAIL_STACK_ERROR;

        vdset_id = H5I_INVALID_HID;
        dapl_id  = H5I_INVALID_HID;
        file_id  = H5I_INVALID_HID;
    }

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(vdset_id);
        H5Pclose(dapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_rw
 *
 * Purpose:     Test that dataset reads/writes produce correctly values
 *              with rtree on/off
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_rw(bool use_tree, hid_t vds_fapl, hid_t src_fapl)
{
    hid_t   file_id  = H5I_INVALID_HID;
    hid_t   dapl_id  = H5I_INVALID_HID;
    hid_t   vdset_id = H5I_INVALID_HID;
    hid_t   space_id = H5I_INVALID_HID;
    hsize_t wdims    = RTREE_MAX_TEST_MAPPINGS / 2;
    int     rbuf[RTREE_MAX_TEST_MAPPINGS];
    int     wbuf[RTREE_MAX_TEST_MAPPINGS];
    int     num_mappings = RTREE_MAX_TEST_MAPPINGS;
    char    vfilename[FILENAME_BUF_SIZE];

    const char *test_str = use_tree ? "R/W behavior with tree enabled" : "R/W behavior with tree disabled";

    TESTING(test_str);

    memset(rbuf, 0, sizeof(int) * RTREE_MAX_TEST_MAPPINGS);
    memset(wbuf, 0, sizeof(int) * RTREE_MAX_TEST_MAPPINGS);

    /* Generate VFD-specific filename for VDS file */
    h5_fixname(FILENAME[3], vds_fapl, vfilename, sizeof(vfilename));

    if ((file_id = H5Fcreate(vfilename, H5F_ACC_TRUNC, H5P_DEFAULT, vds_fapl)) < 0)
        FAIL_STACK_ERROR;

    if ((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Set the spatial tree property */
    if (H5Pset_virtual_spatial_tree(dapl_id, use_tree) < 0)
        FAIL_STACK_ERROR;

    /* Create virtual dataset with specified number of mappings */
    if ((vdset_id = create_virtual_dataset(file_id, dapl_id, num_mappings, src_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Verify initial read values (each element should equal its index) */
    if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    for (int i = 0; i < num_mappings; i++) {
        if (rbuf[i] != i) {
            printf("%d mappings: Data mismatch at [%d]: expected %d, got %d\n", num_mappings, i, i, rbuf[i]);
            FAIL_STACK_ERROR;
        }
    }

    /* Write to first half of dataset with 2*index */
    for (int i = 0; i < num_mappings / 2; i++)
        wbuf[i] = 2 * i;

    if ((space_id = H5Screate_simple(1, (const hsize_t *)&wdims, NULL)) < 0)
        FAIL_STACK_ERROR;

    if (H5Sselect_hyperslab(space_id, H5S_SELECT_SET, (const hsize_t *)&(hsize_t){0}, NULL,
                            (const hsize_t *)&wdims, NULL) < 0)
        FAIL_STACK_ERROR;

    if (H5Dwrite(vdset_id, H5T_NATIVE_INT, space_id, space_id, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read back entire dataset and verify values */
    if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    for (int i = 0; i < num_mappings; i++) {
        int expected = (i < num_mappings / 2) ? (2 * i) : i;
        if (rbuf[i] != expected) {
            printf("%d mappings: Post-write data mismatch at [%d]: expected %d, got %d\n", num_mappings, i,
                   expected, rbuf[i]);
            FAIL_STACK_ERROR;
        }
    }

    /* Cleanup */
    if (H5Dclose(vdset_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(space_id) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Dclose(vdset_id);
        H5Pclose(dapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the R-tree functionality
 *
 * Return:      Success: EXIT_SUCCESS
 *              Failure: EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int   nerrors  = 0;
    hid_t vds_fapl = H5I_INVALID_HID;
    hid_t src_fapl = H5I_INVALID_HID;
    char  srcfilename[FILENAME_BUF_SIZE];
    char  vfilename[FILENAME_BUF_SIZE];
    char  threshfilename[FILENAME_BUF_SIZE];
    char  rwfilename[FILENAME_BUF_SIZE];

    printf("Testing R-tree spatial indexing...\n");

    H5open();

    /* Create file access property lists for VDS and source files */
    if ((vds_fapl = h5_fileaccess()) < 0)
        TEST_ERROR;
    if ((src_fapl = h5_fileaccess()) < 0)
        TEST_ERROR;

    /* Run core R-tree tests */
    nerrors += test_rtree_create() < 0 ? 1 : 0;
    nerrors += test_rtree_search() < 0 ? 1 : 0;
    nerrors += test_rtree_copy() < 0 ? 1 : 0;

    /* Test spatial tree with DAPL property enabled */
    nerrors += test_rtree_dapl(true, true, vds_fapl, src_fapl) < 0 ? 1 : 0;
    nerrors += test_rtree_dapl(true, false, vds_fapl, src_fapl) < 0 ? 1 : 0;
    nerrors += test_rtree_dapl(false, true, vds_fapl, src_fapl) < 0 ? 1 : 0;
    nerrors += test_rtree_dapl(false, false, vds_fapl, src_fapl) < 0 ? 1 : 0;

    /* Test the mapping count threshold */
    nerrors += test_rtree_threshold(true, vds_fapl, src_fapl) < 0 ? 1 : 0;
    // TODO - Fix failure
    nerrors += test_rtree_threshold(false, vds_fapl, src_fapl) < 0 ? 1 : 0;
    nerrors += test_rtree_rw(true, vds_fapl, src_fapl) < 0 ? 1 : 0;
    nerrors += test_rtree_rw(false, vds_fapl, src_fapl) < 0 ? 1 : 0;

    if (nerrors)
        goto error;

    /* Generate VFD-specific filenames for cleanup */
    h5_fixname(FILENAME[0], src_fapl, srcfilename, sizeof(srcfilename));
    h5_fixname(FILENAME[1], vds_fapl, vfilename, sizeof(vfilename));
    h5_fixname(FILENAME[2], vds_fapl, threshfilename, sizeof(threshfilename));
    h5_fixname(FILENAME[3], vds_fapl, rwfilename, sizeof(rwfilename));

    H5E_BEGIN_TRY
    {
        H5Fdelete(srcfilename, src_fapl);
        H5Fdelete(vfilename, vds_fapl);
        H5Fdelete(threshfilename, vds_fapl);
        H5Fdelete(rwfilename, vds_fapl);
    }
    H5E_END_TRY;

    if (H5Pclose(vds_fapl) < 0)
        TEST_ERROR;
    if (H5Pclose(src_fapl) < 0)
        TEST_ERROR;

    printf("All R-tree tests passed.\n");
    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(vds_fapl);
        H5Pclose(src_fapl);
    }
    H5E_END_TRY;

    printf("***** R-TREE TESTS FAILED *****\n");
    return EXIT_FAILURE;
}
