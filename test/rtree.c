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
#define H5RT_FRIEND /*suppress error about including H5RTpkg */
#define H5RT_TESTING
#include "H5RTpkg.h"

/* Other private headers */
#include "H5CXprivate.h" /* API Contexts */
#include "H5VLprivate.h" /* Virtual Object Layer */

#define H5D_FRIEND /*suppress error about including H5Dpkg */
#define H5D_TESTING
#include "H5Dpkg.h"      /* Datasets */

#define RTREE_TEST_BASE_COORD 10000
#define RTREE_TEST_BASE_SIZE  1000

#define RTREE_TEST_CREATE_RANK       8
#define RTREE_TEST_CREATE_NUM_COUNTS 4

#define RTREE_DAPL_FILENAME     "vds_rtree_test.h5"
#define RTREE_DAPL_SRC_FILENAME "vds_src_rtree_test.h5"
#define RTREE_DAPL_VDS_NAME     "vdset"
#define RTREE_DAPL_SRC_DATASET_NAME "src_dset"

#define RTREE_DAPL_DATASET_DIM1 10
#define RTREE_DAPL_DATASET_DIM2 10

static const size_t test_counts[RTREE_TEST_CREATE_NUM_COUNTS] = {1, 100, 500, 10000};

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
static hid_t create_virtual_dataset(hid_t file_id, hid_t dapl_id);

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
 * Purpose:     Helper function to create a virtual dataset with mappings
 *
 * Return:      Success: dataset ID
 *              Failure: H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_virtual_dataset(hid_t file_id, hid_t dapl_id)
{
    hid_t   vspace_id = H5I_INVALID_HID;
    hid_t   srcspace_id = H5I_INVALID_HID;
    hid_t   srcfile_id = H5I_INVALID_HID;
    hid_t   srcdset_id = H5I_INVALID_HID;
    hid_t   vdset_id = H5I_INVALID_HID;
    hid_t   dcpl_id  = H5I_INVALID_HID;
    hsize_t dims[2] = {RTREE_DAPL_DATASET_DIM1, RTREE_DAPL_DATASET_DIM2};
    int     wbuf[RTREE_DAPL_DATASET_DIM1][RTREE_DAPL_DATASET_DIM2];
    int     i, j;

    if ((vspace_id = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    if ((srcspace_id = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Create source file */
    if ((srcfile_id = H5Fcreate(RTREE_DAPL_SRC_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create source dataset */
    if ((srcdset_id = H5Dcreate2(srcfile_id, RTREE_DAPL_SRC_DATASET_NAME, H5T_NATIVE_INT, srcspace_id,
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if (H5Pset_virtual(dcpl_id, vspace_id, "vds_src_rtree_test.h5", RTREE_DAPL_SRC_DATASET_NAME, srcspace_id) < 0)
        goto error;

    /* Create virtual dataset */
    if ((vdset_id = H5Dcreate2(file_id, RTREE_DAPL_VDS_NAME, H5T_NATIVE_INT, vspace_id,
                              H5P_DEFAULT, dcpl_id, dapl_id)) < 0)
        goto error;

    /* Initialize and write data to source dataset */
    for (i = 0; i < RTREE_DAPL_DATASET_DIM1; i++)
        for (j = 0; j < RTREE_DAPL_DATASET_DIM2; j++)
            wbuf[i][j] = i * RTREE_DAPL_DATASET_DIM2 + j;

    if (H5Dwrite(srcdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        goto error;

    /* Cleanup */
    if (H5Sclose(vspace_id) < 0)
        goto error;
    if (H5Sclose(srcspace_id) < 0)
        goto error;
    if (H5Dclose(srcdset_id) < 0)
        goto error;
    if (H5Fclose(srcfile_id) < 0)
        goto error;
    if (H5Pclose(dcpl_id) < 0)
        goto error;

    return vdset_id;
error:
    /* Cleanup */
    H5E_BEGIN_TRY {
        H5Sclose(vspace_id);
        H5Sclose(srcspace_id);
        H5Dclose(srcdset_id);
        H5Fclose(srcfile_id);
        H5Dclose(vdset_id);
        H5Pclose(dcpl_id);
    } H5E_END_TRY;

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_rtree_dapl
 *
 * Purpose:     Test R-tree options on the DCPL
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_rtree_dapl(bool use_tree)
{
    hid_t  file_id = H5I_INVALID_HID;
    hid_t  dapl_id = H5I_INVALID_HID;
    hid_t  vdset_id = H5I_INVALID_HID;

    int rbuf[RTREE_DAPL_DATASET_DIM1][RTREE_DAPL_DATASET_DIM2];
    const char *test_str = NULL;

    /* Internal values for introspection */
    H5D_t *dset = NULL;
    H5O_storage_virtual_t *storage = NULL;

    /* Inverse of use_tree for re-open part of test */
    bool use_tree_inverse = !use_tree;

    if (use_tree) {
        test_str = "spatial tree option enabled";
    } else {
        test_str = "spatial tree option disabled";
    }

    TESTING(test_str);

    /* Create file */
    if ((file_id = H5Fcreate(RTREE_DAPL_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create DAPL */
    if ((dapl_id = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Set the spatial tree property */
    if (H5Pset_dset_use_spatial_tree(dapl_id, use_tree) < 0)
        FAIL_STACK_ERROR;

    /* Create virtual dataset with some mappings */
    if ((vdset_id = create_virtual_dataset(file_id, dapl_id)) < 0)
        FAIL_STACK_ERROR;

    /* Read the entire virtual dataset to force tree initialization */
    if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify read data matches expected pattern from create_virtual_dataset */
    for (int i = 0; i < RTREE_DAPL_DATASET_DIM1; i++) {
        for (int j = 0; j < RTREE_DAPL_DATASET_DIM2; j++) {
            int expected = i * RTREE_DAPL_DATASET_DIM2 + j;
            if (rbuf[i][j] != expected) {
                printf("Data mismatch at [%d][%d]: expected %d, got %d\n", i, j, expected, rbuf[i][j]);
                FAIL_STACK_ERROR;
            }
        }
    }

    /* Get the dataset object - this is using internal API for testing */
    if (NULL == (dset = (H5D_t *)H5VL_object(vdset_id)))
        FAIL_STACK_ERROR;

    if (dset->shared->layout.type != H5D_VIRTUAL)
        FAIL_STACK_ERROR;

    /* Get the virtual storage structure */
    storage = &(dset->shared->layout.storage.u.virt);

    /* Verify tree existence matches expectation */
    if (use_tree) {
        if (storage->tree == NULL) {
            puts("Expected spatial tree to exist but it was NULL");
            FAIL_STACK_ERROR;
        }
        if (storage->is_in_tree == NULL) {
            puts("Expected is_in_tree array to exist but it was NULL");
            FAIL_STACK_ERROR;
        }
    } else {
        if (storage->tree != NULL) {
            puts("Expected spatial tree to be NULL but it exists");
            FAIL_STACK_ERROR;
        }
        if (storage->is_in_tree != NULL) {
            puts("Expected is_in_tree array to be NULL but it exists");
            FAIL_STACK_ERROR;
        }
    }

    /* Close the dataset and re-open it with the opposite value set in DAPL */
    if (H5Dclose(vdset_id) < 0)
        FAIL_STACK_ERROR;
    vdset_id = H5I_INVALID_HID;

    if (H5Pset_dset_use_spatial_tree(dapl_id, use_tree_inverse) < 0)
        FAIL_STACK_ERROR;

    if ((vdset_id = H5Dopen2(file_id, RTREE_DAPL_VDS_NAME, dapl_id)) < 0)
        FAIL_STACK_ERROR;

    /* Read the entire virtual dataset to force tree initialization */
    if (H5Dread(vdset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify read data matches expected pattern from create_virtual_dataset */
    for (int i = 0; i < RTREE_DAPL_DATASET_DIM1; i++) {
        for (int j = 0; j < RTREE_DAPL_DATASET_DIM2; j++) {
            int expected = i * RTREE_DAPL_DATASET_DIM2 + j;
            if (rbuf[i][j] != expected) {
                printf("Data mismatch after re-open at [%d][%d]: expected %d, got %d\n", i, j, expected, rbuf[i][j]);
                FAIL_STACK_ERROR;
            }
        }
    }

    /* Get the dataset object - this is using internal API for testing */
    if (NULL == (dset = (H5D_t *)H5VL_object(vdset_id)))
        FAIL_STACK_ERROR;

    if (dset->shared->layout.type != H5D_VIRTUAL)
        FAIL_STACK_ERROR;


    storage = &(dset->shared->layout.storage.u.virt);

    /* Verify tree existence matches expectation */
    if (use_tree_inverse) {
        if (storage->tree == NULL) {
            puts("Expected spatial tree to exist but it was NULL");
            FAIL_STACK_ERROR;
        }
        if (storage->is_in_tree == NULL) {
            puts("Expected is_in_tree array to exist but it was NULL");
            FAIL_STACK_ERROR;
        }
    } else {
        if (storage->tree != NULL) {
            puts("Expected spatial tree to be NULL but it exists");
            FAIL_STACK_ERROR;
        }
        if (storage->is_in_tree != NULL) {
            puts("Expected is_in_tree array to be NULL but it exists");
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

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(vdset_id);
        H5Pclose(dapl_id);
        H5Fclose(file_id);
    } H5E_END_TRY;

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
    int nerrors = 0;

    printf("Testing R-tree spatial indexing...\n");

    H5open();

    /* Run core R-tree tests */
    nerrors += test_rtree_create() < 0 ? 1 : 0;
    nerrors += test_rtree_search() < 0 ? 1 : 0;
    nerrors += test_rtree_copy() < 0 ? 1 : 0;

    /* Test spatial tree with DCPL property enabled */
    nerrors += test_rtree_dapl(true) < 0 ? 1 : 0;
    nerrors += test_rtree_dapl(false) < 0 ? 1 : 0;

    if (nerrors)
        goto error;

    printf("All R-tree tests passed.\n");
    return EXIT_SUCCESS;

error:
    printf("***** R-TREE TESTS FAILED *****\n");
    return EXIT_FAILURE;
}
